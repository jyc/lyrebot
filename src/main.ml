open Batteries
open Printf
open Lwt
open IRC

let warning s =
  fprintf stderr "Warning: %s\n" s

let error s =
  fprintf stderr "lyrebot: %s\n" s ;
  exit 1

let run_debug = ref false
let debug f =
  if !run_debug then f ()
  else ()

module Mail = struct
  type t = {
    time : float;
    from : string;
    text : string;
  }
end

type plivo_config = {
  auth_id : string;
  auth_token : string;
  src_number : string;
}

type sendq_item = {
  target : string;
  text : string;
}

(* IRC nicknames are case-insensitive. *)
module NickMap = Map.Make(IRC.Nick)
module NickSet = Set.Make(IRC.Nick)

module StringMap = Map.Make(String)

module State = struct
  type t = {
    registered : bool;
    mailboxes : (Mail.t list) NickMap.t;
    neighbors : NickSet.t;
  }

  let empty = {
    registered = false;
    mailboxes = NickMap.empty;
    neighbors = NickSet.empty;
  }
end

(** [Re.all] doesn't work because it will try to match after the last character
    in the previous match. The [bounds] we put on either side means that "@a
    @b" won't match, but "@a  @b" will. To fix this we do something similar wrt
    increasing the search start position, but base it on the start of the
    previous match (because we have @ to start with). *)
let rec exec_all ?(pos=0) re s =
  match Re.exec ~pos re s with
  | g ->
    let pos' = Re.Group.start g 0 + 1 in
    g :: exec_all ~pos:pos' re s
  | exception Not_found -> []

let send outp msg =
  Lwt_io.write outp (Message.to_string msg)
  >>= fun () ->
  Lwt_io.write outp "\r\n"
  >>= fun () ->
  Lwt_io.flush outp

(* RFC 2812 2.3.1 *)
let nick, userprefix_re, mention_re, sms_re =
  (* Can't use word because @ and : are not word characters (?). *)
  let bound = Re.(alt [char ' '; bos; eos]) in
  let special = Re.(alt [char '['; char ']'; char '\\'; char '`'; char '_';
                         char '^'; char '{'; char '|'; char '}']) in
  let nick = Re.(seq [alt [alpha; special];
                      repn (alt [alnum; special; char '-']) 0 (Some 8) ]) in

  nick,
  Re.(compile (seq [group nick; char '!'; group (rep1 any)])),
  Re.(compile (seq [bound; char '@'; group nick; bound])),
  Re.(compile (seq [bound; char '?'; group nick; bound]))

(* We have to do this ourselves instead of using Re.exec_all because Re doesn't
   treat the start of the substring indicated by ~pos as the start of the
   string. *)
let extract_mentions text =
  let rec loop matches rest =
    match Re.exec_opt mention_re rest with
    | Some groups ->
      let nick = Re.Group.get groups 1 in
      loop (nick :: matches) (String.tail rest (Re.Group.stop groups 1))
    | None -> matches
  in
  loop [] text

let extract_smses text =
  let rec loop matches rest =
    match Re.exec_opt sms_re rest with
    | Some groups ->
      let nick = Re.Group.get groups 1 in
      loop (nick :: matches) (String.tail rest (Re.Group.stop groups 1))
    | None -> matches
  in
  loop [] text

let names_nick_re =
  Re.(compile (seq [alt [char '@'; char '+'; epsilon]; group nick]))

let ago s =
  if s < 60. then sprintf "%.0fs ago" s
  else if s < 3600. then sprintf "%.0fm ago" (s /. 60.)
  else if s < 86400. then sprintf "%.1fh ago" (s /. 3600.)
  else sprintf "%.0fd ago" (s /. 86400.)

(** [execute_mentions message state from target text] handles the message sent
    from the user whose nick is [from] in [target] (either [channel] or [nick])
    with text [text].
    [privmsg target text] is used to send responses. *)
let execute_mentions privmsg state from text =
  let { State.mailboxes; neighbors } = state in
  let on_match state nick =
    if NickSet.mem nick neighbors then return state
    else
      privmsg from @@ sprintf
        "I'll forward your message the next time %s logs in." nick
      >>= fun () ->
      let mail = { Mail.time = Unix.time (); from; text } in
      let old =
        if NickMap.mem nick mailboxes then NickMap.find nick mailboxes
        else []
      in
      return { state with
               State.mailboxes = NickMap.add nick (mail :: old) mailboxes }
  in
  Lwt_list.fold_left_s on_match state (extract_mentions text)

(** [and_list xs] returns [xs] separated by "," and "and" following English
    conventions. *)
let and_list = function
  | [] -> ""
  | [x] -> x
  | [x; y] -> x ^ " and " ^ y
  | x :: xs ->
    let out = Buffer.create 17 in
    let add = Buffer.add_string out in
    add x ;
    let rec loop = function
      | [] -> ()
      | [y] ->
        add (", and " ^ y)
      | y :: ys ->
        add (", " ^ y) ;
        loop ys
    in
    loop xs ;
    Buffer.contents out

let execute_smses_report successes failures =
  let out = Buffer.create 17 in
  let adds = Buffer.add_string out in
  let failures' =
    List.map (fun (nick, status) ->
      nick ^ " " ^
      match status with
      | `Internal -> "(internal error)"
      | `NotFound -> "(number not found)"
    ) failures
  in
  if successes <> [] then begin
    adds "Successfully texted " ;
    adds (and_list successes) ;
    adds "."
  end ;
  if failures <> [] then begin
    if successes <> [] then adds " " ;
    adds "Failed to text to " ;
    adds (and_list failures') ;
    adds "."
  end ;
  Buffer.contents out

let execute_smses ~plivo_config ~number_map privmsg from text =
  let on_match (successes, failures) nick =
    match NickMap.find nick number_map with
    | number ->
      (* Note: Plivo’s Message API breaks long SMS text messages into multiple
         SMS and adds a concatenation header to each message so that it can be
         stitched together (i.e., concatenated) at receiver’s mobile device.
         Though, some carriers and handsets do not support long SMS
         concatenation.
         -- https://www.plivo.com/docs/getting-started/send-a-long-sms/ *)
      let text' =
        sprintf "%s (via IRC) says: %s" from text
      in
      begin match
        Plivo.send
          ~auth_id:plivo_config.auth_id ~auth_token:plivo_config.auth_token
          ~src:plivo_config.src_number ~dst:number ~text:text'
      with
      | `Ok -> (nick :: successes, failures)
      | `Error msg ->
        fprintf stderr 
          "Internal error while sending message %S from '%s' to '%s': %s"
          text from nick msg ;
        (successes, (nick, `Internal) :: failures)
      end
    | exception Not_found ->
      (successes, (nick, `NotFound) :: failures)
  in
  let successes, failures =
    List.fold_left on_match ([], []) (extract_smses text)
  in
  if successes <> [] || failures <> [] then
    privmsg from @@ execute_smses_report successes failures
  else return ()

(** [forward message state joined] forwards the messages in the mailbox in
    [state] for [joined] and returns an updated state with that mailbox removed.
    [privmsg target text] is used to send responses. *)
let forward privmsg ({ State.mailboxes } as state) joined =
  let mail =
    if NickMap.mem joined mailboxes then NickMap.find joined mailboxes
    else []
  in
  if mail = [] then return state
  else
    privmsg joined "*** You've got mail! ***"
    >>= fun () ->
    Lwt_list.iter_s (fun { Mail.time; from; text } ->
      let { Unix.tm_hour; tm_min; tm_mday; tm_mon } = Unix.gmtime time in
      let dt = Unix.time () -. time in
      privmsg joined @@ sprintf
        "%02d:%02d %d/%d (%s)  <%s> %s"
        tm_hour tm_min (tm_mon + 1) tm_mday (ago dt) from text
    ) mail
    >>= fun () ->
    return { state with State.mailboxes = NickMap.remove joined mailboxes }

(** [work (inp, outp)] never returns. It sets up the loop for responding to IRC
    messages using [inp] and [outp] channels connected to the IRC server. *)
let work ~plivo_config ~number_map ~nick ~channel ~sendq (inp, outp) =
  let send = send outp in
  let privmsg target text =
    send (Message.make "PRIVMSG" ~trailing:text [target])
  in

  (** [handle state] handles IRC-level commands like PING, end of MOTD, etc.
      It is responsible for identifying on the first PING and joining on the end of MOTD.
      (This seems to work the best based on experience.)
      Bot-level commands are handled in [execute]. *)
  let rec handle ({ State.registered; neighbors } as state) =
    (* Important to [pick] instead of [choose] so that if we do one before the
       other, we cancel the other so we can loop again. *)
    Lwt.pick [
      (Lwt_io.read_line_opt inp
       >>= fun maybe_line ->
       return @@ `Line maybe_line);
      (Lwt_mvar.take sendq
       >>= fun sendq_item ->
       return @@ `Send sendq_item);
    ]
    >>= function
    | `Send { target; text } ->
      privmsg target text
      >>= fun () ->
      handle state
    | `Line None -> return ()
    | `Line (Some line) ->
      begin match Message.of_string line with
      | `Error s ->
        warning @@ sprintf "Received malformed message: %s" s ;
        return ()

      (* Need to respond to PINGs with PONGs. *)
      | `Ok { Message.command = "PING"; trailing = Some trailing } ->
        send (Message.make "PONG" ~trailing [])
        >>= fun () ->
        (if not registered then
           send (Message.make "USER" ~trailing:nick [nick; "0"; "*"])
         else return ())
        >>= fun () ->
        handle { state with State.registered = true }

      (* End of MOTD. Should join a channel. *)
      | `Ok { Message.command = "422" | "376" } ->
        send (Message.make "JOIN" [channel])
        >>= fun () ->
        handle state

      (* Error: not registered. *)
      | `Ok { Message.command = "451"; trailing } ->
        Lwt.fail_with @@ sprintf
          "Registration error: %s" (Option.default "n/a" trailing)

      (* Error: nickname in use. *)
      | `Ok { Message.command = "433" } ->
        Lwt.fail_with "Error: nickname in use."

      (* Names reply. Update our list of users in the channel. *)
      | `Ok { Message.command = "353"; trailing = Some trailing } ->
        let nicks =
          String.nsplit trailing ~by:" "
          |> List.filter_map (fun part ->
            match Re.Group.all (Re.exec names_nick_re part) with
            | [|_; nick|] -> Some nick
            | _ -> assert false
            | exception Not_found -> None
          )
        in
        handle { state with
                 State.neighbors =
                   List.fold_left (fun neighbors nick ->
                     NickSet.add nick neighbors
                   ) neighbors nicks
               }

      (* PART
         For PART/JOIN the RFC seems to indicate that the target be included in
         the middles, but with UnrealIRCd it seems like it was included in the
         trailing. Because we only join one channel right now we'll just ignore
         it. *)
      | `Ok ({ Message.command = "PART" | "QUIT";
               prefix = Some prefix } as msg) ->
        begin match Re.Group.all (Re.exec userprefix_re prefix) with
        | [|_; nick; _|] ->
          handle { state with State.neighbors = NickSet.remove nick neighbors }
        | _ ->
          Lwt.fail_with @@ sprintf
            "Error: unrecognized PART/QUIT message: %s" (Message.show msg)
        end

      (* JOIN *)
      | `Ok ({ Message.command = "JOIN"; prefix = Some prefix } as msg) ->
        begin match Re.Group.all (Re.exec userprefix_re prefix) with
        | [|_; nick; _|] ->
          forward privmsg state nick
          >>= fun state' ->
          handle { state' with State.neighbors = NickSet.add nick neighbors }
        | _ ->
          Lwt.fail_with @@ sprintf
            "Error: unrecognized JOIN message: %s" (Message.show msg)
        end

      (* KILL nick comment *)
      | `Ok { Message.command = "KILL"; middles = (nick :: _) } ->
        handle { state with State.neighbors = NickSet.remove nick neighbors }

      (* KICK channel nick *)
      | `Ok { Message.command = "KICK"; middles = (_ :: nick :: _) } ->
        handle { state with State.neighbors = NickSet.remove nick neighbors }

      (* NICK nickname hopcount *)
      | `Ok ({ Message.command = "NICK"; prefix = Some prefix;
               trailing = Some new_nick } as msg) ->
        begin match Re.Group.all (Re.exec userprefix_re prefix) with
        | [|_; old_nick; _|] ->
          handle { state with State.neighbors =
                                NickSet.update old_nick new_nick neighbors
                 }
        | _ ->
          Lwt.fail_with @@ sprintf
            "Error: unrecognized JOIN message: %s" (Message.show msg)
        end

      (* PRIVMSG *)
      | `Ok ({ Message.command = "PRIVMSG"; prefix = Some prefix;
               middles = [target]; trailing = Some text } as msg) ->
        begin match Re.Group.all (Re.exec userprefix_re prefix) with
        | [|_; nick; _|] when nick <> "" ->
          return nick
        | _ ->
          Lwt.fail_with @@ sprintf
            "Error: Unrecognized user prefix in PRIVMSG message: %s"
            (Message.show msg)
        end
        >>= fun from ->
        debug (fun () ->
          printf "[%s] %s: %s\n%!" target from text
        ) ;
        execute_mentions privmsg state from text
        >>= fun state' ->
        execute_smses ~plivo_config ~number_map privmsg from text
        >>= fun () ->
        handle state'

      | `Ok msg ->
        debug (fun () ->
          print_endline "Unrecognized message:" ;
          print_endline (Message.show msg)
        ) ;
        handle state
      end
  in
  send (Message.make "NICK" [nick])
  >>= fun () ->
  handle State.empty

(** [start] starts the connection and calls [work]. *)
let start ~plivo_config ~number_map ~sendq ~host ~service ~nick ~channel =
  Lwt_unix.getaddrinfo host service Unix.[AI_SOCKTYPE SOCK_STREAM]
  >>= fun ais ->
  match ais with
  | [] ->
    error @@ sprintf
      "getaddrinfo: Failed to determine socket parameters to connect to %s:%s."
      host service ;
  | { Lwt_unix.ai_addr } :: _ ->
    Lwt_io.with_connection ai_addr (fun (inp, outp) ->
      let shutdown _ =
        print_endline "Shutting down..." ;
        (* Had an interesting problem where closing inp/outp from inside here
           caused Lazy.Undefined. I think it's because it was somehow being run
           from inside the same lazy-expression that was forced when the server
           closed its connection with us, so it was like a recursive
           Lazy.force. *)
        Lwt.async (fun () ->
          send outp (Message.make "QUIT" ~trailing:"Received SIGINT." [])
        )
      in
      Sys.(set_signal sigint (Signal_handle shutdown)) ;

      work ~plivo_config ~number_map ~channel ~sendq ~nick (inp, outp)
    )

let handle_plivo
    { auth_id; auth_token; src_number } nick_map sendq channel req =
  try
    let msg_to = Scgi.Request.param_exn ~meth:`POST req "To" in
    let msg_from = Scgi.Request.param_exn ~meth:`POST req "From" in
    let msg_text = Scgi.Request.param_exn ~meth:`POST req "Text" in

    assert (msg_to = src_number) ;

    begin match
      Plivo.send
        ~auth_id ~auth_token:auth_token ~src:src_number ~dst:"19255770306"
        ~text:"✔️"
    with
    | `Ok -> ()
    | `Error s -> prerr_endline s
    end ;

    let from =
      match StringMap.find msg_from nick_map with
      | nick -> nick ^ "#" ^ msg_from
      | exception Not_found -> msg_from
    in

    let text =
      sprintf "%s (via SMS) says: %s" from msg_text
    in

    Lwt_mvar.put sendq { target = channel; text }
    >>= fun () ->
    return { Scgi.Response.
             status = `Ok;
             headers = [];
             body = `String "ok" }
  with
  | Not_found ->
    return { Scgi.Response.
             status = `Bad_request;
             headers = [];
             body = `String "bad" }

let serve_plivo_endpoint ~config ~nick_map ~sendq ~channel ~port =
  let rec loop () =
    let callback req =
      Lwt.catch
        (fun () -> handle_plivo config nick_map sendq channel req)
        (fun e ->
           fprintf stderr "Handler error: %s\n%!" (Printexc.to_string e) ;
           Printexc.print_backtrace stderr ;
           Lwt.fail e
        )
    in
    try Scgi.Server.handler_inet "lyrebot" "127.0.0.1" port callback
    with
    | Unix.Unix_error (Unix.EADDRINUSE, _, _) ->
      fprintf stderr "lyrebot: %d is already in use. Shutting down...\n%!" port ;
      exit 1
    | Unix.Unix_error (Unix.EACCES, "bind", _) ->
      fprintf stderr "lyrebot: You don't have access to port %d. Shutting down...\n%!" port ;
      exit 1
    | Unix.Unix_error _ as e ->
      fprintf stderr "lyrebot: Server error: %s\n%!" (Printexc.to_string e) ;
      Printexc.print_backtrace stderr ;
      loop ()
  in 
  loop ()

let parse_number_map path =
  let unexpected_format () =
    prerr_endline {|lyrebot: Expected {"name": "number", ...}.|} ;
    exit 1
  in
  try
    match Yojson.Basic.from_file path with
    | `Assoc entries ->
      List.fold_left (fun (numbers, nicks) -> function
        | (nick, `String number) ->
          NickMap.add nick number numbers,
          StringMap.add number nick nicks
        | _ -> unexpected_format ()
      ) (NickMap.empty, StringMap.empty) entries
    | _ -> unexpected_format ()
  with e -> 
    fprintf stderr "lyrebot: Failed to parse number map at '%s': %s."
      path (Printexc.to_string e) ;
    Printexc.print_backtrace stderr ;
    exit 1

let () =
  let host = ref "" in
  let service = ref "" in
  let nick = ref "" in
  let channel = ref "" in
  let plivo_auth_id = ref "" in
  let plivo_auth_token = ref "" in
  let plivo_src_number = ref "" in
  let plivo_port = ref 8081 in
  let number_map = ref "" in

  let usage =
    "Usage: lyrebot [options]\n\
     Lyrebot IRC Bot\n\
     Copyright 2016 Jonathan Y. Chan <jyc@fastmail.fm>\n\
     All rights reserved."
  in
  let rec show_usage () =
    Arg.usage (Arg.align specs) usage ;
    exit 1
  and specs = Arg.[
    ("--help", Unit show_usage, "");
    ("-help", Unit show_usage,
     " Print a synopsis of options.");

    ("-host", Set_string host,
     " The host to connect to.");
    ("-service", Set_string service,
     " The service or port to connect to.");
    ("-nick", Set_string nick,
     " The nick to use. Also used for realname, user, etc.");
    ("-channel", Set_string channel,
     " The channel to connect to.");

    ("-plivo-auth-id", Set_string plivo_auth_id,
     " The Plivo authorization ID.");
    ("-plivo-auth-token", Set_string plivo_auth_token,
     " The Plivo authorization token.");
    ("-plivo-src-number", Set_string plivo_src_number,
     " The Plivo source number.");
    ("-plivo-port", Set_int plivo_port,
     " The port to serve the Plivo endpoint SCGI server on.");

    ("-number-map", Set_string number_map,
     " The path to a JSON file mapping from IRC nicks to phone numbers.");

    ("-debug", Set run_debug, "");
  ] in
  let specs = Arg.align specs in

  let anon_fun _ =
    show_usage ()
  in

  Arg.parse specs anon_fun usage ;

  let reqs s =
    if !s = "" then show_usage ()
  in

  reqs host ; reqs service ; reqs nick ; reqs channel ; reqs plivo_auth_id ;
  reqs plivo_auth_token ; reqs plivo_src_number ; reqs number_map ;

  let number_map, nick_map = parse_number_map !number_map in

  let plivo_config =
    { auth_id = !plivo_auth_id;
      auth_token = !plivo_auth_token;
      src_number = !plivo_src_number;
    }
  in
  let sendq = Lwt_mvar.create_empty () in
  let server =
    serve_plivo_endpoint
      ~config:plivo_config ~nick_map ~sendq ~channel:!channel
      ~port:!plivo_port
  in
  let shutdown_waiter, shutdown_wakener = Lwt.wait () in

  let shutdown () =
    Lwt_io.shutdown_server server ;
    Lwt.wakeup shutdown_wakener () ;
    Curl.global_cleanup ()
  in
  let shutdown_signaled _ =
    print_endline "Received signal, shutting down..." ;
    shutdown ()
  in
  Sys.(set_signal sigint (Signal_handle shutdown_signaled)) ;
  Sys.(set_signal sigterm (Signal_handle shutdown_signaled)) ;

  Curl.global_init Curl.CURLINIT_GLOBALALL ;

  Lwt_main.run @@
  Lwt.join [
    (start
      ~plivo_config ~number_map ~sendq ~host:!host ~service:!service
      ~nick:!nick ~channel:!channel
     >>= fun () ->
     print_endline "IRC handling loop stopped, shutting down..." ;
     shutdown () ;
     return ()
    );
    shutdown_waiter
  ]
