open Batteries
open Printf
open Lwt

open IRC
let warning s =
  fprintf stderr "Warning: %s\n" s

let error s =
  fprintf stderr "lyrebird: %s\n" s ;
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

module StringOrd = struct
end

(* Thanks Batteries! *)
module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

module State = struct
  type t = {
    registered : bool;
    mailboxes : (Mail.t list) StringMap.t;
    neighbors : StringSet.t;
  }

  let empty = { 
    registered = false;
    mailboxes = StringMap.empty;
    neighbors = StringSet.empty;
  }
end

let send outp msg =
  Lwt_io.write outp (Message.to_string msg)
  >>= fun () ->
  Lwt_io.write outp "\r\n"
  >>= fun () ->
  Lwt_io.flush outp

(* RFC 2812 2.3.1 *)
let nick, userprefix_re, mention_re = 
  (* Can't use word because @ and : are not word characters (?). *)
  let bound = Re.(alt [char ' '; bos; eos]) in
  let special = Re.(alt [char '['; char ']'; char '\\'; char '`'; char '_'; char '^'; char '{'; char '|'; char '}']) in
  let nick = Re.(seq [alt [alpha; special]; repn (alt [alnum; special; char '-']) 0 (Some 8) ]) in

  nick,
  Re.(compile (seq [group nick; char '!'; group (rep1 any)])),
  Re.(compile (seq [bound; seq [char '@'; group nick]; bound]))

let names_nick_re =
  Re.(compile (seq [alt [char '@'; char '+'; epsilon]; group nick]))

let ago s =
  if s < 60. then sprintf "%.0fs ago" s
  else if s < 3600. then sprintf "%.0fm ago" (s /. 60.)
  else if s < 86400. then sprintf "%.1fh ago" (s /. 3600.)
  else sprintf "%.0fd ago" (s /. 86400.)

(** [work (inp, outp)] never returns. It sets up the loop for responding to IRC
    messages using [inp] and [outp] channels connected to the IRC server. *)
let work ~nick ~channel (inp, outp) =
  let send = send outp in
  let message other ?(target=other) text =
    let text' =
      if target = other then text
      else sprintf "%s: %s" other text
    in
    send (Message.make "PRIVMSG" ~trailing:text' [target])
  in
  (*
  let reply other target text =
    (* If this is a message in the channel, [target] is the channel and we want
       to prefix the message with the nick of the user we're replying to (other).
       If this is a private message, [target] is [nick] and we don't want a prefix,
       but we want [target] to be [other] (otherwise we message ourselves.) *)
    let text', target' =
      if target = nick then text, other
      else sprintf "%s: %s" other text, target
    in
    send (Message.make "PRIVMSG" ~trailing:text' [target'])
  in
  *)

  (** [execute state msg from target text] handles the message [msg] sent from the
        user whose nick is [from] in [target] (either [channel] or [nick]) with
        text [text]. *)
  let rec execute ({ State.mailboxes; neighbors } as state) msg from target text =
    let on_match state g =
      (** [record dest text] tries to record the message [text] for the user
          with nick [dest]. *)
      let record dest text =
        if StringSet.mem dest neighbors then return state
        else 
          message from @@ sprintf "I'll forward your message the next time %s logs in." dest
          >>= fun () ->
          let mail = { Mail.time = Unix.time (); from; text } in
          let old =
            if StringMap.mem dest mailboxes then StringMap.find dest mailboxes
            else []
          in
          return { state with 
                   State.mailboxes = StringMap.add dest (mail :: old) mailboxes }
      in
      match Re.Group.all g with
      | [|_; dest; ""|] ->
        record dest text
      | [|_; ""; dest|] ->
        record dest text
      | _ -> return state
      | exception Not_found -> return state
    in
    Lwt_list.fold_left_s on_match state (Re.all mention_re text)

  and forward ({ State.mailboxes } as state) joined =
    let mail = 
      if StringMap.mem joined mailboxes then StringMap.find joined mailboxes
      else []
    in
    if mail = [] then return state
    else 
      message joined "*** You've got mail! ***"
      >>= fun () ->
      Lwt_list.iter_s (fun { Mail.time; from; text } ->
        let { Unix.tm_hour; tm_min; tm_mday; tm_mon } = Unix.gmtime time in
        let dt = Unix.time () -. time in
        message joined @@ sprintf "%02d:%02d %d/%d (%s)  <%s> %s" tm_hour tm_min (tm_mon + 1) tm_mday (ago dt) from text
      ) mail
      >>= fun () ->
      return { state with State.mailboxes = StringMap.remove joined mailboxes }

  (** [handle state] handles IRC-level commands like PING, end of MOTD, etc.
      It is responsibel for identifying on the first PING and joining on the end of MOTD.
      (This seems to work the best based on experience.)
      Bot-level commands are handled in [execute]. *)
  and handle ({ State.registered; neighbors } as state) =

    Lwt_io.read_line_opt inp 
    >>= function
    | None -> return ()
    | Some line ->
      begin match Message.of_string line with
      | `Error s ->
        warning @@ sprintf "Received malformed message: %s" s ;
        return ()

      (* Need to respond to PINGs with PONGs. *)
      | `Ok { Message.command = "PING"; trailing = Some trailing } ->
        send (Message.make "PONG" ~trailing [])
        >>= fun () ->
        (if not registered then send (Message.make "USER" ~trailing:nick [nick; "0"; "*"])
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
        Lwt.fail_with @@ sprintf "Registration error: %s" (Option.default "n/a" trailing)

      (* Error: nickname in use. *)
      | `Ok { Message.command = "433"; trailing } ->
        Lwt.fail_with "Error: nickname in use." 

      (* Names reply. Update our list of users in the channel. *)
      | `Ok { Message.command = "353"; trailing = Some trailing } ->
        let nicks =
          String.nsplit trailing ~by:" "
          |> List.filter_map (fun part ->
            match Re.Group.all (Re.exec names_nick_re part) with
            | [|_; nick|] -> Some nick
            | _ -> None
            | exception Not_found -> None
          )
        in
        handle { state with
                 State.neighbors = 
                   List.fold_left (fun neighbors nick ->
                     StringSet.add nick neighbors
                   ) neighbors nicks }

      (* PART
         For PART/JOIN the RFC seems to indicate that the target be included in the middles,
         but with UnrealIRCd it seems like it was included in the trailing.
         Because we only join one channel right now we'll just ignore it. *)
      | `Ok ({ Message.command = "PART"; prefix = Some prefix } as msg) ->
        begin match Re.Group.all (Re.exec userprefix_re prefix) with
        | [|_; nick; _|] when nick <> "" ->
          handle { state with State.neighbors = StringSet.remove nick neighbors }
        | _ -> 
          Lwt.fail_with @@ sprintf "Error: unrecognized PART message: %s" (Message.show msg)
        end

      (* JOIN *)
      | `Ok ({ Message.command = "JOIN"; prefix = Some prefix } as msg) ->
        begin match Re.Group.all (Re.exec userprefix_re prefix) with
        | [|_; nick; _|] when nick <> "" ->
          forward state nick
          >>= fun state' ->
          handle { state' with State.neighbors = StringSet.add nick neighbors }
        | _ -> 
          Lwt.fail_with @@ sprintf "Error: unrecognized JOIN message: %s" (Message.show msg)
        end

      (* QUIT *)
      | `Ok ({ Message.command = "QUIT"; prefix = Some prefix } as msg) ->
        begin match Re.Group.all (Re.exec userprefix_re prefix) with
        | [|_; nick; _|] when nick <> "" ->
          handle { state with State.neighbors = StringSet.remove nick neighbors }
        | _ -> 
          Lwt.fail_with @@ sprintf "Error: unrecognized QUIT message: %s" (Message.show msg)
        end

      (* PRIVMSG *)
      | `Ok ({ Message.command = "PRIVMSG"; prefix = Some prefix; middles=[target]; trailing=Some text } as msg) ->
        begin match Re.Group.all (Re.exec userprefix_re prefix) with
        | [|_; nick; _|] when nick <> "" -> 
          return nick
        | _ -> 
          Lwt.fail_with @@ sprintf "Error: Unrecognized user prefix in PRIVMSG message: %s" (Message.show msg)
        end
        >>= fun from ->
        debug (fun () ->
          printf "[%s] %s: %s\n%!" target from text
        ) ;
        execute state msg from target text 
        >>= fun state' ->
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
let start ~host ~service ~nick ~channel =
  Lwt_unix.getaddrinfo host service Unix.[AI_SOCKTYPE SOCK_STREAM]
  >>= fun ais ->
  match ais with
  | [] -> 
    error @@ sprintf "getaddrinfo: Failed to determine socket parameters to connect to %s:%s." host service ;
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

      work ~nick ~channel (inp, outp)
    )

let () =
  let host = ref "" in
  let service = ref "" in
  let nick = ref "" in
  let channel = ref "" in

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

  (* -host, -service, -nick, -channel are required. *)
  reqs host ; reqs service ; reqs nick ; reqs channel ;

  Lwt_main.run (start ~host:!host ~service:!service ~nick:!nick ~channel:!channel)