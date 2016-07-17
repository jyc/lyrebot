open Batteries

let crlf = "\r\n"

module Message = struct
  type t = {
    prefix : string option;
    command : string;
    middles : string list;
    trailing : string option;
  }
  [@@deriving show]

  let make ?prefix ?trailing command middles =
    { prefix; command; middles; trailing }

  let prefix = Re.(seq [char ':'; group (rep1 (compl [char ' ']))])
  let command = Re.(alt [rep1 alpha; repn digit 3 (Some 3)])
  let middle = Re.(seq [compl [char ':'; space]; rep (compl [space])])
  let message_re = Re.(compile (seq [bos;
                                     opt (seq [prefix; space]); 
                                     group command; 
                                     group (rep (seq [space; middle]));
                                     opt (seq [space; char ':'; group (rep any)]);
                                     eos]))

  let space_re = Re.(compile space)

  (* See RFC 2812, 2.3.1 *)
  let of_string s =
    match Re.Group.all (Re.exec message_re s) with
    | [|_; prefix; command; middles'; trailing|] ->
      let middles = Re.split space_re middles' in
      `Ok { prefix = if prefix = "" then None else Some prefix;
            command; middles;
            trailing = if trailing = "" then None else Some trailing }
    | exception Not_found -> `Error "[s] is not a recognized IRC message string."
    | _ -> `Error "[s] is not a recognized IRC message string."

  let header_string { prefix; command; middles; trailing } =
    let out = Buffer.create 17 in
    let addc = Buffer.add_char out in
    let adds = Buffer.add_string out in
    begin match prefix with
    | Some s ->
      addc ':' ;
      adds s ;
      addc ' '
    | None -> ()
    end ;
    adds command ;
    List.iter (fun s ->
      addc ' ' ;
      adds s
    ) middles ;
    begin match trailing with
    | Some _ ->
      adds " :" ;
    | None -> ()
    end ;
    Buffer.contents out

  let to_string message =
    let { trailing } = message in
    let out = Buffer.create 17 in
    Buffer.add_string out (header_string message) ;
    begin match trailing with
    | None -> ()
    | Some s -> 
      Buffer.add_string out s
    end ;
    Buffer.contents out

  let rec split_trailing ?(len=512) message = 
    let header_len = String.length (header_string message) in
    assert (header_len < len) ;
    match message.trailing with
    | None -> [message]
    | Some trailing ->
      let trailing_len = String.length trailing in
    if header_len + trailing_len <= len then [message]
    else
      let hd = { message with
                 trailing = Some (String.head trailing (512 - header_len)) }
      in
      let tl = { message with
                 trailing = Some (String.tail trailing (512 - header_len)) }
      in
      hd :: split_trailing ~len tl

end

module Nick = struct
  type t = string
  let compare a b =
    let a' = String.lowercase_ascii a in
    let b' = String.lowercase_ascii b in
    if a' < b' then -1
    else if a' = b' then 0
    else 1
end
