let send ~auth_id ~auth_token ~src ~dst ~text =
  let request =
    `Assoc [
      "src", `String src;
      "dst", `String dst;
      "text", `String text
    ]
  in
  let result = Buffer.create 17 in
  let error_buffer = ref "" in
  let conn = Curl.init () in
  let result =
    try
      Curl.set_errorbuffer conn error_buffer ;
      Curl.set_writefunction conn (fun data ->
        Buffer.add_string result data ;
        String.length data
      ) ;
      Curl.set_followlocation conn true ;
      Curl.set_httpheader conn ["Content-Type: application/json"] ;
      Curl.set_userpwd conn (auth_id ^ ":" ^ auth_token) ;
      Curl.set_postfields conn (Yojson.Basic.to_string request) ;
      Curl.set_post conn true ;
      Curl.set_url conn @@ "https://api.plivo.com/v1/Account/" ^ auth_id ^ "/Message/" ;
      Curl.perform conn ;

      let code = Curl.get_responsecode conn in
      if code <> 202 then
        `Error ("Non-202 response: " ^ Buffer.contents result)
      else
        `Ok
    with
    | Curl.CurlException (_reason, _code, _str) ->
      `Error ("Curl exception: " ^ !error_buffer) ;
  in
  Curl.cleanup conn ;
  result
