open Lwt.Infix
open Utils

let send_graphql_query ~bot_info query =
  let uri = Uri.of_string "https://api.github.com/graphql" in
  let headers =
    Cohttp.Header.of_list
      [ ("Authorization", "bearer " ^ bot_info.github_token)
      ; ("User-Agent", bot_info.name) ]
  in
  let body =
    `Assoc [("query", `String query#query); ("variables", query#variables)]
  in
  let serialized_body = Yojson.Basic.to_string body in
  Cohttp_lwt_unix.Client.post ~headers ~body:(`String serialized_body) uri
  >>= fun (rsp, body) ->
  Cohttp_lwt.Body.to_string body
  >|= fun body ->
  match Cohttp.Code.(code_of_status rsp.status |> is_success) with
  | false ->
      Error body
  | true -> (
    try
      Ok
        ( Yojson.Basic.from_string body
        |> Yojson.Basic.Util.member "data"
        |> query#parse )
    with
    | Yojson.Json_error err ->
        Error (f "Json error: %s" err)
    | Yojson.Basic.Util.Type_error (err, _) ->
        Error (f "Json type error: %s" err) )
