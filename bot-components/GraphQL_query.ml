open Base
open Bot_info
open Lwt.Infix
open Utils

let send_graphql_query ~bot_info ?(extra_headers = []) query =
  let uri = Uri.of_string "https://api.github.com/graphql" in
  let headers =
    Cohttp.Header.of_list
      ( [ ("Authorization", "bearer " ^ github_token bot_info)
        ; ("User-Agent", bot_info.name) ]
      @ extra_headers )
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
      let json = Yojson.Basic.from_string body in
      let open Yojson.Basic.Util in
      let data = json |> member "data" |> query#parse in
      match member "errors" json with
      | `Null ->
          Ok data
      | errors ->
          let errors =
            to_list errors
            |> List.map ~f:(fun error -> error |> member "message" |> to_string)
          in
          Error
            ( "Server responded to GraphQL request with errors: "
            ^ String.concat ~sep:", " errors )
    with
    | Failure err ->
        Error (f "Exception: %s" err)
    | Yojson.Json_error err ->
        Error (f "Json error: %s" err)
    | Yojson.Basic.Util.Type_error (err, _) ->
        Error (f "Json type error: %s" err) )
