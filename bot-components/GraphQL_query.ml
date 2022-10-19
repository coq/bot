open Base
open Bot_info
open Lwt.Infix
open Utils

let send_graphql_query ~bot_info ?(extra_headers = []) ~api ~query ~parse
    variables =
  let uri =
    ( match api with
    | `GitLab ->
        "https://gitlab.com/api/graphql"
    | `GitHub ->
        "https://api.github.com/graphql" )
    |> Uri.of_string
  in
  let headers =
    Cohttp.Header.of_list
      ( [ ( "Authorization"
          , "Bearer "
            ^
            match api with
            | `GitLab ->
                bot_info.gitlab_token
            | `GitHub ->
                github_token bot_info )
        ; ("User-Agent", bot_info.name)
        ; ("Content-Type", "application/json") ]
      @ extra_headers )
  in
  let request_json =
    `Assoc [("query", `String query); ("variables", variables)]
  in
  let request = Yojson.Basic.to_string request_json in
  Cohttp_lwt_unix.Client.post ~headers ~body:(`String request) uri
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
      let data = json |> member "data" |> parse in
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
        Error
          (f "Json error: %s. Body was:\n%s\nRequest was:\n%s" err body request)
    | Yojson.Basic.Util.Type_error (err, _) ->
        Error
          (f "Json type error: %s. Body was:\n%s. Request was:\n%s" err body
             request ) )
