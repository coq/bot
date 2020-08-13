open Base
open Bot_info
open Cohttp
open Cohttp_lwt_unix
open Lwt

let f = Printf.sprintf

let string_match ~regexp string =
  try
    let _ = Str.search_forward (Str.regexp regexp) string 0 in
    true
  with Stdlib.Not_found -> false

let headers ~(bot_info : bot_info) header_list =
  Header.init ()
  |> (fun headers -> Header.add_list headers header_list)
  |> fun headers -> Header.add headers "User-Agent" bot_info.name

let print_response (resp, body) =
  let code = resp |> Response.status |> Code.code_of_status in
  Lwt_io.printf "Response code: %d.\n" code
  >>= fun () ->
  if code < 200 && code > 299 then
    resp |> Response.headers |> Header.to_string
    |> Lwt_io.printf "Headers: %s\n"
    >>= fun () ->
    body |> Cohttp_lwt.Body.to_string >>= Lwt_io.printf "Body:\n%s\n"
  else Lwt.return ()

let send_request ~bot_info ~body ~uri header_list =
  let headers = headers header_list ~bot_info in
  Client.post ~body ~headers uri >>= print_response

let handle_json action default body =
  try
    let json = Yojson.Basic.from_string body in
    (* print_endline "JSON decoded."; *)
    action json
  with
  | Yojson.Json_error err ->
      Stdio.printf "Json error: %s\n" err ;
      default
  | Yojson.Basic.Util.Type_error (err, _) ->
      Stdio.printf "Json type error: %s\n" err ;
      default

(* GitHub specific *)

let project_api_preview_header =
  [("Accept", "application/vnd.github.inertia-preview+json")]

let generic_get ~bot_info relative_uri ?(header_list = []) ~default json_handler
    =
  let uri = "https://api.github.com/" ^ relative_uri |> Uri.of_string in
  let github_header = [("Authorization", "bearer " ^ bot_info.github_token)] in
  let headers = headers (header_list @ github_header) ~bot_info in
  Client.get ~headers uri
  >>= (fun (_response, body) -> Cohttp_lwt.Body.to_string body)
  >|= handle_json json_handler default
