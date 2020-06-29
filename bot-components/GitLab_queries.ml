open Cohttp_lwt_unix
open Lwt
open Utils

let get_build_trace ~project_id ~build_id ~token =
  let uri =
    "https://gitlab.com/api/v4/projects/" ^ Int.to_string project_id ^ "/jobs/"
    ^ Int.to_string build_id ^ "/trace"
    |> Uri.of_string
  in
  let gitlab_header = [("Private-Token", token)] in
  let headers = headers gitlab_header in
  Client.get ~headers uri
  >>= fun (_response, body) -> Cohttp_lwt.Body.to_string body
