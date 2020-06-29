open Utils

let retry_job ~project_id ~build_id ~token =
  let uri =
    "https://gitlab.com/api/v4/projects/" ^ Int.to_string project_id ^ "/jobs/"
    ^ Int.to_string build_id ^ "/retry"
    |> (fun url ->
         Stdio.printf "URL: %s\n" url ;
         url)
    |> Uri.of_string
  in
  let gitlab_header = [("Private-Token", token)] in
  send_request ~body:Cohttp_lwt.Body.empty ~uri gitlab_header
