open Base
open Bot_info
open Utils

let generic_retry ~bot_info ~url_part =
  let uri =
    "https://gitlab.com/api/v4/" ^ url_part ^ "/retry" |> Uri.of_string
  in
  let gitlab_header = [("Private-Token", bot_info.gitlab_token)] in
  Utils.send_request ~body:Cohttp_lwt.Body.empty ~uri gitlab_header ~bot_info

let retry_job ~bot_info ~project_id ~build_id =
  generic_retry ~bot_info
    ~url_part:
      ( "projects/" ^ Int.to_string project_id ^ "/jobs/"
      ^ Int.to_string build_id )

let play_job ~bot_info ~project_id ~build_id ?(key_value_pairs = []) () =
  let uri =
    Uri.of_string
    @@ Printf.sprintf "https://gitlab.com/api/v4/projects/%d/jobs/%d/play"
         project_id build_id
  in
  let gitlab_header =
    [ ("Private-Token", bot_info.gitlab_token)
    ; ("Content-Type", "application/json") ]
  in
  let body =
    match key_value_pairs with
    | [] ->
        Cohttp_lwt.Body.empty
    | _ ->
        key_value_pairs
        |> List.map ~f:(fun (k, v) -> f {|{ "key": "%s", "value": "%s" }|} k v)
        |> String.concat ~sep:","
        |> f {|{ "job_variables_attributes": [%s] }|}
        |> Cohttp_lwt.Body.of_string
  in
  Utils.send_request ~body ~uri gitlab_header ~bot_info
