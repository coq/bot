open Base
open Bot_info
open Utils

let generic_retry ~bot_info ~gitlab_domain ~url_part =
  let uri =
    f "https://%s/api/v4/%s/retry" gitlab_domain url_part |> Uri.of_string
  in
  match gitlab_name_and_token bot_info gitlab_domain with
  | Error err ->
      Lwt_io.printlf "Error when retrying job %s: %s." url_part err
  | Ok (name, token) ->
      let gitlab_header = [("Private-Token", token)] in
      Utils.send_request ~body:Cohttp_lwt.Body.empty ~uri gitlab_header name

let retry_job ~bot_info ~gitlab_domain ~project_id ~build_id =
  generic_retry ~bot_info ~gitlab_domain
    ~url_part:
      ( "projects/" ^ Int.to_string project_id ^ "/jobs/"
      ^ Int.to_string build_id )

let play_job ~bot_info ~gitlab_domain ~project_id ~build_id
    ?(key_value_pairs = []) () =
  let uri =
    Uri.of_string
    @@ Printf.sprintf "https://%s/api/v4/projects/%d/jobs/%d/play" gitlab_domain
         project_id build_id
  in
  match gitlab_name_and_token bot_info gitlab_domain with
  | Error err ->
      Lwt_io.printlf "Error when playing job %d of project %d: %s." build_id
        project_id err
  | Ok (name, token) ->
      let gitlab_header =
        [("Private-Token", token); ("Content-Type", "application/json")]
      in
      let body =
        match key_value_pairs with
        | [] ->
            Cohttp_lwt.Body.empty
        | _ ->
            key_value_pairs
            |> List.map ~f:(fun (k, v) ->
                   f {|{ "key": "%s", "value": "%s" }|} k v )
            |> String.concat ~sep:","
            |> f {|{ "job_variables_attributes": [%s] }|}
            |> Cohttp_lwt.Body.of_string
      in
      Utils.send_request ~body ~uri gitlab_header name
