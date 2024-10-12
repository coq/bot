open Base
open Cohttp_lwt_unix
open Bot_info
open Utils

let send_graphql_query ~gitlab_domain =
  GraphQL_query.send_graphql_query ~api:(GitLab gitlab_domain)

let get_build_trace ~bot_info ~gitlab_domain ~project_id ~build_id =
  let uri =
    f "https://%s/api/v4/projects/%d/jobs/%d/trace" gitlab_domain project_id
      build_id
    |> Uri.of_string
  in
  let open Lwt_result.Infix in
  gitlab_name_and_token bot_info gitlab_domain
  |> Lwt.return
  >>= fun (name, token) ->
  let gitlab_header = [("Private-Token", token)] in
  let headers = Utils.headers gitlab_header name in
  let open Lwt.Infix in
  Client.get ~headers uri
  >>= fun (_response, body) ->
  Cohttp_lwt.Body.to_string body |> Lwt.map Result.return

let get_retry_nb ~bot_info ~gitlab_domain ~full_name ~build_id ~build_name =
  let open GitLab_GraphQL.GetRetriedJobs in
  let open Lwt.Infix in
  makeVariables ~fullPath:full_name
    ~jobId:
      (build_id |> f {|"gid://gitlab/Ci::Build/%d"|} |> Yojson.Basic.from_string)
    ()
  |> serializeVariables |> variablesToJson
  |> send_graphql_query ~bot_info ~gitlab_domain ~query
       ~parse:(Fn.compose parse unsafe_fromJson)
  >|= function
  | Ok {project= Some {job= Some {pipeline= Some {jobs= Some {count= 0}}}}} ->
      Ok 0
  | Ok
      { project=
          Some
            {job= Some {pipeline= Some {jobs= Some {count; nodes= Some jobs}}}}
      } ->
      if count > Array.length jobs then Error "Too many retried jobs"
      else
        Ok
          (Array.count jobs ~f:(function
            | Some {name= Some name} ->
                String.equal build_name name
            | None | Some {name= None} ->
                false))
  | Ok {project= Some {job= Some {pipeline= Some {jobs= Some {nodes= None}}}}}
    ->
      Error "There are retried jobs but we failed to get them"
  | Ok {project= Some {job= Some {pipeline= Some {jobs= None}}}} ->
      Error "Could not get the number of retried jobs"
  | Ok {project= Some {job= Some {pipeline= None}}} ->
      Error "Could not retrieve the pipeline of the job"
  | Ok {project= Some {job= None}} ->
      Error "Could not retrieve the job"
  | Ok {project= None} ->
      Error "Could not retrieve the project"
  | Error err ->
      Error (f "Request to get retried jobs failed: %s" err)
