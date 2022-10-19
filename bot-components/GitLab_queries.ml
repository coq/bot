open Base
open Cohttp_lwt_unix
open Lwt
open Bot_info
open Utils

let send_graphql_query = GraphQL_query.send_graphql_query ~api:`GitLab

let get_build_trace ~bot_info ~project_id ~build_id =
  let uri =
    "https://gitlab.com/api/v4/projects/" ^ Int.to_string project_id ^ "/jobs/"
    ^ Int.to_string build_id ^ "/trace"
    |> Uri.of_string
  in
  let gitlab_header = [("Private-Token", bot_info.gitlab_token)] in
  let headers = Utils.headers gitlab_header ~bot_info in
  Client.get ~headers uri
  >>= fun (_response, body) -> Cohttp_lwt.Body.to_string body

let get_retry_nb ~bot_info ~full_name ~build_id ~build_name =
  let open GitLab_GraphQL.GetRetriedJobs in
  makeVariables ~fullPath:full_name
    ~jobId:
      (build_id |> f {|"gid://gitlab/Ci::Build/%d"|} |> Yojson.Basic.from_string)
    ()
  |> serializeVariables |> variablesToJson
  |> send_graphql_query ~bot_info ~query
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
                false ) )
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
