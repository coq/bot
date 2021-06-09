open Base
open Cohttp
open GitLab_types
open Utils
open Yojson.Basic.Util

let extract_commit json =
  let open Yojson.Basic.Util in
  let commit_json = json |> member "commit" in
  let message = commit_json |> member "message" |> to_string in
  if
    string_match
      ~regexp:"Bot merge \\([a-zA-Z0-9]*\\) [a-z]* \\([a-zA-Z0-9]*\\)" message
  then (Some (Str.matched_group 1 message), Str.matched_group 2 message)
  else
    (* In the case of build webhooks, the id is a number and the sha is the
       reference of the commit, while in the case of pipeline hooks only id
       is present and represents the sha. *)
    ( None
    , ( match commit_json |> member "sha" with
      | `Null ->
          commit_json |> member "id"
      | sha ->
          sha )
      |> to_string )

let job_info_of_json json =
  let open Yojson.Basic.Util in
  let build_status = json |> member "build_status" |> to_string in
  let build_id = json |> member "build_id" |> to_int in
  let build_name = json |> member "build_name" |> to_string in
  let project_id = json |> member "project_id" |> to_int in
  let base_commit, head_commit = json |> extract_commit in
  let branch = json |> member "ref" |> to_string in
  let repo_url = json |> member "repository" |> member "url" |> to_string in
  let stage = json |> member "build_stage" |> to_string in
  let failure_reason =
    json |> member "build_failure_reason" |> to_string |> Option.some
  in
  let allow_fail = json |> member "build_allow_failure" |> to_bool in
  { build_status
  ; build_id
  ; build_name
  ; stage
  ; failure_reason
  ; allow_fail
  ; common_info= {base_commit; head_commit; branch; repo_url; project_id} }

(* For use to decode builds inside a pipeline webhook *)
let build_info_of_json json =
  let open Yojson.Basic.Util in
  let build_status = json |> member "status" |> to_string in
  let build_id = json |> member "id" |> to_int in
  let build_name = json |> member "name" |> to_string in
  let stage = json |> member "stage" |> to_string in
  let allow_fail = json |> member "allow_failure" |> to_bool in
  { build_status
  ; build_id
  ; build_name
  ; stage
  ; allow_fail
  ; failure_reason= None
  ; common_info= () }

let pipeline_info_of_json json =
  let open Yojson.Basic.Util in
  let pipeline_json = json |> member "object_attributes" in
  let state = pipeline_json |> member "status" |> to_string in
  let pipeline_id = pipeline_json |> member "id" |> to_int in
  let base_commit, head_commit = json |> extract_commit in
  let branch = pipeline_json |> member "ref" |> to_string in
  let project = json |> member "project" in
  let repo_url = project |> member "web_url" |> to_string in
  let project_path = project |> member "path_with_namespace" |> to_string in
  let project_id = project |> member "id" |> to_int in
  let stages =
    pipeline_json |> member "stages" |> to_list |> List.map ~f:to_string
  in
  let builds =
    json |> member "builds" |> to_list |> List.map ~f:build_info_of_json
  in
  { state
  ; pipeline_id
  ; project_path
  ; common_info= {head_commit; base_commit; branch; repo_url; project_id}
  ; stages
  ; builds }

type msg =
  | JobEvent of ci_common_info job_info
  | PipelineEvent of pipeline_info
  | UnsupportedEvent of string

let gitlab_event ~event json =
  match event with
  | "Job Hook" ->
      Ok (JobEvent (job_info_of_json json))
  | "Pipeline Hook" ->
      Ok (PipelineEvent (pipeline_info_of_json json))
  | e ->
      Ok (UnsupportedEvent e)

let receive_gitlab ~secret headers body =
  let open Result in
  ( match Header.get headers "X-Gitlab-Token" with
  | Some header_secret ->
      if Eqaf.equal secret header_secret then return true
      else Error "Webhook password mismatch."
  | None ->
      return false )
  >>= fun signed ->
  match Header.get headers "X-Gitlab-Event" with
  | Some event -> (
    try
      let json = Yojson.Basic.from_string body in
      gitlab_event ~event json |> Result.map ~f:(fun r -> (signed, r))
    with
    | Yojson.Json_error err ->
        Error (f "Json error: %s" err)
    | Type_error (err, _) ->
        Error (f "Json type error: %s" err) )
  | None ->
      Error "Not a GitLab webhook."
