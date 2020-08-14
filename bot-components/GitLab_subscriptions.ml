open Base
open Cohttp
open GitLab_types
open Utils
open Yojson.Basic.Util

let extract_commit json =
  let open Yojson.Basic.Util in
  let commit_json = json |> member "commit" in
  let message = commit_json |> member "message" |> to_string in
  if string_match ~regexp:"Bot merge .* into \\(.*\\)" message then
    Str.matched_group 1 message
  else
    (* In the case of build webhooks, the id is a number and the sha is the
       reference of the commit, while in the case of pipeline hooks only id
       is present and represents the sha. *)
    ( match commit_json |> member "sha" with
    | `Null ->
        commit_json |> member "id"
    | sha ->
        sha )
    |> to_string

let job_info_of_json json =
  let open Yojson.Basic.Util in
  let build_status = json |> member "build_status" |> to_string in
  let build_id = json |> member "build_id" |> to_int in
  let build_name = json |> member "build_name" |> to_string in
  let project_id = json |> member "project_id" |> to_int in
  let commit = json |> extract_commit in
  let branch = json |> member "ref" |> to_string in
  let repo_url = json |> member "repository" |> member "url" |> to_string in
  let failure_reason, allow_fail =
    if String.equal build_status "failed" then
      ( json |> member "build_failure_reason" |> to_string |> Option.some
      , json |> member "build_allow_failure" |> to_bool |> Option.some )
    else (None, None)
  in
  { build_status
  ; build_id
  ; build_name
  ; commit
  ; branch
  ; repo_url
  ; project_id
  ; failure_reason
  ; allow_fail }

let pipeline_info_of_json json =
  let open Yojson.Basic.Util in
  let pipeline_json = json |> member "object_attributes" in
  let state = pipeline_json |> member "status" |> to_string in
  let id = pipeline_json |> member "id" |> to_int in
  let commit = json |> extract_commit in
  let branch =
    json |> member "object_attributes" |> member "ref" |> to_string
  in
  let project_path =
    json |> member "project" |> member "path_with_namespace" |> to_string
  in
  {state; id; commit; branch; project_path}

type msg =
  | JobEvent of job_info
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
