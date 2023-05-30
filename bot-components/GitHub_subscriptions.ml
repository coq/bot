open Base
open Cohttp
open GitHub_types
open Utils
open Yojson.Basic.Util

let issue_info_of_json ?issue_json json =
  let issue_json =
    match issue_json with
    | None ->
        json |> member "issue"
    | Some issue_json ->
        issue_json
  in
  let repo_json = json |> member "repository" in
  { issue=
      { owner= repo_json |> member "owner" |> member "login" |> to_string
      ; repo= repo_json |> member "name" |> to_string
      ; number= issue_json |> member "number" |> to_int }
  ; title= issue_json |> member "title" |> to_string
  ; number= issue_json |> member "number" |> to_int
  ; id= issue_json |> member "node_id" |> GitHub_ID.of_json
  ; user= issue_json |> member "user" |> member "login" |> to_string
  ; labels=
      issue_json |> member "labels" |> to_list
      |> List.map ~f:(fun json -> json |> member "name" |> to_string)
  ; milestoned=
      (match issue_json |> member "milestone" with `Null -> false | _ -> true)
  ; pull_request=
      issue_json |> member "html_url" |> to_string
      |> string_match ~regexp:"https://github.com/[^/]*/[^/]*/pull/[0-9]*"
  ; body= issue_json |> member "body" |> to_string_option
  ; assignees=
      issue_json |> member "assignees" |> to_list
      |> List.map ~f:(fun json -> json |> member "login" |> to_string) }

let commit_info_of_json json =
  { branch=
      { repo_url= json |> member "repo" |> member "html_url" |> to_string
      ; name= json |> member "ref" |> to_string }
  ; sha= json |> member "sha" |> to_string }

let pull_request_info_of_json json =
  let pr_json = json |> member "pull_request" in
  { issue= issue_info_of_json ~issue_json:pr_json json
  ; base= pr_json |> member "base" |> commit_info_of_json
  ; head= pr_json |> member "head" |> commit_info_of_json
  ; merged=
      (match pr_json |> member "merged_at" with `Null -> false | _ -> true)
  ; last_commit_message= None }

let project_card_of_json json =
  let card_json = json |> member "project_card" in
  let column_id = card_json |> member "column_id" |> to_int in
  let regexp =
    "https://api.github.com/repos/\\([^/]*\\)/\\([^/]*\\)/issues/\\([0-9]*\\)"
  in
  match card_json |> member "content_url" with
  | `Null ->
      Ok {issue= None; column_id}
  | `String content_url when string_match ~regexp content_url ->
      let owner = Str.matched_group 1 content_url in
      let repo = Str.matched_group 2 content_url in
      let number = Str.matched_group 3 content_url |> Int.of_string in
      Ok {issue= Some {owner; repo; number}; column_id}
  | `String _ ->
      Error "Could not parse content_url field."
  | _ ->
      Error "content_url field has unexpected type."

let comment_info_of_json ?(review_comment = false) json =
  let comment_json =
    if review_comment then json |> member "review" else json |> member "comment"
  in
  let pull_request =
    if review_comment then Some (pull_request_info_of_json json) else None
  in
  { body=
      ( match comment_json |> member "body" with
      | `String body ->
          body
      | `Null (* body of review comments can be null *) ->
          ""
      | _ ->
          raise (Yojson.Json_error {|Unexpected type for field "body".|}) )
  ; author= comment_json |> member "user" |> member "login" |> to_string
  ; pull_request
  ; issue=
      ( match pull_request with
      | Some pr_info ->
          pr_info.issue
      | None ->
          issue_info_of_json json )
  ; review_comment
  ; id= comment_json |> member "node_id" |> GitHub_ID.of_json }

let repository_info_of_json json =
  let repo = json |> member "repository" in
  { id= repo |> member "id" |> to_int
  ; node_id= repo |> member "node_id" |> GitHub_ID.of_json
  ; owner= repo |> member "owner" |> member "login" |> to_string
  ; name= repo |> member "name" |> to_string }

let check_suite_info_of_json json =
  let check_suite = json |> member "check_suite" in
  let status =
    match check_suite |> member "status" |> to_string with
    | "completed" ->
        COMPLETED
    | "in_progress" ->
        IN_PROGRESS
    | "queued" | _ ->
        QUEUED
  in
  { id= check_suite |> member "id" |> to_int
  ; node_id= check_suite |> member "node_id" |> GitHub_ID.of_json
  ; head_sha= check_suite |> member "head_sha" |> to_string
  ; status }

let check_run_info_of_json json =
  let check_run = json |> member "check_run" in
  let status =
    match check_run |> member "status" |> to_string with
    | "completed" ->
        COMPLETED
    | "in_progress" ->
        IN_PROGRESS
    | "queued" | _ ->
        QUEUED
  in
  { id= check_run |> member "id" |> to_int
  ; node_id= check_run |> member "node_id" |> GitHub_ID.of_json
  ; head_sha= check_run |> member "head_sha" |> to_string
  ; status
  ; check_suite_info= check_suite_info_of_json check_run
  ; repository_info= repository_info_of_json json
  ; external_id= check_run |> member "external_id" |> to_string }

let push_event_info_of_json json =
  let open Yojson.Basic.Util in
  let owner =
    json |> member "repository" |> member "owner" |> member "login" |> to_string
  in
  let repo = json |> member "repository" |> member "name" |> to_string in
  let base_ref = json |> member "ref" |> to_string in
  let head_sha = json |> member "after" |> to_string in
  let commits = json |> member "commits" |> to_list in
  let commits_msg =
    List.map commits ~f:(fun c -> c |> member "message" |> to_string)
  in
  {owner; repo; base_ref; head_sha; commits_msg}

type msg =
  | IssueOpened of issue_info
  | IssueClosed of issue_info
  | RemovedFromProject of project_card_issue
  | PullRequestUpdated of pull_request_action * issue_info pull_request_info
  | BranchCreated of remote_ref_info
  | TagCreated of remote_ref_info
  | CommentCreated of comment_info
  | CheckRunCreated of check_run_info
  | CheckRunUpdated of check_run_info
  | CheckRunReRequested of check_run_info
  | CheckSuiteCreated of check_suite_info
  | CheckSuiteRequested of check_suite_info
  | PushEvent of push_info
  | UnsupportedEvent of string

let github_action ~event ~action json =
  match (event, action) with
  | "pull_request", "opened" ->
      Ok (PullRequestUpdated (PullRequestOpened, pull_request_info_of_json json))
  | "pull_request", "reopened" ->
      Ok
        (PullRequestUpdated (PullRequestReopened, pull_request_info_of_json json)
        )
  | "pull_request", "synchronize" ->
      Ok
        (PullRequestUpdated
           (PullRequestSynchronized, pull_request_info_of_json json) )
  | "pull_request", "closed" ->
      Ok (PullRequestUpdated (PullRequestClosed, pull_request_info_of_json json))
  | "issues", "opened" ->
      Ok (IssueOpened (issue_info_of_json json))
  | "issues", "closed" ->
      Ok (IssueClosed (issue_info_of_json json))
  | "project_card", "deleted" ->
      json |> project_card_of_json
      |> Result.map ~f:(fun card -> RemovedFromProject card)
  | "issue_comment", "created" ->
      Ok (CommentCreated (comment_info_of_json json))
  | "pull_request_review", "submitted" ->
      Ok (CommentCreated (comment_info_of_json json ~review_comment:true))
  | "check_run", "created" ->
      Ok (CheckRunCreated (check_run_info_of_json json))
  | "check_run", "rerequested" ->
      Ok (CheckRunReRequested (check_run_info_of_json json))
  | "check_suite", "requested" ->
      Ok (CheckSuiteRequested (check_suite_info_of_json json))
  | _ ->
      Ok (UnsupportedEvent "Unsupported GitHub action.")

let github_event ~event json =
  match event with
  | "pull_request"
  | "issues"
  | "project_card"
  | "issue_comment"
  | "pull_request_review"
  | "check_run"
  | "check_suite" ->
      github_action ~event ~action:(json |> member "action" |> to_string) json
  | "push" ->
      Ok (PushEvent (push_event_info_of_json json))
  | "create" -> (
      let ref_info =
        { repo_url= json |> member "repository" |> member "html_url" |> to_string
        ; name= json |> member "ref" |> to_string }
      in
      match json |> member "ref_type" |> to_string with
      | "branch" ->
          Ok (BranchCreated ref_info)
      | "tag" ->
          Ok (TagCreated ref_info)
      | ref_type ->
          Error (f "Unexpected ref_type: %s" ref_type) )
  | _ ->
      Ok (UnsupportedEvent "Unsupported GitHub event.")

let receive_github ~secret headers body =
  let open Result in
  ( match Header.get headers "X-Hub-Signature" with
  | Some signature ->
      let expected =
        Mirage_crypto.Hash.SHA1.hmac ~key:(Cstruct.of_string secret)
          (Cstruct.of_string body)
        |> Hex.of_cstruct |> Hex.show |> f "sha1=%s"
      in
      if Eqaf.equal signature expected then return true
      else Error "Webhook signed but with wrong signature."
  | None ->
      return false )
  >>= fun signed ->
  match Header.get headers "X-GitHub-Event" with
  | Some event -> (
    try
      let json = Yojson.Basic.from_string body in
      github_event ~event json |> Result.map ~f:(fun r -> (signed, r))
    with
    | Yojson.Json_error err ->
        Error (f "Json error: %s" err)
    | Type_error (err, _) ->
        Error (f "Json type error: %s" err) )
  | None ->
      Error "Not a GitHub webhook."
