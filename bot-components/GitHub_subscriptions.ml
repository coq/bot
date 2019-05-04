open Base
open Cohttp
open Utils
open Yojson.Basic.Util

type issue = {owner: string; repo: string; number: int}

type issue_info =
  { issue: issue
  ; id: string
  ; user: string
  ; labels: string list
  ; milestoned: bool
  ; pull_request: bool }

type ref_info = {repo_url: string; name: string}

type commit_info = {branch: ref_info; sha: string}

type pull_request_info =
  {issue: issue_info; base: commit_info; head: commit_info; merged: bool}

type project_card = {issue: issue; column_id: int}

type comment_info = {body: string; author: string; issue: issue}

type msg =
  | NoOp of string
  | IssueClosed of issue_info
  | RemovedFromProject of project_card
  | PullRequestUpdated of pull_request_info
  | PullRequestClosed of pull_request_info
  | BranchCreated of ref_info
  | TagCreated of ref_info
  | CommentCreated of comment_info

let issue_info_of_json ?issue_json json =
  let issue_json =
    match issue_json with
    | None -> json |> member "issue"
    | Some issue_json -> issue_json
  in
  let repo_json = json |> member "repository" in
  { issue=
      { owner= repo_json |> member "owner" |> member "login" |> to_string
      ; repo= repo_json |> member "name" |> to_string
      ; number= issue_json |> member "number" |> to_int }
  ; id= issue_json |> member "node_id" |> to_string
  ; user= issue_json |> member "user" |> member "login" |> to_string
  ; labels=
      issue_json |> member "labels" |> to_list
      |> List.map ~f:(fun json -> json |> member "name" |> to_string)
  ; milestoned=
      (match issue_json |> member "milestone" with `Null -> false | _ -> true)
  ; pull_request=
      ( match issue_json |> member "pull_request" with
      | `Null -> false
      | _ -> true ) }

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
  ; merged= pr_json |> member "merged" |> to_bool }

let github_action ~event ~action json =
  match (event, action) with
  | "pull_request", ("opened" | "reopened" | "synchronize") ->
      Ok (PullRequestUpdated (pull_request_info_of_json json))
  | "pull_request", "closed" ->
      Ok (PullRequestClosed (pull_request_info_of_json json))
  | "issues", "closed" -> Ok (IssueClosed (issue_info_of_json json))
  | "project_card", "deleted" -> (
      let card = json |> member "project_card" in
      match card |> member "content_url" with
      | `String content_url ->
          let regexp =
            "https://api.github.com/repos/\\([^/]*\\)/\\([^/]*\\)/issues/\\([0-9]*\\)"
          in
          if string_match ~regexp content_url then (
            let owner = Str.matched_group 1 content_url in
            let repo = Str.matched_group 2 content_url in
            let number = Str.matched_group 3 content_url |> Int.of_string in
            let column_id = card |> member "column_id" |> to_int in
            print_endline
              (f "Issue or PR %s/%s#%d was removed from project column %d."
                 owner repo number column_id) ;
            Ok (RemovedFromProject {issue= {owner; repo; number}; column_id}) )
          else Error "Could not parse content_url field."
      | `Null ->
          Ok (NoOp "GitHub card removed, but no associated issue or PR.")
      | _ -> Error "content_url field has unexpected type." )
  | _ -> Ok (NoOp "Unhandled GitHub action.")

let github_event ~event json =
  match event with
  | "create" -> (
      let ref_info =
        { repo_url=
            json |> member "repository" |> member "html_url" |> to_string
        ; name= json |> member "ref" |> to_string }
      in
      match json |> member "ref_type" |> to_string with
      | "branch" -> Ok (BranchCreated ref_info)
      | "tag" -> Ok (TagCreated ref_info)
      | ref_type -> Error (f "Unexpected ref_type: %s" ref_type) )
  | _ -> Ok (NoOp "Unhandled GitHub event.")

let receive_github headers body =
  match Header.get headers "X-GitHub-Event" with
  | Some event -> (
    try
      let json = Yojson.Basic.from_string body in
      match event with
      | "pull_request" | "issues" | "project_card" ->
          github_action ~event
            ~action:(json |> member "action" |> to_string)
            json
      | _ -> github_event ~event json
    with
    | Yojson.Json_error err -> Error (f "Json error: %s" err)
    | Type_error (err, _) -> Error (f "Json type error: %s" err) )
  | None -> Error "Not a GitHub webhook."
