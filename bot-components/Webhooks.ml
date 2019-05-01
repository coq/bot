open Base
open Cohttp
open Utils
open Yojson.Basic.Util

type issue = {owner: string; repo: string; number: int}

type projectCard = {issue: issue; column_id: int}

type msg =
  | NoOp of string
  | IssueClosed of issue
  | RemovedFromProject of projectCard

let get_owner_repo json =
  let repo_json = json |> member "repository" in
  let repo = repo_json |> member "name" |> to_string in
  let owner = repo_json |> member "owner" |> member "login" |> to_string in
  (owner, repo)

let github_action ~event ~action json =
  match (event, action) with
  | "issues", "closed" ->
      let number = json |> member "issue" |> member "number" |> to_int in
      let owner, repo = get_owner_repo json in
      Ok (IssueClosed {owner; repo; number})
  | "project_card", "deleted" -> (
      let card = json |> member "project_card" in
      match card |> member "content_url" with
      | `String content_url ->
          let regexp =
            "https://api.github.com/repos/[^/]*/[^/]*/issues/\\([0-9]*\\)"
          in
          if string_match ~regexp content_url then (
            let number = Str.matched_group 1 content_url |> Int.of_string in
            let owner, repo = get_owner_repo json in
            let column_id = card |> member "column_id" |> to_int in
            print_endline
              (f "Issue or PR %s/%s#%d was removed from project column %d."
                 owner repo number column_id) ;
            Ok (RemovedFromProject {issue= {owner; repo; number}; column_id}) )
          else Error "Could not parse content_url field."
      | `Null ->
          Ok (NoOp "GitHub card removed, but no associated issue or PR.")
      | _ -> Error "content_url field has unexpected type." )
  | _ -> Ok (NoOp "Unhandled GitHub event or action.")

let receive_github headers body =
  match Header.get headers "X-GitHub-Event" with
  | Some event -> (
    try
      let json = Yojson.Basic.from_string body in
      github_action ~event ~action:(json |> member "action" |> to_string) json
    with
    | Yojson.Json_error err -> Error (f "Json error: %s" err)
    | Type_error (err, _) -> Error (f "Json type error: %s" err) )
  | None -> Error "Not a GitHub webhook."
