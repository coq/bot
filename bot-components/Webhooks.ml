open Base
open Cohttp
open Utils
open Yojson.Basic.Util

type issue = {owner: string; repo: string; number: int}

type msg = NoOp of string | IssueClosed of issue

let handle_json body parse =
  try
    let json = Yojson.Basic.from_string body in
    parse json
  with
  | Yojson.Json_error err -> Error (f "Json error: %s" err)
  | Type_error (err, _) -> Error (f "Json type error: %s" err)

let receive headers body =
  match Header.get headers "X-GitHub-Event" with
  | Some "issues" ->
      handle_json body (fun json ->
          let action = json |> member "action" |> to_string in
          let repo_json = json |> member "repository" in
          let repo = repo_json |> member "name" |> to_string in
          let owner =
            repo_json |> member "owner" |> member "login" |> to_string
          in
          let number = json |> member "issue" |> member "number" |> to_int in
          if String.equal action "closed" then
            Ok (IssueClosed {owner; repo; number})
          else Ok (NoOp "Unhandled GitHub issues action.") )
  | Some _ -> Ok (NoOp "Unhandled GitHub webhook.")
  | None -> Error "Not a GitHub webhook."
