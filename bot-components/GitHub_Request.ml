open Base
open Cohttp
open Cohttp_lwt
open Cohttp_lwt_unix
open Lwt
open Yojson.Basic
open Yojson.Basic.Util

exception GraphQL_Failure of string list

let graphql_query ~access_token query variables =
  let body =
    `Assoc [("query", `String query); ("variables", `Assoc variables)]
    |> pretty_to_string |> Body.of_string
  in
  let headers = Header.init_with "Authorization" ("token " ^ access_token) in
  Client.post ~body ~headers (Uri.of_string "https://api.github.com/graphql")
  >>= (fun (resp, body) ->
        Body.to_string body
        >|= fun body -> (resp |> Response.status |> Code.code_of_status, body)
        )
  >|= fun (code, body) ->
  if code < 200 || code > 299 then
    raise (GraphQL_Failure ["Response code: " ^ Int.to_string code; body]) ;
  let json =
    try from_string body with _ ->
      let message =
        "Unexpected failure while parsing successful request's JSON body."
      in
      raise (GraphQL_Failure [message; body])
  in
  try
    match json |> member "errors" with
    | `Null -> json
    | `List errors ->
        raise
          (GraphQL_Failure
             (List.map errors ~f:(fun error ->
                  error |> member "message" |> to_string )))
    | _ -> failwith "Shouldn't happen"
  with
  | GraphQL_Failure errors -> raise (GraphQL_Failure errors)
  | _ ->
      let message =
        "Unexpected failure while parsing unsuccessful request's JSON body."
      in
      raise (GraphQL_Failure [message; body])

let string_match ~regexp string =
  try
    let _ = Str.search_forward (Str.regexp regexp) string 0 in
    true
  with Not_found -> false

(* Types and fragments *)

module Milestone = struct
  type t = {title: string; description: string}

  let fragment =
    "fragment milestone on Milestone {\n\
    \         title\n\
    \         description\n\
    \       }"

  let from_json json =
    { title= json |> member "title" |> to_string
    ; description= json |> member "description" |> to_string }

  type backport_info =
    { backport_to: string
    ; request_inclusion_column: int
    ; backported_column: int
    ; rejected_milestone: string }

  let get_backport_info bot_name {description} =
    let project_column_regexp =
      "https://github.com/[^/]*/[^/]*/projects/[0-9]+#column-\\([0-9]+\\)"
    in
    let regexp =
      bot_name ^ ": backport to \\([^ ]*\\) (request inclusion column: "
      ^ project_column_regexp ^ "; backported column: " ^ project_column_regexp
      ^ "; move rejected PRs to: "
      ^ "https://github.com/[^/]*/[^/]*/milestone/\\([0-9]+\\)" ^ ")"
    in
    if string_match ~regexp description then
      let backport_to = Str.matched_group 1 description in
      let request_inclusion_column =
        Str.matched_group 2 description |> Int.of_string
      in
      let backported_column =
        Str.matched_group 3 description |> Int.of_string
      in
      let rejected_milestone = Str.matched_group 4 description in
      Some
        { backport_to
        ; request_inclusion_column
        ; backported_column
        ; rejected_milestone }
    else None
end

module ProjectColumn = struct
  type t = {id: string; db_id: int}

  let fragment =
    "fragment projectColumn on ProjectColumn {\n\
    \         id\n\
    \         databaseId\n\
    \       }"

  let from_json json =
    { id= json |> member "id" |> to_string
    ; db_id= json |> member "databaseId" |> to_int }
end

module ProjectCard = struct
  type t = {id: string; column: ProjectColumn.t; columns: ProjectColumn.t list}

  let fragment =
    "fragment projectCard on ProjectCard {\n\
    \         id\n\
    \         column { ... projectColumn }\n\
    \         project {\n\
    \           columns(first:100) {\n\
    \             nodes { ... projectColumn }\n\
    \           }\n\
    \         }\n\
    \       }" ^ ProjectColumn.fragment

  let from_json json =
    { id= json |> member "id" |> to_string
    ; column= json |> member "column" |> ProjectColumn.from_json
    ; columns=
        json |> member "project" |> member "columns" |> member "nodes"
        |> to_list
        |> List.map ~f:ProjectColumn.from_json }
end

(* Queries *)

let pull_request_id_db_id_and_milestone ~access_token owner repo number =
  let query =
    "query prInfo($owner: String!, $repo: String!, $number: Int!) {\n\
    \       repository(owner: $owner,name: $repo) {\n\
    \         pullRequest(number: $number) {\n\
    \           id\n\
    \           databaseId\n\
    \           milestone { ... milestone }\n\
    \         }\n\
    \       }\n\
    \     }" ^ Milestone.fragment
  in
  let variables =
    [("owner", `String owner); ("repo", `String repo); ("number", `Int number)]
  in
  graphql_query ~access_token query variables
  >|= fun json ->
  let pr_json =
    json |> member "data" |> member "repository" |> member "pullRequest"
  in
  let id = pr_json |> member "id" |> to_string in
  let db_id = pr_json |> member "databaseId" |> to_int in
  let milestone = pr_json |> member "milestone" |> Milestone.from_json in
  (id, db_id, milestone)

let pull_request_base_milestone_and_cards ~access_token owner repo number =
  let query =
    "query prInfo($owner: String!, $repo: String!, $number: Int!) {\n\
    \       repository(owner: $owner,name: $repo) {\n\
    \         pullRequest(number: $number) {\n\
    \           milestone { ... milestone }\n\
    \           projectCards(first:100) {\n\
    \             nodes { ... projectCard }\n\
    \           }\n\
    \         }\n\
    \       }\n\
    \     }" ^ Milestone.fragment ^ ProjectCard.fragment
  in
  let variables =
    [("owner", `String owner); ("repo", `String repo); ("number", `Int number)]
  in
  graphql_query ~access_token query variables
  >|= fun json ->
  let pr_json =
    json |> member "data" |> member "repository" |> member "pullRequest"
  in
  let cards =
    pr_json |> member "projectCards" |> member "nodes" |> to_list
    |> List.map ~f:ProjectCard.from_json
  in
  let milestone = pr_json |> member "milestone" |> Milestone.from_json in
  (cards, milestone)

(* Mutations *)

type mv_card_to_column_input = {card_id: string; column_id: string}

let mv_card_to_column ~access_token {card_id; column_id} =
  let mutation =
    "mutation moveCard($card_id:ID!,$column_id:ID!) {\n\
    \       moveProjectCard(input:{cardId:$card_id,columnId:$column_id}) {\n\
    \         clientMutationId\n\
    \       }\n\
    \     }"
  in
  let variables =
    [("card_id", `String card_id); ("column_id", `String column_id)]
  in
  graphql_query ~access_token mutation variables >|= ignore

let post_comment ~access_token id message =
  let mutation =
    "mutation addComment($id:ID!,$message:String!) {\n\
    \       addComment(input:{subjectId:$id,body:$message}) {\n\
    \         clientMutationId\n\
    \       }\n\
    \     }"
  in
  let variables = [("id", `String id); ("message", `String message)] in
  graphql_query ~access_token mutation variables >|= ignore

(* Scratch work *)

let backported_pr_info ~access_token number base_ref =
  pull_request_base_milestone_and_cards ~access_token "coq" "coq" number
  >|= fun (cards, milestone) ->
  let open Option in
  Milestone.get_backport_info "coqbot" milestone
  >>= fun {backport_to; request_inclusion_column; backported_column} ->
  if String.equal ("refs/heads/" ^ backport_to) base_ref then
    List.find_map cards ~f:(fun card ->
        if Int.equal request_inclusion_column card.column.db_id then
          List.find_map card.columns ~f:(fun column ->
              if Int.equal backported_column column.db_id then
                Some {card_id= card.id; column_id= column.id}
              else None )
        else None )
  else None
