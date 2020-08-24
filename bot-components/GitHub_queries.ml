open Base
open Bot_info
open GitHub_GraphQL
open GitHub_types
open Lwt
open Utils

let extract_backport_info ~(bot_info : Bot_info.t) description :
    full_backport_info option =
  let project_column_regexp =
    "https://github.com/[^/]*/[^/]*/projects/[0-9]+#column-\\([0-9]+\\)"
  in
  let regexp =
    bot_info.name ^ ": backport to \\([^ ]*\\) (request inclusion column: "
    ^ project_column_regexp ^ "; backported column: " ^ project_column_regexp
    ^ "; move rejected PRs to: "
    ^ "https://github.com/[^/]*/[^/]*/milestone/\\([0-9]+\\)" ^ ")"
  in
  if string_match ~regexp description then
    let backport_to = Str.matched_group 1 description in
    let request_inclusion_column =
      Str.matched_group 2 description |> Int.of_string
    in
    let backported_column = Str.matched_group 3 description |> Int.of_string in
    let rejected_milestone = Str.matched_group 4 description in
    Some
      { backport_info=
          [{backport_to; request_inclusion_column; backported_column}]
      ; rejected_milestone }
  else
    let begin_regexp = bot_info.name ^ ": \\(.*\\)$" in
    let backport_info_unit =
      "backport to \\([^ ]*\\) (request inclusion column: "
      ^ project_column_regexp ^ "; backported column: " ^ project_column_regexp
      ^ "); \\(.*\\)$"
    in
    let end_regexp =
      "move rejected PRs to: \
       https://github.com/[^/]*/[^/]*/milestone/\\([0-9]+\\)"
    in
    let rec aux string =
      if string_match ~regexp:backport_info_unit string then
        let backport_to = Str.matched_group 1 string in
        let request_inclusion_column =
          Str.matched_group 2 string |> Int.of_string
        in
        let backported_column = Str.matched_group 3 string |> Int.of_string in
        Str.matched_group 4 string |> aux
        |> Option.map ~f:(fun {backport_info; rejected_milestone} ->
               { backport_info=
                   {backport_to; request_inclusion_column; backported_column}
                   :: backport_info
               ; rejected_milestone })
      else if string_match ~regexp:end_regexp string then
        let rejected_milestone = Str.matched_group 1 string in
        Some {backport_info= []; rejected_milestone}
      else None
    in
    if string_match ~regexp:begin_regexp description then
      Str.matched_group 1 description |> aux
    else None

let get_pull_request_milestone_and_cards ~bot_info ~owner ~repo ~number =
  PullRequest_Milestone_and_Cards.make ~owner ~repo ~number ()
  |> GraphQL_query.send_graphql_query ~bot_info
  >|= function
  | Ok result -> (
    match result#repository with
    | Some result -> (
      match result#pullRequest with
      | Some result ->
          let cards =
            match result#projectCards#nodes with
            | None ->
                []
            | Some cards ->
                cards |> Array.to_list |> List.filter_opt
                |> List.map ~f:(fun card ->
                       { id= card#id
                       ; column= card#column
                       ; columns=
                           ( match card#project#columns#nodes with
                           | None ->
                               []
                           | Some columns ->
                               columns |> Array.to_list |> List.filter_opt ) })
          in
          Ok (cards, result#milestone)
      | None ->
          Error (f "Pull request %s/%s#%d does not exist." owner repo number) )
    | None ->
        Error (f "Repository %s/%s does not exist." owner repo) )
  | Error err ->
      Error err

let get_backported_pr_info ~bot_info number base_ref =
  get_pull_request_milestone_and_cards ~bot_info ~owner:"coq" ~repo:"coq"
    ~number
  >|= function
  | Ok (cards, milestone) ->
      (let open Option in
      milestone
      >>= fun milestone ->
      milestone.description
      >>= extract_backport_info ~bot_info
      >>= (fun full_backport_info ->
            full_backport_info.backport_info
            |> List.find ~f:(fun {backport_to} ->
                   String.equal ("refs/heads/" ^ backport_to) base_ref))
      >>= fun {request_inclusion_column; backported_column} ->
      List.find_map cards ~f:(fun card ->
          if
            card.column
            >>= (fun column -> column.databaseId)
            |> Option.equal Int.equal (Some request_inclusion_column)
          then
            List.find_map card.columns ~f:(fun column ->
                if
                  Option.equal Int.equal (Some backported_column)
                    column.databaseId
                then Some {card_id= card.id; column_id= column.id}
                else None)
          else None))
      |> fun res -> Ok res
  | Error err ->
      Error (f "Error in backported_pr_info: %s." err)

let get_pull_request_id_and_milestone ~bot_info ~owner ~repo ~number =
  PullRequest_ID_and_Milestone.make ~owner ~repo ~number ()
  |> GraphQL_query.send_graphql_query ~bot_info
  >|= Result.bind ~f:(fun result ->
          match result#repository with
          | None ->
              Error (f "Repository %s/%s does not exist." owner repo)
          | Some result -> (
            match result#pullRequest with
            | None ->
                Error
                  (f "Pull request %s/%s#%d does not exist." owner repo number)
            | Some pr -> (
              match (pr#databaseId, pr#milestone) with
              | None, _ ->
                  Error
                    (f "Pull request %s/%s#%d does not have a database ID."
                       owner repo number)
              | _, None ->
                  Error
                    (f "Pull request %s/%s#%d does not have a milestone." owner
                       repo number)
              | Some db_id, Some milestone ->
                  Ok
                    ( match milestone#description with
                    | Some description -> (
                      match extract_backport_info ~bot_info description with
                      | Some bp_info ->
                          Some (pr#id, db_id, bp_info)
                      | _ ->
                          None )
                    | _ ->
                        None ) ) ))

let team_membership_of_resp ~org ~team ~user resp =
  match resp#organization with
  | None ->
      Error (f "Organization %s does not exist." org)
  | Some resp -> (
    match resp#team with
    | None ->
        Error (f "Team @%s/%s does not exist." org team)
    | Some resp -> (
      match resp#members#nodes with
      | Some members
        when members
             |> Array.exists ~f:(function
                  | Some member when String.equal member#login user ->
                      true
                  | _ ->
                      false) ->
          Ok true
      | _ ->
          Ok false ) )

let get_team_membership ~bot_info ~org ~team ~user =
  TeamMembership.make ~org ~team ~user ()
  |> GraphQL_query.send_graphql_query ~bot_info
  >|= Result.map_error ~f:(fun err ->
          f "Query get_team_membership failed with %s" err)
  >|= Result.bind ~f:(team_membership_of_resp ~org ~team ~user)

let pull_request_info_of_resp ~owner ~repo ~number resp :
    (string pull_request_info, string) Result.t =
  let repo_url = f "https://github.com/%s/%s" owner repo in
  match resp#repository with
  | None ->
      Error (f "Unknown repository %s/%s." owner repo)
  | Some repository -> (
    match repository#pullRequest with
    | None ->
        Error (f "Unknown pull request %s/%s#%d." owner repo number)
    | Some pull_request -> (
      match pull_request#commits#nodes with
      | None ->
          Error
            (f "No commits found for pull request %s/%s#%d." owner repo number)
      | Some nodes -> (
          let commits = nodes |> Array.to_list |> List.filter_opt in
          match List.hd commits with
          | None ->
              Error
                (f "No commits found for pull request %s/%s#%d." owner repo
                   number)
          | Some node ->
              Ok
                { issue= pull_request#id
                ; base=
                    { branch= {repo_url; name= pull_request#baseRefName}
                    ; sha= pull_request#baseRefOid }
                ; head=
                    { branch= {repo_url; name= pull_request#headRefName}
                    ; sha= pull_request#headRefOid }
                ; merged= pull_request#merged
                ; last_commit_message= Some node#commit#message } ) ) )

let get_pull_request_refs ~bot_info ~owner ~repo ~number =
  PullRequest_Refs.make ~owner ~repo ~number ()
  |> GraphQL_query.send_graphql_query ~bot_info
  >|= Result.map_error ~f:(fun err ->
          f "Query pull_request_info failed with %s" err)
  >|= Result.bind ~f:(pull_request_info_of_resp ~owner ~repo ~number)

let pull_request_reviews_info_of_resp ~owner ~repo ~number resp :
    (pull_request_reviews_info, string) Result.t =
  match resp#repository with
  | None ->
      Error (f "Unknown repository %s/%s." owner repo)
  | Some repository -> (
    match repository#pullRequest with
    | None ->
        Error (f "Unknown pull request %s/%s#%d." owner repo number)
    | Some pull_request -> (
      match
        ( pull_request#baseRef
        , pull_request#files
        , pull_request#approvedReviews
        , pull_request#commentReviews )
      with
      | None, _, _, _ ->
          Error "No base ref found."
      | _, None, _, _ ->
          Error "No files found."
      | _, _, None, _ ->
          Error "No approved reviews found."
      | _, _, _, None ->
          Error "No comment reviews found."
      | Some baseRef, Some files, Some approved_reviews, Some comment_reviews ->
          let filter_authors a =
            a |> Array.to_list |> List.filter_opt
            |> List.filter_map ~f:(fun review ->
                   review#author |> Option.map ~f:(fun (`Actor a) -> a#login))
            |> List.dedup_and_sort ~compare:String.compare
          in
          let approved_reviews =
            match approved_reviews#nodes with
            | None ->
                []
            | Some reviews ->
                filter_authors reviews
          in
          Ok
            { baseRef= baseRef#name
            ; files=
                ( match files#nodes with
                | None ->
                    []
                | Some files ->
                    files |> Array.to_list |> List.filter_opt
                    |> List.map ~f:(fun file -> file#path) )
            ; last_comments=
                ( match pull_request#comments#nodes with
                | None ->
                    []
                | Some comments ->
                    comments |> Array.to_list |> List.filter_opt
                    |> List.filter_map ~f:(fun c ->
                           c#author
                           |> Option.map ~f:(fun (`Actor a) ->
                                  ( { id= c#id
                                    ; author= a#login
                                    ; created_by_email= c#createdViaEmail }
                                    : comment ))) )
            ; approved_reviews
            ; comment_reviews=
                ( match comment_reviews#nodes with
                | None ->
                    []
                | Some reviews ->
                    reviews |> filter_authors
                    |> List.filter ~f:(fun a ->
                           not
                             (List.exists approved_reviews ~f:(String.equal a)))
                )
            ; review_decision=
                ( match pull_request#reviewDecision with
                | None ->
                    NONE
                | Some r -> (
                  match r with
                  | `CHANGES_REQUESTED ->
                      CHANGES_REQUESTED
                  | `APPROVED ->
                      APPROVED
                  | `REVIEW_REQUIRED ->
                      REVIEW_REQUIRED ) ) } ) )

let get_pull_request_reviews_refs ~bot_info ~owner ~repo ~number =
  PullRequestReviewsInfo.make ~owner ~repo ~number ()
  |> GraphQL_query.send_graphql_query ~bot_info
  >|= Result.map_error ~f:(fun err ->
          f "Query pull_request_reviews_info failed with %s" err)
  >|= Result.bind ~f:(pull_request_reviews_info_of_resp ~owner ~repo ~number)

let file_content_of_resp ~owner ~repo resp : (string option, string) Result.t =
  match resp#repository with
  | None ->
      Error (f "Unknown repository %s/%s." owner repo)
  | Some repository -> (
    match repository#file with
    | Some (`Blob b) ->
        Ok b#text
    | Some (`GitObject _) ->
        Ok None
    | None ->
        Ok None )

let get_file_content ~bot_info ~owner ~repo ~branch ~file_name =
  FileContent.make ~owner ~repo ~file:(branch ^ ":" ^ file_name) ()
  |> GraphQL_query.send_graphql_query ~bot_info
  >|= Result.map_error ~f:(fun err -> f "Query file_content failed with %s" err)
  >|= Result.bind ~f:(file_content_of_resp ~owner ~repo)

let default_branch_of_resp ~owner ~repo resp =
  match resp#repository with
  | None ->
      Error (f "Unknown repository %s/%s." owner repo)
  | Some repository -> (
    match repository#defaultBranchRef with
    | None ->
        Error "No default branch found."
    | Some default_branch ->
        Ok (default_branch#name : string) )

let get_default_branch ~bot_info ~owner ~repo =
  DefaultBranch.make ~owner ~repo ()
  |> GraphQL_query.send_graphql_query ~bot_info
  >|= Result.map_error ~f:(fun err ->
          f "Query get_default_branch failed with %s" err)
  >|= Result.bind ~f:(default_branch_of_resp ~owner ~repo)

let closer_info_of_pr pr =
  { milestone_id= pr#milestone |> Option.map ~f:(fun milestone -> milestone#id)
  ; pull_request_id= pr#id }

let closer_info_option_of_closer closer =
  match closer with
  | None | Some `Nonexhaustive ->
      Ok ClosedByOther
  | Some (`PullRequest pr) ->
      Ok (ClosedByPullRequest (closer_info_of_pr pr))
  | Some (`Commit commit) -> (
    match commit#associatedPullRequests with
    | None ->
        Ok ClosedByCommit
    | Some prs -> (
      match prs#nodes with
      | Some [||] ->
          Ok ClosedByCommit
      | Some [|Some pr|] ->
          Ok (ClosedByPullRequest (closer_info_of_pr pr))
      | Some [|None|] ->
          Error "Closer query response is not well-formed."
      | Some _ ->
          Error "Closing commit associated to several pull requests."
      | _ ->
          Error "Closer query response is not well-formed." ) )

let issue_closer_info_of_resp ~owner ~repo ~number resp =
  match resp#repository with
  | None ->
      Error (f "Unknown repository %s/%s." owner repo)
  | Some repository -> (
    match repository#issue with
    | None ->
        Error (f "Unknown issue %s/%s#%d." owner repo number)
    | Some issue -> (
      match issue#timelineItems#nodes with
      | Some [|Some (`ClosedEvent event)|] ->
          event#closer |> closer_info_option_of_closer
          |> Result.map ~f:(function
               | ClosedByPullRequest closer_info ->
                   ClosedByPullRequest
                     { issue_id= issue#id
                     ; milestone_id=
                         issue#milestone
                         |> Option.map ~f:(fun milestone -> milestone#id)
                     ; closer= closer_info }
               | (ClosedByCommit | ClosedByOther) as reason ->
                   reason)
      | _ ->
          Error (f "No close event for issue %s/%s#%d." owner repo number) ) )

let get_issue_closer_info ~bot_info ({owner; repo; number} : issue) =
  Issue_Milestone.make ~owner ~repo ~number ()
  |> GraphQL_query.send_graphql_query ~bot_info
  >|= Result.map_error ~f:(fun err ->
          f "Query issue_milestone failed with %s" err)
  >|= Result.bind ~f:(issue_closer_info_of_resp ~owner ~repo ~number)

(* TODO: use GraphQL API *)

let get_status_check ~repo_full_name ~commit ~context =
  generic_get
    (Printf.sprintf "repos/%s/commits/%s/statuses" repo_full_name commit)
    (fun json ->
      let open Yojson.Basic.Util in
      json |> to_list
      |> List.exists ~f:(fun json ->
             json |> member "context" |> to_string |> String.equal context))

let get_cards_in_column column_id =
  generic_get
    ("projects/columns/" ^ Int.to_string column_id ^ "/cards")
    ~header_list:project_api_preview_header
    (fun json ->
      let open Yojson.Basic.Util in
      json |> to_list
      |> List.filter_map ~f:(fun json ->
             let card_id = json |> member "id" |> to_int in
             let content_url =
               json |> member "content_url" |> to_string_option
               |> Option.value ~default:""
             in
             let regexp = "https://api.github.com/repos/.*/\\([0-9]*\\)" in
             if string_match ~regexp content_url then
               let pr_number = Str.matched_group 1 content_url in
               Some (pr_number, card_id)
             else None))
