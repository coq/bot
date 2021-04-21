open Base
open GitHub_types
open Cohttp_lwt_unix
open Lwt
open Utils

let mv_card_to_column ~bot_info ({card_id; column_id} : mv_card_to_column_input)
    =
  GitHub_GraphQL.MoveCardToColumn.make ~card_id ~column_id ()
  |> GraphQL_query.send_graphql_query ~bot_info
  >|= function
  | Ok _ ->
      ()
  | Error err ->
      Stdio.print_endline (f "Error while moving project card: %s" err)

let post_comment ~bot_info ~id ~message =
  GitHub_GraphQL.PostComment.make ~id ~message ()
  |> GraphQL_query.send_graphql_query ~bot_info
  >|= Result.bind ~f:(fun result ->
          match result#payload with
          | None ->
              Error "No payload"
          | Some result -> (
            match result#commentEdge with
            | None ->
                Error "No comment edge"
            | Some result -> (
              match result#node with
              | None ->
                  Error "No comment node"
              | Some result ->
                  Ok result#url ) ))

let report_on_posting_comment = function
  | Ok url ->
      Lwt_io.printf "Posted a new comment: %s\n" url
  | Error f ->
      Lwt_io.printf "Error while posting a comment: %s\n" f

let update_milestone ~bot_info ~issue ~milestone =
  GitHub_GraphQL.UpdateMilestone.make ~issue ~milestone ()
  |> GraphQL_query.send_graphql_query ~bot_info
  >|= function
  | Ok _ ->
      ()
  | Error err ->
      Stdio.print_endline (f "Error while updating milestone: %s" err)

let merge_pull_request ~bot_info ?merge_method ?commit_headline ?commit_body
    ~pr_id =
  let merge_method =
    Option.map merge_method ~f:(function
      | MERGE ->
          `MERGE
      | REBASE ->
          `REBASE
      | SQUASH ->
          `SQUASH)
  in
  GitHub_GraphQL.MergePullRequest.make ~pr_id ?commit_headline ?commit_body
    ?merge_method ()
  |> GraphQL_query.send_graphql_query ~bot_info
  >|= function
  | Ok _ ->
      ()
  | Error err ->
      Stdio.print_endline (f "Error while merging PR: %s" err)

let reflect_pull_request_milestone ~bot_info issue_closer_info =
  match issue_closer_info.closer.milestone_id with
  | None ->
      Lwt_io.printf "PR closed without a milestone: doing nothing.\n"
  | Some milestone -> (
    match issue_closer_info.milestone_id with
    | None ->
        (* No previous milestone: setting the one of the PR which closed the issue *)
        update_milestone ~bot_info ~issue:issue_closer_info.issue_id ~milestone
    | Some previous_milestone when String.equal previous_milestone milestone ->
        Lwt_io.print "Issue is already in the right milestone: doing nothing.\n"
    | Some _ ->
        update_milestone ~bot_info ~issue:issue_closer_info.issue_id ~milestone
        <&> ( post_comment ~bot_info ~id:issue_closer_info.issue_id
                ~message:
                  "The milestone of this issue was changed to reflect the one \
                   of the pull request that closed it."
            >>= report_on_posting_comment ) )

let string_of_conclusion conclusion =
  match conclusion with
  | ACTION_REQUIRED ->
      `ACTION_REQUIRED
  | CANCELLED ->
      `CANCELLED
  | FAILURE ->
      `FAILURE
  | NEUTRAL ->
      `NEUTRAL
  | SKIPPED ->
      `SKIPPED
  | STALE ->
      `STALE
  | SUCCESS ->
      `SUCCESS
  | TIMED_OUT ->
      `TIMED_OUT

let create_check_run ~bot_info ?conclusion ~name ~repo_id ~head_sha ~status
    ~details_url ~title ?text ~summary ?external_id () =
  let conclusion = Option.map conclusion ~f:string_of_conclusion in
  let status =
    match status with
    | COMPLETED ->
        `COMPLETED
    | IN_PROGRESS ->
        `IN_PROGRESS
    | QUEUED ->
        `QUEUED
  in
  GitHub_GraphQL.NewCheckRun.make ~name ~repoId:repo_id ~headSha:head_sha
    ~status ~title ?text ~summary ~url:details_url ?conclusion
    ?externalId:external_id ()
  |> GraphQL_query.send_graphql_query ~bot_info
  >|= function
  | Ok _ ->
      ()
  | Error err ->
      Stdio.print_endline (f "Error while creating check run: %s" err)

let update_check_run ~bot_info ~check_run_id ~repo_id ~conclusion ?details_url
    ~title ?text ~summary () =
  let conclusion = string_of_conclusion conclusion in
  GitHub_GraphQL.UpdateCheckRun.make ~checkRunId:check_run_id ~repoId:repo_id
    ~conclusion ?url:details_url ~title ?text ~summary ()
  |> GraphQL_query.send_graphql_query ~bot_info
  >|= function
  | Ok _ ->
      ()
  | Error err ->
      Stdio.print_endline (f "Error while updating check run: %s" err)

(* TODO: use GraphQL API *)

let add_rebase_label ~bot_info (issue : issue) =
  let body = Cohttp_lwt.Body.of_string "[ \"needs: rebase\" ]" in
  let uri =
    f "https://api.github.com/repos/%s/%s/issues/%d/labels" issue.owner
      issue.repo issue.number
    |> (fun url ->
         Stdio.printf "URL: %s\n" url ;
         url)
    |> Uri.of_string
  in
  send_request ~body ~uri (github_header bot_info) ~bot_info

let remove_rebase_label ~bot_info (issue : issue) =
  let headers = headers (github_header bot_info) ~bot_info in
  let uri =
    f "https://api.github.com/repos/%s/%s/issues/%d/labels/needs%%3A rebase"
      issue.owner issue.repo issue.number
    |> (fun url ->
         Stdio.printf "URL: %s\n" url ;
         url)
    |> Uri.of_string
  in
  Lwt_io.printf "Sending delete request.\n"
  >>= fun () -> Client.delete ~headers uri >>= print_response

let update_milestone ~bot_info new_milestone (issue : issue) =
  let headers = headers (github_header bot_info) ~bot_info in
  let uri =
    f "https://api.github.com/repos/%s/%s/issues/%d" issue.owner issue.repo
      issue.number
    |> (fun url ->
         Stdio.printf "URL: %s\n" url ;
         url)
    |> Uri.of_string
  in
  let body =
    "{\"milestone\": " ^ new_milestone ^ "}" |> Cohttp_lwt.Body.of_string
  in
  Lwt_io.printf "Sending patch request.\n"
  >>= fun () -> Client.patch ~headers ~body uri >>= print_response

let remove_milestone = update_milestone "null"

let send_status_check ~bot_info ~repo_full_name ~commit ~state ~url ~context
    ~description =
  Lwt_io.printf "Sending status check to %s (commit %s, state %s)\n"
    repo_full_name commit state
  >>= fun () ->
  let body =
    "{\"state\": \"" ^ state ^ "\",\"target_url\":\"" ^ url
    ^ "\", \"description\": \"" ^ description ^ "\", \"context\": \"" ^ context
    ^ "\"}"
    |> Cohttp_lwt.Body.of_string
  in
  let uri =
    "https://api.github.com/repos/" ^ repo_full_name ^ "/statuses/" ^ commit
    |> Uri.of_string
  in
  send_request ~body ~uri (github_header bot_info) ~bot_info

let add_pr_to_column ~bot_info ~pr_id ~column_id =
  let body =
    "{\"content_id\":" ^ Int.to_string pr_id
    ^ ", \"content_type\": \"PullRequest\"}"
    |> (fun body ->
         Stdio.printf "Body:\n%s\n" body ;
         body)
    |> Cohttp_lwt.Body.of_string
  in
  let uri =
    "https://api.github.com/projects/columns/" ^ Int.to_string column_id
    ^ "/cards"
    |> (fun url ->
         Stdio.printf "URL: %s\n" url ;
         url)
    |> Uri.of_string
  in
  send_request ~body ~uri
    (project_api_preview_header @ github_header bot_info)
    ~bot_info
