open Base
open GitHub_GraphQL
open Lwt
open Utils

let mv_card_to_column ~token
    ({card_id; column_id} : GitHub_queries.mv_card_to_column_input) =
  MoveCardToColumn.make ~card_id ~column_id ()
  |> GitHub_queries.send_graphql_query ~token
  >|= function
  | Ok _ -> ()
  | Error err -> print_endline (f "Error while moving project card: %s" err)

let post_comment ~token ~id ~message =
  PostComment.make ~id ~message ()
  |> GitHub_queries.send_graphql_query ~token
  >|= function
  | Ok _ -> ()
  | Error err -> print_endline (f "Error while posting comment: %s" err)

let update_milestone ~token ~issue ~milestone =
  UpdateMilestone.make ~issue ~milestone ()
  |> GitHub_queries.send_graphql_query ~token
  >|= function
  | Ok _ -> ()
  | Error err -> print_endline (f "Error while updating milestone: %s" err)

let reflect_pull_request_milestone ~token
    (issue_closer_info : GitHub_queries.issue_closer_info) =
  match issue_closer_info.closer.milestone_id with
  | None -> Lwt_io.printf "PR closed without a milestone: doing nothing.\n"
  | Some milestone -> (
    match issue_closer_info.milestone_id with
    | None ->
        (* No previous milestone: setting the one of the PR which closed the issue *)
        update_milestone ~token ~issue:issue_closer_info.issue_id ~milestone
    | Some previous_milestone when String.equal previous_milestone milestone ->
        Lwt_io.print
          "Issue is already in the right milestone: doing nothing.\n"
    | Some _ ->
        update_milestone ~token ~issue:issue_closer_info.issue_id ~milestone
        <&> post_comment ~token ~id:issue_closer_info.issue_id
              ~message:
                "The milestone of this issue was changed to reflect the one \
                 of the pull request that closed it." )
