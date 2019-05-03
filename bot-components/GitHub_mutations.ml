open Base
open Lwt
open Utils

let mv_card_to_column ~token
    ({card_id; column_id} : GitHub_queries.mv_card_to_column_input) =
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
  GitHub_queries.graphql_query ~token mutation variables
  >|= function
  | Ok _ -> ()
  | Error err -> print_endline (f "Error while moving project card: %s" err)

let post_comment ~token ~id ~message =
  let mutation =
    "mutation addComment($id:ID!,$message:String!) {\n\
    \       addComment(input:{subjectId:$id,body:$message}) {\n\
    \         clientMutationId\n\
    \       }\n\
    \     }"
  in
  let variables = [("id", `String id); ("message", `String message)] in
  GitHub_queries.graphql_query ~token mutation variables
  >|= function
  | Ok _ -> ()
  | Error err -> print_endline (f "Error while posting comment: %s" err)

let update_milestone ~token ~issue ~milestone =
  let mutation =
    " mutation updateMilestone($issue: ID!, $milestone: ID!) {\n\
    \   updateIssue(input: {id: $issue, milestoneId: $milestone}) {\n\
    \     clientMutationId\n\
    \   }\n\
    \ }\n"
  in
  let variables =
    [("issue", `String issue); ("milestone", `String milestone)]
  in
  GitHub_queries.graphql_query ~token mutation variables
  >|= function
  | Ok _ -> ()
  | Error err -> print_endline (f "Error while updating milestone: %s" err)

let reflect_pull_request_milestone ~token
    (issue_closer_info : GitHub_queries.issue_closer_info) =
  match issue_closer_info.closer with
  | None -> Lwt_io.print "No closer information: doing nothing.\n"
  | Some closer -> (
    match closer.milestone_id with
    | None -> Lwt_io.printf "PR closed without a milestone: doing nothing.\n"
    | Some milestone -> (
      match issue_closer_info.milestone_id with
      | None ->
          (* No previous milestone: setting the one of the PR which closed the issue *)
          update_milestone ~token ~issue:issue_closer_info.issue_id ~milestone
      | Some previous_milestone when String.equal previous_milestone milestone
        ->
          Lwt_io.print
            "Issue is already in the right milestone: doing nothing.\n"
      | Some previous_milestone ->
          update_milestone ~token ~issue:issue_closer_info.issue_id ~milestone
          <&> post_comment ~token ~id:issue_closer_info.issue_id
                ~message:
                  "The milestone of this issue was changed to reflect the one \
                   of the pull request that closed it." ) )
