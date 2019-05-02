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
  GitHub_queries.graphql_query ~token mutation variables >|= ignore

let post_comment ~token id message =
  let mutation =
    "mutation addComment($id:ID!,$message:String!) {\n\
    \       addComment(input:{subjectId:$id,body:$message}) {\n\
    \         clientMutationId\n\
    \       }\n\
    \     }"
  in
  let variables = [("id", `String id); ("message", `String message)] in
  GitHub_queries.graphql_query ~token mutation variables >|= ignore

let issue_milestone_mutation ~token ~issue ~milestone =
  let mutation =
    " mutation issueMilestone($issue: ID!, $milestone: ID!) {\n\
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
  | Error err -> print_endline (f "Error in issue_milestone_mutation: %s" err)

let query_and_mutate ~token
    ({owner; repo; number} : GitHub_subscriptions.issue) = function
  | Ok (issue, None, milestone) ->
      issue_milestone_mutation ~token ~issue ~milestone
  | Ok (issue, Some previous_milestone, milestone) ->
      if String.equal previous_milestone milestone then (
        print_endline
          (f "Issue %s/%s#%d is already in the right milestone" owner repo
             number) ;
        return () )
      else
        issue_milestone_mutation ~token ~issue ~milestone
        <&> post_comment ~token issue
              "The milestone of this issue was changed to reflect the one of \
               the pull request that closed it."
  | Error s ->
      print_endline (f "Error: %s" s) ;
      return ()
