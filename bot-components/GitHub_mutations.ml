open Base
open Lwt
open Utils

let issue_milestone_query =
  GitHub_queries.executable_query
    [%graphql
      {|
      query issueMilestone($owner: String!, $repo: String!, $number: Int!) {
        repository(owner:$owner, name:$repo) {
          issue(number:$number) {
            id
            milestone { id }
            timelineItems(itemTypes:[CLOSED_EVENT],last:1) {
              nodes {
                __typename
                ... on ClosedEvent {
                  closer {
                    __typename
                    ... on PullRequest {
                      milestone { id }
                    }
                    ... on Commit {
                      associatedPullRequests(first: 2) {
                        nodes {
                          milestone { id }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
      |}]

let issue_milestone ~token ~owner ~repo ~number =
  issue_milestone_query ~token ~owner ~repo ~number ()
  >|= function
  | Ok result -> (
    match result#repository with
    | Some repository -> (
      match repository#issue with
      | Some issue -> (
          let issue_id = issue#id in
          let previous_milestone =
            match issue#milestone with
            | Some milestone -> Some milestone#id
            | None -> None
          in
          match (issue#timelineItems)#nodes with
          | Some [Some (`ClosedEvent event)] -> (
            match event#closer with
            | Some closer -> (
              match closer with
              | `PullRequest pr -> (
                match pr#milestone with
                | Some milestone ->
                    Ok (issue_id, previous_milestone, milestone#id)
                | None ->
                    Error
                      (f "Issue %s/%s#%d was closed by a PR without milestone."
                         owner repo number) )
              | `Commit commit -> (
                match commit#associatedPullRequests with
                | Some prs -> (
                  match prs#nodes with
                  | Some [Some pr] -> (
                    match pr#milestone with
                    | Some milestone ->
                        Ok (issue_id, previous_milestone, milestone#id)
                    | None ->
                        Error
                          (f
                             "Issue %s/%s#%d was closed by a commit \
                              associated to a PR without milestone."
                             owner repo number) )
                  | _ ->
                      Error
                        (Printf.sprintf
                           "Issue %s/%s#%d was closed by a commit with no or \
                            several associated pull requests."
                           owner repo number) )
                | None ->
                    Error
                      (Printf.sprintf
                         "Issue %s/%s#%d was closed by a commit with no \
                          associated pull request."
                         owner repo number) ) )
            | None -> Error "Issue was not closed by PR or commit." )
          | _ ->
              Error (f "No close event for issue %s/%s#%d." owner repo number)
          )
      | None -> Error (f "Unknown issue number %s/%s#%d." owner repo number) )
    | None -> Error (f "Unknown repository %s/%s." owner repo) )
  | Error s -> Error (f "Query issue_milestone failed with %s" s)

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
    ({owner; repo; number} : GitHub_subscriptions.issue) () =
  issue_milestone ~token ~owner ~repo ~number
  >>= function
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
        <&> GitHub_queries.post_comment ~token issue
              "The milestone of this issue was changed to reflect the one of \
               the pull request that closed it."
  | Error s ->
      print_endline (f "Error: %s" s) ;
      return ()
