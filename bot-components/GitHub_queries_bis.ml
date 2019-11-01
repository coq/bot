open Base
open Lwt
open Utils

let executable_query (query, kvariables, parse) ~token =
  kvariables (fun variables ->
      let uri = Uri.of_string "https://api.github.com/graphql" in
      let headers =
        Cohttp.Header.of_list
          [("Authorization", "bearer " ^ token); ("User-Agent", "coqbot")]
      in
      let body = `Assoc [("query", `String query); ("variables", variables)] in
      let serialized_body = Yojson.Basic.to_string body in
      Cohttp_lwt_unix.Client.post ~headers ~body:(`String serialized_body) uri
      >>= fun (rsp, body) ->
      Cohttp_lwt.Body.to_string body
      >|= fun body' ->
      match Cohttp.Code.(code_of_status rsp.status |> is_success) with
      | false -> Error body'
      | true -> (
        try Ok (Yojson.Basic.from_string body' |> parse) with
        | Yojson.Json_error err -> Error (f "Json error: %s" err)
        | Yojson.Basic.Util.Type_error (err, _) ->
            Error (f "Json type error: %s" err) ) )

let issue_milestone_query =
  executable_query
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
                      id
                      milestone { id }
                    }
                    ... on Commit {
                      associatedPullRequests(first: 2) {
                        nodes {
                          id
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

type closer_info = {pull_request_id: string; milestone_id: string option}

let closer_info_of_pr pr =
  { milestone_id= pr#milestone |> Option.map ~f:(fun milestone -> milestone#id)
  ; pull_request_id= pr#id }

type 'a closed_by =
  | ClosedByPullRequest of 'a
  | ClosedByCommit
  (* Only used when commit is not associated to a PR *)
  | ClosedByOther

let closer_info_option_of_closer closer =
  match closer with
  | None -> Ok ClosedByOther
  | Some (`PullRequest pr) -> Ok (ClosedByPullRequest (closer_info_of_pr pr))
  | Some (`Commit commit) -> (
    match commit#associatedPullRequests with
    | None -> Ok ClosedByCommit
    | Some prs -> (
      match prs#nodes with
      | Some [] -> Ok ClosedByCommit
      | Some [Some pr] -> Ok (ClosedByPullRequest (closer_info_of_pr pr))
      | Some (_ :: _) ->
          Error "Closing commit associated to several pull requests."
      | _ -> Error "Closer query response is not well-formed." ) )

type issue_closer_info =
  {issue_id: string; milestone_id: string option; closer: closer_info}

let issue_closer_info_of_resp ~owner ~repo ~number resp =
  match resp#repository with
  | None -> Error (f "Unknown repository %s/%s." owner repo)
  | Some repository -> (
    match repository#issue with
    | None -> Error (f "Unknown issue %s/%s#%d." owner repo number)
    | Some issue -> (
      match (issue#timelineItems)#nodes with
      | Some [Some (`ClosedEvent event)] ->
          event#closer |> closer_info_option_of_closer
          |> Result.map ~f:(function
               | ClosedByPullRequest closer_info ->
                   ClosedByPullRequest
                     { issue_id= issue#id
                     ; milestone_id=
                         issue#milestone
                         |> Option.map ~f:(fun milestone -> milestone#id)
                     ; closer= closer_info }
               | (ClosedByCommit | ClosedByOther) as reason -> reason )
      | _ -> Error (f "No close event for issue %s/%s#%d." owner repo number) )
    )

let get_issue_closer_info ~token
    ({owner; repo; number} : GitHub_subscriptions.issue) =
  issue_milestone_query ~token ~owner ~repo ~number ()
  >|= Result.map_error ~f:(fun err ->
          f "Query issue_milestone failed with %s" err )
  >|= Result.bind ~f:(issue_closer_info_of_resp ~owner ~repo ~number)
