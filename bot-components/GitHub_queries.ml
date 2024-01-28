open Base
open Bot_info
open GitHub_types
open Lwt
open Utils

let send_graphql_query = GraphQL_query.send_graphql_query ~api:GitHub

let extract_backport_info ~(bot_info : Bot_info.t) description :
    backport_info list =
  let main_regexp =
    "backport to \\([^ ]*\\) (.*move rejected PRs to: "
    ^ "https://github.com/[^/]*/[^/]*/milestone/\\([0-9]+\\)" ^ ")"
  in
  let begin_regexp = bot_info.github_name ^ ": \\(.*\\)$" in
  let backport_info_unit = main_regexp ^ "; \\(.*\\)$" in
  let end_regexp = main_regexp in
  let rec aux description =
    if string_match ~regexp:backport_info_unit description then
      let backport_to = Str.matched_group 1 description in
      let rejected_milestone =
        Str.matched_group 2 description |> Int.of_string
      in
      Str.matched_group 3 description
      |> aux
      |> List.cons {backport_to; rejected_milestone}
    else if string_match ~regexp:end_regexp description then
      let backport_to = Str.matched_group 1 description in
      let rejected_milestone =
        Str.matched_group 2 description |> Int.of_string
      in
      [{backport_to; rejected_milestone}]
    else []
  in
  if string_match ~regexp:begin_regexp description then
    Str.matched_group 1 description |> aux
  else []

let get_pull_request_cards ~bot_info ~owner ~repo ~number =
  let open GitHub_GraphQL.PullRequest_Cards in
  makeVariables ~owner ~repo ~number ()
  |> serializeVariables |> variablesToJson
  |> send_graphql_query ~bot_info ~query
       ~parse:(Fn.compose parse unsafe_fromJson)
  >|= function
  | Ok result -> (
    match result.repository with
    | Some result -> (
      match result.pullRequest with
      | Some result ->
          let items =
            match result.projectItems.items with
            | None ->
                []
            | Some items ->
                items |> Array.to_list |> List.filter_opt
                |> List.map ~f:(fun item ->
                       (GitHub_ID.of_string item.item_id, item.projectV2.number) )
          in
          Ok items
      | None ->
          Error (f "Pull request %s/%s#%d does not exist." owner repo number) )
    | None ->
        Error (f "Repository %s/%s does not exist." owner repo) )
  | Error err ->
      Error err

let get_pull_request_milestone ~bot_info ~pr_id =
  let open GitHub_GraphQL.PullRequest_Milestone in
  let pr_id = GitHub_ID.to_string pr_id in
  makeVariables ~pr_id () |> serializeVariables |> variablesToJson
  |> send_graphql_query ~bot_info ~query
       ~parse:(Fn.compose parse unsafe_fromJson)
  >|= Result.bind ~f:(fun result ->
          match result.node with
          | Some (`PullRequest result) -> (
            match result.milestone with
            | Some milestone ->
                Ok
                  ( milestone.description
                  |> Option.map ~f:(extract_backport_info ~bot_info)
                  |> Option.value ~default:[] )
            | None ->
                Ok [] )
          | Some _ ->
              Error (f "Node %s is not a pull request." pr_id)
          | None ->
              Error (f "Pull request %s does not exist." pr_id) )

let get_pull_request_id_and_milestone ~bot_info ~owner ~repo ~number =
  let open GitHub_GraphQL.PullRequest_ID_and_Milestone in
  makeVariables ~owner ~repo ~number ()
  |> serializeVariables |> variablesToJson
  |> send_graphql_query ~bot_info ~query
       ~parse:(Fn.compose parse unsafe_fromJson)
  >|= Result.bind ~f:(fun result ->
          match result.repository with
          | None ->
              Error (f "Repository %s/%s does not exist." owner repo)
          | Some result -> (
            match result.pullRequest with
            | None ->
                Error
                  (f "Pull request %s/%s#%d does not exist." owner repo number)
            | Some pr -> (
              match pr.milestone with
              | None ->
                  Error
                    (f "Pull request %s/%s#%d does not have a milestone." owner
                       repo number )
              | Some milestone ->
                  Ok
                    ( GitHub_ID.of_string pr.id
                    , milestone.description
                      |> Option.map ~f:(extract_backport_info ~bot_info)
                      |> Option.value ~default:[] ) ) ) )

let get_pull_request_id ~bot_info ~owner ~repo ~number =
  let open GitHub_GraphQL.PullRequest_ID in
  makeVariables ~owner ~repo ~number ()
  |> serializeVariables |> variablesToJson
  |> send_graphql_query ~bot_info ~query
       ~parse:(Fn.compose parse unsafe_fromJson)
  >|= Result.bind ~f:(fun result ->
          match result.repository with
          | None ->
              Error (f "Repository %s/%s does not exist." owner repo)
          | Some result -> (
            match result.pullRequest with
            | None ->
                Error
                  (f "Pull request %s/%s#%d does not exist." owner repo number)
            | Some pr ->
                Ok (GitHub_ID.of_string pr.id) ) )

let get_milestone_id ~bot_info ~owner ~repo ~number =
  let open GitHub_GraphQL.Milestone_ID in
  makeVariables ~owner ~repo ~number ()
  |> serializeVariables |> variablesToJson
  |> send_graphql_query ~bot_info ~query
       ~parse:(Fn.compose parse unsafe_fromJson)
  >|= Result.bind ~f:(fun result ->
          match result.repository with
          | None ->
              Error (f "Repository %s/%s does not exist." owner repo)
          | Some result -> (
            match result.milestone with
            | None ->
                Error
                  (f "Milestone %d does not exist in repository %s/%s." number
                     owner repo )
            | Some milestone ->
                Ok (GitHub_ID.of_string milestone.id) ) )

let team_membership_of_resp ~org ~team ~user resp =
  let open GitHub_GraphQL.TeamMembership in
  match resp.organization with
  | None ->
      Error (f "Organization %s does not exist." org)
  | Some resp -> (
    match resp.team with
    | None ->
        Error (f "Team @%s/%s does not exist." org team)
    | Some resp -> (
      match resp.members.nodes with
      | Some members
        when members
             |> Array.exists ~f:(function
                  | Some member when String.equal member.login user ->
                      true
                  | _ ->
                      false ) ->
          Ok true
      | _ ->
          Ok false ) )

let get_team_membership ~bot_info ~org ~team ~user =
  let open GitHub_GraphQL.TeamMembership in
  makeVariables ~org ~team ~user ()
  |> serializeVariables |> variablesToJson
  |> send_graphql_query ~bot_info ~query
       ~parse:(Fn.compose parse unsafe_fromJson)
  >|= Result.map_error ~f:(fun err ->
          f "Query get_team_membership failed with %s" err )
  >|= Result.bind ~f:(team_membership_of_resp ~org ~team ~user)

let pull_request_info_of_resp ~owner ~repo ~number resp =
  let open GitHub_GraphQL.PullRequest_Refs in
  match resp.repository with
  | None ->
      Error (f "Unknown repository %s/%s." owner repo)
  | Some repository -> (
    match repository.pullRequest with
    | None ->
        Error (f "Unknown pull request %s/%s#%d." owner repo number)
    | Some pull_request -> (
      match
        ( pull_request.commits.nodes
        , pull_request.baseRepository
        , pull_request.headRepository )
      with
      | None, _, _ ->
          Error
            (f "No commits found for pull request %s/%s#%d." owner repo number)
      | _, None, _ ->
          Error
            (f "No base repository found for pull request %s/%s#%d." owner repo
               number )
      | _, _, None ->
          Error
            (f "No head repository found for pull request %s/%s#%d." owner repo
               number )
      | Some nodes, Some base_repo, Some head_repo -> (
          let commits = nodes |> Array.to_list |> List.filter_opt in
          match List.hd commits with
          | None ->
              Error
                (f "No commits found for pull request %s/%s#%d." owner repo
                   number )
          | Some node ->
              Ok
                { issue= GitHub_ID.of_string pull_request.id
                ; base=
                    { branch=
                        {repo_url= base_repo.url; name= pull_request.baseRefName}
                    ; sha= pull_request.baseRefOid }
                ; head=
                    { branch=
                        {repo_url= head_repo.url; name= pull_request.headRefName}
                    ; sha= pull_request.headRefOid }
                ; merged= pull_request.merged
                ; last_commit_message= Some node.commit.message } ) ) )

let get_pull_request_refs ~bot_info ~owner ~repo ~number =
  let open GitHub_GraphQL.PullRequest_Refs in
  makeVariables ~owner ~repo ~number ()
  |> serializeVariables |> variablesToJson
  |> send_graphql_query ~bot_info ~query
       ~parse:(Fn.compose parse unsafe_fromJson)
  >|= Result.map_error ~f:(fun err ->
          f "Query pull_request_info failed with %s" err )
  >|= Result.bind ~f:(pull_request_info_of_resp ~owner ~repo ~number)

let pull_request_reviews_info_of_resp ~owner ~repo ~number resp :
    (pull_request_reviews_info, string) Result.t =
  let open GitHub_GraphQL.PullRequestReviewsInfo in
  let open MergePullRequestInfo in
  match resp.repository with
  | None ->
      Error (f "Unknown repository %s/%s." owner repo)
  | Some repository -> (
    match repository.pullRequest with
    | None ->
        Error (f "Unknown pull request %s/%s#%d." owner repo number)
    | Some pull_request -> (
      match
        ( pull_request.baseRef
        , pull_request.files
        , pull_request.latestReviews
        , pull_request.latestOpinionatedReviews )
      with
      | None, _, _, _ ->
          Error "No base ref found."
      | _, None, _, _ ->
          Error "No files found."
      | _, _, None, _ ->
          Error "No reviews found."
      | _, _, _, None ->
          Error "No opinionated reviews found."
      | Some baseRef, Some files, Some reviews, Some opinionatedReviews -> (
        match (reviews.nodes, opinionatedReviews.nodes) with
        | None, _ ->
            Error "No reviews found."
        | _, None ->
            Error "No opinionated reviews found."
        | Some reviews, Some opinionatedReviews ->
            let approved_reviews =
              let open Reviews in
              opinionatedReviews |> Array.to_list
              |> List.filter_map
                   ~f:
                     (Option.bind ~f:(fun review ->
                          review.author
                          |> Option.bind ~f:(fun author ->
                                 match review.state with
                                 | `APPROVED ->
                                     Some author.login
                                 | _ ->
                                     None ) ) )
            in
            let comment_reviews =
              let open Reviews in
              reviews |> Array.to_list
              |> List.filter_map
                   ~f:
                     (Option.bind ~f:(fun review ->
                          review.author
                          |> Option.bind ~f:(fun author ->
                                 if
                                   List.mem ~equal:String.equal approved_reviews
                                     author.login
                                 then None
                                 else Some author.login ) ) )
            in
            Ok
              { baseRef= baseRef.name
              ; files=
                  ( match files.nodes with
                  | None ->
                      []
                  | Some files ->
                      files |> Array.to_list |> List.filter_opt
                      |> List.map ~f:(fun file -> file.path) )
              ; last_comments=
                  ( match pull_request.comments.nodes with
                  | None ->
                      []
                  | Some comments ->
                      comments |> Array.to_list |> List.filter_opt
                      |> List.filter_map ~f:(fun c ->
                             c.author
                             |> Option.map ~f:(fun a : comment ->
                                    { id= GitHub_ID.of_string c.id
                                    ; author= a.login
                                    ; created_by_email= c.createdViaEmail } ) )
                  )
              ; approved_reviews
              ; comment_reviews
              ; review_decision=
                  ( match pull_request.reviewDecision with
                  | None ->
                      NONE
                  | Some (`FutureAddedValue _) ->
                      NONE
                  | Some `CHANGES_REQUESTED ->
                      CHANGES_REQUESTED
                  | Some `APPROVED ->
                      APPROVED
                  | Some `REVIEW_REQUIRED ->
                      REVIEW_REQUIRED ) } ) ) )

let get_pull_request_reviews_refs ~bot_info ~owner ~repo ~number =
  let open GitHub_GraphQL.PullRequestReviewsInfo.MergePullRequestInfo in
  makeVariables ~owner ~repo ~number ()
  |> serializeVariables |> variablesToJson
  |> send_graphql_query ~bot_info ~query
       ~parse:(Fn.compose parse unsafe_fromJson)
  >|= Result.map_error ~f:(fun err ->
          f "Query pull_request_reviews_info failed with %s" err )
  >|= Result.bind ~f:(pull_request_reviews_info_of_resp ~owner ~repo ~number)

let file_content_of_resp ~owner ~repo resp : (string option, string) Result.t =
  let open GitHub_GraphQL.FileContent in
  match resp.repository with
  | None ->
      Error (f "Unknown repository %s/%s." owner repo)
  | Some repository -> (
    match repository.file with
    | Some (`Blob b) ->
        Ok b.text
    | Some (`UnspecifiedFragment _) ->
        Ok None
    | None ->
        Ok None )

let get_file_content ~bot_info ~owner ~repo ~branch ~file_name =
  let open GitHub_GraphQL.FileContent in
  makeVariables ~owner ~repo ~file:(branch ^ ":" ^ file_name) ()
  |> serializeVariables |> variablesToJson
  |> send_graphql_query ~bot_info ~query
       ~parse:(Fn.compose parse unsafe_fromJson)
  >|= Result.map_error ~f:(fun err -> f "Query file_content failed with %s" err)
  >|= Result.bind ~f:(file_content_of_resp ~owner ~repo)

let default_branch_of_resp ~owner ~repo resp =
  let open GitHub_GraphQL.DefaultBranch in
  match resp.repository with
  | None ->
      Error (f "Unknown repository %s/%s." owner repo)
  | Some repository -> (
    match repository.defaultBranchRef with
    | None ->
        Error "No default branch found."
    | Some default_branch ->
        Ok (default_branch.name : string) )

let get_default_branch ~bot_info ~owner ~repo =
  let open GitHub_GraphQL.DefaultBranch in
  makeVariables ~owner ~repo ()
  |> serializeVariables |> variablesToJson
  |> send_graphql_query ~bot_info ~query
       ~parse:(Fn.compose parse unsafe_fromJson)
  >|= Result.map_error ~f:(fun err ->
          f "Query get_default_branch failed with %s" err )
  >|= Result.bind ~f:(default_branch_of_resp ~owner ~repo)

let closer_info_of_pr pr =
  let open GitHub_GraphQL.Issue_Milestone in
  let open PullRequest in
  { milestone_id=
      pr.milestone
      |> Option.map ~f:(fun milestone ->
             GitHub_ID.of_string milestone.Milestone.id )
  ; pull_request_id= GitHub_ID.of_string pr.id }

let closer_info_option_of_closer closer =
  let open GitHub_GraphQL.Issue_Milestone in
  let open IssueMilestone in
  match closer with
  | None | Some (`FutureAddedValue _) ->
      Ok ClosedByOther
  | Some (`PullRequest pr) ->
      Ok (ClosedByPullRequest (closer_info_of_pr pr))
  | Some (`Commit commit) -> (
    match commit.associatedPullRequests with
    | None ->
        Ok ClosedByCommit
    | Some prs -> (
      match prs.nodes with
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
  let open GitHub_GraphQL.Issue_Milestone in
  let open IssueMilestone in
  match resp.repository with
  | None ->
      Error (f "Unknown repository %s/%s." owner repo)
  | Some repository -> (
    match repository.issue with
    | None ->
        Error (f "Unknown issue %s/%s#%d." owner repo number)
    | Some issue -> (
      match issue.timelineItems.nodes with
      | Some [|Some (`ClosedEvent event)|] ->
          event.closer |> closer_info_option_of_closer
          |> Result.map ~f:(function
               | ClosedByPullRequest closer_info ->
                   ClosedByPullRequest
                     { issue_id= GitHub_ID.of_string issue.id
                     ; milestone_id=
                         issue.milestone
                         |> Option.map ~f:(fun milestone ->
                                GitHub_ID.of_string milestone.Milestone.id )
                     ; closer= closer_info }
               | (ClosedByCommit | ClosedByOther | NoCloseEvent) as reason ->
                   reason )
      | _ ->
          Result.return NoCloseEvent ) )

let get_issue_closer_info ~bot_info ({owner; repo; number} : issue) =
  let open GitHub_GraphQL.Issue_Milestone.IssueMilestone in
  (* Workaround for #167 until teamwalnut/graphql-ppx#270 is fixed. *)
  let query =
    "query issueMilestone($owner: String!, $repo: String!, $number: Int!) {\n\
    \    repository(owner: $owner, name: $repo) {\n\
    \      issue(number: $number) {\n\
    \        id\n\
    \        milestone {\n\
    \          ...Milestone\n\
    \        }\n\
    \        timelineItems(itemTypes: [CLOSED_EVENT], last: 1) {\n\
    \          nodes {\n\
    \            __typename\n\
    \            ...on ClosedEvent {\n\
    \              closer {\n\
    \                __typename\n\
    \                ...on PullRequest {\n\
    \                  ...PullRequest\n\
    \                }\n\
    \                ...on Commit {\n\
    \                  associatedPullRequests(first: 2) {\n\
    \                    nodes {\n\
    \                      ...PullRequest\n\
    \                    }\n\
    \                  }\n\
    \                }\n\
    \              }\n\
    \            }\n\
    \          }\n\
    \        }\n\
    \      }\n\
    \    }\n\
    \  }\n\
    \  fragment Milestone on Milestone {\n\
    \    id\n\
    \  }\n\
    \  fragment PullRequest on PullRequest {\n\
    \    id\n\
    \    milestone {\n\
    \      ...Milestone\n\
    \    }\n\
    \  }"
  in
  makeVariables ~owner ~repo ~number ()
  |> serializeVariables |> variablesToJson
  |> send_graphql_query ~bot_info ~query
       ~parse:(Fn.compose parse unsafe_fromJson)
  >|= Result.map_error ~f:(fun err ->
          f "Query issue_milestone failed with %s" err )
  >|= Result.bind ~f:(issue_closer_info_of_resp ~owner ~repo ~number)

let repo_id_of_resp ~owner ~repo resp =
  let open GitHub_GraphQL.RepoId in
  match resp.repository with
  | None ->
      Error (f "Unknown repository %s/%s." owner repo)
  | Some repository ->
      Ok (GitHub_ID.of_string repository.id)

let get_repository_id ~bot_info ~owner ~repo =
  let open GitHub_GraphQL.RepoId in
  makeVariables ~owner ~repo ()
  |> serializeVariables |> variablesToJson
  |> send_graphql_query ~bot_info ~query
       ~parse:(Fn.compose parse unsafe_fromJson)
  >|= Result.map_error ~f:(fun err ->
          f "Query get_repository_id failed with %s" err )
  >|= Result.bind ~f:(repo_id_of_resp ~owner ~repo)

let get_status_check ~bot_info ~owner ~repo ~commit ~context =
  let open GitHub_GraphQL.GetCheckRuns in
  makeVariables ~owner ~repo ~commit ~context ~appId:bot_info.app_id ()
  |> serializeVariables |> variablesToJson
  |> send_graphql_query ~bot_info ~query
       ~parse:(Fn.compose parse unsafe_fromJson)
  >|= Result.map_error ~f:(fun err ->
          f "Query get_status_check failed with %s" err )
  >|= Result.bind ~f:(fun resp ->
          match resp.repository with
          | None ->
              Error (f "Unknown repository %s/%s." owner repo)
          | Some repository -> (
            match repository.obj with
            | Some (`Commit commit) ->
                let check =
                  match commit.checkSuites with
                  | None ->
                      false
                  | Some checkSuites -> (
                    match checkSuites.nodes with
                    | None ->
                        false
                    | Some checkSuites -> (
                      match Array.to_list checkSuites with
                      | [] | [None] | _ :: _ :: _ ->
                          false
                      | [Some checkSuite] -> (
                        match checkSuite.checkRuns with
                        | None ->
                            false
                        | Some checkRuns -> (
                          match checkRuns.nodes with
                          | None ->
                              false
                          | Some checkRuns -> (
                            match Array.to_list checkRuns with
                            | [] | [None] | _ :: _ :: _ ->
                                false
                            | [Some checkRun] -> (
                              match checkRun.databaseId with
                              | None ->
                                  false
                              | Some _ ->
                                  true ) ) ) ) ) )
                in
                let status =
                  match commit.status with
                  | None ->
                      false
                  | Some status -> (
                    match status.context with None -> false | Some _ -> true )
                in
                Ok (check || status)
            | _ ->
                Error (f "Unkown commit %s/%s@%s." owner repo commit) ) )

let get_check_tab_info checkRun =
  let open GitHub_GraphQL.GetBaseAndHeadChecks.CheckRuns in
  {name= checkRun.name; summary= checkRun.summary; text= checkRun.text}

type base_and_head_checks_info =
  { pr_id: GitHub_ID.t
  ; base_checks: (check_tab_info * bool option, string * string) Result.t list
  ; head_checks: (check_tab_info * bool option, string * string) Result.t list
  ; draft: bool
  ; labels: string list }

let get_base_and_head_checks ~bot_info ~owner ~repo ~pr_number ~base ~head =
  let appId = bot_info.app_id in
  let open GitHub_GraphQL.GetBaseAndHeadChecks in
  let open GetChecks in
  makeVariables ~appId ~owner ~repo ~prNumber:pr_number ~base ~head ()
  |> serializeVariables |> variablesToJson
  |> send_graphql_query ~bot_info ~query
       ~parse:(Fn.compose parse unsafe_fromJson)
  >|= Result.bind ~f:(fun resp ->
          resp.repository
          |> Result.of_option ~error:(f "Unknown repository %s/%s." owner repo) )
  >|= Result.bind ~f:(fun repository ->
          match (repository.pullRequest, repository.base, repository.head) with
          | None, _, _ ->
              Error (f "Unknown pull request %s/%s#%d" owner repo pr_number)
          | _, None, _ ->
              Error (f "Unkown base commit %s/%s@%s." owner repo base)
          | _, _, None ->
              Error (f "Unkown head commit %s/%s@%s." owner repo head)
          | _, Some (`UnspecifiedFragment _), _
          | _, _, Some (`UnspecifiedFragment _) ->
              Error "Unspecified fragment."
          | Some pull_request, Some (`Commit base_obj), Some (`Commit head_obj)
            -> (
              let extract_check_suites checkSuites =
                let open CheckSuites in
                checkSuites
                |> Option.bind ~f:(fun checkSuites -> checkSuites.nodes)
                |> Option.map ~f:Array.to_list
                |> Option.map ~f:List.filter_opt
              in
              match
                ( extract_check_suites base_obj.checkSuites
                , extract_check_suites head_obj.checkSuites )
              with
              | (None | Some []), _ ->
                  Error
                    (f "No check suite %d for base commit %s/%s@%s." appId owner
                       repo base )
              | _, (None | Some []) ->
                  Error
                    (f "No check suite %d for head commit %s/%s@%s." appId owner
                       repo head )
              | Some (_ :: _ :: _), _ ->
                  Error
                    (f
                       "Got more than one checkSuite %d for base commit \
                        %s/%s@%s."
                       appId owner repo base )
              | _, Some (_ :: _ :: _) ->
                  Error
                    (f
                       "Got more than one checkSuite %d for head commit \
                        %s/%s@%s."
                       appId owner repo head )
              | Some [baseCheckSuite], Some [headCheckSuite] -> (
                  let extract_check_runs checkRuns =
                    let open CheckRuns in
                    checkRuns
                    |> Option.bind ~f:(fun checkRuns -> checkRuns.nodes)
                    |> Option.map ~f:Array.to_list
                    |> Option.map ~f:List.filter_opt
                  in
                  match
                    ( extract_check_runs baseCheckSuite.checkRuns
                    , extract_check_runs headCheckSuite.checkRuns )
                  with
                  | None, _ ->
                      Error
                        (f "No check runs %d for base commit %s/%s@%s." appId
                           owner repo base )
                  | _, None ->
                      Error
                        (f "No check runs %d for head commit %s/%s@%s." appId
                           owner repo head )
                  | Some baseCheckRuns, Some headCheckRuns ->
                      let open CheckRuns in
                      let completed_runs checkRuns description commit =
                        checkRuns
                        |> List.map ~f:(fun checkRun ->
                               match checkRun.conclusion with
                               | None ->
                                   (* In progress *)
                                   Ok (get_check_tab_info checkRun, None)
                               | Some conclusion -> (
                                 match conclusion with
                                 | `ACTION_REQUIRED ->
                                     Error
                                       ( checkRun.name
                                       , f
                                           "A check run (%s) requires an \
                                            action for %s commit %s/%s@%s."
                                           checkRun.name description owner repo
                                           commit )
                                 | `CANCELLED ->
                                     Error
                                       ( checkRun.name
                                       , f
                                           "A check run (%s) was cancelled for \
                                            %s commit %s/%s@%s."
                                           checkRun.name description owner repo
                                           commit )
                                 | `SKIPPED ->
                                     Error
                                       ( checkRun.name
                                       , f
                                           "A check run (%s) was skipped for \
                                            %s commit %s/%s@%s."
                                           checkRun.name description owner repo
                                           commit )
                                 | `STALE ->
                                     Error
                                       ( checkRun.name
                                       , f
                                           "A check run (%s) is stale for %s \
                                            commit %s/%s@%s."
                                           checkRun.name description owner repo
                                           commit )
                                 | `TIMED_OUT ->
                                     Error
                                       ( checkRun.name
                                       , f
                                           "A check run (%s) timed out for %s \
                                            commit %s/%s@%s."
                                           checkRun.name description owner repo
                                           commit )
                                 | `STARTUP_FAILURE ->
                                     Error
                                       ( checkRun.name
                                       , f
                                           "A check run (%s) failed at startup \
                                            for %s commit %s/%s@%s."
                                           checkRun.name description owner repo
                                           commit )
                                 | `FAILURE | `NEUTRAL ->
                                     Ok (get_check_tab_info checkRun, Some false)
                                 | `SUCCESS ->
                                     Ok (get_check_tab_info checkRun, Some true)
                                 | `FutureAddedValue status ->
                                     Error
                                       ( checkRun.name
                                       , f
                                           "Unknown status %s for check run %s \
                                            (for %s commit %s/%s@%s"
                                           status checkRun.name description
                                           owner repo commit ) ) )
                      in
                      let labels : string list option =
                        let open GetChecks in
                        let open Option in
                        pull_request.labels
                        >>= fun labels ->
                        labels.nodes
                        >>= fun labels ->
                        Array.to_list labels
                        |> List.filter_map
                             ~f:(Option.map ~f:(fun label -> label.name))
                        |> Option.return
                      in
                      Ok
                        { pr_id= GitHub_ID.of_string pull_request.id
                        ; base_checks= completed_runs baseCheckRuns "base" base
                        ; head_checks= completed_runs headCheckRuns "head" head
                        ; draft= pull_request.isDraft
                        ; labels= Option.value ~default:[] labels } ) ) )

let get_pipeline_summary ~bot_info ~owner ~repo ~head =
  let appId = bot_info.app_id in
  let open GitHub_GraphQL.GetPipelineSummary in
  makeVariables ~appId ~owner ~repo ~head ()
  |> serializeVariables |> variablesToJson
  |> send_graphql_query ~bot_info ~query
       ~parse:(Fn.compose parse unsafe_fromJson)
  >|= Result.bind ~f:(fun resp ->
          resp.repository
          |> Result.of_option ~error:(f "Unknown repository %s/%s." owner repo) )
  >|= Result.bind ~f:(fun repository ->
          match repository.getPipelineSummaryCommit with
          | None ->
              Error (f "Unknown head commit %s/%s@%s." owner repo head)
          | Some (`UnspecifiedFragment _) ->
              Error "Unspecified fragment."
          | Some (`Commit head_obj) -> (
              let extract_check_suites checkSuites =
                checkSuites
                |> Option.bind ~f:(fun checkSuites -> checkSuites.nodes)
                |> Option.map ~f:Array.to_list
                |> Option.map ~f:List.filter_opt
              in
              match extract_check_suites head_obj.checkSuites with
              | None | Some [] ->
                  Error
                    (f "No check suite %d for head commit %s/%s@%s." appId owner
                       repo head )
              | Some (_ :: _ :: _) ->
                  Error
                    (f
                       "Got more than one checkSuite %d for head commit \
                        %s/%s@%s."
                       appId owner repo head )
              | Some [headCheckSuite] -> (
                match headCheckSuite.getPipelineSummaryCheckRuns with
                | None ->
                    Error
                      (f
                         "No GitLab CI pipeline summary for head commit \
                          %s/%s@%s."
                         owner repo head )
                | Some checkRuns -> (
                  match
                    checkRuns.nodes
                    |> Option.map ~f:Array.to_list
                    |> Option.map ~f:List.filter_opt
                  with
                  | None | Some [] ->
                      Error
                        (f
                           "No GitLab CI pipeline summary for head commit \
                            %s/%s@%s."
                           owner repo head )
                  | Some [headCheckRun] ->
                      Stdlib.Option.to_result
                        ~none:
                          (f
                             "No GitLab CI pipeline summary for head commit \
                              %s/%s@%s."
                             owner repo head )
                        headCheckRun.summary
                  | Some (_ :: _ :: _) ->
                      Error
                        (f
                           "Got more than one checkRun for head commit \
                            %s/%s@%s."
                           owner repo head ) ) ) ) )

let get_list getter =
  let rec get_list_aux cursor accu =
    getter cursor
    >>= function
    | Ok (ans, cursor) -> (
        let accu = List.rev_append ans accu in
        match cursor with
        | None ->
            Lwt.return_ok (List.rev accu)
        | Some cursor ->
            get_list_aux (Some cursor) accu )
    | Error err ->
        Lwt.return_error err
  in
  get_list_aux None []

let get_open_pull_requests_with_label ~bot_info ~owner ~repo ~label =
  let open GitHub_GraphQL.GetOpenPullRequestWithLabel in
  let getter cursor =
    makeVariables ~owner ~repo ~label ~len:100 ?cursor ()
    |> serializeVariables |> variablesToJson
    |> send_graphql_query ~bot_info ~query
         ~parse:(Fn.compose parse unsafe_fromJson)
    >>= function
    | Ok result -> (
      match result.repository with
      | Some result ->
          let result = result.pullRequests in
          let next =
            if result.pageInfo.hasNextPage then result.pageInfo.endCursor
            else None
          in
          let data =
            match result.nodes with
            | None ->
                []
            | Some data ->
                let map o =
                  Option.map
                    ~f:(fun o -> (GitHub_ID.of_string o.id, o.number))
                    o
                in
                List.filter_map ~f:map (Array.to_list data)
          in
          Lwt.return_ok (data, next)
      | None ->
          Lwt.return_error (f "Repository %s/%s does not exist." owner repo) )
    | Error err ->
        Lwt.return_error err
  in
  get_list getter

let get_pull_request_label_timeline ~bot_info ~owner ~repo ~pr_number =
  let open GitHub_GraphQL.GetPullRequestLabelTimeline in
  let open GetPullRequestLabelTimeline in
  let getter cursor =
    makeVariables ~owner ~repo ~prNumber:pr_number ~len:100 ?cursor ()
    |> serializeVariables |> variablesToJson
    |> send_graphql_query ~bot_info ~query
         ~parse:(Fn.compose parse unsafe_fromJson)
    >>= function
    | Ok result -> (
      match result.repository with
      | Some result -> (
        match result.pullRequest with
        | Some result ->
            let result = result.timelineItems in
            let next =
              if result.pageInfo.hasNextPage then result.pageInfo.endCursor
              else None
            in
            let data =
              match result.nodes with
              | None ->
                  []
              | Some data ->
                  let cast s =
                    ISO8601.Permissive.date @@ Yojson.Basic.Util.to_string s
                  in
                  let map = function
                    | None ->
                        None
                    | Some (`LabeledEvent e) ->
                        Some (true, e.labelAdded.name, cast e.labeledAt)
                    | Some (`UnlabeledEvent e) ->
                        Some (false, e.labelRemoved.name, cast e.unlabeledAt)
                    | Some (`FutureAddedValue _) ->
                        None
                  in
                  List.filter_map ~f:map (Array.to_list data)
            in
            Lwt.return_ok (data, next)
        | None ->
            Lwt.return_error
              (f "Unknown pull request %s/%s#%d" owner repo pr_number) )
      | None ->
          Lwt.return_error (f "Repository %s/%s does not exist." owner repo) )
    | Error err ->
        Lwt.return_error err
  in
  get_list getter

let get_label ~bot_info ~owner ~repo ~label =
  let open GitHub_GraphQL.GetLabel in
  makeVariables ~owner ~repo ~label ()
  |> serializeVariables |> variablesToJson
  |> send_graphql_query ~bot_info ~query
       ~parse:(Fn.compose parse unsafe_fromJson)
  >|= Result.map_error ~f:(fun err -> f "Query label failed with %s" err)
  >|= Result.bind ~f:(fun result ->
          match result.repository with
          | Some result ->
              Ok
                (Option.map
                   ~f:(fun {id} -> GitHub_ID.of_string id)
                   result.label )
          | None ->
              Error (f "Repository %s/%s does not exist." owner repo) )

let get_pull_request_labels ~bot_info ~owner ~repo ~pr_number =
  let getter cursor =
    let open GitHub_GraphQL.GetPullRequestLabels in
    makeVariables ~owner ~repo ~prNumber:pr_number ?cursor ~len:100 ()
    |> serializeVariables |> variablesToJson
    |> send_graphql_query ~bot_info ~query
         ~parse:(Fn.compose parse unsafe_fromJson)
    >>= function
    | Ok result -> (
      match result.repository with
      | Some result -> (
        match result.pullRequest with
        | Some result -> (
          match result.labels with
          | Some result ->
              let next =
                if result.pageInfo.hasNextPage then result.pageInfo.endCursor
                else None
              in
              let data =
                match result.nodes with
                | None ->
                    []
                | Some data ->
                    let map o = Option.map ~f:(fun o -> o.name) o in
                    List.filter_map ~f:map (Array.to_list data)
              in
              Lwt.return_ok (data, next)
          | None ->
              Lwt.return_error (f "Error querying labels") )
        | None ->
            Lwt.return
            @@ Error (f "Unknown pull request %s/%s#%d" owner repo pr_number) )
      | None ->
          Lwt.return_error (f "Repository %s/%s does not exist." owner repo) )
    | Error err ->
        Lwt.return_error err
  in
  get_list getter

let get_project_field_values ~bot_info ~organization ~project ~field ~options =
  let open GitHub_GraphQL.GetProjectFieldValues in
  makeVariables ~organization ~project ~field ~options ()
  |> serializeVariables |> variablesToJson
  |> send_graphql_query ~bot_info ~query
       ~parse:(Fn.compose parse unsafe_fromJson)
  >>= function
  | Ok result -> (
    match result.organization with
    | Some result -> (
      match result.projectV2 with
      | Some project -> (
        match project.field with
        | Some (`ProjectV2SingleSelectField field) ->
            let options = field.options |> Array.to_list in
            Lwt.return_ok
              ( GitHub_ID.of_string project.id
              , Some
                  ( GitHub_ID.of_string field.id
                  , List.map ~f:(fun {name; id} -> (name, id)) options ) )
        | Some _ ->
            Lwt.return_error (f "Field %s is not a single select field." field)
        | None ->
            (* We consider the field not existing in the project to be
               acceptable, because it can be created then. *)
            Lwt.return_ok (GitHub_ID.of_string project.id, None) )
      | None ->
          Lwt.return_error
            (f "Unknown project %d of organization %s" project organization) )
    | None ->
        Lwt.return_error (f "Organization %s does not exist." organization) )
  | Error err ->
      Lwt.return_error err

type zip_error =
  {zip_contents: string; zip_name: string; entry_name: string; message: string}

let get_artifact_blob ~bot_info ~owner ~repo ~artifact_id =
  generic_get_zip ~bot_info
    (f "repos/%s/%s/actions/artifacts/%s/zip" owner repo artifact_id)
    (let open Zip in
    List.map ~f:(fun (entry, contents) -> (entry.filename, contents)) )
  |> Lwt_result.map_error (fun (zip_contents, zip_name, entry_name, message) ->
         {zip_contents; zip_name; entry_name; message} )
