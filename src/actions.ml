open Base
open Bot_components
open Bot_components.Bot_info
open Bot_components.GitHub_types
open Bot_components.GitLab_types
open Cohttp
open Cohttp_lwt_unix
open Git_utils
open Helpers
open Lwt.Infix

let owner_team_map =
  Map.of_alist_exn
    (module String)
    [("martijnbastiaan-test-org", "martijnbastiaan-test-team")]

let send_status_check ~bot_info job_info pr_num (gh_owner, gh_repo)
    github_repo_full_name repo_full_name context failure_reason =
  let allow_fail =
    match job_info.allow_fail with Some f -> f | None -> false
  in
  let job_url =
    Printf.sprintf "https://gitlab.com/%s/-/jobs/%d" repo_full_name
      job_info.build_id
  in
  if allow_fail then
    Lwt_io.printf "Job is allowed to fail.\n"
    <&>
    (* If we are in a PR branch, we can post a comment instead of
       reporting a failed status check. *)
    match pr_num with
    | Some number -> (
        GitHub_queries.get_pull_request_refs ~bot_info ~owner:gh_owner
          ~repo:gh_repo ~number
        >>= function
        | Ok {issue= id; head}
        (* Commits reported back by get_pull_request_refs are surrounded in double quotes *)
          when String.equal head.sha (f "\"%s\"" job_info.commit) -> (
            let message =
              f "The job [%s](%s) has failed in allow failure mode%s"
                job_info.build_name job_url
                ( if
                  String.equal job_info.build_name
                    "library:ci-fiat_crypto_legacy"
                then "\nping @JasonGross"
                else "" )
            in
            match bot_info.github_token with
            | ACCESS_TOKEN _t ->
                (* Allow failure messages are reported with the Checks API only. *)
                Lwt.return ()
            | INSTALL_TOKEN _t -> (
                GitHub_queries.get_repository_id ~bot_info ~owner:gh_owner
                  ~repo:gh_repo
                >>= function
                | Ok repo_id ->
                    GitHub_mutations.create_check_run ~bot_info ~name:context
                      ~repo_id ~head_sha:job_info.commit ~conclusion:NEUTRAL
                      ~status:COMPLETED
                      ~title:(failure_reason ^ " on GitLab CI")
                      ~text:"" ~details_url:job_url ~summary:message
                    <&>
                    if
                      String.equal job_info.build_name
                        "library:ci-fiat_crypto_legacy"
                    then GitHub_mutations.post_comment ~bot_info ~id ~message
                    else Lwt.return ()
                | Error _ ->
                    Lwt.return () ) )
        | Ok {head} ->
            Lwt_io.printf
              "We are on a PR branch but the commit (%s) is not the current \
               head of the PR (%s). Doing nothing.\n"
              job_info.commit head.sha
        | Error err ->
            Lwt_io.printf
              "Couldn't get a database id for %s#%d because the following \
               error occured:\n\
               %s\n"
              repo_full_name number err )
    | None ->
        Lwt_io.printf "We are not on a PR branch. Doing nothing.\n"
  else
    Lwt_io.printf "Pushing a status check...\n"
    >>= fun () ->
    match bot_info.github_token with
    | ACCESS_TOKEN _t ->
        GitHub_mutations.send_status_check ~repo_full_name:github_repo_full_name
          ~commit:job_info.commit ~state:"failure" ~url:job_url ~context
          ~description:(failure_reason ^ " on GitLab CI")
          ~bot_info
    | INSTALL_TOKEN _t -> (
        GitHub_queries.get_repository_id ~bot_info ~owner:gh_owner ~repo:gh_repo
        >>= function
        | Ok repo_id ->
            GitHub_mutations.create_check_run ~bot_info ~name:context ~repo_id
              ~head_sha:job_info.commit ~conclusion:FAILURE ~status:COMPLETED
              ~title:(failure_reason ^ " on GitLab CI")
              ~text:"" ~details_url:job_url ~summary:""
        | Error _ ->
            Lwt.return () )

let send_url ~bot_info (gh_owner, gh_repo) job_info github_repo_full_name
    repo_full_name (kind, url) =
  (fun () ->
    let context = Printf.sprintf "%s: %s artifact" job_info.build_name kind in
    let description_base = Printf.sprintf "Link to %s build artifact" kind in
    url |> Uri.of_string |> Client.get
    >>= fun (resp, _) ->
    if resp |> Response.status |> Code.code_of_status |> Int.equal 200 then
      match bot_info.github_token with
      | ACCESS_TOKEN _t ->
          GitHub_mutations.send_status_check
            ~repo_full_name:github_repo_full_name ~commit:job_info.commit
            ~state:"success" ~url ~context ~description:(description_base ^ ".")
            ~bot_info
      | INSTALL_TOKEN _t -> (
          GitHub_queries.get_repository_id ~bot_info ~owner:gh_owner
            ~repo:gh_repo
          >>= function
          | Ok repo_id ->
              GitHub_mutations.create_check_run ~bot_info ~name:context
                ~status:COMPLETED ~repo_id ~head_sha:job_info.commit
                ~details_url:url ~conclusion:SUCCESS
                ~title:(description_base ^ ".") ~text:"" ~summary:""
          | Error _ ->
              Lwt.return () )
    else
      Lwt_io.printf "But we didn't get a 200 code when checking the URL.\n"
      >>= fun () ->
      let job_url =
        Printf.sprintf "https://gitlab.com/%s/-/jobs/%d" repo_full_name
          job_info.build_id
      in
      match bot_info.github_token with
      | ACCESS_TOKEN _t ->
          GitHub_mutations.send_status_check
            ~repo_full_name:github_repo_full_name ~commit:job_info.commit
            ~state:"failure" ~url:job_url ~context
            ~description:(description_base ^ ": not found.")
            ~bot_info
      | INSTALL_TOKEN _t -> (
          GitHub_queries.get_repository_id ~bot_info ~owner:gh_owner
            ~repo:gh_repo
          >>= function
          | Ok repo_id ->
              GitHub_mutations.create_check_run ~bot_info ~name:context
                ~status:COMPLETED ~repo_id ~head_sha:job_info.commit
                ~conclusion:FAILURE
                ~title:(description_base ^ ": not found.")
                ~text:"" ~details_url:job_url ~summary:""
          | Error _ ->
              Lwt.return () ))
  |> Lwt.async

let push_status_check ~bot_info (gh_owner, gh_repo) job_info
    github_repo_full_name repo_full_name =
  match job_info.build_name with
  | "doc:refman" ->
      Stdio.printf
        "This is a successful refman build. Pushing a status check with a \
         link...\n" ;
      let url_base =
        f
          "https://coq.gitlab.io/-/coq/-/jobs/%d/artifacts/_install_ci/share/doc/coq"
          job_info.build_id
      in
      [ ("refman", Printf.sprintf "%s/sphinx/html/index.html" url_base)
      ; ("stdlib", Printf.sprintf "%s/html/stdlib/index.html" url_base) ]
      |> List.iter
           ~f:
             (send_url ~bot_info (gh_owner, gh_repo) job_info
                github_repo_full_name repo_full_name)
      |> Lwt.return
  | "doc:ml-api:odoc" ->
      Stdio.printf
        "This is a successful ml-api build. Pushing a status check with a \
         link...\n" ;
      ( "ml-api"
      , f
          "https://coq.gitlab.io/-/coq/-/jobs/%d/artifacts/_build/default/_doc/_html/index.html"
          job_info.build_id )
      |> send_url ~bot_info (gh_owner, gh_repo) job_info github_repo_full_name
           repo_full_name
      |> Lwt.return
  | _ ->
      Lwt.return ()

let _repeat_request request =
  let rec aux t =
    request
    >>= fun body ->
    if String.is_empty body then Lwt_unix.sleep t >>= fun () -> aux (t *. 2.)
    else Lwt.return body
  in
  aux 2.

type build_failure = Warn | Retry | Ignore

let _trace_action ~repo_full_name trace =
  let trace_size = String.length trace in
  Stdio.printf "Trace size: %d.\n" trace_size ;
  let test regexp = string_match ~regexp trace in
  if test "Job failed: exit code 137" then (
    Stdio.printf "Exit code 137. Retrying...\n" ;
    Retry )
  else if test "Job failed: exit status 255" then (
    Stdio.printf "Exit status 255. Retrying...\n" ;
    Retry )
  else if test "Job failed (system failure)" then (
    Stdio.printf "System failure. Retrying...\n" ;
    Retry )
  else if
    ( test "Uploading artifacts to coordinator... failed"
    || test "Uploading artifacts to coordinator... error" )
    && not (test "Uploading artifacts to coordinator... ok")
  then (
    Stdio.printf "Artifact uploading failure. Retrying...\n" ;
    Retry )
  else if
    test "ERROR: Downloading artifacts from coordinator... error"
    && test "FATAL: invalid argument"
  then (
    Stdio.printf "Artifact downloading failure. Retrying...\n" ;
    Retry )
  else if
    test "transfer closed with outstanding read data remaining"
    || test "HTTP request sent, awaiting response... 50[0-9]"
    || test "The requested URL returned error: 502"
    || test "The remote end hung up unexpectedly"
    || test "error: unable to download 'https://cache.nixos.org/"
  then (
    Stdio.printf "Connectivity issue. Retrying...\n" ;
    Retry )
  else if test "fatal: reference is not a tree" then (
    Stdio.printf "Normal failure: pull request was force-pushed.\n" ;
    Ignore )
  else if
    test "fatal: Remote branch pr-[0-9]* not found in upstream origin"
    || test "fatal: Couldn't find remote ref refs/heads/pr-"
  then (
    Stdio.printf "Normal failure: pull request was closed.\n" ;
    Ignore )
  else if
    String.equal repo_full_name "coq/coq"
    && test "Error response from daemon: manifest for .* not found"
  then (
    Stdio.printf "Docker image not found. Do not report anything specific.\n" ;
    Ignore )
  else Warn

let job_failure ~bot_info job_info pr_num (gh_owner, gh_repo)
    github_repo_full_name repo_full_name context failure_reason =
  Lwt_io.printf "Failed job %d of project %d.\nFailure reason: %s\n"
    job_info.build_id job_info.project_id failure_reason
  >>= fun () ->
  if String.equal failure_reason "runner_system_failure" then
    Lwt_io.printf "Runner failure reported by GitLab CI. Retrying...\n"
    <&> GitLab_mutations.retry_job ~project_id:job_info.project_id
          ~build_id:job_info.build_id ~bot_info
  else if
    String.equal failure_reason "stuck_or_timeout_failure"
    || String.equal failure_reason "job_execution_timeout"
  then
    Lwt_io.printf "Timeout reported by GitLab CI.\n"
    <&> send_status_check ~bot_info job_info pr_num (gh_owner, gh_repo)
          github_repo_full_name repo_full_name context failure_reason
  else if String.equal failure_reason "script_failure" then
    Lwt_io.printf "Script failure reported by GitLab CI.\n"
    <&> send_status_check ~bot_info job_info pr_num (gh_owner, gh_repo)
          github_repo_full_name repo_full_name context failure_reason
    (*
        Lwt_io.printf
          "GitLab CI reports a script failure but it could be something else. \
           Checking the trace...\n"
        >>= fun () ->
        repeat_request (GitLab_queries.get_build_trace ~project_id ~build_id)
        >|= trace_action ~repo_full_name
        >>= function
        | Warn -> Lwt_io.printf "Actual failure.\n" <&> send_status_check ()
        | Retry -> GitLab_mutations.retry_job ~project_id ~build_id
        | Ignore -> Lwt.return ()
        *)
  else
    Lwt_io.printf "Unusual error.\n"
    <&> send_status_check ~bot_info job_info pr_num (gh_owner, gh_repo)
          github_repo_full_name repo_full_name context failure_reason

let job_success ~bot_info (gh_owner, gh_repo) (job_info : job_info)
    github_repo_full_name repo_full_name context =
  GitHub_queries.get_status_check ~repo_full_name:github_repo_full_name
    ~commit:job_info.commit ~context ~bot_info
  >>= function
  | Ok true -> (
      Lwt_io.printf
        "There existed a previous status check for this build, we'll override \
         it.\n"
      >>= fun () ->
      let job_url =
        Printf.sprintf "https://gitlab.com/%s/-/jobs/%d" repo_full_name
          job_info.build_id
      in
      match bot_info.github_token with
      | ACCESS_TOKEN _t ->
          GitHub_mutations.send_status_check
            ~repo_full_name:github_repo_full_name ~commit:job_info.commit
            ~state:"success" ~url:job_url ~context
            ~description:"Test succeeded on GitLab CI after being retried"
            ~bot_info
      | INSTALL_TOKEN _t -> (
          GitHub_queries.get_repository_id ~bot_info ~owner:gh_owner
            ~repo:gh_repo
          >>= function
          | Ok repo_id ->
              GitHub_mutations.create_check_run ~bot_info ~name:context
                ~status:COMPLETED ~repo_id ~head_sha:job_info.commit
                ~conclusion:SUCCESS
                ~title:"Test succeeded on GitLab CI after being retried"
                ~text:"" ~details_url:job_url ~summary:""
          | Error _ ->
              Lwt.return () ) )
  | Ok _ ->
      Lwt.return ()
  | Error e ->
      Lwt_io.printf "%s\n" e

let job_action ~bot_info (job_info : job_info) ~gitlab_mapping =
  let pr_num, branch_or_pr = pr_from_branch job_info.branch in
  let context = f "GitLab CI job %s (%s)" job_info.build_name branch_or_pr in
  let owner, repo =
    let repo_url = job_info.repo_url in
    if not (string_match ~regexp:".*:\\(.*\\)/\\(.*\\).git" repo_url) then
      Stdio.printf "Could not match project name on repository url.\n" ;
    (Str.matched_group 1 repo_url, Str.matched_group 2 repo_url)
  in
  let repo_full_name = owner ^ "/" ^ repo in
  let gh_owner, gh_repo =
    github_repo_of_gitlab_project_path ~gitlab_mapping repo_full_name
  in
  let github_repo_full_name = gh_owner ^ "/" ^ gh_repo in
  if String.equal job_info.build_status "failed" then
    let failure_reason = Option.value_exn job_info.failure_reason in
    job_failure ~bot_info job_info pr_num (gh_owner, gh_repo)
      github_repo_full_name repo_full_name context failure_reason
  else if String.equal job_info.build_status "success" then
    job_success ~bot_info (gh_owner, gh_repo) job_info github_repo_full_name
      repo_full_name context
    <&> push_status_check ~bot_info (gh_owner, gh_repo) job_info
          github_repo_full_name repo_full_name
  else Lwt.return ()

let pipeline_action ~bot_info pipeline_info ~gitlab_mapping : unit Lwt.t =
  let gitlab_full_name = pipeline_info.project_path in
  let repo_full_name =
    match Hashtbl.find gitlab_mapping gitlab_full_name with
    | Some value ->
        value
    | None ->
        Stdio.printf
          "Warning: No correspondence found for GitLab repository %s.\n"
          gitlab_full_name ;
        gitlab_full_name
  in
  match pipeline_info.state with
  | "skipped" ->
      Lwt.return ()
  | _ -> (
      let pipeline_url =
        Printf.sprintf "https://gitlab.com/%s/pipelines/%d" gitlab_full_name
          pipeline_info.id
      in
      match bot_info.github_token with
      | ACCESS_TOKEN _t ->
          let state, description =
            match pipeline_info.state with
            | "success" ->
                ("success", "Pipeline completed on GitLab CI")
            | "pending" ->
                ("pending", "Pipeline is pending on GitLab CI")
            | "running" ->
                ("pending", "Pipeline is running on GitLab CI")
            | "failed" ->
                ("failure", "Pipeline completed with errors on GitLab CI")
            | "cancelled" ->
                ("error", "Pipeline was cancelled on GitLab CI")
            | s ->
                ("error", "Unknown pipeline status: " ^ s)
          in
          GitHub_mutations.send_status_check ~repo_full_name
            ~commit:pipeline_info.commit ~state ~url:pipeline_url
            ~context:
              (f "GitLab CI pipeline (%s)"
                 (pr_from_branch pipeline_info.branch |> snd))
            ~description ~bot_info
      | INSTALL_TOKEN _t -> (
          let owner, repo =
            github_repo_of_gitlab_project_path ~gitlab_mapping repo_full_name
          in
          GitHub_queries.get_repository_id ~bot_info ~owner ~repo
          >>= function
          | Ok repo_id -> (
            match pipeline_info.state with
            | "pending" ->
                GitHub_mutations.create_check_run ~bot_info
                  ~name:
                    (f "GitLab CI pipeline (%s)"
                       (pr_from_branch pipeline_info.branch |> snd))
                  ~repo_id ~head_sha:pipeline_info.commit ~status:QUEUED
                  ?conclusion:None ~title:"Pipeline is pending on GitLab CI"
                  ~text:"" ~details_url:pipeline_url ~summary:""
            | "running" ->
                GitHub_mutations.create_check_run ~bot_info
                  ~name:
                    (f "GitLab CI pipeline (%s)"
                       (pr_from_branch pipeline_info.branch |> snd))
                  ~repo_id ~head_sha:pipeline_info.commit ~status:IN_PROGRESS
                  ?conclusion:None ~title:"Pipeline is running on GitLab CI"
                  ~text:"" ~details_url:pipeline_url ~summary:""
            | _ ->
                let conclusion, title =
                  match pipeline_info.state with
                  | "success" ->
                      (SUCCESS, "Pipeline completed on GitLab CI")
                  | "failed" ->
                      (FAILURE, "Pipeline completed with errors on GitLab CI")
                  | "cancelled" ->
                      (CANCELLED, "Pipeline was cancelled on GitLab CI")
                  | s ->
                      (FAILURE, "Unknown pipeline status: " ^ s)
                in
                GitHub_mutations.create_check_run ~bot_info
                  ~name:
                    (f "GitLab CI pipeline (%s)"
                       (pr_from_branch pipeline_info.branch |> snd))
                  ~repo_id ~head_sha:pipeline_info.commit ~status:COMPLETED
                  ~conclusion ~title ~text:"" ~details_url:pipeline_url
                  ~summary:"" )
          | Error e ->
              Lwt_io.printf "No repo id: %s" e ) )

let coq_bug_minimizer_results_action ~bot_info ~coq_minimizer_repo_token ~key
    ~app_id body =
  if string_match ~regexp:"\\([^\n]+\\)\n\\([^\r]*\\)" body then
    let stamp = Str.matched_group 1 body in
    let message = Str.matched_group 2 body in
    match Str.split (Str.regexp " ") stamp with
    | [id; author; repo_name; branch_name; owner; repo] ->
        (fun () ->
          Github_installations.action_as_github_app ~bot_info ~key ~app_id
            ~owner ~repo
            (GitHub_mutations.post_comment ~id
               ~message:(f "@%s, %s" author message))
          <&> ( execute_cmd
                  (f "git push https://%s:%s@github.com/%s.git --delete '%s'"
                     bot_info.name
                     (Bot_info.get_token coq_minimizer_repo_token)
                     repo_name branch_name)
              >>= function
              | Ok () -> Lwt.return () | Error f -> Lwt_io.printf "Error: %s" f
              ))
        |> Lwt.async ;
        Server.respond_string ~status:`OK ~body:"" ()
    | _ ->
        Server.respond_string ~status:(`Code 400) ~body:"Bad request" ()
  else Server.respond_string ~status:(`Code 400) ~body:"Bad request" ()

let merge_pull_request_action ~bot_info ~comment_info =
  let pr = comment_info.issue in
  if String.equal comment_info.author pr.user then
    GitHub_mutations.post_comment ~bot_info
      ~message:
        (f "@%s: You can't merge the PR because you are the author."
           comment_info.author)
      ~id:pr.id
  else
    match
      List.find comment_info.issue.labels ~f:(fun label ->
          string_match ~regexp:"needs:.*" label)
    with
    | Some l ->
        GitHub_mutations.post_comment ~bot_info
          ~message:
            (f
               "@%s: This PR cannot be merged because there is still a `%s` \
                label."
               comment_info.author l)
          ~id:pr.id
    | None -> (
        if
          not
            (List.exists comment_info.issue.labels ~f:(fun label ->
                 string_match ~regexp:"kind:.*" label))
        then
          GitHub_mutations.post_comment ~bot_info
            ~message:
              (f
                 "@%s: This PR cannot be merged because there is no `kind:` \
                  label."
                 comment_info.author)
            ~id:pr.id
        else if not comment_info.issue.milestoned then
          GitHub_mutations.post_comment ~bot_info
            ~message:
              (f "@%s: This PR cannot be merged because no milestone is set."
                 comment_info.author)
            ~id:pr.id
        else
          GitHub_queries.get_pull_request_reviews_refs ~bot_info
            ~owner:pr.issue.owner ~repo:pr.issue.repo ~number:pr.issue.number
          >>= function
          | Ok reviews_info -> (
              let comment =
                List.find reviews_info.last_comments ~f:(fun c ->
                    String.equal comment_info.id c.id)
              in
              if (not comment_info.review_comment) && Option.is_none comment
              then
                GitHub_mutations.post_comment ~bot_info
                  ~message:
                    (f
                       "@%s: Could not find merge comment because too many \
                        comments were posted since."
                       comment_info.author)
                  ~id:pr.id
              else if
                (not comment_info.review_comment)
                && (Option.value_exn comment).created_by_email
                (* Option.value_exn doesn't raise an exception because comment isn't None at this point*)
              then
                GitHub_mutations.post_comment ~bot_info
                  ~message:
                    (f
                       "@%s: Merge requests sent over e-mail are not accepted \
                        because this put less guarantee on the authenticity of \
                        the author of the request."
                       comment_info.author)
                  ~id:pr.id
              else if not (String.equal reviews_info.baseRef "master") then
                GitHub_mutations.post_comment ~bot_info
                  ~message:
                    (f
                       "@%s: This PR targets branch `%s` instead of `master`. \
                        Only release managers can merge in release branches. \
                        Merging with the bot is not supported."
                       comment_info.author reviews_info.baseRef)
                  ~id:pr.id
              else
                match reviews_info.review_decision with
                | NONE | REVIEW_REQUIRED ->
                    GitHub_mutations.post_comment ~bot_info
                      ~message:
                        (f
                           "@%s: This PR cannot be merged because it hasn't \
                            been approved yet."
                           comment_info.author)
                      ~id:pr.id
                | CHANGES_REQUESTED ->
                    GitHub_mutations.post_comment ~bot_info
                      ~message:
                        (f
                           "@%s: This PR cannot be merged because some changes \
                            are requested."
                           comment_info.author)
                      ~id:pr.id
                | APPROVED -> (
                    if
                      not
                        (List.exists comment_info.issue.assignees
                           ~f:(fun login ->
                             String.equal login comment_info.author))
                    then
                      GitHub_mutations.post_comment ~bot_info
                        ~message:
                          (f
                             "@%s: You can't merge the PR because you're not \
                              among the assignees."
                             comment_info.author)
                        ~id:pr.id
                    else
                      GitHub_queries.get_team_membership ~bot_info ~org:"coq"
                        ~team:"pushers" ~user:comment_info.author
                      >>= function
                      | Ok _ -> (
                          GitHub_mutations.merge_pull_request ~bot_info
                            ~pr_id:pr.id
                            ~commit_headline:
                              (f "Merge PR #%d: %s" pr.issue.number
                                 comment_info.issue.title)
                            ~commit_body:
                              ( List.fold_left reviews_info.approved_reviews
                                  ~init:"" ~f:(fun s r ->
                                    s ^ f "Reviewed-by: %s\n" r)
                              ^ List.fold_left reviews_info.comment_reviews
                                  ~init:"" ~f:(fun s r ->
                                    s ^ f "Ack-by: %s\n" r) )
                            ~merge_method:MERGE
                          >>= fun () ->
                          match
                            List.fold_left ~init:[] reviews_info.files
                              ~f:(fun acc f ->
                                if
                                  string_match
                                    ~regexp:"dev/ci/user-overlays/\\(.*\\)" f
                                then Str.matched_group 1 f :: acc
                                else acc)
                          with
                          | [] ->
                              Lwt.return ()
                          | overlays ->
                              GitHub_mutations.post_comment ~bot_info
                                ~message:
                                  (f
                                     "@%s: Please take care of the following \
                                      overlays:\n\
                                      %s"
                                     comment_info.author
                                     (List.fold_left overlays ~init:""
                                        ~f:(fun s o -> s ^ f "- %s\n" o)))
                                ~id:pr.id )
                      | Error _ ->
                          GitHub_mutations.post_comment ~bot_info
                            ~message:
                              (f
                                 "@%s: You can't merge this PR because you're \
                                  not a member of the `@coq/pushers` team."
                                 comment_info.author)
                            ~id:pr.id ) )
          | Error e ->
              GitHub_mutations.post_comment ~bot_info
                ~message:
                  (f
                     "@%s: Something unexpected happend: %s\n\
                      cc @coq/coqbot-maintainers" comment_info.author e)
                ~id:pr.id )

let update_pr ~bot_info (pr_info : issue_info pull_request_info) ~gitlab_mapping
    ~github_mapping =
  let open Lwt_result.Infix in
  (* Try as much as possible to get unique refnames for local branches. *)
  let local_head_branch =
    f "head-%s-%s" pr_info.head.branch.name pr_info.head.sha
  in
  let local_base_branch =
    f "base-%s-%s" pr_info.base.branch.name pr_info.base.sha
  in
  git_fetch pr_info.base.branch local_base_branch
  |&& git_fetch pr_info.head.branch local_head_branch
  |> execute_cmd
  >>= (fun () ->
        git_make_ancestor ~pr_title:pr_info.issue.title
          ~pr_number:pr_info.issue.number ~base:local_base_branch
          local_head_branch)
  >>= fun ok ->
  if ok then (
    (* Remove rebase label *)
    if pr_info.issue.labels |> List.exists ~f:(String.equal "needs: rebase")
    then
      (fun () ->
        GitHub_mutations.remove_rebase_label pr_info.issue.issue ~bot_info)
      |> Lwt.async ;
    (* Force push *)
    let open Lwt.Infix in
    gitlab_ref ~issue:pr_info.issue.issue ~gitlab_mapping ~github_mapping
      ~bot_info
    >|= (fun remote_ref ->
          git_push ~force:true ~remote_ref ~local_ref:local_head_branch)
    >>= execute_cmd )
  else (
    (* Remove rebase label *)
    (fun () -> GitHub_mutations.add_rebase_label pr_info.issue.issue ~bot_info)
    |> Lwt.async ;
    (* Add fail status check *)
    match bot_info.github_token with
    | ACCESS_TOKEN _t ->
        GitHub_mutations.send_status_check
          ~repo_full_name:
            (f "%s/%s" pr_info.issue.issue.owner pr_info.issue.issue.repo)
          ~commit:pr_info.head.sha ~state:"error" ~url:""
          ~context:"GitLab CI pipeline (pull request)"
          ~description:
            "Pipeline did not run on GitLab CI because PR has conflicts with \
             base branch."
          ~bot_info
        |> Lwt_result.ok
    | INSTALL_TOKEN _t -> (
        let open Lwt.Infix in
        GitHub_queries.get_repository_id ~bot_info
          ~owner:pr_info.issue.issue.owner ~repo:pr_info.issue.issue.repo
        >>= function
        | Ok repo_id ->
            GitHub_mutations.create_check_run ~bot_info
              ~name:"GitLab CI pipeline (pull request)" ~status:COMPLETED
              ~repo_id ~head_sha:pr_info.head.sha ~conclusion:FAILURE
              ~title:
                "Pipeline did not run on GitLab CI because PR has conflicts \
                 with base branch."
              ~text:"" ~details_url:"" ~summary:""
            |> Lwt_result.ok
        | Error e ->
            Lwt.return (Error e) ) )

let run_ci_action ~bot_info ~comment_info ~gitlab_mapping ~github_mapping
    ~signed =
  match Map.find owner_team_map comment_info.issue.issue.owner with
  | Some team when signed ->
      (fun () ->
        (let open Lwt_result.Infix in
        GitHub_queries.get_team_membership ~bot_info
          ~org:comment_info.issue.issue.owner ~team ~user:comment_info.author
        >>= (fun is_member ->
              if is_member then (
                Stdio.printf "Authorized user: pushing to GitLab.\n" ;
                match comment_info.pull_request with
                | Some pr_info ->
                    update_pr pr_info ~bot_info ~gitlab_mapping ~github_mapping
                | None ->
                    let {owner; repo; number} = comment_info.issue.issue in
                    GitHub_queries.get_pull_request_refs ~bot_info ~owner ~repo
                      ~number
                    >>= fun pr_info ->
                    update_pr
                      {pr_info with issue= comment_info.issue}
                      ~bot_info ~gitlab_mapping ~github_mapping )
              else
                Lwt_io.print "Unauthorized user: doing nothing.\n"
                |> Lwt_result.ok)
        |> Fn.flip Lwt_result.bind_lwt_err (fun err ->
               Lwt_io.printf "Error: %s\n" err))
        >>= fun _ -> Lwt.return ())
      |> Lwt.async ;
      Server.respond_string ~status:`OK
        ~body:
          (f
             "Received a request to run CI: checking that @%s is a member of \
              @%s/%s before doing so."
             comment_info.issue.user comment_info.issue.issue.owner team)
        ()
  | Some _ ->
      Server.respond_string ~status:(Code.status_of_code 403)
        ~body:"Webhook requires secret." ()
  | None ->
      (* TODO: check if user is member of the host organization. *)
      Server.respond_string ~status:`OK
        ~body:
          (f
             "Received a request to run CI but no team defined for \
              organization %s: nothing to do."
             comment_info.issue.issue.owner)
        ()

let pull_request_closed_action ~bot_info
    (pr_info : GitHub_types.issue_info GitHub_types.pull_request_info)
    ~gitlab_mapping ~github_mapping =
  let open Lwt.Infix in
  gitlab_ref ~issue:pr_info.issue.issue ~gitlab_mapping ~github_mapping
    ~bot_info
  >|= (fun remote_ref -> git_delete ~remote_ref)
  >>= execute_cmd >|= ignore
  <&>
  if not pr_info.merged then
    Lwt_io.printf
      "PR was closed without getting merged: remove the milestone.\n"
    >>= fun () ->
    GitHub_mutations.remove_milestone pr_info.issue.issue ~bot_info
  else
    (* TODO: if PR was merged in master without a milestone, post an alert *)
    Lwt.return ()

let pull_request_updated_action ~bot_info
    ~(action : GitHub_types.pull_request_action)
    ~(pr_info : GitHub_types.issue_info GitHub_types.pull_request_info)
    ~gitlab_mapping ~github_mapping ~signed =
  ( match (action, pr_info.base.branch.repo_url) with
  | PullRequestOpened, "https://github.com/coq/coq"
    when String.equal pr_info.base.branch.name pr_info.head.branch.name ->
      (fun () ->
        GitHub_mutations.post_comment ~bot_info ~id:pr_info.issue.id
          ~message:
            (f
               "Hello, thanks for your pull request!\n\
                In the future, we strongly recommend that you *do not* use %s \
                as the name of your branch when submitting a pull request.\n\
                By the way, you may be interested in reading [our contributing \
                guide](https://github.com/coq/coq/blob/master/CONTRIBUTING.md)."
               pr_info.base.branch.name))
      |> Lwt.async
  | _ ->
      () ) ;
  match Map.find owner_team_map pr_info.issue.issue.owner with
  | Some team when signed ->
      (fun () ->
        (let open Lwt_result.Infix in
        GitHub_queries.get_team_membership ~bot_info
          ~org:pr_info.issue.issue.owner ~team ~user:pr_info.issue.user
        >>= (fun is_member ->
              if is_member then (
                Stdio.printf "Authorized user: pushing to GitLab.\n" ;
                update_pr pr_info ~bot_info ~gitlab_mapping ~github_mapping )
              else
                Lwt_io.print "Unauthorized user: doing nothing.\n"
                |> Lwt_result.ok)
        |> Fn.flip Lwt_result.bind_lwt_err (fun err ->
               Lwt_io.printf "Error: %s\n" err))
        >>= fun _ -> Lwt.return ())
      |> Lwt.async ;
      Server.respond_string ~status:`OK
        ~body:
          (f
             "Pull request was (re)opened / synchronized. Checking that user \
              %s is a member of @%s/%s before pushing to GitLab."
             pr_info.issue.user pr_info.issue.issue.owner team)
        ()
  | Some _ ->
      Server.respond_string ~status:(Code.status_of_code 403)
        ~body:"Webhook requires secret." ()
  | None ->
      (fun () ->
        update_pr pr_info ~bot_info ~gitlab_mapping ~github_mapping
        >>= fun _ -> Lwt.return ())
      |> Lwt.async ;
      Server.respond_string ~status:`OK
        ~body:
          (f
             "Pull request %s/%s#%d was (re)opened / synchronized: \
              (force-)pushing to GitLab."
             pr_info.issue.issue.owner pr_info.issue.issue.repo
             pr_info.issue.issue.number)
        ()

let rec adjust_milestone ~bot_info ~issue ~sleep_time =
  (* We implement an exponential backoff strategy to try again
     after 5, 25, and 125 seconds, if the issue was closed by a
     commit not yet associated to a pull request. *)
  GitHub_queries.get_issue_closer_info ~bot_info issue
  >>= function
  | Ok (ClosedByPullRequest result) ->
      GitHub_mutations.reflect_pull_request_milestone ~bot_info result
  | Ok ClosedByCommit ->
      (* May be worth trying again later. *)
      if Float.(sleep_time > 200.) then
        Lwt_io.print "Closed by commit not associated to any pull request.\n"
      else
        Lwt_io.printf
          "Closed by commit not yet associated to any pull request... Trying \
           again in %f seconds.\n"
          sleep_time
        >>= (fun () -> Lwt_unix.sleep sleep_time)
        >>= fun () ->
        adjust_milestone ~issue ~sleep_time:(sleep_time *. 5.) ~bot_info
  | Ok ClosedByOther ->
      (* Not worth trying again *)
      Lwt_io.print "Not closed by pull request or commit.\n"
  | Error err ->
      Lwt_io.print (f "Error: %s\n" err)

let project_action ~bot_info ~(issue : issue) ~column_id =
  GitHub_queries.get_pull_request_id_and_milestone ~bot_info ~owner:"coq"
    ~repo:"coq" ~number:issue.number
  >>= function
  | Error err ->
      Lwt_io.printf "Error: %s\n" err
  | Ok None ->
      Lwt_io.printf "Could not find backporting info for PR.\n"
  | Ok (Some (id, _, {backport_info; rejected_milestone}))
    when List.exists backport_info ~f:(fun {request_inclusion_column} ->
             Int.equal request_inclusion_column column_id) ->
      Lwt_io.printf
        "This was a request inclusion column: PR was rejected.\n\
         Change of milestone requested to: %s\n"
        rejected_milestone
      >>= fun () ->
      GitHub_mutations.update_milestone rejected_milestone issue ~bot_info
      <&> GitHub_mutations.post_comment ~bot_info ~id
            ~message:
              "This PR was postponed. Please update accordingly the milestone \
               of any issue that this fixes as this cannot be done \
               automatically."
  | _ ->
      Lwt_io.printf "This was not a request inclusion column: ignoring.\n"

let push_action ~bot_info ~base_ref ~commits_msg =
  Stdio.printf "Merge and backport commit messages:\n" ;
  let commit_action commit_msg =
    if string_match ~regexp:"^Merge PR #\\([0-9]*\\):" commit_msg then
      let pr_number = Str.matched_group 1 commit_msg |> Int.of_string in
      Lwt_io.printf "%s\nPR #%d was merged.\n" commit_msg pr_number
      >>= fun () ->
      GitHub_queries.get_pull_request_id_and_milestone ~bot_info ~owner:"coq"
        ~repo:"coq" ~number:pr_number
      >>= fun pr_info ->
      match pr_info with
      | Ok (Some (_, pr_id, {backport_info})) ->
          backport_info
          |> Lwt_list.iter_p
               (fun {backport_to; request_inclusion_column; backported_column}
               ->
                 if "refs/heads/" ^ backport_to |> String.equal base_ref then
                   Lwt_io.printf
                     "PR was merged into the backportig branch directly.\n"
                   >>= fun () ->
                   GitHub_mutations.add_pr_to_column ~pr_id
                     ~column_id:backported_column ~bot_info
                 else
                   Lwt_io.printf "Backporting to %s was requested.\n"
                     backport_to
                   >>= fun () ->
                   GitHub_mutations.add_pr_to_column ~pr_id
                     ~column_id:request_inclusion_column ~bot_info)
      | Ok None ->
          Lwt_io.printf "Did not get any backporting info.\n"
      | Error err ->
          Lwt_io.printf "Error: %s\n" err
    else if string_match ~regexp:"^Backport PR #\\([0-9]*\\):" commit_msg then
      let pr_number = Str.matched_group 1 commit_msg |> Int.of_string in
      Lwt_io.printf "%s\nPR #%d was backported.\n" commit_msg pr_number
      >>= fun () ->
      GitHub_queries.get_backported_pr_info ~bot_info pr_number base_ref
      >>= function
      | Ok (Some ({card_id; column_id} as input)) ->
          Lwt_io.printf "Moving card %s to column %s.\n" card_id column_id
          >>= fun () -> GitHub_mutations.mv_card_to_column ~bot_info input
      | Ok None ->
          Lwt_io.printf "Could not find backporting info for backported PR.\n"
      | Error e ->
          Lwt_io.printf "%s\n" e
    else Lwt.return ()
  in
  Lwt_list.iter_s commit_action commits_msg
