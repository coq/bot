open Base
open Bot_components
open Cohttp
open Cohttp_lwt_unix
open Lwt.Infix
open Utils

let toml_data = Config.toml_of_file (Sys.get_argv ()).(1)

let port = Config.port toml_data

let gitlab_access_token = Config.gitlab_access_token toml_data

let github_access_token = Config.github_access_token toml_data

let github_webhook_secret = Config.github_webhook_secret toml_data

let gitlab_webhook_secret = Config.gitlab_webhook_secret toml_data

let bot_name = Config.bot_name toml_data

let bot_domain = Config.bot_domain toml_data bot_name

let bot_email = Config.bot_email toml_data bot_name

let bot_info =
  { github_token= github_access_token
  ; gitlab_token= gitlab_access_token
  ; bot_name }

let github_mapping, gitlab_mapping = Config.make_mappings_table toml_data

let github_of_gitlab = Hashtbl.find gitlab_mapping

let gitlab_of_github = Hashtbl.find github_mapping

let gitlab_repo ~owner ~name =
  f "https://oauth2:%s@gitlab.com/%s/%s.git" gitlab_access_token owner name

let report_status command report code =
  Error (f "Command \"%s\" %s %d\n" command report code)

let ( |&& ) command1 command2 = command1 ^ " && " ^ command2

let execute_cmd command =
  Lwt_unix.system command
  >|= fun status ->
  match status with
  | Unix.WEXITED code ->
      if Int.equal code 0 then Ok ()
      else report_status command "exited with status" code
  | Unix.WSIGNALED signal ->
      report_status command "was killed by signal number" signal
  | Unix.WSTOPPED signal ->
      report_status command "was stopped by signal number" signal

let git_fetch ?(force = true)
    (remote_ref : GitHub_subscriptions.remote_ref_info) local_branch_name =
  f "git fetch -fu %s %s%s:refs/heads/%s" remote_ref.repo_url
    (if force then "+" else "")
    remote_ref.name local_branch_name

let git_push ?(force = true)
    ~(remote_ref : GitHub_subscriptions.remote_ref_info) ~local_ref =
  f "git push %s %s%s:refs/heads/%s" remote_ref.repo_url
    (if force then " +" else " ")
    local_ref remote_ref.name

let git_delete ~remote_ref = git_push ~force:false ~remote_ref ~local_ref:""

let git_make_ancestor ~base head =
  f "./make_ancestor.sh %s %s" base head
  |> Lwt_unix.system
  >|= fun status ->
  match status with
  | Unix.WEXITED 0 ->
      Ok true (* merge successful *)
  | Unix.WEXITED 10 ->
      Ok false (* merge unsuccessful *)
  | Unix.WEXITED code ->
      Error (f "git_make_ancestor script exited with status %d." code)
  | Unix.WSIGNALED signal ->
      Error (f "git_make_ancestor script killed by signal %d." signal)
  | Unix.WSTOPPED signal ->
      Error (f "git_make_ancestor script stopped by signal %d." signal)

let git_coq_bug_minimizer ~script ~comment_thread_id ~comment_author ~bot_info
    ~bot_domain =
  f "./coq_bug_minimizer.sh '%s' %s %s %s %s %s" script comment_thread_id
    comment_author bot_info.github_token bot_name bot_domain
  |> Lwt_unix.system
  >|= fun status ->
  match status with
  | Unix.WEXITED 0 ->
      Ok true (* push successful *)
  | Unix.WEXITED code ->
      Error (f "coq_bug_minimizer script exited with status %d." code)
  | Unix.WSIGNALED signal ->
      Error (f "coq_bug_minimizer script killed by signal %d." signal)
  | Unix.WSTOPPED signal ->
      Error (f "coq_bug_minimizer script stopped by signal %d." signal)

let first_line_of_string s =
  if string_match ~regexp:"\\(.*\\)\n" s then Str.matched_group 1 s else s

let run_coq_minimizer ~script ~comment_thread_id ~comment_author () =
  git_coq_bug_minimizer ~script ~comment_thread_id ~comment_author ~bot_info
    ~bot_domain
  >>= function
  | Ok ok ->
      if ok then
        GitHub_mutations.post_comment ~id:comment_thread_id
          ~message:
            (f
               "Hey @%s, the coq bug minimizer is running your script, I'll \
                come back to you with the results once it's done."
               comment_author)
          ~bot_info
      else Lwt.return ()
  | Error f ->
      Lwt_io.printf "Error: %s" f

let extract_commit json =
  let open Yojson.Basic.Util in
  let commit_json = json |> member "commit" in
  let message = commit_json |> member "message" |> to_string in
  if string_match ~regexp:"Bot merge .* into \\(.*\\)" message then
    Str.matched_group 1 message
  else
    (* In the case of build webhooks, the id is a number and the sha is the
       reference of the commit, while in the case of pipeline hooks only id
       is present and represents the sha. *)
    ( match commit_json |> member "sha" with
    | `Null ->
        commit_json |> member "id"
    | sha ->
        sha )
    |> to_string

let gitlab_ref ~(issue : GitHub_subscriptions.issue) =
  let gh_repo = issue.owner ^ "/" ^ issue.repo in
  let open Lwt.Infix in
  (* First, we check our hashtable for a key named after the GitHub
     repository and return the associated GitLab repository. If the
     key is not found, we load the config file from the default branch.
     Last (backward-compatibility) we assume the GitLab and GitHub
     projects are named the same. *)
  ( match gitlab_of_github gh_repo with
  | None -> (
      Stdio.printf "No correspondence found for GitHub repository %s/%s.\n"
        issue.owner issue.repo ;
      GitHub_queries.get_default_branch ~bot_info ~owner:issue.owner
        ~repo:issue.repo
      >>= function
      | Ok branch -> (
          GitHub_queries.get_file_content ~bot_info ~owner:issue.owner
            ~repo:issue.repo ~branch ~file_name:(f "%s.toml" bot_name)
          >>= function
          | Ok (Some content) ->
              let gl_repo =
                Option.value
                  (Config.subkey_value
                     (Config.toml_of_string content)
                     "mapping" "gitlab")
                  ~default:gh_repo
              in
              ( match Hashtbl.add gitlab_mapping ~key:gl_repo ~data:gh_repo with
              | `Duplicate ->
                  ()
              | `Ok ->
                  () ) ;
              ( match Hashtbl.add github_mapping ~key:gh_repo ~data:gl_repo with
              | `Duplicate ->
                  ()
              | `Ok ->
                  () ) ;
              Lwt.return gl_repo
          | _ ->
              Lwt.return gh_repo )
      | _ ->
          Lwt.return gh_repo )
  | Some r ->
      Lwt.return r )
  >|= fun gl_repo ->
  let owner, name =
    match Str.split (Str.regexp "/") gl_repo with
    | [owner; repo] ->
        (owner, repo)
    | _ ->
        (issue.owner, issue.repo)
  in
  ( {name= f "pr-%d" issue.number; repo_url= gitlab_repo ~owner ~name}
    : GitHub_subscriptions.remote_ref_info )

let pull_request_updated
    (pr_info :
      GitHub_subscriptions.issue_info GitHub_subscriptions.pull_request_info) ()
    =
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
  >>= (fun () -> git_make_ancestor ~base:local_base_branch local_head_branch)
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
    gitlab_ref ~issue:pr_info.issue.issue
    >|= (fun remote_ref ->
          git_push ~force:true ~remote_ref ~local_ref:local_head_branch)
    >>= execute_cmd )
  else (
    (* Remove rebase label *)
    (fun () -> GitHub_mutations.add_rebase_label pr_info.issue.issue ~bot_info)
    |> Lwt.async ;
    (* Add fail status check *)
    GitHub_mutations.send_status_check
      ~repo_full_name:
        (f "%s/%s" pr_info.issue.issue.owner pr_info.issue.issue.repo)
      ~commit:pr_info.head.sha ~state:"error" ~url:""
      ~context:"GitLab CI pipeline (pull request)"
      ~description:
        "Pipeline did not run on GitLab CI because PR has conflicts with base \
         branch."
      ~bot_info
    |> Lwt_result.ok )

let pull_request_closed
    (pr_info :
      GitHub_subscriptions.issue_info GitHub_subscriptions.pull_request_info) ()
    =
  let open Lwt.Infix in
  gitlab_ref ~issue:pr_info.issue.issue
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

let project_action ~(issue : GitHub_subscriptions.issue) ~column_id () =
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

let push_action ~base_ref ~commits_msg =
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
               (fun { GitHub_queries.backport_to
                    ; request_inclusion_column
                    ; backported_column }
                    ->
                 if "refs/heads/" ^ backport_to |> String.equal base_ref then
                   Lwt_io.printf
                     "PR was merged into the backportig branch directly.\n"
                   >>= fun () ->
                   GitHub_mutations.add_pr_to_column pr_id backported_column
                     ~bot_info
                 else
                   Lwt_io.printf "Backporting to %s was requested.\n"
                     backport_to
                   >>= fun () ->
                   GitHub_mutations.add_pr_to_column pr_id
                     request_inclusion_column ~bot_info)
      | Ok None ->
          Lwt_io.printf "Did not get any backporting info.\n"
      | Error err ->
          Lwt_io.printf "Error: %s\n" err
    else if string_match ~regexp:"^Backport PR #\\([0-9]*\\):" commit_msg then
      let pr_number = Str.matched_group 1 commit_msg |> Int.of_string in
      Lwt_io.printf "%s\nPR #%d was backported.\n" commit_msg pr_number
      >>= fun () ->
      GitHub_queries.backported_pr_info ~bot_info pr_number base_ref
      >>= function
      | Some ({card_id; column_id} as input) ->
          Lwt_io.printf "Moving card %s to column %s.\n" card_id column_id
          >>= fun () -> GitHub_mutations.mv_card_to_column ~bot_info input
      | None ->
          Lwt_io.printf "Could not find backporting info for backported PR.\n"
    else Lwt.return ()
  in
  fun () -> Lwt_list.iter_s commit_action commits_msg

let repeat_request request =
  let rec aux t =
    request
    >>= fun body ->
    if String.is_empty body then Lwt_unix.sleep t >>= fun () -> aux (t *. 2.)
    else Lwt.return body
  in
  aux 2.

type build_failure = Warn | Retry | Ignore

let trace_action ~repo_full_name trace =
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

let pr_from_branch branch =
  if string_match ~regexp:"^pr-\\([0-9]*\\)$" branch then
    (Some (Str.matched_group 1 branch |> Int.of_string), "pull request")
  else (None, "branch")

let job_action (job_info : GitLab_subscriptions.job_info) =
  let pr_num, branch_or_pr = pr_from_branch job_info.branch in
  let context = f "GitLab CI job %s (%s)" job_info.build_name branch_or_pr in
  let owner, repo =
    let repo_url = job_info.repo_url in
    if not (string_match ~regexp:".*:\\(.*\\)/\\(.*\\).git" repo_url) then
      Stdio.printf "Could not match project name on repository url.\n" ;
    (Str.matched_group 1 repo_url, Str.matched_group 2 repo_url)
  in
  let repo_full_name = owner ^ "/" ^ repo in
  let github_repo_full_name =
    Option.value
      (github_of_gitlab repo_full_name)
      ~default:
        ( Stdio.printf "No correspondence found for GitLab repository %s.\n"
            repo_full_name ;
          repo_full_name )
  in
  let gh_owner, gh_repo =
    match Str.split (Str.regexp "/") github_repo_full_name with
    | [owner_; repo_] ->
        (owner_, repo_)
    | _ ->
        (owner, repo)
  in
  let send_url (kind, url) =
    (fun () ->
      let context = Printf.sprintf "%s: %s artifact" job_info.build_name kind in
      let description_base = Printf.sprintf "Link to %s build artifact" kind in
      url |> Uri.of_string |> Client.get
      >>= fun (resp, _) ->
      if resp |> Response.status |> Code.code_of_status |> Int.equal 200 then
        GitHub_mutations.send_status_check ~repo_full_name:github_repo_full_name
          ~commit:job_info.commit ~state:"success" ~url ~context
          ~description:(description_base ^ ".") ~bot_info
      else
        Lwt_io.printf "But we didn't get a 200 code when checking the URL.\n"
        >>= fun () ->
        GitHub_mutations.send_status_check ~repo_full_name:github_repo_full_name
          ~commit:job_info.commit ~state:"failure"
          ~url:
            (Printf.sprintf "https://gitlab.com/%s/-/jobs/%d" repo_full_name
               job_info.build_id)
          ~context
          ~description:(description_base ^ ": not found.")
          ~bot_info)
    |> Lwt.async
  in
  if String.equal job_info.build_status "failed" then
    let failure_reason = Option.value_exn job_info.failure_reason in
    let send_status_check () =
      let allow_fail =
        match job_info.allow_fail with Some f -> f | None -> false
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
            | Ok {issue= id; head; last_commit_message= Some commit_message}
            (* Commits reported back by get_pull_request_refs are surrounded in double quotes *)
              when String.equal head.sha (f "\"%s\"" job_info.commit) ->
                GitHub_mutations.post_comment ~bot_info ~id
                  ~message:
                    (f
                       "For your complete information, the job \
                        [%s](https://gitlab.com/%s/-/jobs/%d) in allow failure \
                        mode has failed for commit %s: %s%s"
                       job_info.build_name repo_full_name job_info.build_id
                       job_info.commit
                       (first_line_of_string commit_message)
                       ( if
                         String.equal job_info.build_name
                           "library:ci-fiat_crypto_legacy"
                       then "\nping @JasonGross"
                       else "" ))
            | Ok {head} ->
                Lwt_io.printf
                  "We are on a PR branch but the commit (%s) is not the \
                   current head of the PR (%s). Doing nothing.\n"
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
        GitHub_mutations.send_status_check ~repo_full_name:github_repo_full_name
          ~commit:job_info.commit ~state:"failure"
          ~url:
            (Printf.sprintf "https://gitlab.com/%s/-/jobs/%d" repo_full_name
               job_info.build_id)
          ~context
          ~description:(failure_reason ^ " on GitLab CI")
          ~bot_info
    in
    (fun () ->
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
        <&> send_status_check ()
      else if String.equal failure_reason "script_failure" then
        Lwt_io.printf "Script failure reported by GitLab CI.\n"
        <&> send_status_check ()
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
      else Lwt_io.printf "Unusual error.\n" <&> send_status_check ())
    |> Lwt.async
  else if String.equal job_info.build_status "success" then (
    (fun () ->
      GitHub_queries.get_status_check ~repo_full_name:github_repo_full_name
        ~commit:job_info.commit ~context ~bot_info
      >>= fun b ->
      if b then
        Lwt_io.printf
          "There existed a previous status check for this build, we'll \
           override it.\n"
        <&> GitHub_mutations.send_status_check
              ~repo_full_name:github_repo_full_name ~commit:job_info.commit
              ~state:"success"
              ~url:
                (Printf.sprintf "https://gitlab.com/%s/-/jobs/%d" repo_full_name
                   job_info.build_id)
              ~context
              ~description:"Test succeeded on GitLab CI after being retried"
              ~bot_info
      else Lwt.return ())
    |> Lwt.async ;
    if String.equal job_info.build_name "doc:refman" then (
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
      |> List.iter ~f:send_url )
    else if String.equal job_info.build_name "doc:ml-api:odoc" then (
      Stdio.printf
        "This is a successful ml-api build. Pushing a status check with a \
         link...\n" ;
      ( "ml-api"
      , f
          "https://coq.gitlab.io/-/coq/-/jobs/%d/artifacts/_build/default/_doc/_html/index.html"
          job_info.build_id )
      |> send_url ) )

let pipeline_action (pipeline_info : GitLab_subscriptions.pipeline_info) :
    unit Lwt.t =
  let gitlab_full_name = pipeline_info.project_path in
  let repo_full_name =
    Option.value
      (github_of_gitlab gitlab_full_name)
      ~default:
        ( Stdio.printf "No correspondence found for GitLab repository %s.\n"
            gitlab_full_name ;
          gitlab_full_name )
  in
  match pipeline_info.state with
  | "skipped" ->
      Lwt.return ()
  | _ ->
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
        ~commit:pipeline_info.commit ~state
        ~url:
          (Printf.sprintf "https://gitlab.com/%s/pipelines/%d" gitlab_full_name
             pipeline_info.id)
        ~context:
          (f "GitLab CI pipeline (%s)"
             (pr_from_branch pipeline_info.branch |> snd))
        ~description ~bot_info

let owner_team_map =
  Map.of_alist_exn
    (module String)
    [("martijnbastiaan-test-org", "martijnbastiaan-test-team")]

(* TODO: deprecate unsigned webhooks *)

let callback _conn req body =
  let body = Cohttp_lwt.Body.to_string body in
  (* print_endline "Request received."; *)
  match Uri.path (Request.uri req) with
  | "/gitlab" -> (
      body
      >>= fun body ->
      match
        GitLab_subscriptions.receive_gitlab ~secret:gitlab_webhook_secret
          (Request.headers req) body
      with
      | Ok (_, `JobEvent job_info) ->
          job_action job_info ;
          Server.respond_string ~status:`OK ~body:"Job event." ()
      | Ok (_, `PipelineEvent pipeline_info) ->
          (fun () -> pipeline_action pipeline_info) |> Lwt.async ;
          Server.respond_string ~status:`OK ~body:"Pipeline event." ()
      | Ok (_, `UnsupportedEvent e) ->
          Server.respond_string ~status:`OK
            ~body:(f "Unsupported event %s." e)
            ()
      | Error s ->
          Server.respond_string ~status:(Code.status_of_code 400)
            ~body:(f "Error: %s" s) () )
  | "/github" -> (
      body
      >>= fun body ->
      match
        GitHub_subscriptions.receive_github ~secret:github_webhook_secret
          (Request.headers req) body
      with
      | Ok (_, GitHub_subscriptions.PushEvent {base_ref; commits_msg}) ->
          push_action ~base_ref ~commits_msg |> Lwt.async ;
          Server.respond_string ~status:`OK ~body:"Processing push event." ()
      | Ok
          ( _
          , GitHub_subscriptions.PullRequestUpdated (PullRequestClosed, pr_info)
          ) ->
          pull_request_closed pr_info |> Lwt.async ;
          Server.respond_string ~status:`OK
            ~body:
              (f
                 "Pull request %s/%s#%d was closed: removing the branch from \
                  GitLab."
                 pr_info.issue.issue.owner pr_info.issue.issue.repo
                 pr_info.issue.issue.number)
            ()
      | Ok (signed, GitHub_subscriptions.PullRequestUpdated (action, pr_info))
        -> (
          ( match (action, pr_info.base.branch.repo_url) with
          | PullRequestOpened, "https://github.com/coq/coq"
            when String.equal pr_info.base.branch.name pr_info.head.branch.name
            ->
              (fun () ->
                GitHub_mutations.post_comment ~bot_info ~id:pr_info.issue.id
                  ~message:
                    (f
                       "Hello, thanks for your pull request!\n\
                        In the future, we strongly recommend that you *do not* \
                        use %s as the name of your branch when submitting a \
                        pull request.\n\
                        By the way, you may be interested in reading [our \
                        contributing \
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
                        pull_request_updated pr_info () )
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
                     "Pull request was (re)opened / synchronized. Checking \
                      that user %s is a member of @%s/%s before pushing to \
                      GitLab."
                     pr_info.issue.user pr_info.issue.issue.owner team)
                ()
          | Some _ ->
              Server.respond_string ~status:(Code.status_of_code 403)
                ~body:"Webhook requires secret." ()
          | None ->
              (fun () ->
                pull_request_updated pr_info () >>= fun _ -> Lwt.return ())
              |> Lwt.async ;
              Server.respond_string ~status:`OK
                ~body:
                  (f
                     "Pull request %s/%s#%d was (re)opened / synchronized: \
                      (force-)pushing to GitLab."
                     pr_info.issue.issue.owner pr_info.issue.issue.repo
                     pr_info.issue.issue.number)
                () )
      | Ok (_, GitHub_subscriptions.IssueClosed {issue}) ->
          (* TODO: only for projects that requested this feature *)
          (* We implement an exponential backoff strategy to try again
             after 5, 25, and 125 seconds, if the issue was closed by a
             commit not yet associated to a pull request. *)
          let rec adjust_milestone sleep_time () =
            GitHub_queries.get_issue_closer_info ~bot_info issue
            >>= function
            | Ok (GitHub_queries.ClosedByPullRequest result) ->
                GitHub_mutations.reflect_pull_request_milestone ~bot_info result
            | Ok GitHub_queries.ClosedByCommit ->
                (* May be worth trying again later. *)
                if Float.(sleep_time > 200.) then
                  Lwt_io.print
                    "Closed by commit not associated to any pull request.\n"
                else
                  Lwt_io.printf
                    "Closed by commit not yet associated to any pull \
                     request... Trying again in %f seconds.\n"
                    sleep_time
                  >>= (fun () -> Lwt_unix.sleep sleep_time)
                  >>= adjust_milestone (sleep_time *. 5.)
            | Ok GitHub_queries.ClosedByOther ->
                (* Not worth trying again *)
                Lwt_io.print "Not closed by pull request or commit.\n"
            | Error err ->
                Lwt_io.print (f "Error: %s\n" err)
          in
          adjust_milestone 5. |> Lwt.async ;
          Server.respond_string ~status:`OK
            ~body:
              (f "Issue %s/%s#%d was closed: checking its milestone."
                 issue.owner issue.repo issue.number)
            ()
      | Ok
          ( _
          , GitHub_subscriptions.RemovedFromProject
              ({issue= Some issue; column_id} as card) ) ->
          project_action ~issue ~column_id |> Lwt.async ;
          Server.respond_string ~status:`OK
            ~body:
              (f
                 "Issue or PR %s/%s#%d was removed from project column %d: \
                  checking if this was a backporting column."
                 issue.owner issue.repo issue.number card.column_id)
            ()
      | Ok (_, GitHub_subscriptions.RemovedFromProject _) ->
          Server.respond_string ~status:`OK
            ~body:"Note card removed from project: nothing to do." ()
      | Ok
          (_, GitHub_subscriptions.IssueOpened ({body= Some body} as issue_info))
        ->
          let body = trim_comments body in
          if
            string_match
              ~regexp:(f "@%s:? [Mm]inimize[^```]*```\\([^```]+\\)```" bot_name)
              body
          then
            run_coq_minimizer ~script:(Str.matched_group 1 body)
              ~comment_thread_id:issue_info.id ~comment_author:issue_info.user
            |> Lwt.async ;
          Server.respond_string ~status:`OK ~body:"Handling minimization." ()
      | Ok (signed, GitHub_subscriptions.CommentCreated comment_info) ->
          let body = trim_comments comment_info.body in
          if
            string_match
              ~regexp:(f "@%s:? [Mm]inimize[^```]*```\\([^```]+\\)```" bot_name)
              body
          then (
            run_coq_minimizer ~script:(Str.matched_group 1 body)
              ~comment_thread_id:
                ( match comment_info.pull_request with
                | None ->
                    comment_info.issue.id
                | Some pr ->
                    pr.issue.id )
              ~comment_author:comment_info.author
            |> Lwt.async ;
            Server.respond_string ~status:`OK ~body:"Handling minimization." ()
            )
          else if
            string_match ~regexp:(f "@%s:? [Rr]un CI now" bot_name) body
            && comment_info.issue.pull_request
          then
            match Map.find owner_team_map comment_info.issue.issue.owner with
            | Some team when signed ->
                (fun () ->
                  (let open Lwt_result.Infix in
                  GitHub_queries.get_team_membership ~bot_info
                    ~org:comment_info.issue.issue.owner ~team
                    ~user:comment_info.author
                  >>= (fun is_member ->
                        if is_member then (
                          Stdio.printf "Authorized user: pushing to GitLab.\n" ;
                          match comment_info.pull_request with
                          | Some pr_info ->
                              pull_request_updated pr_info ()
                          | None ->
                              let {GitHub_subscriptions.owner; repo; number} =
                                comment_info.issue.issue
                              in
                              GitHub_queries.get_pull_request_refs ~bot_info
                                ~owner ~repo ~number
                              >>= fun pr_info ->
                              pull_request_updated
                                {pr_info with issue= comment_info.issue}
                                () )
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
                       "Received a request to run CI: checking that @%s is a \
                        member of @%s/%s before doing so."
                       comment_info.issue.user comment_info.issue.issue.owner
                       team)
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
          else if
            string_match ~regexp:(f "@%s:? [Mm]erge now" bot_name) body
            && comment_info.issue.pull_request
            && String.equal comment_info.issue.issue.owner "coq"
            && String.equal comment_info.issue.issue.repo "coq"
            && signed
          then (
            (fun () ->
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
                           "@%s: This PR cannot be merged because there is \
                            still a `%s` label."
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
                             "@%s: This PR cannot be merged because there is \
                              no `kind:` label."
                             comment_info.author)
                        ~id:pr.id
                    else if not comment_info.issue.milestoned then
                      GitHub_mutations.post_comment ~bot_info
                        ~message:
                          (f
                             "@%s: This PR cannot be merged because no \
                              milestone is set."
                             comment_info.author)
                        ~id:pr.id
                    else
                      GitHub_queries.get_pull_request_reviews_refs ~bot_info
                        ~owner:pr.issue.owner ~repo:pr.issue.repo
                        ~number:pr.issue.number
                      >>= function
                      | Ok reviews_info -> (
                          let comment =
                            List.find reviews_info.last_comments ~f:(fun c ->
                                String.equal comment_info.id c.id)
                          in
                          if
                            (not comment_info.review_comment)
                            && Option.is_none comment
                          then
                            GitHub_mutations.post_comment ~bot_info
                              ~message:
                                (f
                                   "@%s: Could not find merge comment because \
                                    too many comments were posted since."
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
                                   "@%s: Merge requests sent over e-mail are \
                                    not accepted because this put less \
                                    guarantee on the authenticity of the \
                                    author of the request."
                                   comment_info.author)
                              ~id:pr.id
                          else if
                            not (String.equal reviews_info.baseRef "master")
                          then
                            GitHub_mutations.post_comment ~bot_info
                              ~message:
                                (f
                                   "@%s: This PR targets branch `%s` instead \
                                    of `master`. Only release managers can \
                                    merge in release branches. Merging with \
                                    the bot is not supported."
                                   comment_info.author reviews_info.baseRef)
                              ~id:pr.id
                          else
                            match reviews_info.review_decision with
                            | NONE | REVIEW_REQUIRED ->
                                GitHub_mutations.post_comment ~bot_info
                                  ~message:
                                    (f
                                       "@%s: This PR cannot be merged because \
                                        it hasn't been approved yet."
                                       comment_info.author)
                                  ~id:pr.id
                            | CHANGES_REQUESTED ->
                                GitHub_mutations.post_comment ~bot_info
                                  ~message:
                                    (f
                                       "@%s: This PR cannot be merged because \
                                        some changes are requested."
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
                                         "@%s: You can't merge the PR because \
                                          you're not among the assignees."
                                         comment_info.author)
                                    ~id:pr.id
                                else
                                  GitHub_queries.get_team_membership ~bot_info
                                    ~org:"coq" ~team:"pushers"
                                    ~user:comment_info.author
                                  >>= function
                                  | Ok _ -> (
                                      GitHub_mutations.merge_pull_request
                                        ~bot_info ~pr_id:pr.id
                                        ~commit_headline:
                                          (f "Merge PR #%d: %s" pr.issue.number
                                             comment_info.issue.title)
                                        ~commit_body:
                                          ( List.fold_left
                                              reviews_info.approved_reviews
                                              ~init:"" ~f:(fun s r ->
                                                s ^ f "Reviewed-by: %s\n" r)
                                          ^ List.fold_left
                                              reviews_info.comment_reviews
                                              ~init:"" ~f:(fun s r ->
                                                s ^ f "Ack-by: %s\n" r) )
                                        ~merge_method:`MERGE
                                      >>= fun () ->
                                      match
                                        List.fold_left ~init:[]
                                          reviews_info.files ~f:(fun acc f ->
                                            if
                                              string_match
                                                ~regexp:
                                                  "dev/ci/user-overlays/\\(.*\\)"
                                                f
                                            then Str.matched_group 1 f :: acc
                                            else acc)
                                      with
                                      | [] ->
                                          Lwt.return ()
                                      | overlays ->
                                          GitHub_mutations.post_comment
                                            ~bot_info
                                            ~message:
                                              (f
                                                 "@%s: Please take care of the \
                                                  following overlays:\n\
                                                  %s"
                                                 comment_info.author
                                                 (List.fold_left overlays
                                                    ~init:"" ~f:(fun s o ->
                                                      s ^ f "- %s\n" o)))
                                            ~id:pr.id )
                                  | Error _ ->
                                      GitHub_mutations.post_comment ~bot_info
                                        ~message:
                                          (f
                                             "@%s: You can't merge this PR \
                                              because you're not a member of \
                                              the `@coq/pushers` team."
                                             comment_info.author)
                                        ~id:pr.id ) )
                      | Error e ->
                          GitHub_mutations.post_comment ~bot_info
                            ~message:
                              (f
                                 "@%s: Something unexpected happend: %s\n\
                                  cc @coq/coqbot-maintainers"
                                 comment_info.author e)
                            ~id:pr.id ))
            |> Lwt.async ;
            Server.respond_string ~status:`OK
              ~body:(f "Received a request to merge the PR.")
              () )
          else
            Server.respond_string ~status:`OK
              ~body:(f "Unhandled comment: %s." body)
              ()
      | Ok (_, GitHub_subscriptions.NoOp s) ->
          Server.respond_string ~status:`OK ~body:(f "No action taken: %s" s) ()
      | Ok _ ->
          Server.respond_string ~status:`OK
            ~body:"No action taken: event or action is not yet supported." ()
      | Error s ->
          Server.respond_string ~status:(Code.status_of_code 400)
            ~body:(f "Error: %s" s) () )
  | "/coq-bug-minimizer" ->
      body
      >>= fun body ->
      if string_match ~regexp:"\\([^\n]+\\)\n\\([^\r]*\\)" body then
        let stamp = Str.matched_group 1 body in
        let message = Str.matched_group 2 body in
        match Str.split (Str.regexp " ") stamp with
        | [id; author; repo_name; branch_name] ->
            (fun () ->
              GitHub_mutations.post_comment ~bot_info ~id
                ~message:(f "@%s, %s" author message)
              <&> ( execute_cmd
                      (f
                         "git push https://%s:%s@github.com/%s.git --delete \
                          '%s'"
                         bot_name github_access_token repo_name branch_name)
                  >>= function
                  | Ok () ->
                      Lwt.return ()
                  | Error f ->
                      Lwt_io.printf "Error: %s" f ))
            |> Lwt.async ;
            Server.respond_string ~status:`OK ~body:"" ()
        | _ ->
            Server.respond_string ~status:(`Code 400) ~body:"Bad request" ()
      else Server.respond_string ~status:(`Code 400) ~body:"Bad request" ()
  | _ ->
      Server.respond_not_found ()

let server =
  (fun () ->
    Lwt_io.printf "Initializing repository...\n"
    <&> ( "git init --bare"
        |&& f "git config user.email \"%s\"" bot_email
        |&& f "git config user.name \"%s\"" bot_name
        |> execute_cmd >|= ignore ))
  |> Lwt.async ;
  let mode = `TCP (`Port port) in
  Server.create ~mode (Server.make ~callback ())

let () =
  Lwt.async_exception_hook :=
    fun exn -> Stdio.printf "Error: Unhandled exception: %s" (Exn.to_string exn)

let () = Lwt_main.run server
