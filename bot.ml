open Bot_components
open Cohttp
open Cohttp_lwt_unix
open Lwt.Infix
open Utils

let port = try Sys.getenv "PORT" |> int_of_string with Not_found -> 8000

let username = Sys.getenv "USERNAME"

let password = Sys.getenv "PASSWORD"

let credentials = `Basic (username, password)

let gitlab_access_token = Sys.getenv "GITLAB_ACCESS_TOKEN"

let gitlab_header = [("PRIVATE-TOKEN", gitlab_access_token)]

let github_access_token = Sys.getenv "GITHUB_ACCESS_TOKEN"

let github_webhook_secret = Sys.getenv "GITHUB_WEBHOOK_SECRET"

open Base

let project_api_preview_header =
  [("Accept", "application/vnd.github.inertia-preview+json")]

let gitlab_repo = f "https://%s:%s@gitlab.com/%s/%s.git" username password

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
    (remote_ref : GitHub_subscriptions.remote_ref_info) local_ref =
  f "git push %s %s%s:refs/heads/%s" remote_ref.repo_url
    (if force then " +" else " ")
    local_ref remote_ref.name

let git_delete remote_ref = git_push ~force:false remote_ref ""

let git_make_ancestor ~base head =
  f "./make_ancestor.sh %s %s" base head
  |> Lwt_unix.system
  >|= fun status ->
  match status with
  | Unix.WEXITED 0 -> Ok true (* merge successful *)
  | Unix.WEXITED 10 -> Ok false (* merge unsuccessful *)
  | Unix.WEXITED code ->
      Error (f "git_make_ancestor script exited with status %d." code)
  | Unix.WSIGNALED signal ->
      Error (f "git_make_ancestor script killed by signal %d." signal)
  | Unix.WSTOPPED signal ->
      Error (f "git_make_ancestor script stopped by signal %d." signal)

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
    | `Null -> commit_json |> member "id"
    | sha -> sha )
    |> to_string

let print_response (resp, body) =
  let code = resp |> Response.status |> Code.code_of_status in
  Lwt_io.printf "Response code: %d.\n" code
  >>= fun () ->
  if code < 200 && code > 299 then
    resp |> Response.headers |> Header.to_string
    |> Lwt_io.printf "Headers: %s\n"
    >>= fun () ->
    body |> Cohttp_lwt.Body.to_string >>= Lwt_io.printf "Body:\n%s\n"
  else Lwt.return ()

let headers header_list =
  Header.init ()
  |> (fun headers -> Header.add_list headers header_list)
  |> (fun headers -> Header.add headers "User-Agent" "coqbot")
  |> fun headers -> Header.add_authorization headers credentials

let send_request ~body ~uri header_list =
  let headers = headers header_list in
  Client.post ~body ~headers uri >>= print_response

let add_rebase_label (issue : GitHub_subscriptions.issue) =
  let body = Cohttp_lwt.Body.of_string "[ \"needs: rebase\" ]" in
  let uri =
    f "https://api.github.com/repos/%s/%s/issues/%d/labels" issue.owner
      issue.repo issue.number
    |> (fun url ->
         Stdio.printf "URL: %s\n" url ;
         url )
    |> Uri.of_string
  in
  send_request ~body ~uri []

let remove_rebase_label (issue : GitHub_subscriptions.issue) =
  let headers = headers [] in
  let uri =
    f "https://api.github.com/repos/%s/%s/issues/%d/labels/needs%%3A rebase"
      issue.owner issue.repo issue.number
    |> (fun url ->
         Stdio.printf "URL: %s\n" url ;
         url )
    |> Uri.of_string
  in
  Lwt_io.printf "Sending delete request.\n"
  >>= fun () -> Client.delete ~headers uri >>= print_response

let update_milestone new_milestone (issue : GitHub_subscriptions.issue) =
  let headers = headers [] in
  let uri =
    f "https://api.github.com/repos/%s/%s/issues/%d" issue.owner issue.repo
      issue.number
    |> (fun url ->
         Stdio.printf "URL: %s\n" url ;
         url )
    |> Uri.of_string
  in
  let body =
    "{\"milestone\": " ^ new_milestone ^ "}" |> Cohttp_lwt.Body.of_string
  in
  Lwt_io.printf "Sending patch request.\n"
  >>= fun () -> Client.patch ~headers ~body uri >>= print_response

let remove_milestone = update_milestone "null"

let add_pr_to_column pr_id column_id =
  let body =
    "{\"content_id\":" ^ Int.to_string pr_id
    ^ ", \"content_type\": \"PullRequest\"}"
    |> (fun body ->
         Stdio.printf "Body:\n%s\n" body ;
         body )
    |> Cohttp_lwt.Body.of_string
  in
  let uri =
    "https://api.github.com/projects/columns/" ^ Int.to_string column_id
    ^ "/cards"
    |> (fun url ->
         Stdio.printf "URL: %s\n" url ;
         url )
    |> Uri.of_string
  in
  send_request ~body ~uri project_api_preview_header

let send_status_check ~repo_full_name ~commit ~state ~url ~context ~description
    =
  Lwt_io.printf "Sending status check to %s (commit %s, state %s)\n"
    repo_full_name commit state
  >>= fun () ->
  let body =
    "{\"state\": \"" ^ state ^ "\",\"target_url\":\"" ^ url
    ^ "\", \"description\": \"" ^ description ^ "\", \"context\": \"" ^ context
    ^ "\"}"
    |> (fun body ->
         Stdio.printf "Body:\n %s\n" body ;
         body )
    |> Cohttp_lwt.Body.of_string
  in
  let uri =
    "https://api.github.com/repos/" ^ repo_full_name ^ "/statuses/" ^ commit
    |> Uri.of_string
  in
  send_request ~body ~uri []

let retry_job ~project_id ~build_id =
  let uri =
    "https://gitlab.com/api/v4/projects/" ^ Int.to_string project_id ^ "/jobs/"
    ^ Int.to_string build_id ^ "/retry"
    |> (fun url ->
         Stdio.printf "URL: %s\n" url ;
         url )
    |> Uri.of_string
  in
  send_request ~body:Cohttp_lwt.Body.empty ~uri gitlab_header

let handle_json action default body =
  try
    let json = Yojson.Basic.from_string body in
    (* print_endline "JSON decoded."; *)
    action json
  with
  | Yojson.Json_error err ->
      Stdio.printf "Json error: %s\n" err ;
      default
  | Yojson.Basic.Util.Type_error (err, _) ->
      Stdio.printf "Json type error: %s\n" err ;
      default

let generic_get relative_uri ?(header_list = []) ~default json_handler =
  let uri = "https://api.github.com/" ^ relative_uri |> Uri.of_string in
  let headers = headers header_list in
  Client.get ~headers uri
  >>= (fun (_response, body) -> Cohttp_lwt.Body.to_string body)
  >|= handle_json json_handler default

let get_pull_request_info pr_number =
  GitHub_queries.pull_request_id_db_id_and_milestone ~token:github_access_token
    ~owner:"coq" ~repo:"coq" ~number:pr_number ()
  >|= function
  | Ok result -> (
    match result#repository with
    | Some repo -> (
      match repo#pullRequest with
      | Some pr -> (
        match (pr#databaseId, pr#milestone) with
        | Some db_id, Some milestone -> (
          match milestone#description with
          | Some description -> (
            match
              GitHub_queries.Milestone.get_backport_info "coqbot" description
            with
            | Some bp_info -> Some (pr#id, db_id, bp_info)
            | _ -> None )
          | _ -> None )
        | _ -> None )
      | _ -> None )
    | _ -> None )
  | _ -> None

let get_status_check ~repo_full_name ~commit ~context =
  generic_get
    (Printf.sprintf "repos/%s/commits/%s/statuses" repo_full_name commit)
    ~default:false (fun json ->
      let open Yojson.Basic.Util in
      json |> to_list
      |> List.exists ~f:(fun json ->
             json |> member "context" |> to_string |> String.equal context ) )

let get_cards_in_column column_id =
  generic_get
    ("projects/columns/" ^ Int.to_string column_id ^ "/cards")
    ~header_list:project_api_preview_header ~default:[]
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
             else None ) )

let gitlab_ref (issue : GitHub_subscriptions.issue) :
    GitHub_subscriptions.remote_ref_info =
  {name= f "pr-%d" issue.number; repo_url= gitlab_repo issue.owner issue.repo}

let pull_request_updated (pr_info : GitHub_subscriptions.pull_request_info) ()
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
    then (fun () -> remove_rebase_label pr_info.issue.issue) |> Lwt.async ;
    (* Force push *)
    git_push (gitlab_ref pr_info.issue.issue) local_head_branch |> execute_cmd )
  else (
    (* Remove rebase label *)
    (fun () -> add_rebase_label pr_info.issue.issue) |> Lwt.async ;
    (* Add fail status check *)
    send_status_check
      ~repo_full_name:
        (f "%s/%s" pr_info.issue.issue.owner pr_info.issue.issue.repo)
      ~commit:pr_info.head.sha ~state:"error" ~url:""
      ~context:"GitLab CI pipeline (pull request)"
      ~description:
        "Pipeline did not run on GitLab CI because PR has conflicts with base \
         branch."
    |> Lwt_result.ok )

let pull_request_closed (pr_info : GitHub_subscriptions.pull_request_info) () =
  git_delete (gitlab_ref pr_info.issue.issue)
  |> execute_cmd >|= ignore
  <&>
  if not pr_info.merged then
    Lwt_io.printf
      "PR was closed without getting merged: remove the milestone.\n"
    >>= fun () -> remove_milestone pr_info.issue.issue
  else
    (* TODO: if PR was merged in master without a milestone, post an alert *)
    Lwt.return ()

let project_action ~(issue : GitHub_subscriptions.issue) ~column_id () =
  get_pull_request_info issue.number
  >>= function
  | None -> Lwt_io.printf "Could not find backporting info for PR.\n"
  | Some (id, _, {request_inclusion_column; rejected_milestone})
    when Int.equal request_inclusion_column column_id ->
      Lwt_io.printf
        "This was a request inclusion column: PR was rejected.\n\
         Change of milestone requested to: %s\n"
        rejected_milestone
      >>= fun () ->
      update_milestone rejected_milestone issue
      <&> GitHub_mutations.post_comment ~token:github_access_token ~id
            ~message:
              "This PR was postponed. Please update accordingly the milestone \
               of any issue that this fixes as this cannot be done \
               automatically."
  | _ -> Lwt_io.printf "This was not a request inclusion column: ignoring.\n"

let push_action json =
  Stdio.printf "Merge and backport commit messages:\n" ;
  let open Yojson.Basic.Util in
  let base_ref = json |> member "ref" |> to_string in
  let commit_action commit =
    let commit_msg = commit |> member "message" |> to_string in
    if string_match ~regexp:"Merge PR #\\([0-9]*\\):" commit_msg then
      let pr_number = Str.matched_group 1 commit_msg |> Int.of_string in
      Lwt_io.printf "%s\nPR #%d was merged.\n" commit_msg pr_number
      >>= fun () ->
      get_pull_request_info pr_number
      >>= fun pr_info ->
      match pr_info with
      | Some
          (_, pr_id, {backport_to; request_inclusion_column; backported_column})
        ->
          if "refs/heads/" ^ backport_to |> String.equal base_ref then
            Lwt_io.printf
              "PR was merged into the backportig branch directly.\n"
            >>= fun () -> add_pr_to_column pr_id backported_column
          else
            Lwt_io.printf "Backporting to %s was requested.\n" backport_to
            >>= fun () -> add_pr_to_column pr_id request_inclusion_column
      | None -> Lwt_io.printf "Did not get any backporting info.\n"
    else if string_match ~regexp:"Backport PR #\\([0-9]*\\):" commit_msg then
      let pr_number = Str.matched_group 1 commit_msg |> Int.of_string in
      Lwt_io.printf "%s\nPR #%d was backported.\n" commit_msg pr_number
      >>= fun () ->
      GitHub_queries.backported_pr_info ~token:github_access_token pr_number
        base_ref
      >>= function
      | Some ({card_id; column_id} as input) ->
          Lwt_io.printf "Moving card %s to column %s.\n" card_id column_id
          >>= fun () ->
          GitHub_mutations.mv_card_to_column ~token:github_access_token input
      | None ->
          Lwt_io.printf "Could not find backporting info for backported PR.\n"
    else Lwt.return ()
  in
  (fun () ->
    json |> member "commits" |> to_list |> Lwt_list.iter_s commit_action )
  |> Lwt.async

let get_build_trace ~project_id ~build_id =
  let uri =
    "https://gitlab.com/api/v4/projects/" ^ Int.to_string project_id ^ "/jobs/"
    ^ Int.to_string build_id ^ "/trace"
    |> Uri.of_string
  in
  let headers = headers gitlab_header in
  Client.get ~headers uri
  >>= fun (_response, body) -> Cohttp_lwt.Body.to_string body

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
  else if test "fatal: Remote branch pr-[0-9]* not found in upstream origin"
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

let branch_or_pr branch_name =
  if string_match ~regexp:"^pr-[0-9]*$" branch_name then "pull request"
  else "branch"

let job_action json =
  let open Yojson.Basic.Util in
  let build_status = json |> member "build_status" |> to_string in
  let build_id = json |> member "build_id" |> to_int in
  let build_name = json |> member "build_name" |> to_string in
  let commit = json |> extract_commit in
  let branch = json |> member "ref" |> to_string in
  let context = f "GitLab CI job %s (%s)" build_name (branch_or_pr branch) in
  let gitlab_full_name =
    json |> member "project_name" |> to_string
    |> Str.global_replace (Str.regexp " ") ""
  in
  let repo_full_name =
    if String.equal gitlab_full_name "near-protocol/nearcore" then
      "nearprotocol/nearcore"
    else gitlab_full_name
  in
  let send_url (kind, url) =
    (fun () ->
      let context = Printf.sprintf "%s: %s artifact" build_name kind in
      let description_base = Printf.sprintf "Link to %s build artifact" kind in
      url |> Uri.of_string |> Client.get
      >>= fun (resp, _) ->
      if resp |> Response.status |> Code.code_of_status |> Int.equal 200 then
        send_status_check ~repo_full_name ~commit ~state:"success" ~url
          ~context ~description:(description_base ^ ".")
      else
        Lwt_io.printf "But we didn't get a 200 code when checking the URL.\n"
        >>= fun () ->
        send_status_check ~repo_full_name ~commit ~state:"failure"
          ~url:
            (Printf.sprintf "https://gitlab.com/%s/-/jobs/%d" gitlab_full_name
               build_id)
          ~context
          ~description:(description_base ^ ": not found.") )
    |> Lwt.async
  in
  if String.equal build_status "failed" then
    let project_id = json |> member "project_id" |> to_int in
    let failure_reason = json |> member "build_failure_reason" |> to_string in
    let allow_fail = json |> member "build_allow_failure" |> to_bool in
    let send_status_check () =
      if allow_fail then Lwt_io.printf "Job is allowed to fail.\n"
      else
        Lwt_io.printf "Pushing a status check...\n"
        >>= fun () ->
        send_status_check ~repo_full_name ~commit ~state:"failure"
          ~url:
            (Printf.sprintf "https://gitlab.com/%s/-/jobs/%d" gitlab_full_name
               build_id)
          ~context
          ~description:(failure_reason ^ " on GitLab CI")
    in
    (fun () ->
      Lwt_io.printf "Failed job %d of project %d.\nFailure reason: %s\n"
        build_id project_id failure_reason
      >>= fun () ->
      if String.equal failure_reason "runner_system_failure" then
        Lwt_io.printf "Runner failure reported by GitLab CI. Retrying...\n"
        <&> retry_job ~project_id ~build_id
      else if String.equal failure_reason "stuck_or_timeout_failure" then
        Lwt_io.printf "Timeout reported by GitLab CI.\n"
        <&> send_status_check ()
      else if String.equal failure_reason "script_failure" then
        Lwt_io.printf
          "GitLab CI reports a script failure but it could be something else. \
           Checking the trace...\n"
        >>= fun () ->
        repeat_request (get_build_trace ~project_id ~build_id)
        >|= trace_action ~repo_full_name
        >>= function
        | Warn -> Lwt_io.printf "Actual failure.\n" <&> send_status_check ()
        | Retry -> retry_job ~project_id ~build_id
        | Ignore -> Lwt.return ()
      else Lwt_io.printf "Unusual error.\n" <&> send_status_check () )
    |> Lwt.async
  else if String.equal build_status "success" then (
    (fun () ->
      get_status_check ~repo_full_name ~commit ~context
      >>= fun b ->
      if b then
        Lwt_io.printf
          "There existed a previous status check for this build, we'll \
           override it.\n"
        <&> send_status_check ~repo_full_name ~commit ~state:"success"
              ~url:
                (Printf.sprintf "https://gitlab.com/%s/-/jobs/%d"
                   gitlab_full_name build_id)
              ~context
              ~description:"Test succeeded on GitLab CI after being retried"
      else Lwt.return () )
    |> Lwt.async ;
    if String.equal build_name "doc:refman" then (
      Stdio.printf
        "This is a successful refman build. Pushing a status check with a \
         link...\n" ;
      let url_base =
        f
          "https://coq.gitlab.io/-/coq/-/jobs/%d/artifacts/_install_ci/share/doc/coq"
          build_id
      in
      [ ("refman", Printf.sprintf "%s/sphinx/html/index.html" url_base)
      ; ("stdlib", Printf.sprintf "%s/html/stdlib/index.html" url_base) ]
      |> List.iter ~f:send_url )
    else if String.equal build_name "doc:ml-api:odoc" then (
      Stdio.printf
        "This is a successful ml-api build. Pushing a status check with a \
         link...\n" ;
      ( "ml-api"
      , f
          "https://coq.gitlab.io/-/coq/-/jobs/%d/artifacts/_build/default/_doc/_html/index.html"
          build_id )
      |> send_url ) )

let pipeline_action json =
  let open Yojson.Basic.Util in
  let pipeline_json = json |> member "object_attributes" in
  let state = pipeline_json |> member "status" |> to_string in
  let id = pipeline_json |> member "id" |> to_int in
  let commit = json |> extract_commit in
  let branch =
    json |> member "object_attributes" |> member "ref" |> to_string
  in
  let gitlab_full_name =
    json |> member "project" |> member "path_with_namespace" |> to_string
  in
  let repo_full_name =
    if String.equal gitlab_full_name "near-protocol/nearcore" then
      "nearprotocol/nearcore"
    else gitlab_full_name
  in
  match state with
  | "skipped" -> ()
  | _ ->
      let state, description =
        match state with
        | "success" -> ("success", "Pipeline completed on GitLab CI")
        | "pending" -> ("pending", "Pipeline is pending on GitLab CI")
        | "running" -> ("pending", "Pipeline is running on GitLab CI")
        | "failed" -> ("failure", "Pipeline completed with errors on GitLab CI")
        | "cancelled" -> ("error", "Pipeline was cancelled on GitLab CI")
        | _ -> ("error", "Unknown pipeline status: " ^ state)
      in
      (fun () ->
        send_status_check ~repo_full_name ~commit ~state
          ~url:
            (Printf.sprintf "https://gitlab.com/%s/pipelines/%d"
               gitlab_full_name id)
          ~context:(f "GitLab CI pipeline (%s)" (branch_or_pr branch))
          ~description )
      |> Lwt.async

let owner_team_map =
  Map.of_alist_exn
    (module String)
    [("martijnbastiaan-test-org", "martijnbastiaan-test-team")]

(* TODO: deprecate unsigned webhooks *)

let callback _conn req body =
  let body = Cohttp_lwt.Body.to_string body in
  (* print_endline "Request received."; *)
  let handle_request action =
    (fun () -> body >|= handle_json action ()) |> Lwt.async ;
    Server.respond_string ~status:`OK ~body:"" ()
  in
  match Uri.path (Request.uri req) with
  | "/job" -> handle_request job_action
  | "/pipeline" -> handle_request pipeline_action
  | "/push" -> handle_request push_action
  | "/pull_request" | "/github" -> (
      body
      >>= fun body ->
      match
        GitHub_subscriptions.receive_github ~secret:github_webhook_secret
          (Request.headers req) body
      with
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
                GitHub_mutations.post_comment ~token:github_access_token
                  ~id:pr_info.issue.id
                  ~message:
                    (f
                       "Hello, thanks for your pull request!\n\
                        In the future, we strongly recommend that you *do \
                        not* use %s as the name of your branch when \
                        submitting a pull request.\n\
                        By the way, you may be interested in reading [our \
                        contributing \
                        guide](https://github.com/coq/coq/blob/master/CONTRIBUTING.md)."
                       pr_info.base.branch.name) )
              |> Lwt.async
          | _ -> () ) ;
          match Map.find owner_team_map pr_info.issue.issue.owner with
          | Some team when signed ->
              (fun () ->
                let open Lwt_result.Infix in
                GitHub_queries.get_team_membership ~token:github_access_token
                  ~org:pr_info.issue.issue.owner ~team ~user:pr_info.issue.user
                >>= (fun is_member ->
                      if is_member then (
                        Stdio.printf "Authorized user: pushing to GitLab.\n" ;
                        pull_request_updated pr_info () )
                      else
                        Lwt_io.print "Unauthorized user: doing nothing.\n"
                        |> Lwt_result.ok )
                |> Fn.flip Lwt_result.bind_lwt_err (fun err ->
                       Lwt_io.printf "Error: %s\n" err ) )
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
              pull_request_updated pr_info |> Lwt.async ;
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
          (fun () ->
            GitHub_queries.get_issue_closer_info ~token:github_access_token
              issue
            >>= function
            | Ok result ->
                GitHub_mutations.reflect_pull_request_milestone
                  ~token:github_access_token result
            | Error err -> Lwt_io.print (f "Error: %s\n" err) )
          |> Lwt.async ;
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
          ( _
          , GitHub_subscriptions.CommentCreated {issue= {pull_request= false}}
          ) ->
          Server.respond_string ~status:`OK
            ~body:"Issue comment: nothing to do." ()
      | Ok (signed, GitHub_subscriptions.CommentCreated comment_info)
        when string_match ~regexp:"@coqbot: [Rr]un CI now" comment_info.body -> (
        match Map.find owner_team_map comment_info.issue.issue.owner with
        | Some team when signed ->
            (fun () ->
              let open Lwt_result.Infix in
              GitHub_queries.get_team_membership ~token:github_access_token
                ~org:comment_info.issue.issue.owner ~team
                ~user:comment_info.author
              >>= (fun is_member ->
                    if is_member then (
                      Stdio.printf "Authorized user: pushing to GitLab.\n" ;
                      match comment_info.pull_request with
                      | Some pr_info -> pull_request_updated pr_info ()
                      | None ->
                          GitHub_queries.get_pull_request_info
                            ~token:github_access_token comment_info.issue
                          >>= fun pr_info -> pull_request_updated pr_info () )
                    else
                      Lwt_io.print "Unauthorized user: doing nothing.\n"
                      |> Lwt_result.ok )
              |> Fn.flip Lwt_result.bind_lwt_err (fun err ->
                     Lwt_io.printf "Error: %s\n" err ) )
            |> Lwt.async ;
            Server.respond_string ~status:`OK
              ~body:
                (f
                   "Received a request to run CI: checking that @%s is a \
                    member of @%s/%s before doing so."
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
              () )
      | Ok (_signed, GitHub_subscriptions.CommentCreated comment_info)
        when string_match ~regexp:"@coqbot: [Mm]erge now" comment_info.body ->
          (* Should be restricted to coq *)
          Server.respond_string ~status:`OK
            ~body:
              (f "Received a request to merge the PR: not implemented yet.")
            ()
      | Ok (_, GitHub_subscriptions.CommentCreated _) ->
          Server.respond_string ~status:`OK
            ~body:
              (f
                 "Pull request comment doesn't contain any request to \
                  @coqbot: nothing to do.")
            ()
      | Ok (_, GitHub_subscriptions.NoOp s) ->
          Server.respond_string ~status:`OK
            ~body:(f "No action taken: %s" s)
            ()
      | Ok _ ->
          Server.respond_string ~status:`OK
            ~body:"No action taken: event or action is not yet supported." ()
      | Error s ->
          Server.respond_string ~status:(Code.status_of_code 400)
            ~body:(f "Error: %s" s) () )
  | _ -> Server.respond_not_found ()

let server =
  (fun () ->
    Lwt_io.printf "Initializing repository...\n"
    <&> ( "git config --global user.email \"coqbot@users.noreply.github.com\""
        |&& "git config --global user.name \"coqbot\"" |&& "git init --bare"
        |> execute_cmd >|= ignore ) )
  |> Lwt.async ;
  let mode = `TCP (`Port port) in
  Server.create ~mode (Server.make ~callback ())

let () =
  Lwt.async_exception_hook :=
    fun exn ->
      Stdio.printf "Error: Unhandled exception: %s" (Exn.to_string exn)

let () = Lwt_main.run server
