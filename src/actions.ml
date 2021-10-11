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

type coq_job_info =
  { docker_image: string
  ; build_dependency: string
  ; compiler: string
  ; compiler_edge: string
  ; opam_variant: string
  ; opam_switch: string }

let send_status_check ~bot_info job_info ~pr_num (gh_owner, gh_repo)
    ~github_repo_full_name ~gitlab_repo_full_name ~context ~failure_reason
    ~external_id ~trace =
  let job_url =
    f "https://gitlab.com/%s/-/jobs/%d" gitlab_repo_full_name job_info.build_id
  in
  let trace_lines =
    trace
    |> Str.global_replace (Str.regexp "\027\\[[0-9]*;[0-9]*m") ""
    |> Str.global_replace (Str.regexp "\027\\[0K") ""
    |> Str.global_replace (Str.regexp "section_start:[0-9]*:[a-z_]*\r") ""
    |> Str.global_replace (Str.regexp "section_end:[0-9]*:[a-z_]*\r") ""
    |> String.split_lines
  in
  let title, last_index_of_error =
    (* We try to find the line starting with "Error" only in the case
       of an actual script failure. *)
    match failure_reason with
    | "script_failure" ->
        ( "Test has failed on GitLab CI"
        , trace_lines
          |> List.filter_mapi ~f:(fun i line ->
                 if String.is_prefix ~prefix:"Error" line then Some i else None)
          |> List.last )
    | "job_execution_timeout" ->
        ("Test has reached timeout on GitLab CI", None)
    | _ ->
        (failure_reason ^ " on GitLab CI", None)
  in
  let trace_description, short_trace =
    (* If we have a last index of error, we display 40 lines starting
       at the line before (which should include the filename).
       Otherwise, we display only the last 40 lines of the trace *)
    match last_index_of_error with
    | None ->
        ( f
            "We show below the last 40 lines of the trace from GitLab (the \
             complete trace is available [here](%s))."
            job_url
        , trace_lines
          |> Fn.flip List.drop (List.length trace_lines - 40)
          |> String.concat ~sep:"\n" )
    | Some index_of_error ->
        ( f
            "We show below an excerpt from the trace from GitLab starting \
             around the last detected \"Error\" (the complete trace is \
             available [here](%s))."
            job_url
        , trace_lines
          |> Fn.flip List.drop (index_of_error - 1)
          |> Fn.flip List.take 40 |> String.concat ~sep:"\n" )
  in
  let coq_job_info =
    let open Option in
    let find regexp =
      List.find_map trace_lines ~f:(fun line ->
          if string_match ~regexp line then Some (Str.matched_group 1 line)
          else None)
    in
    find "^Using Docker executor with image \\([^ ]+\\)"
    >>= fun docker_image ->
    find "^Downloading artifacts for \\(build:[^ ]+\\)"
    >>= fun build_dependency ->
    find "^COMPILER=\\(.*\\)"
    >>= fun compiler ->
    find "^COMPILER_EDGE=\\(.*\\)"
    >>= fun compiler_edge ->
    find "^OPAM_VARIANT=\\(.*\\)"
    >>= fun opam_variant ->
    find "^OPAM_SWITCH=\\(.*\\)"
    >>= fun opam_switch ->
    Some
      { docker_image
      ; build_dependency
      ; compiler
      ; compiler_edge
      ; opam_variant
      ; opam_switch }
  in
  let summary_tail =
    ( match coq_job_info with
    | Some
        { docker_image
        ; build_dependency
        ; compiler
        ; compiler_edge
        ; opam_variant
        ; opam_switch= ("base" | "edge") as opam_switch } ->
        let switch_name =
          ( match opam_switch with
          | "base" ->
              compiler
          | "edge" ->
              compiler_edge
          | _ ->
              failwith "opam_switch was already determined to be base or edge"
          )
          ^ opam_variant
        in
        f
          "This job ran on the Docker image `%s`, depended on the build job \
           `%s` with OCaml `%s`.\n\n"
          docker_image build_dependency switch_name
    | Some {opam_switch} ->
        Stdio.printf "Unrecognized OPAM_SWITCH: %s.\n" opam_switch ;
        ""
    | None ->
        "" )
    ^ trace_description
  in
  let text = "```\n" ^ short_trace ^ "\n```" in
  if job_info.allow_fail then
    Lwt_io.printf "Job is allowed to fail.\n"
    <&> ( match bot_info.github_install_token with
        | None ->
            (* Allow failure messages are reported with the Checks API only. *)
            Lwt.return_unit
        | Some _ -> (
            GitHub_queries.get_repository_id ~bot_info ~owner:gh_owner
              ~repo:gh_repo
            >>= function
            | Ok repo_id ->
                GitHub_mutations.create_check_run ~bot_info ~name:context
                  ~repo_id ~head_sha:job_info.common_info.head_commit
                  ~conclusion:NEUTRAL ~status:COMPLETED ~title
                  ~details_url:job_url
                  ~summary:("This job is allowed to fail.\n\n" ^ summary_tail)
                  ~text ~external_id ()
            | Error e ->
                Lwt_io.printf "No repo id: %s\n" e ) )
    <&>
    (* If we are in a PR branch, we can post a comment. *)
    if String.equal job_info.build_name "library:ci-fiat_crypto_legacy" then
      let message =
        f "The job [%s](%s) has failed in allow failure mode\nping @JasonGross"
          job_info.build_name job_url
      in
      match pr_num with
      | Some number -> (
          GitHub_queries.get_pull_request_refs ~bot_info ~owner:gh_owner
            ~repo:gh_repo ~number
          >>= function
          | Ok {issue= id; head}
          (* Commits reported back by get_pull_request_refs are surrounded in double quotes *)
            when String.equal head.sha
                   (f "\"%s\"" job_info.common_info.head_commit) ->
              GitHub_mutations.post_comment ~bot_info ~id ~message
              >>= GitHub_mutations.report_on_posting_comment
          | Ok {head} ->
              Lwt_io.printf
                "We are on a PR branch but the commit (%s) is not the current \
                 head of the PR (%s). Doing nothing.\n"
                job_info.common_info.head_commit head.sha
          | Error err ->
              Lwt_io.printf
                "Couldn't get a database id for %s#%d because the following \
                 error occured:\n\
                 %s\n"
                gitlab_repo_full_name number err )
      | None ->
          Lwt_io.printf "We are not on a PR branch. Doing nothing.\n"
    else Lwt.return_unit
  else
    Lwt_io.printf "Pushing a status check...\n"
    <&>
    match bot_info.github_install_token with
    | None ->
        GitHub_mutations.send_status_check ~repo_full_name:github_repo_full_name
          ~commit:job_info.common_info.head_commit ~state:"failure" ~url:job_url
          ~context ~description:title ~bot_info
    | Some _ -> (
        GitHub_queries.get_repository_id ~bot_info ~owner:gh_owner ~repo:gh_repo
        >>= function
        | Ok repo_id ->
            GitHub_mutations.create_check_run ~bot_info ~name:context ~repo_id
              ~head_sha:job_info.common_info.head_commit ~conclusion:FAILURE
              ~status:COMPLETED ~title ~details_url:job_url
              ~summary:
                ( "This job has failed. If you need to, you can restart it \
                   directly in the GitHub interface using the \"Re-run\" \
                   button.\n\n" ^ summary_tail )
              ~text ~external_id ()
        | Error e ->
            Lwt_io.printf "No repo id: %s\n" e )

let send_doc_url_aux ~bot_info job_info (kind, url) =
  let context = f "%s: %s artifact" job_info.build_name kind in
  let description_base = f "Link to %s build artifact" kind in
  url |> Uri.of_string |> Client.get
  >>= fun (resp, _) ->
  if resp |> Response.status |> Code.code_of_status |> Int.equal 200 then
    GitHub_mutations.send_status_check ~repo_full_name:"coq/coq"
      ~commit:job_info.common_info.head_commit ~state:"success" ~url ~context
      ~description:(description_base ^ ".") ~bot_info
  else
    Lwt_io.printf "But we didn't get a 200 code when checking the URL.\n"
    <&>
    let job_url = f "https://gitlab.com/coq/coq/-/jobs/%d" job_info.build_id in
    GitHub_mutations.send_status_check ~repo_full_name:"coq/coq"
      ~commit:job_info.common_info.head_commit ~state:"failure" ~url:job_url
      ~context
      ~description:(description_base ^ ": not found.")
      ~bot_info

let send_doc_url ~bot_info ~github_repo_full_name job_info =
  match (github_repo_full_name, job_info.build_name) with
  | "coq/coq", "doc:refman" ->
      Lwt_io.printf
        "This is a successful refman build. Pushing a status check with a \
         link...\n"
      <&>
      let url_base =
        f
          "https://coq.gitlab.io/-/coq/-/jobs/%d/artifacts/_install_ci/share/doc/coq"
          job_info.build_id
      in
      [ ("refman", f "%s/sphinx/html/index.html" url_base)
      ; ("stdlib", f "%s/html/stdlib/index.html" url_base) ]
      |> List.map ~f:(send_doc_url_aux ~bot_info job_info)
      |> Lwt.all |> Lwt.map ignore
  | "coq/coq", "doc:ml-api:odoc" ->
      Lwt_io.printf
        "This is a successful ml-api build. Pushing a status check with a \
         link...\n"
      <&>
      let url_base =
        f
          "https://coq.gitlab.io/-/coq/-/jobs/%d/artifacts/_build/default/_doc/_html/index.html"
          job_info.build_id
      in
      ("ml-api", url_base) |> send_doc_url_aux ~bot_info job_info
  | _ ->
      Lwt.return_unit

type build_failure = Warn of string | Retry | Ignore

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
    ( test "Uploading artifacts.*to coordinator... failed"
    || test "Uploading artifacts.*to coordinator... error" )
    && not (test "Uploading artifacts.*to coordinator... ok")
  then (
    Stdio.printf "Artifact uploading failure. Retrying...\n" ;
    Retry )
  else if
    test "ERROR: Downloading artifacts.*from coordinator... error"
    && test "FATAL: invalid argument"
  then (
    Stdio.printf "Artifact downloading failure. Retrying...\n" ;
    Retry )
  else if
    test "transfer closed with outstanding read data remaining"
    || test "HTTP request sent, awaiting response... 50[0-9]"
    || test "The requested URL returned error: 502"
    (*|| test "[Tt]he remote end hung up unexpectedly"*)
    (* Can happen with (actual) issues with overlays. *)
    || test "error: unable to download 'https://cache.nixos.org/"
    || test "fatal: unable to access .* Couldn't connect to server"
    || test "fatal: unable to access .* Could not resolve host"
    || test "Resolving .* failed: Temporary failure in name resolution"
  then (
    Stdio.printf "Connectivity issue. Retrying...\n" ;
    Retry )
  else if test "fatal: reference is not a tree" then (
    Stdio.printf "Normal failure: pull request was force-pushed.\n" ;
    Ignore )
  else if
    test "fatal: Remote branch pr-[0-9]* not found in upstream origin"
    || test "fatal: [Cc]ouldn't find remote ref refs/heads/pr-"
  then (
    Stdio.printf "Normal failure: pull request was closed.\n" ;
    Ignore )
  else if
    String.equal repo_full_name "coq/coq"
    && test "Error response from daemon: manifest for .* not found"
  then (
    Stdio.printf "Docker image not found. Do not report anything specific.\n" ;
    Ignore )
  else Warn trace

let job_failure ~bot_info job_info ~pr_num (gh_owner, gh_repo)
    ~github_repo_full_name ~gitlab_repo_full_name ~context ~failure_reason
    ~external_id =
  let build_id = job_info.build_id in
  let project_id = job_info.common_info.project_id in
  Lwt_io.printf "Failed job %d of project %d.\nFailure reason: %s\n" build_id
    project_id failure_reason
  >>= fun () ->
  if String.equal failure_reason "runner_system_failure" then
    Lwt_io.printf "Runner failure reported by GitLab CI. Retrying...\n"
    <&> GitLab_mutations.retry_job ~project_id ~build_id ~bot_info
  else
    Lwt_io.printf
      "Failure reason reported by GitLab CI: %s.\nRetrieving the trace...\n"
      failure_reason
    <&> ( GitLab_queries.get_build_trace ~bot_info ~project_id ~build_id
        >|= trace_action ~repo_full_name:gitlab_repo_full_name
        >>= function
        | Warn trace ->
            Lwt_io.printf "Actual failure.\n"
            <&> send_status_check ~bot_info job_info ~pr_num (gh_owner, gh_repo)
                  ~github_repo_full_name ~gitlab_repo_full_name ~context
                  ~failure_reason ~external_id ~trace
        | Retry ->
            GitLab_mutations.retry_job ~bot_info ~project_id ~build_id
        | Ignore ->
            Lwt.return_unit )

let job_success_or_pending ~bot_info (gh_owner, gh_repo)
    ({build_id} as job_info) ~github_repo_full_name ~gitlab_repo_full_name
    ~context ~state ~external_id =
  GitHub_queries.get_status_check ~bot_info ~owner:gh_owner ~repo:gh_repo
    ~commit:job_info.common_info.head_commit ~context
  >>= function
  | Ok true -> (
      Lwt_io.printf
        "There existed a previous status check for this build, we'll override \
         it.\n"
      <&>
      let job_url =
        f "https://gitlab.com/%s/-/jobs/%d" gitlab_repo_full_name build_id
      in
      let state, status, conclusion, description =
        match state with
        | "success" ->
            ( "success"
            , COMPLETED
            , Some SUCCESS
            , "Test succeeded on GitLab CI after being retried" )
        | "created" ->
            ( "pending"
            , QUEUED
            , None
            , "Test pending on GitLab CI after being retried" )
        | "running" ->
            ( "pending"
            , IN_PROGRESS
            , None
            , "Test running on GitLab CI after being retried" )
        | _ ->
            failwith
              (f "Error: job_success_or_pending received unknown state %s."
                 state)
      in
      match bot_info.github_install_token with
      | None ->
          GitHub_mutations.send_status_check ~bot_info
            ~repo_full_name:github_repo_full_name
            ~commit:job_info.common_info.head_commit ~state ~url:job_url
            ~context ~description
      | Some _ -> (
          GitHub_queries.get_repository_id ~bot_info ~owner:gh_owner
            ~repo:gh_repo
          >>= function
          | Ok repo_id ->
              GitHub_mutations.create_check_run ~bot_info ~name:context ~status
                ~repo_id ~head_sha:job_info.common_info.head_commit ?conclusion
                ~title:description ~details_url:job_url ~summary:"" ~external_id
                ()
          | Error e ->
              Lwt_io.printf "No repo id: %s\n" e ) )
  | Ok _ ->
      Lwt.return_unit
  | Error e ->
      Lwt_io.printf "%s\n" e

let job_action ~bot_info ({build_name} as job_info) ~gitlab_mapping =
  let pr_num, branch_or_pr = pr_from_branch job_info.common_info.branch in
  let context = f "GitLab CI job %s (%s)" build_name branch_or_pr in
  let owner, repo =
    let repo_url = job_info.common_info.repo_url in
    if not (string_match ~regexp:".*:\\(.*\\)/\\(.*\\).git" repo_url) then
      failwith "Could not match project name on repository url.\n" ;
    (Str.matched_group 1 repo_url, Str.matched_group 2 repo_url)
  in
  let gitlab_repo_full_name = owner ^ "/" ^ repo in
  let gh_owner, gh_repo =
    github_repo_of_gitlab_project_path ~gitlab_mapping gitlab_repo_full_name
  in
  let github_repo_full_name = gh_owner ^ "/" ^ gh_repo in
  let external_id =
    f "projects/%d/jobs/%d" job_info.common_info.project_id job_info.build_id
  in
  match job_info.build_status with
  | "failed" ->
      let failure_reason = Option.value_exn job_info.failure_reason in
      job_failure ~bot_info job_info ~pr_num (gh_owner, gh_repo)
        ~github_repo_full_name ~gitlab_repo_full_name ~context ~failure_reason
        ~external_id
  | "success" as state ->
      job_success_or_pending ~bot_info (gh_owner, gh_repo) job_info
        ~github_repo_full_name ~gitlab_repo_full_name ~context ~state
        ~external_id
      <&> send_doc_url ~bot_info job_info ~github_repo_full_name
  | ("created" | "running") as state ->
      job_success_or_pending ~bot_info (gh_owner, gh_repo) job_info
        ~github_repo_full_name ~gitlab_repo_full_name ~context ~state
        ~external_id
  | "cancelled" | "canceled" ->
      (* Ideally we should check if a status was already reported for
         this job.  But it is important to avoid making dozens of
         requests at once when a pipeline is canceled.  So we should
         have a caching mechanism to limit this case to a single
         request. *)
      Lwt.return_unit
  | unknown_state ->
      Lwt_io.printf "Unknown job status: %s\n" unknown_state

let create_pipeline_summary ?summary_top pipeline_info pipeline_url =
  let sorted_builds =
    pipeline_info.builds
    |> List.sort ~compare:(fun build1 build2 ->
           String.compare build1.build_name build2.build_name)
  in
  pipeline_info.stages
  |> List.concat_map ~f:(fun stage ->
         sorted_builds
         |> List.filter_map ~f:(fun build ->
                if String.equal build.stage stage then
                  Some
                    (f "  - [%s](%s/-/jobs/%d)" build.build_name
                       pipeline_info.common_info.repo_url build.build_id)
                else None)
         |> List.cons ("- " ^ stage))
  |> List.cons
       (f "This [GitLab pipeline](%s) contains the following stages and jobs:"
          pipeline_url)
  |> (match summary_top with Some text -> List.cons text | None -> Fn.id)
  |> String.concat ~sep:"\n\n"

type ci_minimization_info =
  { target: string
  ; full_target: string
  ; docker_image: string
  ; opam_switch: string
  ; failing_urls: string
  ; passing_urls: string }

let run_ci_minimization ~bot_info ~comment_thread_id ~owner ~repo ~pr_number
    ~base ~head ~ci_minimization_infos ~bug_file_contents =
  (* for convenience of control flow, we always create the temporary
     file, but we only pass in the file name if the bug file contents
     is non-None *)
  Lwt_io.with_temp_file (fun (bug_file_name, bug_file_ch) ->
      Lwt_io.write bug_file_ch (Option.value ~default:"" bug_file_contents)
      >>= fun () ->
      Lwt_io.flush bug_file_ch
      >>= fun () ->
      let bug_file_name =
        Option.map ~f:(fun _ -> bug_file_name) bug_file_contents
      in
      Lwt_list.map_s
        (fun {target; opam_switch; failing_urls; passing_urls; docker_image} ->
          git_run_ci_minimization ~bot_info ~comment_thread_id ~owner ~repo
            ~pr_number ~docker_image ~target ~opam_switch ~failing_urls
            ~passing_urls ~base ~head ~bug_file_name
          >>= fun result -> Lwt.return (target, result) )
        ci_minimization_infos )
  >>= fun results ->
  results
  |> List.partition_map ~f:(function
       | target, Ok () ->
           Either.First target
       | target, Error f ->
           Either.Second (target, f))
  |> Lwt.return

type ci_minimization_job_suggestion_info =
  { base_job_failed: bool
  ; base_job_errored: string option
  ; head_job_succeeded: bool
  ; missing_error: bool
  ; non_v_file: string option
  ; job_kind: string (*; overlayed: bool*) }

let ci_minimization_extract_job_specific_info ~head_pipeline_summary
    ~base_pipeline_summary ~base_checks_errors ~base_checks = function
  | ( {name= full_name; summary= Some summary; text= Some text}
    , head_job_succeeded ) ->
      let base_job_errored =
        List.find_map
          ~f:(fun (base_name, err) ->
            if String.equal full_name base_name then Some err else None)
          base_checks_errors
      in
      let base_job_failed =
        List.exists
          ~f:(fun ({name= base_name}, success_base) ->
            String.equal full_name base_name && not success_base)
          base_checks
      in
      if string_match ~regexp:"\\([^: ]*\\):\\(ci-[A-Za-z0-9_-]*\\)" full_name
      then
        let name = Str.matched_group 0 full_name in
        let job_kind = Str.matched_group 1 full_name in
        let target = Str.matched_group 2 full_name in
        let extract_artifact_url job_name summary =
          if
            string_match
              ~regexp:(f "\\[%s\\](\\([^)]+\\))" (Str.quote job_name))
              summary
          then Some (Str.matched_group 1 summary ^ "/artifacts/download")
          else None
        in
        let collapse_summary name summary =
          f "<details><summary>%s</summary>\n\n%s\n</details>\n" name summary
        in
        if
          string_match
            ~regexp:
              "This job ran on the Docker image `\\([^`]+\\)`, depended on the \
               build job `\\([^`]+\\)` with OCaml `\\([^`]+\\)`.\n\n"
            summary
        then
          let docker_image, build_job, opam_switch =
            ( Str.matched_group 1 summary
            , Str.matched_group 2 summary
            , Str.matched_group 3 summary )
          in
          let missing_error, non_v_file =
            if
              string_match
                ~regexp:
                  "\n\
                   File \"\\([^\"]*\\)\", line [0-9]*, characters [0-9]*-[0-9]*:\n\
                   Error:"
                text
            then
              let filename = Str.matched_group 1 text in
              ( false
              , if String.is_suffix ~suffix:".v" filename then None
                else Some filename )
            else (true, None)
          in
          match
            ( extract_artifact_url build_job base_pipeline_summary
            , extract_artifact_url build_job head_pipeline_summary
            , extract_artifact_url name base_pipeline_summary
            , extract_artifact_url name head_pipeline_summary )
          with
          | ( Some base_build_url
            , Some head_build_url
            , Some base_job_url
            , Some head_job_url ) ->
              Ok
                ( { base_job_failed
                  ; base_job_errored
                  ; missing_error
                  ; non_v_file
                  ; job_kind
                  ; head_job_succeeded (*; overlayed= false (* XXX FIXME *)*) }
                , { target
                  ; full_target= name
                  ; docker_image
                  ; opam_switch
                  ; failing_urls= head_build_url ^ " " ^ head_job_url
                  ; passing_urls= base_build_url ^ " " ^ base_job_url } )
          | None, _, _, _ ->
              Error
                (f "Could not find base build job url for %s in:\n%s" build_job
                   (collapse_summary "Base Pipeline Summary"
                      base_pipeline_summary))
          | _, None, _, _ ->
              Error
                (f "Could not find head build job url for %s in:\n%s" build_job
                   (collapse_summary "Head Pipeline Summary"
                      head_pipeline_summary))
          | _, _, None, _ ->
              Error
                (f "Could not find base job url for %s in:\n%s" name
                   (collapse_summary "Base Pipeline Summary"
                      base_pipeline_summary))
          | _, _, _, None ->
              Error
                (f "Could not find head job url for %s in:\n%s" name
                   (collapse_summary "Head Pipeline Summary"
                      head_pipeline_summary))
        else
          Error
            (f "Could not find needed parameters for job %s in summary:\n%s\n"
               name
               (collapse_summary "Summary" summary))
      else
        Error (f "Could not separate '%s' into job_kind:ci-target." full_name)
  | {name; summary= None}, _ ->
      Error (f "Could not find summary for job %s." name)
  | {name; text= None}, _ ->
      Error (f "Could not find text for job %s." name)

type ci_minimization_pr_info =
  { comment_thread_id: string
  ; base: string
  ; head: string
  ; pr_number: int
  ; draft: bool
  ; labels: string list
  ; base_pipeline_finished: bool
  ; head_pipeline_finished: bool
  ; failed_test_suite_jobs: string list }

let shorten_ci_check_name target =
  target
  |> Str.global_replace (Str.regexp "GitLab CI job") ""
  |> Str.global_replace (Str.regexp "(pull request)") ""
  |> Str.global_replace (Str.regexp "(branch)") ""
  |> Stdlib.String.trim

let fetch_ci_minimization_info ~bot_info ~owner ~repo ~pr_number
    ~head_pipeline_summary =
  Lwt_io.printlf "I'm going to look for failed tests to minimize on PR #%d."
    pr_number
  >>= fun () ->
  GitHub_queries.get_pull_request_refs ~bot_info ~owner ~repo ~number:pr_number
  >>= function
  | Error err ->
      Lwt.return_error
        ( None
        , f "Error while fetching PR refs for %s/%s#%d for CI minimization: %s"
            owner repo pr_number err )
  | Ok {base= {sha= base}; head= {sha= head}} -> (
      (* TODO: figure out why there are quotes, cf https://github.com/coq/bot/issues/61 *)
      let base = Str.global_replace (Str.regexp "\"") "" base in
      let head = Str.global_replace (Str.regexp "\"") "" head in
      GitHub_queries.get_base_and_head_checks ~bot_info ~owner ~repo ~pr_number
        ~base ~head
      >>= function
      | Error err ->
          Lwt.return_error
            ( None
            , f "Error while looking for failed library tests to minimize: %s"
                err )
      | Ok {pr_id; base_checks; head_checks; draft; labels} -> (
          let partition_errors =
            List.partition_map ~f:(function
              | Error (name, error) ->
                  Either.First (shorten_ci_check_name name, error)
              | Ok (result, status) ->
                  Either.Second
                    ( {result with name= shorten_ci_check_name result.name}
                    , status ))
          in
          let base_checks_errors, base_checks = partition_errors base_checks in
          let head_checks_errors, head_checks = partition_errors head_checks in
          head_checks_errors
          |> Lwt_list.iter_p (fun (_, error) ->
                 Lwt_io.printlf
                   "Non-fatal error while looking for failed tests of PR #%d \
                    to minimize: %s"
                   pr_number error)
          >>= fun () ->
          let extract_pipeline_check =
            List.partition3_map ~f:(fun (check_tab_info, success) ->
                if
                  String.is_prefix ~prefix:"GitLab CI pipeline"
                    check_tab_info.name
                then `Fst (check_tab_info, Option.is_some success)
                else
                  match success with
                  | Some success ->
                      `Snd (check_tab_info, success)
                  | None ->
                      `Trd check_tab_info)
          in
          match
            ( extract_pipeline_check base_checks
            , ((head_pipeline_summary, true), extract_pipeline_check head_checks)
            )
          with
          | ( ( [({summary= Some base_pipeline_summary}, base_pipeline_finished)]
              , base_checks
              , unfinished_base_checks )
            , ( ( (Some head_pipeline_summary, head_pipeline_finished)
                , (_, head_checks, unfinished_head_checks) )
              | ( (None, _)
                , ( [ ( {summary= Some head_pipeline_summary}
                      , head_pipeline_finished ) ]
                  , head_checks
                  , unfinished_head_checks ) ) ) ) ->
              Lwt_io.printf
                "Looking for failed tests to minimize among %d head checks (%d \
                 base checks) (head checks: %s) (unfinished head checks: %s) \
                 (base checks: %s) (unfinished base checks: %s).\n"
                (List.length head_checks) (List.length base_checks)
                ( head_checks
                |> List.map ~f:(fun ({name}, _) -> name)
                |> String.concat ~sep:", " )
                ( unfinished_head_checks
                |> List.map ~f:(fun {name} -> name)
                |> String.concat ~sep:", " )
                ( base_checks
                |> List.map ~f:(fun ({name}, _) -> name)
                |> String.concat ~sep:", " )
                ( unfinished_base_checks
                |> List.map ~f:(fun {name} -> name)
                |> String.concat ~sep:", " )
              >>= fun () ->
              let failed_test_suite_jobs =
                List.filter_map head_checks ~f:(fun ({name}, success) ->
                    if string_match ~regexp:"test-suite" name && not success
                    then Some name
                    else None)
                @ List.filter_map head_checks_errors ~f:(fun (name, _) ->
                      if string_match ~regexp:"test-suite" name then Some name
                      else None)
              in
              let possible_jobs_to_minimize, unminimizable_jobs =
                head_checks
                |> List.partition_map ~f:(fun (({name}, _) as head_check) ->
                       match
                         ci_minimization_extract_job_specific_info
                           ~head_pipeline_summary ~base_pipeline_summary
                           ~base_checks_errors ~base_checks head_check
                       with
                       | Error err ->
                           Either.Second (name, err)
                       | Ok result ->
                           Either.First result)
              in
              let unminimizable_jobs =
                unminimizable_jobs
                @ ( unfinished_head_checks
                  |> List.map ~f:(fun {name} ->
                         (name, f "Job %s is still in progress." name)) )
              in
              Lwt.return_ok
                ( { comment_thread_id= pr_id
                  ; base
                  ; head
                  ; pr_number
                  ; draft
                  ; labels
                  ; base_pipeline_finished
                  ; head_pipeline_finished
                  ; failed_test_suite_jobs }
                , possible_jobs_to_minimize
                , unminimizable_jobs )
          | (_, _, _), ((None, _), ([({summary= None}, _)], _, _)) ->
              Lwt.return_error
                ( Some pr_id
                , f
                    "Could not find pipeline check summary for head commit %s \
                     and no summary was passed."
                    head )
          | (_, _, _), ((None, _), ([], _, _)) ->
              Lwt.return_error
                ( Some pr_id
                , f
                    "Could not find pipeline check for head commit %s and no \
                     summary was passed.  (Found checks: %s)"
                    head
                    ( head_checks
                    |> List.map ~f:(fun ({name}, _) -> name)
                    |> String.concat ~sep:", " ) )
          | (_, _, _), ((None, _), ((_ :: _ :: _ as pipeline_head_checks), _, _))
            ->
              Lwt.return_error
                ( Some pr_id
                , f
                    "Found several pipeline checks instead of one for head \
                     commit %s and no summary was passed.  (Found checks: %s)"
                    head
                    ( pipeline_head_checks
                    |> List.map ~f:(fun ({name}, _) -> name)
                    |> String.concat ~sep:", " ) )
          | ([({summary= None}, _)], _, _), ((_, _), (_, _, _)) ->
              Lwt.return_error
                ( Some pr_id
                , f "Could not find pipeline check summary for base commit %s."
                    base )
          | ([], _, _), ((_, _), (_, _, _)) ->
              Lwt.return_error
                ( Some pr_id
                , f
                    "Could not find pipeline check for base commit %s.  (Found \
                     checks: %s)"
                    base
                    ( base_checks
                    |> List.map ~f:(fun ({name}, _) -> name)
                    |> String.concat ~sep:", " ) )
          | ((_ :: _ :: _ as pipeline_base_checks), _, _), ((_, _), (_, _, _))
            ->
              Lwt.return_error
                ( Some pr_id
                , f
                    "Found several pipeline checks instead of one for base \
                     commit %s.  (Found checks: %s)"
                    base
                    ( pipeline_base_checks
                    |> List.map ~f:(fun ({name}, _) -> name)
                    |> String.concat ~sep:", " ) ) ) )

type ci_minimization_request =
  | Auto
  | RequestSuggested
  | RequestAll
  | RequestExplicit of string list

type ci_minimization_suggestion_kind =
  (* this is a job that we suggest minimization for *)
  | Suggested
  (* we don't suggest running this job, but it's okay to; give a reason that we don't suggest it *)
  | Possible of string
  (* we expect that running this job won't work; give a reason *)
  | Bad of string

(* For grammatical correctness, all messages are expected to follow "because" *)
let ci_minimization_suggest ~base
    { base_job_failed
    ; base_job_errored
    ; head_job_succeeded
    ; missing_error
    ; non_v_file
    ; job_kind (*; overlayed*) } =
  if head_job_succeeded then Bad "job succeeded!"
  else if missing_error then Bad "no error message was found"
  else
    match (base_job_errored, non_v_file) with
    | _, Some filename ->
        Bad (f "error message did not occur in a .v file (%s)" filename)
    | Some err, _ ->
        Possible (f "base job at %s errored with message %s" base err)
    | None, None ->
        if base_job_failed then Possible (f "base job at %s failed" base)
        else if
          (*if overlayed then Possible (f "an overlay is present")
            else*)
          not (List.exists ~f:(String.equal job_kind) ["library"; "plugin"])
        then
          Possible
            (f "the job is a %s which is not a library nor a plugin" job_kind)
        else Suggested

type ci_pr_minimization_suggestion =
  | Suggest
  | RunAutomatically
  | Silent of string

let suggest_ci_minimization_for_pr = function
  (* don't suggest if there are failed test-suite jobs (TODO: decide about async?) *)
  | {failed_test_suite_jobs= _ :: _ as failed_test_suite_jobs} ->
      Silent
        (f "the following test-suite jobs failed: %s"
           (String.concat ~sep:", " failed_test_suite_jobs))
  (* This next case is a dummy case so OCaml doesn't complain about us
     never using RunAutomatically; we should probably remove it when
     we add a criterion for running minimization automatically *)
  | {labels}
    when List.exists ~f:(String.equal "coqbot request ci minimization") labels
    ->
      RunAutomatically
  | {labels} when List.exists ~f:(String.equal "kind: infrastructure") labels ->
      Silent "this PR is labeled with kind: infrastructure"
  | {draft= true} ->
      Suggest
  | _ ->
      Suggest

let minimize_failed_tests ~bot_info ~owner ~repo ~pr_number
    ~head_pipeline_summary ~request ~comment_on_error ~bug_file_contents =
  fetch_ci_minimization_info ~bot_info ~owner ~repo ~pr_number
    ~head_pipeline_summary
  >>= function
  | Ok
      ( ( { comment_thread_id
          ; base
          ; head
          ; base_pipeline_finished
          ; head_pipeline_finished } as ci_minimization_pr_info )
      , possible_jobs_to_minimize
      , unminimizable_jobs ) -> (
      let compare_minimization_info {target= target1} {target= target2} =
        String.compare target1 target2
      in
      let unminimizable_jobs =
        unminimizable_jobs
        |> List.sort ~compare:(fun (name1, _) (name2, _) ->
               String.compare name1 name2)
      in
      possible_jobs_to_minimize
      |> List.sort ~compare:(fun (_, info1) (_, info2) ->
             compare_minimization_info info1 info2)
      |> List.map ~f:(fun (suggestion_info, minimization_info) ->
             (ci_minimization_suggest ~base suggestion_info, minimization_info))
      |> List.partition3_map ~f:(function
           | Suggested, minimization_info ->
               `Fst minimization_info
           | Possible reason, minimization_info ->
               `Snd (reason, minimization_info)
           | Bad reason, minimization_info ->
               `Trd (reason, minimization_info))
      |> fun ( suggested_jobs_to_minimize
             , possible_jobs_to_minimize
             , bad_jobs_to_minimize ) ->
      let suggested_and_possible_jobs_to_minimize =
        suggested_jobs_to_minimize
        @ List.map
            ~f:(fun (_, minimization_info) -> minimization_info)
            possible_jobs_to_minimize
        |> List.sort ~compare:compare_minimization_info
      in
      let jobs_to_minimize, suggest_minimization =
        match
          (request, suggest_ci_minimization_for_pr ci_minimization_pr_info)
        with
        | Auto, RunAutomatically
          when base_pipeline_finished && head_pipeline_finished ->
            (suggested_jobs_to_minimize, Ok ())
        | Auto, RunAutomatically ->
            (* XXX TODO: What should we do in the "run automatically case" when base or head pipeline has not finished? *)
            (suggested_jobs_to_minimize, Ok ())
        | Auto, Suggest ->
            ([], Ok ())
        | Auto, Silent reason ->
            ([], Error reason)
        | RequestSuggested, (RunAutomatically | Suggest) ->
            (suggested_jobs_to_minimize, Ok ())
        | RequestSuggested, Silent reason ->
            (suggested_jobs_to_minimize, Error reason)
        | RequestAll, _ ->
            ( suggested_and_possible_jobs_to_minimize
            , Error "all minimizable jobs were already requested" )
        | RequestExplicit requests, _ ->
            ( suggested_and_possible_jobs_to_minimize
              |> List.filter ~f:(fun {target (*; full_target*)} ->
                     List.exists
                       ~f:(fun request ->
                         String.equal target request
                         (*|| String.equal full_target request*))
                       requests)
            , Error "the user requested an explicit list of jobs" )
      in
      ( match jobs_to_minimize with
      | [] ->
          Lwt_io.printlf
            "Found no jobs to initiate CI minimization on for PR #%d" pr_number
      | _ ->
          Lwt_io.printlf "Initiating CI minimization for PR #%d on jobs: %s"
            pr_number
            ( jobs_to_minimize
            |> List.map ~f:(fun {target} -> target)
            |> String.concat ~sep:", " ) )
      >>= fun () ->
      run_ci_minimization ~bot_info ~comment_thread_id ~owner ~repo
        ~pr_number:(Int.to_string pr_number) ~base ~head
        ~ci_minimization_infos:jobs_to_minimize ~bug_file_contents
      >>= fun (jobs_minimized, jobs_that_could_not_be_minimized) ->
      let pluralize word ?plural ls =
        match (ls, plural) with
        | [_], _ ->
            word
        | _, Some plural ->
            plural
        | _, _ ->
            word ^ "s"
      in
      (* Construct a comment body *)
      let unminimizable_jobs_description ~f =
        match
          unminimizable_jobs |> List.filter ~f:(fun (name, _) -> f name)
        with
        | [] ->
            None
        | [(name, err)] ->
            Some
              (Printf.sprintf "The job %s could not be minimized because %s.\n"
                 name err)
        | unminimizable_jobs ->
            Some
              ( "The following jobs could not be minimized:\n"
              ^ ( unminimizable_jobs
                |> List.map ~f:(fun (name, err) ->
                       Printf.sprintf "- %s (%s)" name err)
                |> String.concat ~sep:"\n" )
              ^ "\n\n" )
      in
      let bad_jobs_description ~f =
        match
          bad_jobs_to_minimize
          |> List.filter ~f:(fun (_, {target (*; full_target*)}) ->
                 f target (*|| f full_target*))
        with
        | [] ->
            None
        | [(reason, {target})] ->
            Some
              (Printf.sprintf "The job %s was not minimized because %s.\n"
                 target reason)
        | bad_jobs ->
            Some
              ( "The following jobs were not minimized:\n"
              ^ ( bad_jobs
                |> List.map ~f:(fun (reason, {target}) ->
                       Printf.sprintf "- %s because %s" target reason)
                |> String.concat ~sep:"\n" )
              ^ "\n\n" )
      in
      let bad_and_unminimizable_jobs_description ~f =
        match (bad_jobs_description ~f, unminimizable_jobs_description ~f) with
        | None, None ->
            None
        | Some msg, None | None, Some msg ->
            Some msg
        | Some msg1, Some msg2 ->
            Some (msg1 ^ msg2)
      in
      let failed_minimization_description =
        match jobs_that_could_not_be_minimized with
        | [] ->
            None
        | _ :: _ ->
            Some
              ( "I failed to trigger minimization on the following jobs:\n"
              ^ ( jobs_that_could_not_be_minimized
                |> List.map ~f:(fun (name, err) ->
                       Printf.sprintf "- %s (%s)" name err)
                |> String.concat ~sep:"\n" )
              ^ "\n\n" )
      in
      let unfinished_pipelines_description =
        (if base_pipeline_finished then [] else [f "base commit (%s)" base])
        @ if head_pipeline_finished then [] else [f "head commit (%s)" head]
      in
      let try_again_msg =
        match unfinished_pipelines_description with
        | [] ->
            ""
        | ls ->
            f "\nHowever, you may want to try again once the %s for the %s %s."
              (pluralize "pipeline" ls)
              (ls |> String.concat ~sep:" and ")
              (pluralize "finishes" ~plural:"finish" ls)
      in
      let may_wish_to_wait_msg =
        match unfinished_pipelines_description with
        | [] ->
            ""
        | ls ->
            f "\nHowever, you may want to wait until the %s for the %s %s."
              (pluralize "pipeline" ls)
              (ls |> String.concat ~sep:" and ")
              (pluralize "finishes" ~plural:"finish" ls)
      in
      let note_some_head_unfinished_msg =
        if head_pipeline_finished then ""
        else
          f
            "\n\
             Some jobs may have been missed because the pipeline for the head \
             commit (%s) has not yet finished."
            head
      in
      let note_some_base_unfinished_msg =
        if base_pipeline_finished then ""
        else
          f
            "\n\
             However, minimization may fail because the pipeline for the base \
             commit (%s) has not yet finished."
            base
      in
      ( match (request, jobs_minimized, failed_minimization_description) with
      | RequestAll, [], None ->
          Lwt.return_some
            ( match
                bad_and_unminimizable_jobs_description ~f:(fun _ -> true)
              with
            | None ->
                f "No valid CI jobs detected for %s.%s" head try_again_msg
            | Some msg ->
                f
                  "I attempted to run all CI jobs at commit %s for \
                   minimization, but was unable to find any jobs to \
                   minimize.%s\n\n\
                   %s"
                  head try_again_msg msg )
      | RequestAll, _, _ ->
          ( match bad_and_unminimizable_jobs_description ~f:(fun _ -> true) with
          | Some msg ->
              Lwt_io.printlf
                "When attempting to run CI Minimization by request all on \
                 %s/%s@%s for PR #%d:\n\
                 %s"
                owner repo head pr_number msg
          | None ->
              Lwt.return_unit )
          >>= fun () ->
          ( match jobs_minimized with
          | [] ->
              f
                "I did not succeed at triggering minimization on any jobs at \
                 commit %s.%s"
                head try_again_msg
          | _ :: _ ->
              f
                "I am now %s minimization at commit %s on %s. I'll come back \
                 to you with the results once it's done.%s"
                ( if Option.is_none bug_file_contents then "running"
                else "resuming" )
                head
                (jobs_minimized |> String.concat ~sep:", ")
                note_some_head_unfinished_msg )
          ^ "\n\n"
          ^ Option.value ~default:"" failed_minimization_description
          |> Lwt.return_some
      | RequestExplicit requests, _, _ ->
          (* N.B. requests may be things like library:ci-cross_crypto,
             while the job targets are things like GitLab CI job
             library:ci-cross_crypto (pull request) *)
          requests
          |> List.partition3_map ~f:(fun request ->
                 match
                   ( List.exists
                       ~f:(string_match ~regexp:(Str.quote request))
                       jobs_minimized
                   , List.find
                       ~f:(fun (target, _) ->
                         string_match ~regexp:(Str.quote request) target)
                       jobs_that_could_not_be_minimized
                   , List.find
                       ~f:(fun (target, _) ->
                         string_match ~regexp:(Str.quote request) target)
                       unminimizable_jobs
                   , List.find
                       ~f:(fun (_, {target}) ->
                         string_match ~regexp:(Str.quote request) target)
                       bad_jobs_to_minimize )
                 with
                 | true, _, _, _ ->
                     `Fst request
                 | false, Some (target, err), _, _ ->
                     `Snd
                       (f "%s: failed to trigger minimization (%s)" target err)
                 | false, None, Some (target, err), _ ->
                     `Snd (f "%s could not be minimized (%s)" target err)
                 | false, None, None, Some (reason, {target}) ->
                     `Snd (f "%s was not minimized because %s" target reason)
                 | false, None, None, None ->
                     `Trd request)
          |> fun (successful_requests, unsuccessful_requests, unfound_requests) ->
          let unsuccessful_requests_report =
            match unsuccessful_requests with
            | [] ->
                None
            | [msg] ->
                Some msg
            | _ ->
                Some
                  ( "The following requests were not fulfilled:\n"
                  ^ ( unsuccessful_requests
                    |> List.map ~f:(fun msg -> "- " ^ msg)
                    |> String.concat ~sep:"\n" )
                  ^ "\n\n" )
          in
          let unfound_requests_report =
            let all_jobs =
              List.map
                ~f:(fun (target, _) -> target)
                jobs_that_could_not_be_minimized
              @ List.map ~f:(fun (target, _) -> target) unminimizable_jobs
              @ List.map ~f:(fun (_, {target}) -> target) bad_jobs_to_minimize
              |> List.sort ~compare:String.compare
            in
            match unfound_requests with
            | [] ->
                None
            | [request] ->
                Some
                  (f
                     "requested target '%s' could not be found among the jobs \
                      %s.%s"
                     request
                     (all_jobs |> String.concat ~sep:", ")
                     note_some_head_unfinished_msg)
            | _ :: _ :: _ ->
                Some
                  (f
                     "requested targets %s could not be found among the jobs \
                      %s.%s"
                     (unfound_requests |> String.concat ~sep:", ")
                     (all_jobs |> String.concat ~sep:", ")
                     note_some_head_unfinished_msg)
          in
          let unsuccessful_requests_report =
            match (unsuccessful_requests_report, unfound_requests_report) with
            | None, None ->
                None
            | Some msg, None ->
                Some msg
            | None, Some msg ->
                Some ("The " ^ msg)
            | Some msg1, Some msg2 ->
                Some (msg1 ^ "\nAdditionally, the " ^ msg2)
          in
          ( match (successful_requests, unsuccessful_requests_report) with
          | [], None ->
              "No CI minimization requests made?"
          | [], Some msg ->
              "I was unable to minimize any of the CI targets that you \
               requested." ^ try_again_msg ^ "\n" ^ msg
          | _ :: _, _ ->
              f
                "I am now %s minimization at commit %s on requested %s %s. \
                 I'll come back to you with the results once it's done.%s\n\n\
                 %s"
                ( if Option.is_none bug_file_contents then "running"
                else "resuming" )
                head
                (pluralize "target" successful_requests)
                (successful_requests |> String.concat ~sep:", ")
                note_some_base_unfinished_msg
                (Option.value ~default:"" unsuccessful_requests_report) )
          |> Lwt.return_some
      | RequestSuggested, [], None ->
          ( match possible_jobs_to_minimize with
          | [] ->
              f "No CI jobs are available to be minimized for commit %s.%s" head
                try_again_msg
          | _ :: _ ->
              f
                "You requested minimization of suggested failing CI jobs, but \
                 no jobs were suggested at commit %s. You can trigger \
                 minimization of %s with `ci minimize all` or by requesting \
                 some targets by name.%s"
                head
                ( possible_jobs_to_minimize
                |> List.map ~f:(fun (_, {target}) -> target)
                |> String.concat ~sep:", " )
                may_wish_to_wait_msg )
          |> Lwt.return_some
      | RequestSuggested, [], Some failed_minimization_description ->
          f
            "I attempted to minimize suggested failing CI jobs at commit %s, \
             but was unable to succeed on any jobs.%s\n\
             %s"
            head try_again_msg failed_minimization_description
          |> Lwt.return_some
      | RequestSuggested, _ :: _, _ ->
          f
            "I have initiated minimization at commit %s for the suggested %s \
             %s as requested.%s\n\n\
             %s"
            head
            (pluralize "target" jobs_minimized)
            (jobs_minimized |> String.concat ~sep:", ")
            try_again_msg
            (Option.value ~default:"" failed_minimization_description)
          |> Lwt.return_some
      | Auto, jobs_minimized, failed_minimization_description -> (
          ( match bad_and_unminimizable_jobs_description ~f:(fun _ -> true) with
          | Some msg ->
              Lwt_io.printlf
                "When attempting to run CI Minimization by auto on %s/%s@%s \
                 for PR #%d:\n\
                 %s"
                owner repo head pr_number msg
          | None ->
              Lwt.return_unit )
          >>= fun () ->
          let suggest_jobs =
            match suggested_jobs_to_minimize with
            | [] ->
                None
            | _ ->
                Some
                  (f
                     "If you tag me saying `@%s ci minimize`, I will minimize \
                      the following %s: %s.\n"
                     bot_info.name
                     (pluralize "target" suggested_jobs_to_minimize)
                     ( suggested_jobs_to_minimize
                     |> List.map ~f:(fun {target} -> target)
                     |> String.concat ~sep:", " ))
          in
          let suggest_only_all_jobs =
            let pre_message =
              f
                "If you tag me saying `@%s ci minimize all`, I will \
                 additionally minimize the following %s (which I do not \
                 suggest minimizing):"
                bot_info.name
                (pluralize "target" possible_jobs_to_minimize)
            in
            match possible_jobs_to_minimize with
            | [] ->
                None
            | [(reason, {target})] ->
                Some (f "%s %s (because %s)\n\n\n" pre_message target reason)
            | _ ->
                Some
                  (f "%s\n%s\n\n\n" pre_message
                     ( possible_jobs_to_minimize
                     |> List.map ~f:(fun (reason, {target}) ->
                            f "- %s (because %s)" target reason)
                     |> String.concat ~sep:"\n" ))
          in
          let suggest_all_jobs =
            match (suggest_jobs, suggest_only_all_jobs) with
            | None, None ->
                None
            | Some msg, None | None, Some msg ->
                Some (msg ^ may_wish_to_wait_msg)
            | Some msg1, Some msg2 ->
                Some (msg1 ^ msg2 ^ may_wish_to_wait_msg)
          in
          match
            ( jobs_minimized
            , failed_minimization_description
            , suggest_all_jobs
            , suggest_minimization )
          with
          | [], None, None, _ ->
              Lwt_io.printlf
                "No candidates found for minimization on %s/%s@%s for PR #%d."
                owner repo head pr_number
              >>= fun () -> Lwt.return_none
          | [], None, Some suggestion_msg, Ok () ->
              f
                "Hey, I have detected that there were CI failures at commit %s \
                 without any failure in the test-suite.\n\
                 I checked that the corresponding jobs for the base commit %s \
                 succeeded. You can ask me to try to extract a minimal test \
                 case from this so that it can be added to the test-suite.\n\
                 %s"
                head base suggestion_msg
              |> Lwt.return_some
          | [], None, Some suggestion_msg, Error reason ->
              Lwt_io.printlf
                "Candidates found for minimization on %s/%s@%s for PR #%d, but \
                 I am not commenting because minimization is not suggested \
                 because %s:\n\
                 %s"
                owner repo head pr_number reason suggestion_msg
              >>= fun () -> Lwt.return_none
          | [], Some failed_minimization_description, _, _ ->
              Lwt_io.printlf
                "Candidates found for auto minimization on %s/%s@%s for PR \
                 #%d, but all attempts to trigger minimization failed:\n\
                 %s"
                owner repo head pr_number failed_minimization_description
              >>= fun () -> Lwt.return_none
          | _ :: _, _, _, _ ->
              f
                "Hey, I have detected that the %s %s failed on the CI at \
                 commit %s without any failure in the test-suite.\n\
                 I checked that the corresponding %s for the base commit %s \
                 succeeded. Now, I'm trying to extract a minimal test case \
                 from this so that it can be added to the test-suite. I'll \
                 come back to you with the results once it's done.\n\
                 %s"
                (pluralize "job" jobs_minimized)
                (jobs_minimized |> String.concat ~sep:", ")
                head
                (pluralize "job" jobs_minimized)
                base
                (Option.value ~default:"" suggest_only_all_jobs)
              |> Lwt.return_some ) )
      >>= function
      | Some message ->
          GitHub_mutations.post_comment ~id:comment_thread_id ~message ~bot_info
          >>= GitHub_mutations.report_on_posting_comment
      | None ->
          Lwt_io.printlf
            "NOT commenting with CI minimization information at %s/%s@%s (PR \
             #%d)."
            owner repo head pr_number )
  | Error (Some comment_thread_id, err) when comment_on_error ->
      GitHub_mutations.post_comment ~id:comment_thread_id
        ~message:
          (f "Error while attempting to find job minimization information:\n%s"
             err)
        ~bot_info
      >>= GitHub_mutations.report_on_posting_comment
  | Error (_, err) ->
      Lwt_io.printlf
        "Error while attempting to find jobs to minimize from PR #%d:\n%s"
        pr_number err

let ci_minimize ~bot_info ~comment_info ~requests ~comment_on_error
    ~bug_file_contents =
  minimize_failed_tests ~bot_info ~owner:comment_info.issue.issue.owner
    ~repo:comment_info.issue.issue.repo ~pr_number:comment_info.issue.number
    ~head_pipeline_summary:None
    ~request:
      ( match requests with
      | [] ->
          RequestSuggested
      | ["all"] ->
          RequestAll
      | requests ->
          RequestExplicit requests )
    ~comment_on_error ~bug_file_contents

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
  let pr_number, _ = pr_from_branch pipeline_info.common_info.branch in
  match pipeline_info.state with
  | "skipped" ->
      Lwt.return_unit
  | _ -> (
      let pipeline_url =
        f "%s/pipelines/%d" pipeline_info.common_info.repo_url
          pipeline_info.pipeline_id
      in
      let external_id =
        f "projects/%d/pipelines/%d" pipeline_info.common_info.project_id
          pipeline_info.pipeline_id
      in
      let state, status, conclusion, title, summary_top =
        match pipeline_info.state with
        | "pending" ->
            ("pending", QUEUED, None, "Pipeline is pending on GitLab CI", None)
        | "running" ->
            ( "pending"
            , IN_PROGRESS
            , None
            , "Pipeline is running on GitLab CI"
            , None )
        | "success" ->
            ( "success"
            , COMPLETED
            , Some SUCCESS
            , "Pipeline completed on GitLab CI"
            , None )
        | "failed" ->
            ( "failure"
            , COMPLETED
            , Some FAILURE
            , "Pipeline completed with errors on GitLab CI"
            , Some
                "*If you need to restart the entire pipeline, you may do so \
                 directly in the GitHub interface using the \"Re-run\" \
                 button.*" )
        | "cancelled" | "canceled" ->
            ( "error"
            , COMPLETED
            , Some CANCELLED
            , "Pipeline was cancelled on GitLab CI"
            , None )
        | s ->
            ( "error"
            , COMPLETED
            , Some FAILURE
            , "Unknown pipeline status: " ^ s
            , None )
      in
      match bot_info.github_install_token with
      | None ->
          GitHub_mutations.send_status_check ~repo_full_name
            ~commit:pipeline_info.common_info.head_commit ~state
            ~url:pipeline_url
            ~context:
              (f "GitLab CI pipeline (%s)"
                 (pr_from_branch pipeline_info.common_info.branch |> snd))
            ~description:title ~bot_info
      | Some _ -> (
          let owner, repo =
            github_repo_of_gitlab_project_path ~gitlab_mapping repo_full_name
          in
          GitHub_queries.get_repository_id ~bot_info ~owner ~repo
          >>= function
          | Error e ->
              Lwt_io.printf "No repo id: %s\n" e
          | Ok repo_id -> (
              let summary =
                create_pipeline_summary ?summary_top pipeline_info pipeline_url
              in
              GitHub_mutations.create_check_run ~bot_info
                ~name:
                  (f "GitLab CI pipeline (%s)"
                     (pr_from_branch pipeline_info.common_info.branch |> snd))
                ~repo_id ~head_sha:pipeline_info.common_info.head_commit ~status
                ?conclusion ~title ~details_url:pipeline_url ~summary
                ~external_id ()
              >>= fun () ->
              Lwt_unix.sleep 5.
              >>= fun () ->
              match (owner, repo, pipeline_info.state, pr_number) with
              | "coq", "coq", "failed", Some pr_number ->
                  minimize_failed_tests ~bot_info ~owner ~repo ~pr_number
                    ~head_pipeline_summary:(Some summary) ~request:Auto
                    ~comment_on_error:false ~bug_file_contents:None
              | _ ->
                  Lwt.return_unit ) ) )

let run_coq_minimizer ~bot_info ~script ~comment_thread_id ~comment_author
    ~owner ~repo =
  git_coq_bug_minimizer ~bot_info ~script ~comment_thread_id ~comment_author
    ~owner ~repo
  >>= function
  | Ok () ->
      GitHub_mutations.post_comment ~id:comment_thread_id
        ~message:
          (f
             "Hey @%s, the coq bug minimizer is running your script, I'll come \
              back to you with the results once it's done."
             comment_author)
        ~bot_info
      >>= GitHub_mutations.report_on_posting_comment
  | Error f ->
      Lwt_io.printf "Error: %s\n" f

let coq_bug_minimizer_results_action ~bot_info ~ci ~key ~app_id body =
  if string_match ~regexp:"\\([^\n]+\\)\n\\([^\r]*\\)" body then
    let stamp = Str.matched_group 1 body in
    let message = Str.matched_group 2 body in
    match Str.split (Str.regexp " ") stamp with
    | [id; author; repo_name; branch_name; owner; repo] ->
        (fun () ->
          Github_installations.action_as_github_app ~bot_info ~key ~app_id
            ~owner ~repo
            (GitHub_mutations.post_comment ~id
               ~message:(if ci then message else f "@%s, %s" author message))
          >>= GitHub_mutations.report_on_posting_comment
          <&> ( execute_cmd
                  (* To delete the branch we need to identify as
                     coqbot the GitHub user, who is a collaborator on
                     the run-coq-bug-minimizer repo, not coqbot the
                     GitHub App *)
                  (f "git push https://%s:%s@github.com/%s.git --delete '%s'"
                     bot_info.name bot_info.github_pat repo_name branch_name)
              >>= function
              | Ok () ->
                  Lwt.return_unit
              | Error f ->
                  Lwt_io.printf "Error: %s\n" f ))
        |> Lwt.async ;
        Server.respond_string ~status:`OK ~body:"" ()
    | _ ->
        Server.respond_string ~status:(`Code 400) ~body:"Bad request" ()
  else Server.respond_string ~status:(`Code 400) ~body:"Bad request" ()

let coq_bug_minimizer_resume_ci_minimization_action ~bot_info ~key ~app_id body
    =
  if string_match ~regexp:"\\([^\n]+\\)\n\\([^\r]*\\)" body then
    let stamp = Str.matched_group 1 body in
    let message = Str.matched_group 2 body in
    match Str.split (Str.regexp " ") stamp with
    | [ comment_thread_id
      ; _author
      ; _repo_name
      ; _branch_name
      ; owner
      ; repo
      ; pr_number ] -> (
        message |> String.split ~on:'\n'
        |> function
        | docker_image
          :: target
             :: opam_switch
                :: failing_urls
                   :: passing_urls :: base :: head :: bug_file_lines ->
            (let bug_file_contents = String.concat ~sep:"\n" bug_file_lines in
             fun () ->
               init_git_bare_repository ~bot_info
               >>= fun () ->
               Github_installations.action_as_github_app ~bot_info ~key ~app_id
                 ~owner ~repo
                 (run_ci_minimization ~comment_thread_id ~owner ~repo ~base
                    ~pr_number ~head
                    ~ci_minimization_infos:
                      [ { target
                        ; opam_switch
                        ; failing_urls
                        ; passing_urls
                        ; docker_image
                        ; full_target= target (* dummy value *) } ]
                    ~bug_file_contents:(Some bug_file_contents))
               >>= function
               | [], [] ->
                   Lwt_io.printlf
                     "Somehow no jobs were returned from minimization \
                      resumption?\n\
                      %s"
                     message
               | jobs_minimized, jobs_that_could_not_be_minimized -> (
                   ( match jobs_minimized with
                   | [] ->
                       Lwt.return_unit
                   | _ ->
                       Lwt_io.printlf "Resuming minimization of %s"
                         (jobs_minimized |> String.concat ~sep:", ") )
                   >>= fun () ->
                   match
                     jobs_that_could_not_be_minimized
                     |> List.map ~f:(fun (job, reason) ->
                            f "%s because %s" job reason)
                   with
                   | [] ->
                       Lwt.return_unit
                   | msgs ->
                       Lwt_io.printlf "Could not resume minimization of %s"
                         (msgs |> String.concat ~sep:", ") ))
            |> Lwt.async ;
            Server.respond_string ~status:`OK
              ~body:"Handling CI minimization resumption." ()
        | _ ->
            Server.respond_string ~status:(Code.status_of_code 400)
              ~body:
                (f
                   "Error: resume-ci-minimization called without enough \
                    arguments:\n\
                    %s"
                   message)
              () )
    | _ ->
        Server.respond_string ~status:(`Code 400) ~body:"Bad request" ()
  else Server.respond_string ~status:(`Code 400) ~body:"Bad request" ()

let rec merge_pull_request_action ~bot_info ?(t = 1.) comment_info =
  let pr = comment_info.issue in
  let reasons_for_not_merging =
    List.filter_opt
      [ ( if String.equal comment_info.author pr.user then
          Some "you are the author"
        else if
        List.exists
          ~f:(String.equal comment_info.author)
          comment_info.issue.assignees
      then None
        else Some "you're not among the assignees" )
      ; comment_info.issue.labels
        |> List.find ~f:(fun label -> string_match ~regexp:"needs:.*" label)
        |> Option.map ~f:(fun l -> f "there is still a `%s` label" l)
      ; ( if
          comment_info.issue.labels
          |> List.exists ~f:(fun label -> string_match ~regexp:"kind:.*" label)
        then None
        else Some "there is no `kind:` label" )
      ; ( if comment_info.issue.milestoned then None
        else Some "no milestone is set" ) ]
  in
  ( match reasons_for_not_merging with
  | _ :: _ ->
      let reasons = reasons_for_not_merging |> String.concat ~sep:" and " in
      Lwt.return_error
        (f "@%s: You can't merge the PR because %s." comment_info.author
           reasons)
  | [] -> (
      GitHub_queries.get_pull_request_reviews_refs ~bot_info
        ~owner:pr.issue.owner ~repo:pr.issue.repo ~number:pr.issue.number
      >>= function
      | Ok reviews_info -> (
          let comment =
            List.find reviews_info.last_comments ~f:(fun c ->
                String.equal comment_info.id c.id)
          in
          if (not comment_info.review_comment) && Option.is_none comment then
            if Float.(t > 5.) then
              Lwt.return_error
                "Something unexpected happened: did not find merge comment \
                 after retrying three times.\n\
                 cc @coq/coqbot-maintainers"
            else
              Lwt_unix.sleep t
              >>= fun () ->
              merge_pull_request_action ~t:(t *. 2.) ~bot_info comment_info
              >>= fun () -> Lwt.return_ok ()
          else if
            (not comment_info.review_comment)
            && (Option.value_exn comment).created_by_email
            (* Option.value_exn doesn't raise an exception because comment isn't None at this point*)
          then
            Lwt.return_error
              (f
                 "@%s: Merge requests sent over e-mail are not accepted \
                  because this puts less guarantee on the authenticity of the \
                  author of the request."
                 comment_info.author)
          else if not (String.equal reviews_info.baseRef "master") then
            Lwt.return_error
              (f
                 "@%s: This PR targets branch `%s` instead of `master`. Only \
                  release managers can merge in release branches. Merging with \
                  the bot is not supported."
                 comment_info.author reviews_info.baseRef)
          else
            match reviews_info.review_decision with
            | NONE | REVIEW_REQUIRED ->
                Lwt.return_error
                  (f
                     "@%s: You can't merge the PR because it hasn't been \
                      approved yet."
                     comment_info.author)
            | CHANGES_REQUESTED ->
                Lwt.return_error
                  (f
                     "@%s: You can't merge the PR because some changes are \
                      requested."
                     comment_info.author)
            | APPROVED -> (
                GitHub_queries.get_team_membership ~bot_info ~org:"coq"
                  ~team:"pushers" ~user:comment_info.author
                >>= function
                | Ok false ->
                    (* User not found in the team *)
                    Lwt.return_error
                      (f
                         "@%s: You can't merge this PR because you're not a \
                          member of the `@coq/pushers` team."
                         comment_info.author)
                | Ok true -> (
                    GitHub_mutations.merge_pull_request ~bot_info ~pr_id:pr.id
                      ~commit_headline:
                        (f "Merge PR #%d: %s" pr.issue.number
                           comment_info.issue.title)
                      ~commit_body:
                        ( List.fold_left reviews_info.approved_reviews ~init:""
                            ~f:(fun s r -> s ^ f "Reviewed-by: %s\n" r)
                        ^ List.fold_left reviews_info.comment_reviews ~init:""
                            ~f:(fun s r -> s ^ f "Ack-by: %s\n" r)
                        ^ f "Co-authored-by: %s <%s@users.noreply.github.com>\n"
                            comment_info.author comment_info.author )
                      ~merge_method:MERGE
                    >>= fun () ->
                    match
                      List.fold_left ~init:[] reviews_info.files
                        ~f:(fun acc f ->
                          if
                            string_match ~regexp:"dev/ci/user-overlays/\\(.*\\)"
                              f
                          then Str.matched_group 1 f :: acc
                          else acc)
                    with
                    | [] ->
                        Lwt.return_ok ()
                    | overlays ->
                        GitHub_mutations.post_comment ~bot_info ~id:pr.id
                          ~message:
                            (f
                               "@%s: Please take care of the following overlays:\n\
                                %s"
                               comment_info.author
                               (List.fold_left overlays ~init:"" ~f:(fun s o ->
                                    s ^ f "- %s\n" o)))
                        >>= GitHub_mutations.report_on_posting_comment
                        >>= fun () -> Lwt.return_ok () )
                | Error e ->
                    Lwt.return_error
                      (f
                         "Something unexpected happened: %s\n\
                          cc @coq/coqbot-maintainers" e) ) )
      | Error e ->
          Lwt.return_error
            (f "Something unexpected happened: %s\ncc @coq/coqbot-maintainers"
               e) ) )
  >>= function
  | Ok () ->
      Lwt.return_unit
  | Error err ->
      GitHub_mutations.post_comment ~bot_info ~message:err ~id:pr.id
      >>= GitHub_mutations.report_on_posting_comment

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
  let rebase_label = "needs: rebase" in
  let stale_label = "stale" in
  let issue = pr_info.issue.issue in
  GitHub_queries.get_label ~bot_info ~owner:issue.owner ~repo:issue.repo
    ~label:rebase_label
  >>= fun rebase_label_id ->
  if ok then (
    (* Remove rebase / stale label *)
    GitHub_queries.get_label ~bot_info ~owner:issue.owner ~repo:issue.repo
      ~label:stale_label
    >>= fun stale_label_id ->
    let map (label, id) =
      if pr_info.issue.labels |> List.exists ~f:(String.equal label) then id
      else None
    in
    let labels =
      List.filter_map ~f:map
        [(stale_label, stale_label_id); (rebase_label, rebase_label_id)]
    in
    if not (List.is_empty labels) then
      (fun () ->
        GitHub_mutations.remove_labels ~pr_id:pr_info.issue.id ~labels ~bot_info)
      |> Lwt.async ;
    let open Lwt.Infix in
    (* In Coq repo, if something has changed in
       dev/ci/docker/, we rebuild the Docker image*)
    let get_options =
      if
        String.equal pr_info.issue.issue.owner "coq"
        && String.equal pr_info.issue.issue.repo "coq"
      then
        git_test_modified ~base:pr_info.base.sha ~head:pr_info.head.sha
          "dev/ci/docker/.*Dockerfile.*"
        >>= function
        | Ok true ->
            Lwt.return "-o ci.variable=\"SKIP_DOCKER=false\""
        | Ok false ->
            Lwt.return ""
        | Error e ->
            Lwt_io.printf
              "Error while checking if something has changed in dev/ci/docker:\n\
               %s\n"
              e
            >>= fun () -> Lwt.return ""
      else Lwt.return ""
    in
    (* Force push *)
    get_options
    >>= fun options ->
    gitlab_ref ~issue:pr_info.issue.issue ~gitlab_mapping ~github_mapping
      ~bot_info
    >|= (fun remote_ref ->
          git_push ~force:true ~options ~remote_ref ~local_ref:local_head_branch
            () )
    >>= execute_cmd )
  else (
    (* Add rebase label if it exists *)
    ( match rebase_label_id with
    | None ->
        ()
    | Some label_id ->
        (fun () ->
          GitHub_mutations.add_labels ~pr_id:pr_info.issue.id ~labels:[label_id]
            ~bot_info)
        |> Lwt.async ) ;
    (* Add fail status check *)
    match bot_info.github_install_token with
    | None ->
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
    | Some _ -> (
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
              ~details_url:"" ~summary:"" ()
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
        >>= fun _ -> Lwt.return_unit)
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
    Lwt.return_unit

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
               pr_info.base.branch.name)
        >>= GitHub_mutations.report_on_posting_comment)
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
        >>= fun _ -> Lwt.return_unit)
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
        >>= fun _ -> Lwt.return_unit)
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
  (* We implement an exponential backoff strategy to try again after
     5, 25, and 125 seconds, if the issue was closed by a commit not
     yet associated to a pull request or if we couldn't find the close
     event. *)
  GitHub_queries.get_issue_closer_info ~bot_info issue
  >>= function
  | Ok (ClosedByPullRequest result) ->
      GitHub_mutations.reflect_pull_request_milestone ~bot_info result
  | Ok ClosedByCommit when Float.(sleep_time > 200.) ->
      Lwt_io.print "Closed by commit not associated to any pull request.\n"
  | Ok NoCloseEvent when Float.(sleep_time > 200.) ->
      Lwt_io.printf "Error: no close event after 200 seconds.\n"
  | Ok (ClosedByCommit | NoCloseEvent) ->
      (* May be worth trying again later. *)
      Lwt_io.printf
        "Closed by commit not yet associated to any pull request or no close \
         event yet...\n\
        \ Trying again in %f seconds.\n"
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
      <&> ( GitHub_mutations.post_comment ~bot_info ~id
              ~message:
                "This PR was postponed. Please update accordingly the \
                 milestone of any issue that this fixes as this cannot be done \
                 automatically."
          >>= GitHub_mutations.report_on_posting_comment )
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
    else Lwt.return_unit
  in
  Lwt_list.iter_s commit_action commits_msg

let days_elapsed ts =
  (* Yes, I know this is wrong because of DST and black holes but it should
     still be correct enough *)
  Float.to_int ((Unix.time () -. ts) /. (3600. *. 24.))

let rec apply_throttle len action args =
  if List.is_empty args || len <= 0 then Lwt.return_unit
  else
    let args, rem = List.split_n args len in
    Lwt_list.map_p action args
    >>= fun ans ->
    let n = List.count ~f:(fun b -> b) ans in
    apply_throttle (len - n) action rem

let apply_after_label ~bot_info ~owner ~repo ~after ~label ~action ~throttle ()
    =
  GitHub_queries.get_open_pull_requests_with_label ~bot_info ~owner ~repo ~label
  >>= function
  | Ok prs ->
      let iter (pr_id, pr_number) =
        GitHub_queries.get_pull_request_label_timeline ~bot_info ~owner ~repo
          ~pr_number
        >>= function
        | Ok timeline ->
            let find (set, name, ts) =
              if set && String.equal name label then Some ts else None
            in
            (* Look for most recent label setting *)
            let timeline = List.rev timeline in
            let days =
              match List.find_map ~f:find timeline with
              | None ->
                  (* even with a race condition it cannot happen *)
                  failwith
                    (f "Anomaly: Label \"%s\" absent from timeline of PR #%i"
                       label pr_number)
              | Some ts ->
                  days_elapsed ts
            in
            if days >= after then action pr_id pr_number else Lwt.return false
        | Error e ->
            Lwt_io.print (f "Error: %s\n" e) >>= fun () -> Lwt.return false
      in
      apply_throttle throttle iter prs
  | Error err ->
      Lwt_io.print (f "Error: %s\n" err)

let coq_check_needs_rebase_pr ~bot_info ~owner ~repo ~warn_after ~close_after
    ~throttle =
  let rebase_label = "needs: rebase" in
  let stale_label = "stale" in
  GitHub_queries.get_label ~bot_info ~owner ~repo ~label:stale_label
  >>= function
  | Ok None ->
      Lwt.return_unit
  | Ok (Some stale_id) ->
      let action pr_id pr_number =
        GitHub_queries.get_pull_request_labels ~bot_info ~owner ~repo ~pr_number
        >>= function
        | Ok labels ->
            let has_label l = List.mem labels ~equal:String.equal l in
            if not (has_label stale_label || has_label "needs: independent fix")
            then
              GitHub_mutations.post_comment ~id:pr_id
                ~message:
                  (f
                     "The \"%s\" label was set more than %i days ago. If the \
                      PR is not rebased in %i days, it will be automatically \
                      closed."
                     rebase_label warn_after close_after)
                ~bot_info
              >>= GitHub_mutations.report_on_posting_comment
              >>= fun () ->
              GitHub_mutations.add_labels ~bot_info ~labels:[stale_id] ~pr_id
              >>= fun () -> Lwt.return true
            else Lwt.return false
        | Error err ->
            Lwt_io.print (f "Error: %s\n" err) >>= fun () -> Lwt.return false
      in
      apply_after_label ~bot_info ~owner ~repo ~after:warn_after
        ~label:rebase_label ~action ~throttle ()
  | Error err ->
      Lwt_io.print (f "Error: %s\n" err)

let coq_check_stale_pr ~bot_info ~owner ~repo ~after ~throttle =
  let label = "stale" in
  let action pr_id _pr_number =
    GitHub_mutations.post_comment ~id:pr_id
      ~message:
        (f
           "This PR was not rebased after %i days despite the warning, it is \
            now closed."
           after)
      ~bot_info
    >>= GitHub_mutations.report_on_posting_comment
    >>= fun () ->
    GitHub_mutations.close_pull_request ~bot_info ~pr_id
    >>= fun () -> Lwt.return true
  in
  apply_after_label ~bot_info ~owner ~repo ~after ~label ~action ~throttle ()
