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
open Lwt.Syntax

type coq_job_info =
  { docker_image: string
  ; dependencies: string list
  ; targets : string list
  ; compiler: string
  ; opam_variant: string }

let send_status_check ~bot_info job_info ~pr_num (gh_owner, gh_repo)
    ~github_repo_full_name ~gitlab_domain ~gitlab_repo_full_name ~context
    ~failure_reason ~external_id ~trace =
  let job_url =
    f "https://%s/%s/-/jobs/%d" gitlab_domain gitlab_repo_full_name
      job_info.build_id
  in
  let trace_lines =
    trace
    |> Str.global_replace (Str.regexp "\027\\[[0-9;]*m") ""
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
                 if String.is_prefix ~prefix:"Error" line then Some i else None )
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
    let find regexps =
      List.find_map trace_lines ~f:(fun line ->
          List.find_map regexps ~f:(fun regexp ->
              if string_match ~regexp line then Some (Str.matched_group 1 line)
              else None ) )
    in
    let find_all regexps =
      List.filter_map trace_lines ~f:(fun line ->
          List.find_map regexps ~f:(fun regexp ->
              if string_match ~regexp line then Some (Str.matched_group 1 line)
              else None ) )
    in
    find
      [ "^Using Docker executor with image \\([^ ]+\\)"
      ; "options=Options(docker='\\([^']+\\)')" ]
    >>= fun docker_image ->
    let dependencies = find_all ["^Downloading artifacts for \\([^ ]+\\)"] in
    (* The CI script prints "CI_TARGETS=foo bar" through "env" if it is non-default,
       then "CI_TARGETS = foo bar" even if it is the default (from job name).
       We use the later. *)
    find ["^CI_TARGETS = \\(.*\\)"] >>= fun targets ->
    let targets = String.split ~on:' ' targets in
    find ["^COMPILER=\\(.*\\)"]
    >>= fun compiler ->
    find ["^OPAM_VARIANT=\\(.*\\)"]
    >>= fun opam_variant ->
    Some {docker_image; dependencies; targets; compiler; opam_variant}
  in
  let* summary_tail_prefix =
    match coq_job_info with
    | Some {docker_image; dependencies; targets; compiler; opam_variant} ->
        let switch_name = compiler ^ opam_variant in
        let dependencies = String.concat ~sep:"` `" dependencies in
        let targets = String.concat ~sep:"` `" targets in 
        Lwt.return
          (f
             "This job ran on the Docker image `%s` with OCaml `%s` and depended on jobs \
              `%s`. It built targets `%s`.\n\n"
             docker_image switch_name dependencies targets )
    | None ->
        Lwt.return ""
  in
  let summary_tail = summary_tail_prefix ^ trace_description in
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
                let open Lwt.Syntax in
                let+ _ =
                  GitHub_mutations.create_check_run ~bot_info ~name:context
                    ~repo_id ~head_sha:job_info.common_info.head_commit
                    ~conclusion:NEUTRAL ~status:COMPLETED ~title
                    ~details_url:job_url
                    ~summary:("This job is allowed to fail.\n\n" ^ summary_tail)
                    ~text ~external_id ()
                in
                ()
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
                   (f {|"%s"|} job_info.common_info.head_commit) ->
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
            let open Lwt.Syntax in
            let+ _ =
              GitHub_mutations.create_check_run ~bot_info ~name:context ~repo_id
                ~head_sha:job_info.common_info.head_commit ~conclusion:FAILURE
                ~status:COMPLETED ~title ~details_url:job_url
                ~summary:
                  ( "This job has failed. If you need to, you can restart it \
                     directly in the GitHub interface using the \"Re-run\" \
                     button.\n\n" ^ summary_tail )
                ~text ~external_id ()
            in
            ()
        | Error e ->
            Lwt_io.printf "No repo id: %s\n" e )

let rec send_doc_url_aux ~bot_info job_info ~fallback_urls (kind, url) =
  let context = f "%s: %s artifact" job_info.build_name kind in
  let description_base = f "Link to %s build artifact" kind in
  let open Lwt.Syntax in
  let status_code url =
    let+ resp, _ = url |> Uri.of_string |> Client.get in
    resp |> Response.status |> Code.code_of_status
  in
  let success_response url =
    GitHub_mutations.send_status_check ~repo_full_name:"coq/coq"
      ~commit:job_info.common_info.head_commit ~state:"success" ~url ~context
      ~description:(description_base ^ ".") ~bot_info
  in
  let fail_response code =
    Lwt_io.printf "But we got a %d code when checking the URL.\n" code
    <&>
    let job_url =
      f "https://gitlab.inria.fr/coq/coq/-/jobs/%d" job_info.build_id
    in
    GitHub_mutations.send_status_check ~repo_full_name:"coq/coq"
      ~commit:job_info.common_info.head_commit ~state:"failure" ~url:job_url
      ~context
      ~description:(description_base ^ ": not found.")
      ~bot_info
  in
  let error_code url =
    let+ status_code = status_code url in
    if Int.equal 200 status_code then None else Some status_code
  in
  let* code = error_code url in
  match code with
  | None ->
      success_response url
  | Some code -> (
    match fallback_urls with
    | [] ->
        fail_response code
    | url :: fallback_urls ->
        send_doc_url_aux ~bot_info ~fallback_urls job_info (kind, url) )

let send_doc_url_job ~bot_info ?(fallback_artifacts = []) job_info doc_key
    artifact =
  Lwt_io.printf
    "This is a successful %s build. Pushing a status check with a link...\n"
    doc_key
  <&>
  let build_url artifact =
    f "https://coq.gitlabpages.inria.fr/-/coq/-/jobs/%d/artifacts/%s"
      job_info.build_id artifact
  in
  send_doc_url_aux ~bot_info job_info
    ~fallback_urls:(List.map ~f:build_url fallback_artifacts)
    (doc_key, build_url artifact)

let send_doc_url ~bot_info ~github_repo_full_name job_info =
  match (github_repo_full_name, job_info.build_name) with
  | "coq/coq", ("doc:refman" | "doc:ci-refman") ->
      (* Used to be a non-Dune job, now a Dune job, thus we need a fallback *)
      send_doc_url_job ~bot_info
        ~fallback_artifacts:["_install_ci/share/doc/coq/sphinx/html/index.html"]
        job_info "refman" "_build/default/doc/refman-html/index.html"
  | ( "coq/coq"
    , ( "doc:stdlib" (* only after complete switch to Dune *)
      | "doc:stdlib:dune" (* only before complete switch to Dune *) ) ) ->
      send_doc_url_job ~bot_info job_info "stdlib"
        "_build/default/doc/stdlib/html/index.html"
  | "coq/coq", "doc:ml-api:odoc" ->
      send_doc_url_job ~bot_info job_info "ml-api"
        "_build/default/_doc/_html/index.html"
  | _ ->
      Lwt.return_unit

module BenchResults = struct
  type t =
    { summary_table: string
    ; failures: string
    ; slow_table: string
    ; slow_number: int
    ; fast_table: string
    ; fast_number: int }
end

let fetch_bench_results ~job_info () =
  let open BenchResults in
  let open Lwt.Syntax in
  let fetch_artifact url =
    url |> Uri.of_string |> Client.get
    >>= fun (resp, body) ->
    let status_code = resp |> Response.status |> Code.code_of_status in
    if Int.equal 200 status_code then
      body |> Cohttp_lwt.Body.to_string >>= Lwt.return_ok
    else Lwt.return_error (f "Recieved status %d from %s." status_code url)
  in
  let artifact_url file =
    f
      "https://coq.gitlabpages.inria.fr/-/coq/-/jobs/%d/artifacts/_bench/timings/%s"
      job_info.build_id file
  in
  let* summary_table = artifact_url "bench_summary" |> fetch_artifact in
  let* failures =
    let* failures_or_err = artifact_url "bench_failures" |> fetch_artifact in
    match failures_or_err with
    | Ok s ->
        Lwt.return s
    | Error err ->
        Lwt_io.printlf "Error fetching bench_failures: %s" err
        >>= fun () -> Lwt.return ""
  in
  let* slow_table =
    let* slow_table_or_err = artifact_url "slow_table.html" |> fetch_artifact in
    match slow_table_or_err with
    | Ok s ->
        Lwt.return s
    | Error _ -> (
        let* slow_table_or_err = artifact_url "slow_table" |> fetch_artifact in
        match slow_table_or_err with
        | Ok s ->
            Lwt.return (code_wrap s)
        | Error err ->
            Lwt_io.printlf "Error fetching slow_table: %s" err
            >>= fun () -> Lwt.return "" )
  in
  let* fast_table =
    let* fast_table_or_err = artifact_url "fast_table.html" |> fetch_artifact in
    match fast_table_or_err with
    | Ok s ->
        Lwt.return s
    | Error _ -> (
        let* fast_table_or_err = artifact_url "fast_table" |> fetch_artifact in
        match fast_table_or_err with
        | Ok s ->
            Lwt.return (code_wrap s)
        | Error err ->
            Lwt_io.printlf "Error fetching fast_table: %s" err
            >>= fun () -> Lwt.return "" )
  in
  match summary_table with
  | Error e ->
      Lwt.return_error
        (f "Could not fetch table artifacts for bench summary: %s\n" e)
  | Ok summary_table -> (
      (* The tables include how many entries there are, this is useful
         information to know. *)
      let parse_quantity table table_name =
        let regexp = {|.*TOP \([0-9]*\)|} in
        if Helpers.string_match ~regexp table then
          Str.matched_group 1 table |> Int.of_string |> Lwt.return_ok
        else Lwt.return_error (f "parsing %s table." table_name)
      in
      let* slow_number = parse_quantity slow_table "slow" in
      let* fast_number = parse_quantity fast_table "fast" in
      match (slow_number, fast_number) with
      | Error e, _ | _, Error e ->
          Lwt.return_error (f "Fetch bench regex issue: %s" e)
      | Ok slow_number, Ok fast_number ->
          Lwt.return_ok
            { summary_table
            ; failures
            ; slow_table
            ; slow_number
            ; fast_table
            ; fast_number } )

let bench_text = function
  | Ok results ->
      (* Formatting helpers *)
      let header2 str = f "## %s" str in
      (* Document *)
      let open BenchResults in
      [ header2 ":checkered_flag: Bench Summary:"
      ; code_wrap results.summary_table
      ; results.failures
      ; header2 @@ f ":turtle: Top %d slow downs:" results.slow_number
      ; results.slow_table
      ; header2 @@ f ":rabbit2: Top %d speed ups:" results.fast_number
      ; results.fast_table ]
      |> String.concat ~sep:"\n" |> Lwt.return
  | Error e ->
      f "Error occured when creating bench summary: %s\n" e |> Lwt.return

let bench_comment ~bot_info ~owner ~repo ~number ~gitlab_url ?check_url
    (results : (BenchResults.t, string) Result.t) =
  GitHub_queries.get_pull_request_id ~bot_info ~owner ~repo ~number
  >>= function
  | Ok id -> (
    match results with
    | Ok results -> (
        (* Formatting heleprs *)
        let details summary text =
          f "<details>\n<summary>%s</summary>\n\n%s\n\n</details>\n" summary
            text
        in
        let link text url = f "[%s](%s)" text url in
        [ ":checkered_flag: Bench results:"
        ; code_wrap results.summary_table
        ; results.failures
        ; details
            (f ":turtle: Top %d slow downs" results.slow_number)
            results.slow_table
        ; details
            (f ":rabbit2: Top %d speed ups" results.fast_number)
            results.fast_table
        ; "- " ^ link ":chair: GitLab Bench Job" gitlab_url ]
        @ Option.value_map
            ~f:(fun x -> ["- " ^ link ":spiral_notepad: Bench Check Summary" x])
            ~default:[] check_url
        |> String.concat ~sep:"\n"
        |> fun message ->
        GitHub_mutations.post_comment ~bot_info ~id ~message
        >>= function
        | Ok _ ->
            Lwt.return_unit
        | Error e ->
            Lwt_io.printlf "Unable to post bench comment for pr #%d: %s" number
              e )
    | Error e ->
        Lwt_io.printlf "Unable to fetch_results for bench for pr #%d: %s" number
          e )
  | Error e ->
      Lwt_io.printlf "Unable to get_pull_request_id for bench for pr #%d: %s"
        number e

let update_bench_status ~bot_info job_info (gh_owner, gh_repo) ~external_id
    ~number =
  let open Lwt.Syntax in
  match number with
  | None ->
      Lwt_io.printlf "No PR number provided for bench summary so aborting."
  | Some number -> (
      GitHub_queries.get_repository_id ~bot_info ~owner:gh_owner ~repo:gh_repo
      >>= function
      | Error e ->
          Lwt_io.printlf "No repo id for bench job: %s" e
      | Ok repo_id -> (
          Lwt_io.printl "Pushing status check for bench job."
          <&>
          let gitlab_url =
            f "https://gitlab.inria.fr/coq/coq/-/jobs/%d" job_info.build_id
          in
          let summary =
            f "## GitLab Job URL:\n[GitLab Bench Job](%s)\n" gitlab_url
          in
          let state = job_info.build_status in
          let context = "bench" in
          let create_check_run ~status ?conclusion ~title ?(text = "") () =
            GitHub_mutations.create_check_run ~bot_info ~name:context ~status
              ~repo_id ~head_sha:job_info.common_info.head_commit ?conclusion
              ~title ~details_url:gitlab_url ~summary ~text ~external_id ()
            >>= function
            | Ok url ->
                let* () =
                  Lwt_io.printlf "Bench Check Summary updated: %s" url
                in
                Lwt.return_some url
            | Error e ->
                let* () =
                  Lwt_io.printlf "Bench Check Summary URL missing: %s" e
                in
                Lwt.return_none
          in
          match state with
          | "success" ->
              let* results = fetch_bench_results ~job_info () in
              let* text = bench_text results in
              let* check_url =
                create_check_run ~status:COMPLETED ~conclusion:SUCCESS
                  ~title:"Bench completed successfully" ~text ()
              in
              let* () =
                bench_comment ~bot_info ~owner:gh_owner ~repo:gh_repo ~number
                  ~gitlab_url ?check_url results
              in
              Lwt.return_unit
          | "failed" ->
              let* results = fetch_bench_results ~job_info () in
              let* text = bench_text results in
              let* check_url =
                create_check_run ~status:COMPLETED ~conclusion:NEUTRAL
                  ~title:"Bench completed with failures" ~text ()
              in
              let* () =
                bench_comment ~bot_info ~owner:gh_owner ~repo:gh_repo ~number
                  ~gitlab_url ?check_url results
              in
              Lwt.return_unit
          | "running" ->
              let* _ =
                create_check_run ~status:IN_PROGRESS ~title:"Bench in progress"
                  ()
              in
              Lwt.return_unit
          | "cancelled" | "canceled" ->
              let* _ =
                create_check_run ~status:COMPLETED ~conclusion:CANCELLED
                  ~title:"Bench has been cancelled" ()
              in
              Lwt.return_unit
          | "created" ->
              Lwt_io.printlf "Bench job has been created, ignoring info update."
          | _ ->
              Lwt_io.printlf "Unknown state for bench job: %s" state ) )

type build_failure = Warn of string | Retry of string | Ignore of string

let trace_action ~repo_full_name trace =
  trace |> String.length
  |> Lwt_io.printlf "Trace size: %d."
  >>= fun () ->
  Lwt.return
    (let test regexp = string_match ~regexp trace in
     if test "Job failed: exit code 137" then Retry "Exit code 137"
     else if test "Job failed: exit status 255" then Retry "Exit status 255"
     else if test "Job failed (system failure)" then Retry "System failure"
     else if
       ( test "Uploading artifacts.*to coordinator... failed"
       || test "Uploading artifacts.*to coordinator... error" )
       && not (test "Uploading artifacts.*to coordinator... ok")
     then Retry "Artifact uploading failure"
     else if
       test "ERROR: Downloading artifacts.*from coordinator... error"
       && test "FATAL: invalid argument"
     then Retry "Artifact downloading failure"
     else if
       test "transfer closed with outstanding read data remaining"
       || test "HTTP request sent, awaiting response... 50[0-9]"
       || test "The requested URL returned error: 502"
       || test "received unexpected HTTP status: 50[0-9]"
       || test "unexpected status from GET request to.*: 50[0-9]"
       (*|| test "[Tt]he remote end hung up unexpectedly"*)
       (* Can happen with (actual) issues with overlays. *)
       || test "error: unable to download 'https://cache.nixos.org/"
       || test "fatal: unable to access .* Couldn't connect to server"
       || test "fatal: unable to access .* Could not resolve host"
       || test "Resolving .* failed: Temporary failure in name resolution"
       || test "unexpected status code .*: 401 Unauthorized"
     then Retry "Connectivity issue"
     else if test "fatal: reference is not a tree" then
       Ignore "Normal failure: pull request was force-pushed."
     else if
       test "fatal: Remote branch pr-[0-9]* not found in upstream origin"
       || test "fatal: [Cc]ouldn't find remote ref refs/heads/pr-"
     then Ignore "Normal failure: pull request was closed."
     else if
       String.equal repo_full_name "coq/coq"
       && test "Error response from daemon: manifest for .* not found"
     then Ignore "Docker image not found. Do not report anything specific."
     else Warn trace )

let job_failure ~bot_info job_info ~pr_num (gh_owner, gh_repo)
    ~github_repo_full_name ~gitlab_domain ~gitlab_repo_full_name ~context
    ~failure_reason ~external_id =
  let build_id = job_info.build_id in
  let project_id = job_info.common_info.project_id in
  Lwt_io.printf "Failed job %d of project %d.\nFailure reason: %s\n" build_id
    project_id failure_reason
  >>= fun () ->
  ( if String.equal failure_reason "runner_system_failure" then
      Lwt.return (Retry "Runner failure reported by GitLab CI")
    else
      Lwt_io.printlf
        "Failure reason reported by GitLab CI: %s.\nRetrieving the trace..."
        failure_reason
      >>= fun () ->
      GitLab_queries.get_build_trace ~bot_info ~gitlab_domain ~project_id
        ~build_id
      >>= function
      | Ok trace ->
          trace_action ~repo_full_name:gitlab_repo_full_name trace
      | Error err ->
          Lwt.return (Ignore (f "Error while retrieving the trace: %s." err)) )
  >>= function
  | Warn trace ->
      Lwt_io.printf "Actual failure.\n"
      <&> send_status_check ~bot_info job_info ~pr_num (gh_owner, gh_repo)
            ~github_repo_full_name ~gitlab_domain ~gitlab_repo_full_name
            ~context ~failure_reason ~external_id ~trace
  | Retry reason -> (
      Lwt_io.printlf "%s... Checking whether to retry the job." reason
      >>= fun () ->
      GitLab_queries.get_retry_nb ~bot_info ~gitlab_domain
        ~full_name:gitlab_repo_full_name ~build_id
        ~build_name:job_info.build_name
      >>= function
      | Ok retry_nb when retry_nb < 3 ->
          Lwt_io.printlf
            "The job has been retried less than three times before (number of \
             retries = %d). Retrying..."
            retry_nb
          >>= fun () ->
          GitLab_mutations.retry_job ~bot_info ~gitlab_domain ~project_id
            ~build_id
      | Ok retry_nb ->
          Lwt_io.printlf
            "The job has been retried %d times before. Not retrying." retry_nb
      | Error e ->
          Lwt_io.printlf "Error while getting the number of retries: %s" e )
  | Ignore reason ->
      Lwt_io.printl reason

let job_success_or_pending ~bot_info (gh_owner, gh_repo)
    ({build_id} as job_info) ~github_repo_full_name ~gitlab_domain
    ~gitlab_repo_full_name ~context ~state ~external_id =
  GitHub_queries.get_status_check ~bot_info ~owner:gh_owner ~repo:gh_repo
    ~commit:job_info.common_info.head_commit ~context
  >>= function
  | Ok true -> (
      Lwt_io.printf
        "There existed a previous status check for this build, we'll override \
         it.\n"
      <&>
      let job_url =
        f "https://%s/%s/-/jobs/%d" gitlab_domain gitlab_repo_full_name build_id
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
                 state )
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
              let open Lwt.Syntax in
              let+ _ =
                GitHub_mutations.create_check_run ~bot_info ~name:context
                  ~status ~repo_id ~head_sha:job_info.common_info.head_commit
                  ?conclusion ~title:description ~details_url:job_url
                  ~summary:"" ~external_id ()
              in
              ()
          | Error e ->
              Lwt_io.printf "No repo id: %s\n" e ) )
  | Ok _ ->
      Lwt.return_unit
  | Error e ->
      Lwt_io.printf "%s\n" e

let job_action ~bot_info
    ({build_name; common_info= {http_repo_url}} as job_info) ~gitlab_mapping =
  let pr_num, branch_or_pr = pr_from_branch job_info.common_info.branch in
  let context = f "GitLab CI job %s (%s)" build_name branch_or_pr in
  match parse_gitlab_repo_url ~http_repo_url with
  | Error e ->
      Lwt_io.printlf "Error in job_action: %s" e
  | Ok (gitlab_domain, gitlab_repo_full_name) -> (
      let gh_owner, gh_repo =
        github_repo_of_gitlab_project_path ~gitlab_mapping ~gitlab_domain
          ~gitlab_repo_full_name
      in
      let github_repo_full_name = gh_owner ^ "/" ^ gh_repo in
      let external_id =
        f "%s,projects/%d/jobs/%d" http_repo_url job_info.common_info.project_id
          job_info.build_id
      in
      match (github_repo_full_name, job_info.build_name) with
      | "coq/coq", "bench" ->
          update_bench_status ~bot_info job_info (gh_owner, gh_repo)
            ~external_id ~number:pr_num
      | _, _ -> (
        match job_info.build_status with
        | "failed" ->
            let failure_reason = Option.value_exn job_info.failure_reason in
            job_failure ~bot_info job_info ~pr_num (gh_owner, gh_repo)
              ~github_repo_full_name ~gitlab_domain ~gitlab_repo_full_name
              ~context ~failure_reason ~external_id
        | "success" as state ->
            job_success_or_pending ~bot_info (gh_owner, gh_repo) job_info
              ~github_repo_full_name ~gitlab_domain ~gitlab_repo_full_name
              ~context ~state ~external_id
            <&> send_doc_url ~bot_info job_info ~github_repo_full_name
        | ("created" | "running") as state ->
            job_success_or_pending ~bot_info (gh_owner, gh_repo) job_info
              ~github_repo_full_name ~gitlab_domain ~gitlab_repo_full_name
              ~context ~state ~external_id
        | "cancelled" | "canceled" | "pending" ->
            (* Ideally we should check if a status was already reported for
               this job.  But it is important to avoid making dozens of
               requests at once when a pipeline is canceled.  So we should
               have a caching mechanism to limit this case to a single
               request. *)
            Lwt.return_unit
        | unknown_state ->
            Lwt_io.printlf "Unknown job status: %s" unknown_state ) )

let create_pipeline_summary ?summary_top pipeline_info pipeline_url =
  let variables =
    List.map pipeline_info.variables ~f:(fun (key, value) ->
        f "- %s: %s" key value )
    |> String.concat ~sep:"\n"
  in
  let sorted_builds =
    pipeline_info.builds
    |> List.sort ~compare:(fun build1 build2 ->
           String.compare build1.build_name build2.build_name )
  in
  let stage_summary =
    pipeline_info.stages
    |> List.concat_map ~f:(fun stage ->
           sorted_builds
           |> List.filter_map ~f:(fun build ->
                  if String.equal build.stage stage then
                    Some
                      (f "  - [%s](%s/-/jobs/%d)" build.build_name
                         pipeline_info.common_info.http_repo_url build.build_id )
                  else None )
           |> List.cons ("- " ^ stage) )
    |> String.concat ~sep:"\n"
  in
  [ f "This [GitLab pipeline](%s) sets the following variables:" pipeline_url
  ; variables
  ; "It contains the following stages and jobs:"
  ; stage_summary
  ; f "GitLab Project ID: %d" pipeline_info.common_info.project_id ]
  |> (match summary_top with Some text -> List.cons text | None -> Fn.id)
  |> String.concat ~sep:"\n\n"

type ci_minimization_info =
  { target: string
  ; full_target: string
  ; ci_targets: string list
  ; docker_image: string
  ; opam_switch: string
  ; failing_urls: string
  ; passing_urls: string }

type coqbot_minimize_script_data =
  | MinimizeScript of {quote_kind: string; body: string}
  | MinimizeAttachment of {description: string; url: string}

type artifact_info =
  | ArtifactInfo of
      {artifact_owner: string; artifact_repo: string; artifact_id: string}

let parse_github_artifact_url url =
  let github_prefix = "https://github.com/" in
  let regexp =
    Str.quote github_prefix
    ^ "\\([^/]+\\)/\\([^/]+\\)/\\(actions/runs\\|suites\\)/.*/artifacts/\\([0-9]+\\)"
  in
  if string_match ~regexp url then
    Some
      (ArtifactInfo
         { artifact_owner= Str.matched_group 1 url
         ; artifact_repo= Str.matched_group 2 url
         ; artifact_id= Str.matched_group 4 url } )
  else None

type artifact_error =
  | ArtifactEmpty
  | ArtifactContainsMultipleFiles of string list
  | ArtifactDownloadError of string

type run_ci_minimization_error =
  | ArtifactError of
      {url: string; artifact: artifact_info; artifact_error: artifact_error}
  | DownloadError of {url: string; error: string}

let run_ci_minimization_error_to_string = function
  | ArtifactError
      { url= artifact_url
      ; artifact= ArtifactInfo {artifact_owner; artifact_repo; artifact_id}
      ; artifact_error } -> (
    match artifact_error with
    | ArtifactEmpty ->
        f "Could not resume minimization with [empty artifact](%s)" artifact_url
    | ArtifactContainsMultipleFiles filenames ->
        f
          "Could not resume minimization because [artifact](%s) contains more \
           than one file: %s"
          artifact_url
          (String.concat ~sep:", " filenames)
    | ArtifactDownloadError error ->
        f
          "Could not resume minimization because [artifact %s/%s:%s](%s) \
           failed to download:\n\
           %s"
          artifact_owner artifact_repo artifact_id artifact_url error )
  | DownloadError {url; error} ->
      f
        "Could not resume minimization because [artifact](%s) failed to \
         download:\n\
         %s"
        url error

let run_ci_minimization ~bot_info ~comment_thread_id ~owner ~repo ~pr_number
    ~base ~head ~ci_minimization_infos ~bug_file ~minimizer_extra_arguments =
  let open Lwt_result.Infix in
  (* for convenience of control flow, we always create the temporary
     file, but we only pass in the file name if the bug file contents
     is non-None *)
  Lwt_io.with_temp_file (fun (bug_file_name, bug_file_ch) ->
      (let open Lwt.Infix in
       match bug_file with
       | None ->
           Lwt.return_ok ()
       | Some (MinimizeScript {body= bug_file_contents}) ->
           Lwt_io.write bug_file_ch bug_file_contents >>= Lwt.return_ok
       | Some (MinimizeAttachment {url}) -> (
         match parse_github_artifact_url url with
         | Some
             ( ArtifactInfo {artifact_owner; artifact_repo; artifact_id} as
               artifact ) -> (
             Lwt_io.printlf
               "Downloading artifact %s/%s:%s for %s/%s#%s (%s) (parsed from \
                %s)"
               artifact_owner artifact_repo artifact_id owner repo pr_number
               (GitHub_ID.to_string comment_thread_id)
               url
             >>= fun () ->
             GitHub_queries.get_artifact_blob ~bot_info ~owner:artifact_owner
               ~repo:artifact_repo ~artifact_id
             >>= function
             | Ok [(_filename, bug_file_contents)] ->
                 Lwt_io.write bug_file_ch bug_file_contents >>= Lwt.return_ok
             | Ok [] ->
                 Lwt.return_error
                   (ArtifactError {url; artifact; artifact_error= ArtifactEmpty})
             | Ok files ->
                 files
                 |> List.map ~f:(fun (filename, _contents) -> filename)
                 |> fun artifact_filenames ->
                 Lwt.return_error
                   (ArtifactError
                      { url
                      ; artifact
                      ; artifact_error=
                          ArtifactContainsMultipleFiles artifact_filenames } )
             | Error message ->
                 Lwt.return_error
                   (ArtifactError
                      { url
                      ; artifact
                      ; artifact_error= ArtifactDownloadError message } ) )
         | None ->
             download_to ~uri:(Uri.of_string url) bug_file_ch
             |> Lwt_result.map_error (fun error -> DownloadError {url; error}) )
      )
      >>= fun () ->
      let open Lwt.Infix in
      Lwt_io.flush bug_file_ch
      >>= fun () ->
      let bug_file_name = Option.map ~f:(fun _ -> bug_file_name) bug_file in
      Lwt_list.map_s
        (fun {target; ci_targets; opam_switch; failing_urls; passing_urls; docker_image} ->
          git_run_ci_minimization ~bot_info ~comment_thread_id ~owner ~repo
            ~pr_number ~docker_image ~ci_targets ~target ~opam_switch ~failing_urls
            ~passing_urls ~base ~head ~minimizer_extra_arguments ~bug_file_name
          >>= fun result -> Lwt.return (target, result) )
        ci_minimization_infos
      >>= Lwt.return_ok )
  >>= fun results ->
  results
  |> List.partition_map ~f:(function
       | target, Ok () ->
           Either.First target
       | target, Error f ->
           Either.Second (target, f) )
  |> Lwt.return_ok

type ci_minimization_job_suggestion_info =
  { base_job_failed: bool
  ; base_job_errored: string option
  ; head_job_succeeded: bool
  ; missing_error: bool
  ; non_v_file: string option
  ; job_kind: string
  ; job_target: string (*; overlayed: bool*) }

let ci_minimization_extract_job_specific_info ~head_pipeline_summary
    ~base_pipeline_summary ~base_checks_errors ~base_checks = function
  | ( {name= full_name; summary= Some summary; text= Some text}
    , head_job_succeeded ) ->
      let base_job_errored =
        List.find_map
          ~f:(fun (base_name, err) ->
            if String.equal full_name base_name then Some err else None )
          base_checks_errors
      in
      let base_job_failed =
        List.exists
          ~f:(fun ({name= base_name}, success_base) ->
            String.equal full_name base_name && not success_base )
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
              "This job ran on the Docker image `\\([^`]+\\)` with OCaml `\\([^`]+\\)` and depended on jobs \
               \\(\\(`[^`]+` ?\\)+\\). It built targets \\(\\(`[^`]+` ?\\)+\\).\n\n"
            summary
        then
          let docker_image, opam_switch, dependencies, targets =
            ( Str.matched_group 1 summary
            , Str.matched_group 2 summary
            , Str.matched_group 3 summary
            , Str.matched_group 4 summary )
          in
          let dependencies = Str.split (Str.regexp "[ `]+") dependencies in
          let ci_targets = Str.split (Str.regexp "[ `]+") targets in
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
          let extract_artifacts url =
            List.partition_map ~f:(fun name ->
                match extract_artifact_url name url with
                | Some v -> First v
                | None -> Second name)
              (name::dependencies)
          in
          match
            ( extract_artifacts base_pipeline_summary
            , extract_artifacts head_pipeline_summary )
          with
          | ( (base_urls, [])
            , (head_urls, []) ) ->
              Ok
                ( { base_job_failed
                  ; base_job_errored
                  ; missing_error
                  ; non_v_file
                  ; job_kind
                  ; head_job_succeeded
                  ; job_target= target (*; overlayed= false (* XXX FIXME *)*) }
                , { target
                  ; full_target= name
                  ; ci_targets 
                  ; docker_image
                  ; opam_switch
                  ; failing_urls= String.concat ~sep:" " head_urls
                  ; passing_urls= String.concat ~sep:" " base_urls } )
          | (_, ((_ :: _) as base_failed)), _ ->
              Error
                (f "Could not find base dependencies artifacts for %s in:\n%s"
                   (String.concat ~sep:" " base_failed)
                   (collapse_summary "Base Pipeline Summary"
                      base_pipeline_summary ) )
          | _, (_, ((_ :: _) as head_failed)) ->
              Error
                (f "Could not find head dependencies artifacts for %s in:\n%s"
                   (String.concat ~sep:" " head_failed)
                   (collapse_summary "Head Pipeline Summary"
                      head_pipeline_summary ) )
        else
          Error
            (f "Could not find needed parameters for job %s in summary:\n%s\n"
               name
               (collapse_summary "Summary" summary) )
      else
        Error (f "Could not separate '%s' into job_kind:ci-target." full_name)
  | {name; summary= None}, _ ->
      Error (f "Could not find summary for job %s." name)
  | {name; text= None}, _ ->
      Error (f "Could not find text for job %s." name)

type ci_minimization_pr_info =
  { comment_thread_id: GitHub_ID.t
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
    ~head_pipeline_summary ?base_sha ?head_sha () =
  let open Lwt.Syntax in
  let* () =
    Lwt_io.printlf "I'm going to look for failed tests to minimize on PR #%d."
      pr_number
  in
  let* refs =
    match (base_sha, head_sha) with
    | None, _ | _, None ->
        let open Lwt_result.Syntax in
        let+ {base= {sha= base}; head= {sha= head}} =
          GitHub_queries.get_pull_request_refs ~bot_info ~owner ~repo
            ~number:pr_number
        in
        (base, head)
    | Some base, Some head ->
        Lwt.return_ok (base, head)
  in
  match refs with
  | Error err ->
      Lwt.return_error
        ( None
        , f "Error while fetching PR refs for %s/%s#%d for CI minimization: %s"
            owner repo pr_number err )
  | Ok (base, head) -> (
      (* TODO: figure out why there are quotes, cf https://github.com/coq/bot/issues/61 *)
      let base = Str.global_replace (Str.regexp {|"|}) "" base in
      let head = Str.global_replace (Str.regexp {|"|}) "" head in
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
                    , status ) )
          in
          let base_checks_errors, base_checks = partition_errors base_checks in
          let head_checks_errors, head_checks = partition_errors head_checks in
          head_checks_errors
          |> Lwt_list.iter_p (fun (_, error) ->
                 Lwt_io.printlf
                   "Non-fatal error while looking for failed tests of PR #%d \
                    to minimize: %s"
                   pr_number error )
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
                      `Trd check_tab_info )
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
                    else None )
                @ List.filter_map head_checks_errors ~f:(fun (name, _) ->
                      if string_match ~regexp:"test-suite" name then Some name
                      else None )
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
                           Either.First result )
              in
              let unminimizable_jobs =
                unminimizable_jobs
                @ ( unfinished_head_checks
                  |> List.map ~f:(fun {name} ->
                         (name, f "Job %s is still in progress." name) ) )
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
    ; job_kind
    ; job_target (*; overlayed*) } =
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
        else if String.equal job_target "ci-coq_tools" then
          Possible
            (f
               "coq-tools is too sensitive to the output of coqc to be \
                minimized at this time (instead, @JasonGross can help diagnose \
                and fix the issue)" )
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
           (String.concat ~sep:", " failed_test_suite_jobs) )
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

let format_options_for_getopts options =
  " " ^ options ^ " " |> Str.global_replace (Str.regexp "[\n\r\t]") " "

let getopts options ~opt =
  map_string_matches
    ~regexp:(f " %s\\(\\.\\|[ =:-]\\|: \\)\\([^ ]+\\) " opt)
    ~f:(fun () -> Str.matched_group 2 options)
    options

let getopt options ~opt =
  options |> getopts ~opt |> List.hd |> Option.value ~default:""

let accumulate_extra_minimizer_arguments options =
  let extra_args = getopts ~opt:"extra-arg" options in
  let inline_stdlib = getopt ~opt:"inline-stdlib" options in
  ( if String.equal inline_stdlib "yes" then Lwt.return ["--inline-coqlib"]
    else
      ( if not (String.equal inline_stdlib "") then
          Lwt_io.printlf
            "Ignoring invalid option to inline-stdlib '%s' not equal to 'yes'"
            inline_stdlib
        else Lwt.return_unit )
      >>= fun () -> Lwt.return_nil )
  >>= fun inline_stdlib_args -> inline_stdlib_args @ extra_args |> Lwt.return

let minimize_failed_tests ~bot_info ~owner ~repo ~pr_number
    ~head_pipeline_summary ~request ~comment_on_error ~bug_file ~options
    ?base_sha ?head_sha () =
  let options = format_options_for_getopts options in
  accumulate_extra_minimizer_arguments options
  >>= fun minimizer_extra_arguments ->
  Lwt_io.printlf
    "Parsed options for the bug minimizer at %s/%s#%d from '%s' into \
     {minimizer_extra_arguments: '%s'}"
    owner repo pr_number options
    (String.concat ~sep:" " minimizer_extra_arguments)
  >>= fun () ->
  fetch_ci_minimization_info ~bot_info ~owner ~repo ~pr_number
    ~head_pipeline_summary ?base_sha ?head_sha ()
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
               String.compare name1 name2 )
      in
      possible_jobs_to_minimize
      |> List.sort ~compare:(fun (_, info1) (_, info2) ->
             compare_minimization_info info1 info2 )
      |> List.map ~f:(fun (suggestion_info, minimization_info) ->
             (ci_minimization_suggest ~base suggestion_info, minimization_info) )
      |> List.partition3_map ~f:(function
           | Suggested, minimization_info ->
               `Fst minimization_info
           | Possible reason, minimization_info ->
               `Snd (reason, minimization_info)
           | Bad reason, minimization_info ->
               `Trd (reason, minimization_info) )
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
                         (*|| String.equal full_target request*) )
                       requests )
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
        ~ci_minimization_infos:jobs_to_minimize ~minimizer_extra_arguments
        ~bug_file
      >>= function
      | Ok (jobs_minimized, jobs_that_could_not_be_minimized) -> (
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
                  (Printf.sprintf
                     "The job %s could not be minimized because %s.\n" name err )
            | unminimizable_jobs ->
                Some
                  ( "The following jobs could not be minimized:\n"
                  ^ ( unminimizable_jobs
                    |> List.map ~f:(fun (name, err) ->
                           Printf.sprintf "- %s (%s)" name err )
                    |> String.concat ~sep:"\n" )
                  ^ "\n\n" )
          in
          let bad_jobs_description ~f =
            match
              bad_jobs_to_minimize
              |> List.filter ~f:(fun (_, {target (*; full_target*)}) ->
                     f target (*|| f full_target*) )
            with
            | [] ->
                None
            | [(reason, {target})] ->
                Some
                  (Printf.sprintf "The job %s was not minimized because %s.\n"
                     target reason )
            | bad_jobs ->
                Some
                  ( "The following jobs were not minimized:\n"
                  ^ ( bad_jobs
                    |> List.map ~f:(fun (reason, {target}) ->
                           Printf.sprintf "- %s because %s" target reason )
                    |> String.concat ~sep:"\n" )
                  ^ "\n\n" )
          in
          let bad_and_unminimizable_jobs_description ~f =
            match
              (bad_jobs_description ~f, unminimizable_jobs_description ~f)
            with
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
                           Printf.sprintf "- %s (%s)" name err )
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
                f
                  "\n\
                   However, you may want to try again once the %s for the %s \
                   %s."
                  (pluralize "pipeline" ls)
                  (ls |> String.concat ~sep:" and ")
                  (pluralize "finishes" ~plural:"finish" ls)
          in
          let may_wish_to_wait_msg =
            match unfinished_pipelines_description with
            | [] ->
                ""
            | ls ->
                f
                  "\n\n\
                   :warning: :hourglass: You may want to wait until the %s for \
                   the %s %s."
                  (pluralize "pipeline" ls)
                  (ls |> String.concat ~sep:" and ")
                  (pluralize "finishes" ~plural:"finish" ls)
          in
          let note_some_head_unfinished_msg =
            if head_pipeline_finished then ""
            else
              f
                "\n\
                 Some jobs may have been missed because the pipeline for the \
                 head commit (%s) has not yet finished."
                head
          in
          let note_some_base_unfinished_msg =
            if base_pipeline_finished then ""
            else
              f
                "\n\
                 However, minimization may fail because the pipeline for the \
                 base commit (%s) has not yet finished."
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
              ( match
                  bad_and_unminimizable_jobs_description ~f:(fun _ -> true)
                with
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
                    "I did not succeed at triggering minimization on any jobs \
                     at commit %s.%s"
                    head try_again_msg
              | _ :: _ ->
                  (* TODO: change https://github.com/coq-community/run-coq-bug-minimizer/actions to a link to the particular action run when we can get that information *)
                  f
                    "I am now [%s \
                     minimization](https://github.com/coq-community/run-coq-bug-minimizer/actions) \
                     at commit %s on %s. I'll come back to you with the \
                     results once it's done.%s"
                    (if Option.is_none bug_file then "running" else "resuming")
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
                             string_match ~regexp:(Str.quote request) target )
                           jobs_that_could_not_be_minimized
                       , List.find
                           ~f:(fun (target, _) ->
                             string_match ~regexp:(Str.quote request) target )
                           unminimizable_jobs
                       , List.find
                           ~f:(fun (_, {target}) ->
                             string_match ~regexp:(Str.quote request) target )
                           bad_jobs_to_minimize )
                     with
                     | true, _, _, _ ->
                         `Fst request
                     | false, Some (target, err), _, _ ->
                         `Snd
                           (f "%s: failed to trigger minimization (%s)" target
                              err )
                     | false, None, Some (target, err), _ ->
                         `Snd (f "%s could not be minimized (%s)" target err)
                     | false, None, None, Some (reason, {target}) ->
                         `Snd
                           (f "%s was not minimized because %s" target reason)
                     | false, None, None, None ->
                         `Trd request )
              |> fun ( successful_requests
                     , unsuccessful_requests
                     , unfound_requests ) ->
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
                  @ List.map
                      ~f:(fun (_, {target}) -> target)
                      bad_jobs_to_minimize
                  |> List.sort ~compare:String.compare
                in
                match unfound_requests with
                | [] ->
                    None
                | [request] ->
                    Some
                      (f
                         "requested target '%s' could not be found among the \
                          jobs %s.%s"
                         request
                         (all_jobs |> String.concat ~sep:", ")
                         note_some_head_unfinished_msg )
                | _ :: _ :: _ ->
                    Some
                      (f
                         "requested targets %s could not be found among the \
                          jobs %s.%s"
                         (unfound_requests |> String.concat ~sep:", ")
                         (all_jobs |> String.concat ~sep:", ")
                         note_some_head_unfinished_msg )
              in
              let unsuccessful_requests_report =
                match
                  (unsuccessful_requests_report, unfound_requests_report)
                with
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
                  (* TODO: change https://github.com/coq-community/run-coq-bug-minimizer/actions to a link to the particular action run when we can get that information *)
                  f
                    "I am now [%s \
                     minimization](https://github.com/coq-community/run-coq-bug-minimizer/actions) \
                     at commit %s on requested %s %s. I'll come back to you \
                     with the results once it's done.%s\n\n\
                     %s"
                    (if Option.is_none bug_file then "running" else "resuming")
                    head
                    (pluralize "target" successful_requests)
                    (successful_requests |> String.concat ~sep:", ")
                    note_some_base_unfinished_msg
                    (Option.value ~default:"" unsuccessful_requests_report) )
              |> Lwt.return_some
          | RequestSuggested, [], None ->
              ( match possible_jobs_to_minimize with
              | [] ->
                  f "No CI jobs are available to be minimized for commit %s.%s"
                    head try_again_msg
              | _ :: _ ->
                  f
                    "You requested minimization of suggested failing CI jobs, \
                     but no jobs were suggested at commit %s. You can trigger \
                     minimization of %s with `ci minimize all` or by \
                     requesting some targets by name.%s"
                    head
                    ( possible_jobs_to_minimize
                    |> List.map ~f:(fun (_, {target}) -> target)
                    |> String.concat ~sep:", " )
                    may_wish_to_wait_msg )
              |> Lwt.return_some
          | RequestSuggested, [], Some failed_minimization_description ->
              f
                "I attempted to minimize suggested failing CI jobs at commit \
                 %s, but was unable to succeed on any jobs.%s\n\
                 %s"
                head try_again_msg failed_minimization_description
              |> Lwt.return_some
          | RequestSuggested, _ :: _, _ ->
              (* TODO: change https://github.com/coq-community/run-coq-bug-minimizer/actions to a link to the particular action run when we can get that information *)
              f
                "I have [initiated \
                 minimization](https://github.com/coq-community/run-coq-bug-minimizer/actions) \
                 at commit %s for the suggested %s %s as requested.%s\n\n\
                 %s"
                head
                (pluralize "target" jobs_minimized)
                (jobs_minimized |> String.concat ~sep:", ")
                try_again_msg
                (Option.value ~default:"" failed_minimization_description)
              |> Lwt.return_some
          | Auto, jobs_minimized, failed_minimization_description -> (
              ( match
                  bad_and_unminimizable_jobs_description ~f:(fun _ -> true)
                with
              | Some msg ->
                  Lwt_io.printlf
                    "When attempting to run CI Minimization by auto on \
                     %s/%s@%s for PR #%d:\n\
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
                         ":runner: <code>@%s ci minimize</code> will minimize \
                          the following %s: %s"
                         bot_info.github_name
                         (pluralize "target" suggested_jobs_to_minimize)
                         ( suggested_jobs_to_minimize
                         |> List.map ~f:(fun {target} -> target)
                         |> String.concat ~sep:", " ) )
              in
              let suggest_only_all_jobs =
                let pre_message =
                  f
                    "- If you tag me saying `@%s ci minimize all`, I will \
                     additionally minimize the following %s (which I do not \
                     suggest minimizing):"
                    bot_info.github_name
                    (pluralize "target" possible_jobs_to_minimize)
                in
                match possible_jobs_to_minimize with
                | [] ->
                    None
                | [(reason, {target})] ->
                    Some
                      (f "%s %s (because %s)\n\n\n" pre_message target reason)
                | _ ->
                    Some
                      (f "%s\n%s\n\n\n" pre_message
                         ( possible_jobs_to_minimize
                         |> List.map ~f:(fun (reason, {target}) ->
                                f "  - %s (because %s)" target reason )
                         |> String.concat ~sep:"\n" ) )
              in
              match
                ( jobs_minimized
                , failed_minimization_description
                , suggest_jobs
                , suggest_only_all_jobs
                , suggest_minimization )
              with
              | [], None, None, None, _ ->
                  Lwt_io.printlf
                    "No candidates found for minimization on %s/%s@%s for PR \
                     #%d."
                    owner repo head pr_number
                  >>= fun () -> Lwt.return_none
              | [], None, None, Some msg, _ ->
                  Lwt_io.printlf
                    "No suggested candidates found for minimization on \
                     %s/%s@%s for PR #%d:\n\
                     %s"
                    owner repo head pr_number msg
                  >>= fun () -> Lwt.return_none
              | [], None, Some suggestion_msg, _, Error reason ->
                  Lwt_io.printlf
                    "Candidates found for minimization on %s/%s@%s for PR #%d, \
                     but I am not commenting because minimization is not \
                     suggested because %s:\n\
                     %s\n\
                     %s"
                    owner repo head pr_number reason suggestion_msg
                    (Option.value ~default:"" suggest_only_all_jobs)
                  >>= fun () -> Lwt.return_none
              | [], Some failed_minimization_description, _, _, _ ->
                  Lwt_io.printlf
                    "Candidates found for auto minimization on %s/%s@%s for PR \
                     #%d, but all attempts to trigger minimization failed:\n\
                     %s"
                    owner repo head pr_number failed_minimization_description
                  >>= fun () -> Lwt.return_none
              | [], None, Some suggestion_msg, _, Ok () ->
                  f
                    ":red_circle: CI %s at commit %s without any failure in \
                     the test-suite\n\n\
                     :heavy_check_mark: Corresponding %s for the base commit \
                     %s succeeded\n\n\
                     :grey_question: Ask me to try to extract %s that can be \
                     added to the test-suite\n\n\
                     <details><summary>%s</summary>\n\n\
                     - You can also pass me a specific list of targets to \
                     minimize as arguments.\n\
                     %s\n\
                     </details>%s"
                    (pluralize "failure" suggested_jobs_to_minimize)
                    head
                    (pluralize "job" suggested_jobs_to_minimize)
                    base
                    (pluralize "a minimal test case"
                       ~plural:"minimal test cases" suggested_jobs_to_minimize )
                    suggestion_msg
                    (Option.value ~default:"" suggest_only_all_jobs)
                    may_wish_to_wait_msg
                  |> Lwt.return_some
              | _ :: _, _, _, _, _ ->
                  f
                    ":red_circle: CI %s at commit %s without any failure in \
                     the test-suite\n\n\
                     :heavy_check_mark: Corresponding %s for the base commit \
                     %s succeeded\n\n\
                     <details><summary>:runner: I have automatically started \
                     minimization for %s to augment the test-suite</summary>\n\n\
                     - You can also pass me a specific list of targets to \
                     minimize as arguments.\n\
                     %s\n\
                     </details>"
                    (pluralize "failure" jobs_minimized)
                    head
                    (pluralize "job" jobs_minimized)
                    base
                    (jobs_minimized |> String.concat ~sep:", ")
                    (Option.value ~default:"" suggest_only_all_jobs)
                  |> Lwt.return_some ) )
          >>= function
          | Some message ->
              GitHub_mutations.post_comment ~id:comment_thread_id ~message
                ~bot_info
              >>= GitHub_mutations.report_on_posting_comment
          | None ->
              Lwt_io.printlf
                "NOT commenting with CI minimization information at %s/%s@%s \
                 (PR #%d)."
                owner repo head pr_number )
      | Error err ->
          let message = run_ci_minimization_error_to_string err in
          if comment_on_error then
            GitHub_mutations.post_comment ~id:comment_thread_id ~message
              ~bot_info
            >>= GitHub_mutations.report_on_posting_comment
          else
            Lwt_io.printlf "Error while attempting to minimize from PR #%d:\n%s"
              pr_number message )
  | Error (Some comment_thread_id, err) when comment_on_error ->
      GitHub_mutations.post_comment ~id:comment_thread_id
        ~message:
          (f "Error while attempting to find job minimization information:\n%s"
             err )
        ~bot_info
      >>= GitHub_mutations.report_on_posting_comment
  | Error (_, err) ->
      Lwt_io.printlf
        "Error while attempting to find jobs to minimize from PR #%d:\n%s"
        pr_number err

let ci_minimize ~bot_info ~comment_info ~requests ~comment_on_error ~options
    ~bug_file =
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
    ~comment_on_error ~options ~bug_file ()

let pipeline_action ~bot_info ({common_info= {http_repo_url}} as pipeline_info)
    ~gitlab_mapping : unit Lwt.t =
  let pr_number, _ = pr_from_branch pipeline_info.common_info.branch in
  match pipeline_info.state with
  | "skipped" ->
      Lwt.return_unit
  | _ -> (
      let pipeline_url =
        f "%s/-/pipelines/%d" http_repo_url pipeline_info.pipeline_id
      in
      let external_id =
        f "%s,projects/%d/pipelines/%d" http_repo_url
          pipeline_info.common_info.project_id pipeline_info.pipeline_id
      in
      match github_repo_of_gitlab_url ~gitlab_mapping ~http_repo_url with
      | Error err ->
          Lwt_io.printlf "Error in pipeline action: %s" err
      | Ok (gh_owner, gh_repo) -> (
          let state, status, conclusion, title, summary_top =
            (* For the Coq repo only, we report whether this was a full or a light CI *)
            let full_ci =
              match (gh_owner, gh_repo) with
              | "coq", "coq" -> (
                try
                  List.find_map
                    ~f:(fun (key, value) ->
                      if String.equal key "FULL_CI" then
                        Some (Bool.of_string value)
                      else None )
                    pipeline_info.variables
                with _ -> None )
              | _ ->
                  None
            in
            let qualified_pipeline =
              match full_ci with
              | Some true ->
                  "Full pipeline"
              | Some false ->
                  "Light pipeline"
              | None ->
                  "Pipeline"
            in
            match pipeline_info.state with
            | "pending" ->
                ( "pending"
                , QUEUED
                , None
                , f "%s is pending on GitLab CI" qualified_pipeline
                , None )
            | "running" ->
                ( "pending"
                , IN_PROGRESS
                , None
                , f "%s is running on GitLab CI" qualified_pipeline
                , None )
            | "success" ->
                ( "success"
                , COMPLETED
                , Some
                    ( match full_ci with
                    | Some false ->
                        NEUTRAL
                    | Some true | None ->
                        SUCCESS )
                , f "%s completed successfully on GitLab CI" qualified_pipeline
                , None )
            | "failed" ->
                ( "failure"
                , COMPLETED
                , Some FAILURE
                , f "%s completed with errors on GitLab CI" qualified_pipeline
                , Some
                    "*If you need to restart the entire pipeline, you may do \
                     so directly in the GitHub interface using the \"Re-run\" \
                     button.*" )
            | "cancelled" | "canceled" ->
                ( "error"
                , COMPLETED
                , Some CANCELLED
                , f "%s was cancelled on GitLab CI" qualified_pipeline
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
              GitHub_mutations.send_status_check
                ~repo_full_name:(gh_owner ^ "/" ^ gh_repo)
                ~commit:pipeline_info.common_info.head_commit ~state
                ~url:pipeline_url
                ~context:
                  (f "GitLab CI pipeline (%s)"
                     (pr_from_branch pipeline_info.common_info.branch |> snd) )
                ~description:title ~bot_info
          | Some _ -> (
              GitHub_queries.get_repository_id ~bot_info ~owner:gh_owner
                ~repo:gh_repo
              >>= function
              | Error e ->
                  Lwt_io.printf "No repo id: %s\n" e
              | Ok repo_id -> (
                  let summary =
                    create_pipeline_summary ?summary_top pipeline_info
                      pipeline_url
                  in
                  GitHub_mutations.create_check_run ~bot_info
                    ~name:
                      (f "GitLab CI pipeline (%s)"
                         (pr_from_branch pipeline_info.common_info.branch |> snd) )
                    ~repo_id ~head_sha:pipeline_info.common_info.head_commit
                    ~status ?conclusion ~title ~details_url:pipeline_url
                    ~summary ~external_id ()
                  >>= fun _ ->
                  Lwt_unix.sleep 5.
                  >>= fun () ->
                  match (gh_owner, gh_repo, pipeline_info.state, pr_number) with
                  | "coq", "coq", "failed", Some pr_number ->
                      minimize_failed_tests ~bot_info ~owner:gh_owner
                        ~repo:gh_repo ~pr_number
                        ~head_pipeline_summary:(Some summary) ~request:Auto
                        ~comment_on_error:false ~options:"" ~bug_file:None
                        ?base_sha:pipeline_info.common_info.base_commit
                        ~head_sha:pipeline_info.common_info.head_commit ()
                  | _ ->
                      Lwt.return_unit ) ) ) )

let run_coq_minimizer ~bot_info ~script ~comment_thread_id ~comment_author
    ~owner ~repo ~options =
  let options = format_options_for_getopts options in
  let getopt_version opt =
    options |> getopt ~opt |> Str.replace_first (Str.regexp "^[vV]") ""
  in
  accumulate_extra_minimizer_arguments options
  >>= fun minimizer_extra_arguments ->
  let coq_version = getopt_version "[Cc]oq" in
  let ocaml_version = getopt_version "[Oo][Cc]aml" in
  Lwt_io.printlf
    "Parsed options for the bug minimizer at %s/%s@%s from '%s' into \
     {coq_version: '%s'; ocaml_version: '%s'; minimizer_extra_arguments: '%s'}"
    owner repo
    (GitHub_ID.to_string comment_thread_id)
    options coq_version ocaml_version
    (String.concat ~sep:" " minimizer_extra_arguments)
  >>= fun () ->
  ( match script with
  | MinimizeScript {quote_kind; body} ->
      if
        List.mem ~equal:String.equal
          ["shell"; "sh"; "shell-script"; "bash"; "zsh"]
          (String.lowercase quote_kind)
        || String.is_prefix ~prefix:"#!" body
      then
        Lwt_io.printlf "Assuming script (quote_kind: %s) is a shell script"
          quote_kind
        >>= fun () -> Lwt.return body
      else
        Lwt_io.printlf "Assuming script (quote_kind: %s) is a .v file"
          quote_kind
        >>= fun () ->
        let fname = "thebug.v" in
        Lwt.return
          (f "#!/usr/bin/env bash\ncat > %s <<'EOF'\n%s\nEOF\ncoqc -q %s" fname
             body fname )
  | MinimizeAttachment {description; url} ->
      Lwt.return
        ( "#!/usr/bin/env bash\n"
        ^ Stdlib.Filename.quote_command "./handle-web-file.sh" [description; url]
        ) )
  >>= fun script ->
  git_coq_bug_minimizer ~bot_info ~script ~comment_thread_id ~comment_author
    ~owner ~repo ~coq_version ~ocaml_version ~minimizer_extra_arguments
  >>= function
  | Ok () ->
      (* TODO: change https://github.com/coq-community/run-coq-bug-minimizer/actions to a link to the particular action run when we can get that information *)
      GitHub_mutations.post_comment ~id:comment_thread_id
        ~message:
          (f
             "Hey @%s, the coq bug minimizer [is \
              running](https://github.com/coq-community/run-coq-bug-minimizer/actions) \
              your script, I'll come back to you with the results once it's \
              done."
             comment_author )
        ~bot_info
      >>= GitHub_mutations.report_on_posting_comment
  | Error e ->
      Lwt_io.printf "Error: %s\n" e
      >>= fun () ->
      GitHub_mutations.post_comment ~id:comment_thread_id
        ~message:
          (f
             "Error encountered when attempting to start the coq bug minimizer:\n\
              %s\n\n\
              cc @JasonGross" e )
        ~bot_info
      >>= GitHub_mutations.report_on_posting_comment

let coq_bug_minimizer_results_action ~bot_info ~ci ~key ~app_id body =
  if string_match ~regexp:"\\([^\n]+\\)\n\\([^\r]*\\)" body then
    let stamp = Str.matched_group 1 body in
    let message = Str.matched_group 2 body in
    match Str.split (Str.regexp " ") stamp with
    | [id; author; repo_name; branch_name; owner; _repo; _ (*pr_number*)]
    | [id; author; repo_name; branch_name; owner; _repo] ->
        (fun () ->
          Github_installations.action_as_github_app ~bot_info ~key ~app_id
            ~owner
            (GitHub_mutations.post_comment ~id:(GitHub_ID.of_string id)
               ~message:(if ci then message else f "@%s, %s" author message) )
          >>= GitHub_mutations.report_on_posting_comment
          <&> ( execute_cmd
                  (* To delete the branch we need to identify as
                     coqbot the GitHub user, who is a collaborator on
                     the run-coq-bug-minimizer repo, not coqbot the
                     GitHub App *)
                  (f "git push https://%s:%s@github.com/%s.git --delete '%s'"
                     bot_info.github_name bot_info.github_pat repo_name
                     branch_name )
              >>= function
              | Ok () ->
                  Lwt.return_unit
              | Error f ->
                  Lwt_io.printf "Error: %s\n" f ) )
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
        | docker_image :: target :: opam_switch :: failing_urls :: passing_urls
          :: base :: head :: extra_arguments_joined :: bug_file_lines ->
            (let minimizer_extra_arguments =
               String.split ~on:' ' extra_arguments_joined
             in
             let bug_file_contents = String.concat ~sep:"\n" bug_file_lines in
             fun () ->
               init_git_bare_repository ~bot_info
               >>= fun () ->
               Github_installations.action_as_github_app ~bot_info ~key ~app_id
                 ~owner
                 (run_ci_minimization
                    ~comment_thread_id:(GitHub_ID.of_string comment_thread_id)
                    ~owner ~repo ~base ~pr_number ~head
                    ~minimizer_extra_arguments
                    ~ci_minimization_infos:
                      [ { target
                        ; ci_targets = [] (* dummmy value *)
                        ; opam_switch
                        ; failing_urls
                        ; passing_urls
                        ; docker_image
                        ; full_target= target (* dummy value *) } ]
                    ~bug_file:
                      (Some
                         (MinimizeScript
                            {quote_kind= ""; body= bug_file_contents} ) ) )
               >>= function
               | Ok ([], []) ->
                   Lwt_io.printlf
                     "Somehow no jobs were returned from minimization \
                      resumption?\n\
                      %s"
                     message
               | Ok (jobs_minimized, jobs_that_could_not_be_minimized) -> (
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
                            f "%s because %s" job reason )
                   with
                   | [] ->
                       Lwt.return_unit
                   | msgs ->
                       Lwt_io.printlf "Could not resume minimization of %s"
                         (msgs |> String.concat ~sep:", ") )
               | Error err ->
                   Lwt_io.printlf
                     "Internal error (should not happen because no url was \
                      passed):\n\
                      Could not resume minimization of %s for %s/%s#%s:\n\
                      %s"
                     target owner repo pr_number
                     (run_ci_minimization_error_to_string err) )
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
                   message )
              () )
    | _ ->
        Server.respond_string ~status:(`Code 400) ~body:"Bad request" ()
  else Server.respond_string ~status:(`Code 400) ~body:"Bad request" ()

let rec merge_pull_request_action ~bot_info ?(t = 1.) comment_info =
  let pr = comment_info.issue in
  let reasons_for_not_merging =
    List.filter_opt
      [ ( if String.equal comment_info.author pr.user then
            Some "You are the author."
          else if
            List.exists
              ~f:(String.equal comment_info.author)
              comment_info.issue.assignees
          then None
          else Some "You are not among the assignees." )
      ; comment_info.issue.labels
        |> List.find ~f:(fun label -> string_match ~regexp:"needs:.*" label)
        |> Option.map ~f:(fun l -> f "There is still a `%s` label." l)
      ; ( if
            comment_info.issue.labels
            |> List.exists ~f:(fun label ->
                   string_match ~regexp:"kind:.*" label )
          then None
          else Some "There is no `kind:` label." )
      ; ( if comment_info.issue.milestoned then None
          else Some "No milestone has been set." ) ]
  in
  ( match reasons_for_not_merging with
  | _ :: _ ->
      let bullet_reasons =
        reasons_for_not_merging |> List.map ~f:(fun x -> "- " ^ x)
      in
      let reasons = bullet_reasons |> String.concat ~sep:"\n" in
      Lwt.return_error
        (f "@%s: You cannot merge this PR because:\n%s" comment_info.author
           reasons )
  | [] -> (
      GitHub_queries.get_pull_request_reviews_refs ~bot_info
        ~owner:pr.issue.owner ~repo:pr.issue.repo ~number:pr.issue.number
      >>= function
      | Ok reviews_info -> (
          let comment =
            List.find reviews_info.last_comments ~f:(fun c ->
                GitHub_ID.equal comment_info.id c.id )
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
                 comment_info.author )
          else if not (String.equal reviews_info.baseRef "master") then
            Lwt.return_error
              (f
                 "@%s: This PR targets branch `%s` instead of `master`. Only \
                  release managers can merge in release branches. If you are \
                  the release manager for this branch, you should use the \
                  `dev/tools/merge-pr.sh` script to merge this PR. Merging \
                  with the bot is not supported yet."
                 comment_info.author reviews_info.baseRef )
          else
            match reviews_info.review_decision with
            | NONE | REVIEW_REQUIRED ->
                Lwt.return_error
                  (f
                     "@%s: You can't merge the PR because it hasn't been \
                      approved yet."
                     comment_info.author )
            | CHANGES_REQUESTED ->
                Lwt.return_error
                  (f
                     "@%s: You can't merge the PR because some changes are \
                      requested."
                     comment_info.author )
            | APPROVED -> (
                GitHub_queries.get_team_membership ~bot_info ~org:"coq"
                  ~team:"pushers" ~user:comment_info.author
                >>= function
                | Ok false ->
                    (* User not found in the team *)
                    Lwt.return_error
                      (f
                         "@%s: You can't merge this PR because you're not a \
                          member of the `@coq/pushers` team. Look at the \
                          contributing guide for how to join this team."
                         comment_info.author )
                | Ok true -> (
                    GitHub_mutations.merge_pull_request ~bot_info ~pr_id:pr.id
                      ~commit_headline:
                        (f "Merge PR #%d: %s" pr.issue.number
                           comment_info.issue.title )
                      ~commit_body:
                        ( List.fold_left reviews_info.approved_reviews ~init:""
                            ~f:(fun s r -> s ^ f "Reviewed-by: %s\n" r )
                        ^ List.fold_left reviews_info.comment_reviews ~init:""
                            ~f:(fun s r -> s ^ f "Ack-by: %s\n" r )
                        ^ f "Co-authored-by: %s <%s@users.noreply.github.com>\n"
                            comment_info.author comment_info.author )
                      ~merge_method:MERGE ()
                    >>= fun () ->
                    match
                      List.fold_left ~init:[] reviews_info.files
                        ~f:(fun acc f ->
                          if
                            string_match ~regexp:"dev/ci/user-overlays/\\(.*\\)"
                              f
                          then
                            let f = Str.matched_group 1 f in
                            if String.equal f "README.md" then acc else f :: acc
                          else acc )
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
                                    s ^ f "- %s\n" o ) ) )
                        >>= GitHub_mutations.report_on_posting_comment
                        >>= fun () -> Lwt.return_ok () )
                | Error e ->
                    Lwt.return_error
                      (f
                         "Something unexpected happened: %s\n\
                          cc @coq/coqbot-maintainers" e ) ) )
      | Error e ->
          Lwt.return_error
            (f "Something unexpected happened: %s\ncc @coq/coqbot-maintainers" e)
      ) )
  >>= function
  | Ok () ->
      Lwt.return_unit
  | Error err ->
      GitHub_mutations.post_comment ~bot_info ~message:err ~id:pr.id
      >>= GitHub_mutations.report_on_posting_comment

let add_remove_labels ~bot_info ~add (issue : issue_info) labels =
  let open Lwt.Syntax in
  let* labels =
    let open Lwt.Infix in
    labels
    |> Lwt_list.filter_map_p (fun label ->
           GitHub_queries.get_label ~bot_info ~owner:issue.issue.owner
             ~repo:issue.issue.repo ~label
           >|= function
           | Ok (Some label) ->
               Some label
           | Ok None ->
               (* Warn when a label is not found *)
               (fun () ->
                 Lwt_io.printlf
                   "Warning: Label %s not found in repository %s/%s." label
                   issue.issue.owner issue.issue.repo )
               |> Lwt.async ;
               None
           | Error err ->
               (* Print any other error, but do not prevent acting on other labels *)
               (fun () ->
                 Lwt_io.printlf
                   "Error while querying for label %s in repository %s/%s: %s"
                   label issue.issue.owner issue.issue.repo err )
               |> Lwt.async ;
               None )
  in
  match labels with
  | [] ->
      (* Nothing to do *)
      Lwt.return_unit
  | _ ->
      if add then GitHub_mutations.add_labels ~bot_info ~issue:issue.id ~labels
      else GitHub_mutations.remove_labels ~bot_info ~issue:issue.id ~labels

let add_labels_if_absent ~bot_info (issue : issue_info) labels =
  (* We construct the list of labels to add by filtering out the labels that
     are already present. *)
  (fun () ->
    List.filter labels ~f:(fun label ->
        not (List.mem issue.labels label ~equal:String.equal) )
    |> add_remove_labels ~bot_info ~add:true issue )
  |> Lwt.async

let remove_labels_if_present ~bot_info (issue : issue_info) labels =
  (* We construct the list of labels to remove by keeping only the labels that
     are present. *)
  (fun () ->
    List.filter labels ~f:(fun label ->
        List.mem issue.labels label ~equal:String.equal )
    |> add_remove_labels ~bot_info ~add:false issue )
  |> Lwt.async

(* TODO: ensure there's no race condition for 2 push with very close timestamps *)
let mirror_action ~bot_info ?(force = true) ~gitlab_domain ~owner ~repo
    ~base_ref ~head_sha () =
  (let open Lwt_result.Infix in
   let local_ref = base_ref ^ "-" ^ head_sha in
   let gh_ref =
     {repo_url= f "https://github.com/%s/%s" owner repo; name= base_ref}
   in
   (* TODO: generalize to use repository mappings, with enhanced security *)
   gitlab_repo ~bot_info ~gitlab_domain ~gitlab_full_name:(owner ^ "/" ^ repo)
   |> Lwt.return
   >>= fun gl_repo ->
   let gl_ref = {repo_url= gl_repo; name= base_ref} in
   git_fetch gh_ref local_ref |> execute_cmd
   >>= fun () -> git_push ~force ~remote_ref:gl_ref ~local_ref () |> execute_cmd
  )
  >>= function
  | Ok () ->
      Lwt.return_unit
  | Error e ->
      Lwt_io.printlf
        "Error while mirroring branch/tag %s of repository %s/%s: %s" base_ref
        owner repo e

(* TODO: ensure there's no race condition for 2 push with very close timestamps *)
let update_pr ?full_ci ?(skip_author_check = false) ~bot_info
    (pr_info : issue_info pull_request_info) ~gitlab_mapping ~github_mapping =
  let open Lwt_result.Infix in
  (* Try as much as possible to get unique refnames for local branches. *)
  let local_head_branch =
    f "head-%s-%s" pr_info.head.branch.name pr_info.head.sha
  in
  let local_base_branch =
    f "base-%s-%s" pr_info.base.branch.name pr_info.base.sha
  in
  git_fetch pr_info.base.branch ("refs/heads/" ^ local_base_branch)
  |&& git_fetch pr_info.head.branch ("refs/heads/" ^ local_head_branch)
  |> execute_cmd
  >>= (fun () ->
        git_make_ancestor ~pr_title:pr_info.issue.title
          ~pr_number:pr_info.issue.number ~base:local_base_branch
          local_head_branch )
  >>= fun ok ->
  let needs_full_ci_label = "needs: full CI" in
  let rebase_label = "needs: rebase" in
  let stale_label = "stale" in
  let open Lwt_result.Syntax in
  if ok then (
    (* Remove rebase / stale label *)
    remove_labels_if_present ~bot_info pr_info.issue [rebase_label; stale_label] ;
    (* In the Coq repo, we want to prevent untrusted contributors from
       circumventing the fact that the bench job is a manual job by changing
       the CI configuration. *)
    let* can_trigger_ci =
      if
        String.equal pr_info.issue.issue.owner "coq"
        && String.equal pr_info.issue.issue.repo "coq"
        && not skip_author_check
      then
        let* config_modified =
          git_test_modified ~base:pr_info.base.sha ~head:pr_info.head.sha
            ".*gitlab.*\\.yml"
        in
        if config_modified then (
          Lwt.async (fun () ->
              Lwt_io.printlf
                "CI configuration modified in PR coq/coq#%d, checking if %s is \
                 a member of @coq/contributors..."
                pr_info.issue.number pr_info.issue.user ) ;
          (* This is an approximation:
             we are checking who the PR author is and not who is pushing. *)
          GitHub_queries.get_team_membership ~bot_info ~org:"coq"
            ~team:"contributors" ~user:pr_info.issue.user )
        else Lwt.return_ok true
      else Lwt.return_ok true
    in
    let open Lwt.Infix in
    if not can_trigger_ci then (
      (* Since we cannot trigger CI, in particular, we still need to run a full CI *)
      add_labels_if_absent ~bot_info pr_info.issue [needs_full_ci_label] ;
      GitHub_mutations.post_comment ~bot_info ~id:pr_info.issue.id
        ~message:
          "I am not triggering a CI run on this PR because the CI \
           configuration has been modified. CI can be triggered manually by an \
           authorized contributor."
      >>= GitHub_mutations.report_on_posting_comment
      >>= fun () -> Lwt.return_ok () )
    else
      (* In Coq repo, we have several special cases:
         1. if something has changed in dev/ci/docker/, we rebuild the Docker image
         2. if there was a special label set, we run a full CI
      *)
      let get_options =
        if
          String.equal pr_info.issue.issue.owner "coq"
          && String.equal pr_info.issue.issue.repo "coq"
        then
          Lwt.all
            [ ( git_test_modified ~base:pr_info.base.sha ~head:pr_info.head.sha
                  "dev/ci/docker/.*Dockerfile.*"
              >>= function
              | Ok true ->
                  Lwt.return {|-o ci.variable="SKIP_DOCKER=false"|}
              | Ok false ->
                  Lwt.return ""
              | Error e ->
                  Lwt_io.printf
                    "Error while checking if something has changed in \
                     dev/ci/docker:\n\
                     %s\n"
                    e
                  >>= fun () -> Lwt.return "" )
            ; (let request_full_ci_label = "request: full CI" in
               match full_ci with
               | Some false ->
                   (* Light CI requested *)
                   add_labels_if_absent ~bot_info pr_info.issue
                     [needs_full_ci_label] ;
                   Lwt.return {| -o ci.variable="FULL_CI=false" |}
               | Some true ->
                   (* Full CI requested *)
                   remove_labels_if_present ~bot_info pr_info.issue
                     [needs_full_ci_label; request_full_ci_label] ;
                   Lwt.return {| -o ci.variable="FULL_CI=true" |}
               | None ->
                   (* Nothing requested with the command,
                      check if the request label is present *)
                   if
                     pr_info.issue.labels
                     |> List.exists ~f:(fun l ->
                            String.equal l request_full_ci_label )
                   then (
                     (* Full CI requested *)
                     remove_labels_if_present ~bot_info pr_info.issue
                       [needs_full_ci_label; request_full_ci_label] ;
                     Lwt.return {| -o ci.variable="FULL_CI=true" |} )
                   else (
                     (* Nothing requested *)
                     add_labels_if_absent ~bot_info pr_info.issue
                       [needs_full_ci_label] ;
                     Lwt.return {| -o ci.variable="FULL_CI=false" |} ) ) ]
          >|= fun options -> String.concat ~sep:" " options
        else Lwt.return ""
      in
      (* Force push *)
      get_options
      >>= fun options ->
      let open Lwt_result.Infix in
      gitlab_ref ~issue:pr_info.issue.issue ~gitlab_mapping ~github_mapping
        ~bot_info
      >>= fun remote_ref ->
      git_push ~force:true ~options ~remote_ref ~local_ref:local_head_branch ()
      |> execute_cmd )
  else (
    (* Add rebase label if it exists *)
    add_labels_if_absent ~bot_info pr_info.issue [rebase_label] ;
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
        let open Lwt.Syntax in
        GitHub_queries.get_repository_id ~bot_info
          ~owner:pr_info.issue.issue.owner ~repo:pr_info.issue.issue.repo
        >>= function
        | Ok repo_id ->
            (let+ _ =
               GitHub_mutations.create_check_run ~bot_info
                 ~name:"GitLab CI pipeline (pull request)" ~status:COMPLETED
                 ~repo_id ~head_sha:pr_info.head.sha ~conclusion:FAILURE
                 ~title:
                   "Pipeline did not run on GitLab CI because PR has conflicts \
                    with base branch."
                 ~details_url:"" ~summary:"" ()
             in
             () )
            |> Lwt_result.ok
        | Error e ->
            Lwt.return (Error e) ) )

let inform_user_not_in_contributors ~bot_info comment_info =
  GitHub_mutations.post_comment ~bot_info ~id:comment_info.issue.id
    ~message:
      (f
         "Sorry, @%s, I only accept requests from members of the \
          `@coq/contributor` team. If you are a regular contributor, you can \
          request to join the team by asking any core developer."
         comment_info.author )
  >>= GitHub_mutations.report_on_posting_comment

let run_ci_action ~bot_info ~comment_info ?full_ci ~gitlab_mapping
    ~github_mapping () =
  let team = "contributors" in
  (fun () ->
    (let open Lwt_result.Infix in
     GitHub_queries.get_team_membership ~bot_info ~org:"coq" ~team
       ~user:comment_info.author
     >>= (fun is_member ->
           if is_member then
             let open Lwt.Syntax in
             let* () = Lwt_io.printl "Authorized user: pushing to GitLab." in
             match comment_info.pull_request with
             | Some pr_info ->
                 update_pr ~skip_author_check:true pr_info ~bot_info
                   ~gitlab_mapping ~github_mapping
             | None ->
                 let {owner; repo; number} = comment_info.issue.issue in
                 GitHub_queries.get_pull_request_refs ~bot_info ~owner ~repo
                   ~number
                 >>= fun pr_info ->
                 update_pr ?full_ci ~skip_author_check:true
                   {pr_info with issue= comment_info.issue}
                   ~bot_info ~gitlab_mapping ~github_mapping
           else
             (* We inform the author of the request that they are not authorized. *)
             inform_user_not_in_contributors ~bot_info comment_info
             |> Lwt_result.ok )
     |> Fn.flip Lwt_result.bind_lwt_error (fun err ->
            Lwt_io.printf "Error: %s\n" err ) )
    >>= fun _ -> Lwt.return_unit )
  |> Lwt.async ;
  Server.respond_string ~status:`OK
    ~body:
      (f
         "Received a request to run CI: checking that @%s is a member of \
          @%s/%s before doing so."
         comment_info.author comment_info.issue.issue.owner team )
    ()

let pull_request_closed_action ~bot_info
    (pr_info : GitHub_types.issue_info GitHub_types.pull_request_info)
    ~gitlab_mapping ~github_mapping =
  let open Lwt.Infix in
  gitlab_ref ~issue:pr_info.issue.issue ~gitlab_mapping ~github_mapping
    ~bot_info
  >>= (function
        | Ok remote_ref ->
            git_delete ~remote_ref |> execute_cmd >|= ignore
        | Error err ->
            Lwt_io.printlf "Error: %s" err )
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
    ~gitlab_mapping ~github_mapping =
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
               pr_info.base.branch.name )
        >>= GitHub_mutations.report_on_posting_comment )
      |> Lwt.async
  | _ ->
      () ) ;
  (fun () ->
    update_pr pr_info ~bot_info ~gitlab_mapping ~github_mapping
    >>= fun _ -> Lwt.return_unit )
  |> Lwt.async ;
  Server.respond_string ~status:`OK
    ~body:
      (f
         "Pull request %s/%s#%d was (re)opened / synchronized: (force-)pushing \
          to GitLab."
         pr_info.issue.issue.owner pr_info.issue.issue.repo
         pr_info.issue.issue.number )
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

let project_action ~bot_info ~pr_id ~backport_to () =
  GitHub_queries.get_pull_request_milestone ~bot_info ~pr_id
  >>= function
  | Error err ->
      Lwt_io.printf "Error: %s\n" err
  | Ok backport_info -> (
    match
      List.find_map backport_info
        ~f:(fun {backport_to= backport_to'; rejected_milestone} ->
          if String.equal backport_to backport_to' then Some rejected_milestone
          else None )
    with
    | None ->
        Lwt_io.printf
          "PR already not in milestone with backporting info for branch %s.\n"
          backport_to
    | Some rejected_milestone -> (
        Lwt_io.printf
          "PR is in milestone for which backporting to %s was rejected.\n\
           Change of milestone requested.\n"
          backport_to
        >>= fun () ->
        GitHub_queries.get_milestone_id ~bot_info ~owner:"coq" ~repo:"coq"
          ~number:rejected_milestone
        >>= function
        | Ok milestone ->
            GitHub_mutations.update_milestone ~bot_info ~issue:pr_id ~milestone
            <&> ( GitHub_mutations.post_comment ~bot_info ~id:pr_id
                    ~message:
                      "This PR was postponed. Please update accordingly the \
                       milestone of any issue that this fixes as this cannot \
                       be done automatically."
                >>= GitHub_mutations.report_on_posting_comment )
        | Error err ->
            Lwt_io.printlf "Error while obtaining milestone ID: %s" err ) )

let add_to_column ~bot_info ~backport_to id option =
  let field = backport_to ^ " status" in
  GitHub_queries.get_project_field_values ~bot_info ~organization:"coq"
    ~project:11 ~field ~options:[|option|]
  >>= fun project_info ->
  ( match project_info with
  | Ok (project_id, Some (field_id, [(option', field_value_id)]))
    when String.equal option option' ->
      Lwt.return_ok (project_id, field_id, field_value_id)
  | Ok (_, Some (_, [])) ->
      Lwt.return_error
        (f "Error: Could not find '%s' option in the field." option)
  | Ok (_, Some _) ->
      Lwt.return_error
        (f "Error: Unexpected result when looking for '%s'." option)
  | Ok (project_id, None) -> (
      Lwt_io.printlf
        "Required backporting field '%s' does not exist yet. Creating it..."
        field
      >>= fun () ->
      GitHub_mutations.create_new_release_management_field ~bot_info ~project_id
        ~field
      >>= function
      | Ok (field_id, options) -> (
        match
          List.find_map options ~f:(fun (option', field_value_id) ->
              if String.equal option option' then Some field_value_id else None )
        with
        | Some field_value_id ->
            Lwt.return_ok (project_id, field_id, field_value_id)
        | None ->
            Lwt.return_error
              (f
                 "Error new field '%s status' was created, but does not have a \
                  '%s' option."
                 field option ) )
      | Error err ->
          Lwt.return_error
            (f "Error while creating new backporting field '%s': %s" field err)
      )
  | Error err ->
      Lwt.return_error (f "Error while getting project field values: %s" err) )
  >>= function
  | Ok (project_id, field_id, field_value_id) -> (
      ( match id with
      | `PR_ID card_id ->
          GitHub_mutations.add_card_to_project ~bot_info ~card_id ~project_id
      | `Card_ID card_id ->
          Lwt.return_ok card_id )
      >>= fun result ->
      match result with
      | Ok card_id ->
          GitHub_mutations.update_field_value ~bot_info ~card_id ~project_id
            ~field_id ~field_value_id
      | Error err ->
          Lwt_io.printf "Error while adding card to project: %s\n" err )
  | Error err ->
      Lwt_io.printl err

let coq_push_action ~bot_info ~base_ref ~commits_msg =
  let* () = Lwt_io.printl "Merge and backport commit messages:" in
  let commit_action commit_msg =
    if
      string_match ~regexp:"^Merge \\(PR\\|pull request\\) #\\([0-9]*\\)"
        commit_msg
    then
      let pr_number = Str.matched_group 2 commit_msg |> Int.of_string in
      Lwt_io.printf "%s\nPR #%d was merged.\n" commit_msg pr_number
      >>= fun () ->
      GitHub_queries.get_pull_request_id_and_milestone ~bot_info ~owner:"coq"
        ~repo:"coq" ~number:pr_number
      >>= fun pr_info ->
      match pr_info with
      | Ok (pr_id, backport_info) ->
          backport_info
          |> Lwt_list.iter_p (fun {backport_to} ->
                 if "refs/heads/" ^ backport_to |> String.equal base_ref then
                   Lwt_io.printf
                     "PR was merged into the backporting branch directly.\n"
                   >>= fun () ->
                   add_to_column ~bot_info ~backport_to (`PR_ID pr_id) "Shipped"
                 else if String.equal base_ref "refs/heads/master" then
                   (* For now, we hard code that PRs are only backported
                      from master.  In the future, we could make this
                      configurable in the milestone description or in
                      some configuration file. *)
                   Lwt_io.printf "Backporting to %s was requested.\n"
                     backport_to
                   >>= fun () ->
                   add_to_column ~bot_info ~backport_to (`PR_ID pr_id)
                     "Request inclusion"
                 else
                   Lwt_io.printf
                     "PR was merged into a branch that is not the backporting \
                      branch nor the master branch.\n" )
      | Error err ->
          Lwt_io.printf "Error: %s\n" err
    else if string_match ~regexp:"^Backport PR #\\([0-9]*\\):" commit_msg then
      let pr_number = Str.matched_group 1 commit_msg |> Int.of_string in
      Lwt_io.printf "%s\nPR #%d was backported.\n" commit_msg pr_number
      >>= fun () ->
      GitHub_queries.get_pull_request_cards ~bot_info ~owner:"coq" ~repo:"coq"
        ~number:pr_number
      >>= function
      | Ok items -> (
          let backport_to =
            String.chop_prefix_if_exists ~prefix:"refs/heads/" base_ref
          in
          let card_id =
            items |> List.find_map ~f:(function id, 11 -> Some id | _ -> None)
          in
          match card_id with
          | Some card_id ->
              Lwt_io.printlf
                "Pull request coq/coq#%d found in project 11. Updating its \
                 fields."
                pr_number
              >>= fun () ->
              add_to_column ~bot_info ~backport_to (`Card_ID card_id) "Shipped"
          | None ->
              (* We could do something in this case, like post a comment to
                 the PR and add the PR to the project. *)
              Lwt_io.printlf "Pull request coq/coq#%d not found in project 11."
                pr_number )
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
                    (f {|Anomaly: Label "%s" absent from timeline of PR #%i|}
                       label pr_number )
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
                     rebase_label warn_after close_after )
                ~bot_info
              >>= GitHub_mutations.report_on_posting_comment
              >>= fun () ->
              GitHub_mutations.add_labels ~bot_info ~labels:[stale_id]
                ~issue:pr_id
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
           after )
      ~bot_info
    >>= GitHub_mutations.report_on_posting_comment
    >>= fun () ->
    GitHub_mutations.close_pull_request ~bot_info ~pr_id
    >>= fun () -> Lwt.return true
  in
  apply_after_label ~bot_info ~owner ~repo ~after ~label ~action ~throttle ()

let run_bench ~bot_info ?key_value_pairs comment_info =
  (* Do we want to use this more often? *)
  let open Lwt.Syntax in
  let pr = comment_info.issue in
  let owner = pr.issue.owner in
  let repo = pr.issue.repo in
  let pr_number = pr.number in
  (* We need the GitLab build_id and project_id. Currently there is no good way
     to query this data so we have to jump through some somewhat useless hoops in
     order to get our hands on this information. TODO: do this more directly.*)
  let* gitlab_check_summary =
    GitHub_queries.get_pull_request_refs ~bot_info ~owner ~repo
      ~number:pr_number
    >>= function
    | Error err ->
        Lwt.return_error
          (f
             "Error while fetching PR refs for %s/%s#%d for running bench job: \
              %s"
             owner repo pr_number err )
    | Ok {base= _; head= {sha= head}} ->
        let head = Str.global_replace (Str.regexp {|"|}) "" head in
        GitHub_queries.get_pipeline_summary ~bot_info ~owner ~repo ~head
  in
  (* Parsing the summary into (build_id, project_id) *)
  let* process_summary =
    match gitlab_check_summary with
    | Error err ->
        Lwt.return_error err
    | Ok summary -> (
      try
        let build_id =
          let regexp =
            f {|.*%s\([0-9]*\)|}
              (Str.quote "[bench](https://gitlab.inria.fr/coq/coq/-/jobs/")
          in
          ( if Helpers.string_match ~regexp summary then
              Str.matched_group 1 summary
            else raise @@ Stdlib.Failure "Could not find GitLab bench job ID" )
          |> Stdlib.int_of_string
        in
        let project_id =
          let regexp = {|.*GitLab Project ID: \([0-9]*\)|} in
          ( if Helpers.string_match ~regexp summary then
              Str.matched_group 1 summary
            else raise @@ Stdlib.Failure "Could not find GitLab Project ID" )
          |> Int.of_string
        in
        Lwt.return_ok (build_id, project_id)
      with Stdlib.Failure s ->
        Lwt.return_error
          (f
             "Error while regexing summary for %s/%s#%d for running bench job: \
              %s"
             owner repo pr_number s ) )
  in
  let* allowed_to_bench =
    GitHub_queries.get_team_membership ~bot_info ~org:"coq" ~team:"contributors"
      ~user:comment_info.author
  in
  match (allowed_to_bench, process_summary) with
  | Ok true, Ok (build_id, project_id) ->
      (* Permission to bench has been granted *)
      GitLab_mutations.play_job ~bot_info ~gitlab_domain:"gitlab.inria.fr"
        ~project_id ~build_id ?key_value_pairs ()
  | Error err, _ | _, Error err ->
      GitHub_mutations.post_comment ~bot_info ~message:err ~id:pr.id
      >>= GitHub_mutations.report_on_posting_comment
  | Ok false, _ ->
      (* User not found in the team *)
      inform_user_not_in_contributors ~bot_info comment_info
