open Base
open Cohttp
open Cohttp_lwt_unix
open Lwt.Infix
open Bot_components
open Botlib
open Actions
open Git_utils
open Github_installations
open Helpers

let toml_data = Config.toml_of_file (Sys.get_argv ()).(1)

let port = Config.port toml_data

let github_access_token = Config.github_access_token toml_data

let github_webhook_secret = Config.github_webhook_secret toml_data

(* TODO: make webhook secret project-specific *)
let gitlab_webhook_secret = Config.gitlab_webhook_secret toml_data

let daily_schedule_secret = Config.daily_schedule_secret toml_data

let github_bot_name = Config.github_bot_name toml_data

let key = Config.github_private_key ()

let app_id = Config.github_app_id toml_data

let bot_info : Bot_components.Bot_info.t =
  { github_pat= github_access_token
  ; github_install_token= None
  ; gitlab_instances= Config.gitlab_instances toml_data
  ; github_name= github_bot_name
  ; email= Config.bot_email toml_data
  ; domain= Config.bot_domain toml_data
  ; app_id }

let github_mapping, gitlab_mapping = Config.make_mappings_table toml_data

let string_of_installation_tokens =
  Hashtbl.fold ~init:"" ~f:(fun ~key ~data acc ->
      acc ^ f "Owner: %s, token: %s,  expire at: %f\n" key (fst data) (snd data) )

(* TODO: deprecate unsigned webhooks *)

let callback _conn req body =
  let ( coqbot_minimize_text_of_body
      , coqbot_ci_minimize_text_of_body
      , coqbot_resume_ci_minimize_text_of_body ) =
    let extract_minimize_file body =
      body
      |> Str.split (Str.regexp_string "\n```")
      |> List.hd |> Option.value ~default:""
    in
    let extract_minimize_script quote_kind body =
      MinimizeScript
        { quote_kind= quote_kind |> Str.global_replace (Str.regexp "[ \r]") ""
        ; body= body |> extract_minimize_file }
    in
    let extract_minimize_url url =
      url |> Str.global_replace (Str.regexp "^[` ]+\\|[` ]+$") ""
    in
    let extract_minimize_attachment ?(description = "") url =
      MinimizeAttachment {description; url= url |> extract_minimize_url}
    in
    let parse_minimiation_requests requests =
      requests
      |> Str.global_replace (Str.regexp "[ ,]+") " "
      |> String.split ~on:' '
      |> List.map ~f:Stdlib.String.trim
      (* remove trailing : in case the user stuck a : at the end of the line *)
      |> List.map ~f:(Str.global_replace (Str.regexp ":$") "")
      |> List.filter ~f:(fun r -> not (String.is_empty r))
    in
    let coqbot_minimize_text_of_body body =
      if
        string_match
          ~regexp:
            ( f
                "@%s:? [Mm]inimize\\([^`]*\\)```\\([^\n\
                 ]*\\)\n\
                 \\(\\(.\\|\n\
                 \\)+\\)"
            @@ Str.quote github_bot_name )
          body
      then
        (* avoid internal server errors from unclear execution order *)
        let options, quote_kind, body =
          ( Str.matched_group 1 body
          , Str.matched_group 2 body
          , Str.matched_group 3 body )
        in
        Some (options, extract_minimize_script quote_kind body)
      else if
        string_match
          ~regexp:
            ( f "@%s? [Mm]inimize\\([^`]*\\)\\[\\([^]]*\\)\\] *(\\([^)]*\\))"
            @@ Str.quote github_bot_name )
          body
      then
        (* avoid internal server errors from unclear execution order *)
        let options, description, url =
          ( Str.matched_group 1 body
          , Str.matched_group 2 body
          , Str.matched_group 3 body )
        in
        Some (options, extract_minimize_attachment ~description url)
      else None
    in
    let coqbot_ci_minimize_text_of_body body =
      if
        string_match
          ~regexp:
            ( f "@%s:?\\( [^\n]*\\)\\b[Cc][Ii][- ][Mm]inimize:?\\([^\n]*\\)"
            @@ Str.quote github_bot_name )
          body
      then
        let options, requests =
          (Str.matched_group 1 body, Str.matched_group 2 body)
        in
        Some (options, requests |> parse_minimiation_requests)
      else None
    in
    let coqbot_resume_ci_minimize_text_of_body body =
      if
        string_match
          ~regexp:
            ( f
                "@%s:?\\( [^\n\
                 ]*\\)\\bresume [Cc][Ii][- ][Mm]inimiz\\(e\\|ation\\):?\\([^\n\
                 ]*\\)\n\
                 +```\\([^\n\
                 ]*\\)\n\
                 \\(\\(.\\|\n\
                 \\)+\\)"
            @@ Str.quote github_bot_name )
          body
      then
        let options, requests, quote_kind, body =
          ( Str.matched_group 1 body
          , Str.matched_group 3 body
          , Str.matched_group 4 body
          , Str.matched_group 5 body )
        in
        Some
          ( options
          , requests |> parse_minimiation_requests
          , extract_minimize_script quote_kind body )
      else if
        string_match
          ~regexp:
            ( f
                "@%s:?\\( [^\n\
                 ]*\\)\\bresume [Cc][Ii][- ][Mm]inimiz\\(e\\|ation\\):?[ \n\
                 ]+\\([^ \n\
                 ]+\\)[ \n\
                 ]+\\[\\([^]]*\\)\\] *(\\([^)]*\\))"
            @@ Str.quote github_bot_name )
          body
      then
        let options, requests, description, url =
          ( Str.matched_group 1 body
          , Str.matched_group 3 body
          , Str.matched_group 4 body
          , Str.matched_group 5 body )
        in
        Some
          ( options
          , requests |> parse_minimiation_requests
          , extract_minimize_attachment ~description url )
      else if
        string_match
          ~regexp:
            ( f
                "@%s:?\\( [^\n\
                 ]*\\)\\bresume [Cc][Ii][- ][Mm]inimiz\\(e\\|ation\\):?[ \n\
                 ]+\\([^ \n\
                 ]+\\)[ \n\
                 ]+\\(https?://[^ \n\
                 ]+\\)"
            @@ Str.quote github_bot_name )
          body
      then
        let options, requests, url =
          ( Str.matched_group 1 body
          , Str.matched_group 3 body
          , Str.matched_group 4 body )
        in
        Some
          ( options
          , requests |> parse_minimiation_requests
          , extract_minimize_attachment url )
      else None
    in
    ( coqbot_minimize_text_of_body
    , coqbot_ci_minimize_text_of_body
    , coqbot_resume_ci_minimize_text_of_body )
  in
  let body = Cohttp_lwt.Body.to_string body in
  (* print_endline "Request received."; *)
  match Uri.path (Request.uri req) with
  | "/job" | "/pipeline" (* legacy endpoints *) | "/gitlab" -> (
      body
      >>= fun body ->
      match
        GitLab_subscriptions.receive_gitlab ~secret:gitlab_webhook_secret
          (Request.headers req) body
      with
      | Ok (_, JobEvent ({common_info= {http_repo_url}} as job_info)) -> (
        match github_repo_of_gitlab_url ~gitlab_mapping ~http_repo_url with
        | Error error_msg ->
            (fun () -> Lwt_io.printl error_msg) |> Lwt.async ;
            Server.respond_string ~status:`Bad_request ~body:error_msg ()
        | Ok (owner, _) ->
            (fun () ->
              action_as_github_app ~bot_info ~key ~app_id ~owner
                (job_action ~gitlab_mapping job_info) )
            |> Lwt.async ;
            Server.respond_string ~status:`OK ~body:"Job event." () )
      | Ok (_, PipelineEvent ({common_info= {http_repo_url}} as pipeline_info))
        -> (
        match github_repo_of_gitlab_url ~gitlab_mapping ~http_repo_url with
        | Error error_msg ->
            (fun () -> Lwt_io.printl error_msg) |> Lwt.async ;
            Server.respond_string ~status:`Bad_request ~body:error_msg ()
        | Ok (owner, _) ->
            (fun () ->
              action_as_github_app ~bot_info ~key ~app_id ~owner
                (pipeline_action ~gitlab_mapping pipeline_info) )
            |> Lwt.async ;
            Server.respond_string ~status:`OK ~body:"Pipeline event." () )
      | Ok (_, UnsupportedEvent e) ->
          Server.respond_string ~status:`OK
            ~body:(f "Unsupported event %s." e)
            ()
      | Error ("Webhook password mismatch." as e) ->
          (fun () -> Lwt_io.printl e) |> Lwt.async ;
          Server.respond_string ~status:(Code.status_of_code 401)
            ~body:(f "Error: %s" e) ()
      | Error e ->
          Server.respond_string ~status:(Code.status_of_code 400)
            ~body:(f "Error: %s" e) () )
  | "/push" | "/pull_request" (* legacy endpoints *) | "/github" -> (
      body
      >>= fun body ->
      match
        GitHub_subscriptions.receive_github ~secret:github_webhook_secret
          (Request.headers req) body
      with
      | Ok
          ( Some install_id
          , PushEvent
              {owner= "rocq-prover"; repo= "rocq"; base_ref; head_sha; commits_msg} ) ->
          (fun () ->
            init_git_bare_repository ~bot_info
            >>= fun () ->
            action_as_github_app_from_install_id ~bot_info ~key ~app_id
              ~install_id
              (rocq_push_action ~base_ref ~commits_msg)
            <&> action_as_github_app_from_install_id ~bot_info ~key ~app_id
                  ~install_id
                  (mirror_action ~gitlab_domain:"gitlab.inria.fr" ~owner:"rocq-prover"
                     ~repo:"rocq" ~base_ref ~head_sha () ) )
          |> Lwt.async ;
          Server.respond_string ~status:`OK
            ~body:
              "Processing push event on the Rocq Prover repository: analyzing merge / \
               backporting info."
            ()
      | Ok (Some install_id, PushEvent {owner; repo; base_ref; head_sha; _})
        -> (
        match (owner, repo) with
        | "rocq-community", ("docker-base" | "docker-coq" | "docker-rocq") ->
            (fun () ->
              init_git_bare_repository ~bot_info
              >>= fun () ->
              action_as_github_app_from_install_id ~bot_info ~key ~app_id
                ~install_id
                (mirror_action ~gitlab_domain:"gitlab.com" ~owner ~repo
                   ~base_ref ~head_sha () ) )
            |> Lwt.async ;
            Server.respond_string ~status:`OK
              ~body:
                (f
                   "Processing push event on %s/%s repository: mirroring \
                    branch on GitLab."
                   owner repo )
              ()
        | "math-comp", ("docker-mathcomp" | "math-comp") | "rocq-prover", "opam" ->
            (fun () ->
              init_git_bare_repository ~bot_info
              >>= fun () ->
              action_as_github_app_from_install_id ~bot_info ~key ~app_id
                ~install_id
                (mirror_action ~gitlab_domain:"gitlab.inria.fr" ~owner ~repo
                   ~base_ref ~head_sha () ) )
            |> Lwt.async ;
            Server.respond_string ~status:`OK
              ~body:
                (f
                   "Processing push event on %s/%s repository: mirroring \
                    branch on GitLab."
                   owner repo )
              ()
        | _ ->
            Server.respond_string ~status:`OK ~body:"Ignoring push event." () )
      | Ok (_, PullRequestUpdated (PullRequestClosed, pr_info)) ->
          (fun () ->
            init_git_bare_repository ~bot_info
            >>= fun () ->
            action_as_github_app ~bot_info ~key ~app_id
              ~owner:pr_info.issue.issue.owner
              (pull_request_closed_action ~gitlab_mapping ~github_mapping
                 pr_info ) )
          |> Lwt.async ;
          Server.respond_string ~status:`OK
            ~body:
              (f
                 "Pull request %s/%s#%d was closed: removing the branch from \
                  GitLab."
                 pr_info.issue.issue.owner pr_info.issue.issue.repo
                 pr_info.issue.issue.number )
            ()
      | Ok (_, PullRequestUpdated (action, pr_info)) ->
          init_git_bare_repository ~bot_info
          >>= fun () ->
          action_as_github_app ~bot_info ~key ~app_id
            ~owner:pr_info.issue.issue.owner
            (pull_request_updated_action ~action ~pr_info ~gitlab_mapping
               ~github_mapping )
      | Ok (_, IssueClosed {issue}) ->
          (* TODO: only for projects that requested this feature *)
          (fun () ->
            action_as_github_app ~bot_info ~key ~app_id ~owner:issue.owner
              (adjust_milestone ~issue ~sleep_time:5.) )
          |> Lwt.async ;
          Server.respond_string ~status:`OK
            ~body:
              (f "Issue %s/%s#%d was closed: checking its milestone."
                 issue.owner issue.repo issue.number )
            ()
      | Ok
          ( Some (1062161 as install_id) (* Rocq's installation number *)
          , PullRequestCardEdited
              { project_number= 11 (* Rocq's backporting project number *)
              ; pr_id
              ; field
              ; old_value= "Request inclusion"
              ; new_value= "Rejected" } )
        when String.is_suffix ~suffix:" status" field ->
          let backport_to = String.drop_suffix field 7 in
          (fun () ->
            action_as_github_app_from_install_id ~bot_info ~key ~app_id
              ~install_id
              (project_action ~pr_id ~backport_to ()) )
          |> Lwt.async ;
          Server.respond_string ~status:`OK
            ~body:
              (f
                 "PR proposed for backporting was rejected from inclusion in \
                  %s. Updating the milestone."
                 backport_to )
            ()
      | Ok (_, PullRequestCardEdited _) ->
          Server.respond_string ~status:`OK
            ~body:"Unsupported pull request card edition." ()
      | Ok (_, IssueOpened ({body= Some body} as issue_info)) -> (
          let body =
            body |> trim_comments |> strip_quoted_bot_name ~github_bot_name
          in
          match coqbot_minimize_text_of_body body with
          | Some (options, script) ->
              (fun () ->
                init_git_bare_repository ~bot_info
                >>= fun () ->
                action_as_github_app ~bot_info ~key ~app_id
                  ~owner:issue_info.issue.owner
                  (run_coq_minimizer ~script ~comment_thread_id:issue_info.id
                     ~comment_author:issue_info.user
                     ~owner:issue_info.issue.owner ~repo:issue_info.issue.repo
                     ~options ) )
              |> Lwt.async ;
              Server.respond_string ~status:`OK ~body:"Handling minimization."
                ()
          | None ->
              Server.respond_string ~status:`OK
                ~body:(f "Unhandled new issue: %s" body)
                () )
      | Ok (install_id, CommentCreated comment_info) -> (
          let body =
            comment_info.body |> trim_comments
            |> strip_quoted_bot_name ~github_bot_name
          in
          match coqbot_minimize_text_of_body body with
          | Some (options, script) ->
              (fun () ->
                init_git_bare_repository ~bot_info
                >>= fun () ->
                action_as_github_app ~bot_info ~key ~app_id
                  ~owner:comment_info.issue.issue.owner
                  (run_coq_minimizer ~script
                     ~comment_thread_id:comment_info.issue.id
                     ~comment_author:comment_info.author
                     ~owner:comment_info.issue.issue.owner
                     ~repo:comment_info.issue.issue.repo ~options ) )
              |> Lwt.async ;
              Server.respond_string ~status:`OK ~body:"Handling minimization."
                ()
          | None -> (
            (* Since both ci minimization resumption and ci
               minimization will match the resumption string, and we
               don't want to parse "resume" as an option, we test
               resumption first *)
            match coqbot_resume_ci_minimize_text_of_body body with
            | Some (options, requests, bug_file) ->
                (fun () ->
                  init_git_bare_repository ~bot_info
                  >>= fun () ->
                  action_as_github_app ~bot_info ~key ~app_id
                    ~owner:comment_info.issue.issue.owner
                    (ci_minimize ~comment_info ~requests ~comment_on_error:true
                       ~options ~bug_file:(Some bug_file) ) )
                |> Lwt.async ;
                Server.respond_string ~status:`OK
                  ~body:"Handling CI minimization resumption." ()
            | None -> (
              match coqbot_ci_minimize_text_of_body body with
              | Some (options, requests) ->
                  (fun () ->
                    init_git_bare_repository ~bot_info
                    >>= fun () ->
                    action_as_github_app ~bot_info ~key ~app_id
                      ~owner:comment_info.issue.issue.owner
                      (ci_minimize ~comment_info ~requests
                         ~comment_on_error:true ~options ~bug_file:None ) )
                  |> Lwt.async ;
                  Server.respond_string ~status:`OK
                    ~body:"Handling CI minimization." ()
              | None ->
                  if
                    string_match
                      ~regexp:
                        ( f "@%s:? [Rr]un \\(full\\|light\\|\\) ?[Cc][Ii]"
                        @@ Str.quote github_bot_name )
                      body
                    && comment_info.issue.pull_request
                    && String.equal comment_info.issue.issue.owner "rocq-prover"
                    && String.equal comment_info.issue.issue.repo "rocq"
                    && Option.is_some install_id
                  then
                    let full_ci =
                      match Str.matched_group 1 body with
                      | "full" ->
                          Some true
                      | "light" ->
                          Some false
                      | "" ->
                          None
                      | _ ->
                          failwith "Impossible group value."
                    in
                    init_git_bare_repository ~bot_info
                    >>= fun () ->
                    action_as_github_app ~bot_info ~key ~app_id
                      ~owner:comment_info.issue.issue.owner
                      (run_ci_action ~comment_info ?full_ci ~gitlab_mapping
                         ~github_mapping () )
                  else if
                    string_match
                      ~regexp:
                        (f "@%s:? [Mm]erge now" @@ Str.quote github_bot_name)
                      body
                    && comment_info.issue.pull_request
                    && String.equal comment_info.issue.issue.owner "rocq-prover"
                    && String.equal comment_info.issue.issue.repo "rocq"
                    && Option.is_some install_id
                  then (
                    (fun () ->
                      action_as_github_app ~bot_info ~key ~app_id
                        ~owner:comment_info.issue.issue.owner
                        (merge_pull_request_action comment_info) )
                    |> Lwt.async ;
                    Server.respond_string ~status:`OK
                      ~body:(f "Received a request to merge the PR.")
                      () )
                  else if
                    string_match
                      ~regexp:
                        (f "@%s:? [Bb]ench native" @@ Str.quote github_bot_name)
                      body
                    && comment_info.issue.pull_request
                    && String.equal comment_info.issue.issue.owner "rocq-prover"
                    && String.equal comment_info.issue.issue.repo "rocq"
                    && Option.is_some install_id
                  then (
                    (fun () ->
                      action_as_github_app ~bot_info ~key ~app_id
                        ~owner:comment_info.issue.issue.owner
                        (run_bench
                           ~key_value_pairs:[("coq_native", "yes")]
                           comment_info ) )
                    |> Lwt.async ;
                    Server.respond_string ~status:`OK
                      ~body:(f "Received a request to start the bench.")
                      () )
                  else if
                    string_match
                      ~regexp:(f "@%s:? [Bb]ench" @@ Str.quote github_bot_name)
                      body
                    && comment_info.issue.pull_request
                    && String.equal comment_info.issue.issue.owner "rocq-prover"
                    && String.equal comment_info.issue.issue.repo "rocq"
                    && Option.is_some install_id
                  then (
                    (fun () ->
                      action_as_github_app ~bot_info ~key ~app_id
                        ~owner:comment_info.issue.issue.owner
                        (run_bench comment_info) )
                    |> Lwt.async ;
                    Server.respond_string ~status:`OK
                      ~body:(f "Received a request to start the bench.")
                      () )
                  else
                    Server.respond_string ~status:`OK
                      ~body:(f "Unhandled comment: %s" body)
                      () ) ) )
      | Ok (None, CheckRunReRequested _) ->
          Server.respond_string ~status:(Code.status_of_code 401)
            ~body:"Request to rerun check run must be signed." ()
      | Ok (Some _, CheckRunReRequested {external_id}) -> (
          if String.is_empty external_id then
            Server.respond_string ~status:(Code.status_of_code 400)
              ~body:"Request to rerun check run but empty external ID." ()
          else
            let external_id_parsed =
              match String.split ~on:',' external_id with
              | [http_repo_url; url_part] -> (
                match Helpers.parse_gitlab_repo_url ~http_repo_url with
                | Error _ ->
                    None
                | Ok (gitlab_domain, _) ->
                    Some (gitlab_domain, url_part) )
              | [url_part] ->
                  (* Backward compatibility *)
                  Some ("gitlab.com", url_part)
              | _ ->
                  None
            in
            match external_id_parsed with
            | None ->
                Server.respond_string ~status:(Code.status_of_code 400)
                  ~body:
                    (f
                       "Request to rerun check run but external ID is not \
                        well-formed: %s"
                       external_id )
                  ()
            | Some (gitlab_domain, url_part) ->
                (fun () ->
                  GitLab_mutations.generic_retry ~bot_info ~gitlab_domain
                    ~url_part )
                |> Lwt.async ;
                Server.respond_string ~status:`OK
                  ~body:
                    (f
                       "Received a request to re-run a job / pipeline \
                        (External ID : %s)."
                       external_id )
                  () )
      | Ok (_, UnsupportedEvent s) ->
          Server.respond_string ~status:`OK ~body:(f "No action taken: %s" s) ()
      | Ok _ ->
          Server.respond_string ~status:`OK
            ~body:"No action taken: event or action is not yet supported." ()
      | Error ("Webhook signed but with wrong signature." as e) ->
          (fun () -> Lwt_io.printl e) |> Lwt.async ;
          Server.respond_string ~status:(Code.status_of_code 401)
            ~body:(f "Error: %s" e) ()
      | Error e ->
          Server.respond_string ~status:(Code.status_of_code 400)
            ~body:(f "Error: %s" e) () )
  | "/coq-bug-minimizer" ->
      body
      >>= fun body ->
      coq_bug_minimizer_results_action ~ci:false body ~bot_info ~key ~app_id
  | "/ci-minimization" ->
      body
      >>= fun body ->
      coq_bug_minimizer_results_action ~ci:true body ~bot_info ~key ~app_id
  | "/resume-ci-minimization" ->
      body
      >>= fun body ->
      coq_bug_minimizer_resume_ci_minimization_action body ~bot_info ~key
        ~app_id
  | "/check-stale-pr" -> (
      body
      >>= fun body ->
      match String.split ~on:':' body with
      | [owner; repo; secret] ->
          if String.equal secret daily_schedule_secret then (
            let warn_after = 30 in
            let close_after = 30 in
            (fun () ->
              action_as_github_app ~bot_info ~key ~app_id ~owner
                (rocq_check_needs_rebase_pr ~owner ~repo ~warn_after ~close_after
                   ~throttle:6 )
              >>= fun () ->
              action_as_github_app ~bot_info ~key ~app_id ~owner
                (rocq_check_stale_pr ~owner ~repo ~after:close_after ~throttle:4)
              )
            |> Lwt.async ;
            Server.respond_string ~status:`OK
              ~body:"Stale pull requests updated" () )
          else
            Server.respond_error ~status:`Unauthorized ~body:"Incorrect secret"
              ()
      | _ ->
          Server.respond_string ~status:(Code.status_of_code 400)
            ~body:(f "Error: ill-formed request")
            () )
  | _ ->
      Server.respond_not_found ()

let launch =
  let mode = `TCP (`Port port) in
  Server.create ~mode (Server.make ~callback ())

let () =
  Lwt.async_exception_hook :=
    fun exn ->
      (fun () ->
        Lwt_io.printlf "Error: Unhandled exception: %s" (Exn.to_string exn) )
      |> Lwt.async

(* RNG seeding: https://github.com/mirage/mirage-crypto#faq *)
let () = Mirage_crypto_rng_lwt.initialize (module Mirage_crypto_rng.Fortuna)

let () = Lwt_main.run launch
