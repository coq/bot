open Actions
open Base
open Bot_components
open Cohttp
open Cohttp_lwt_unix
open Git_utils
open Github_installations
open Helpers
open Lwt.Infix

let toml_data = Config.toml_of_file (Sys.get_argv ()).(1)

let port = Config.port toml_data

let gitlab_access_token = Config.gitlab_access_token toml_data

let github_access_token = Config.github_access_token toml_data

let github_webhook_secret = Config.github_webhook_secret toml_data

let gitlab_webhook_secret = Config.gitlab_webhook_secret toml_data

let bot_name = Config.bot_name toml_data

let key = Config.github_private_key

let app_id = Config.github_app_id toml_data

let bot_info : Bot_components.Bot_info.t =
  { github_pat= github_access_token
  ; github_install_token= None
  ; gitlab_token= gitlab_access_token
  ; name= bot_name
  ; email= Config.bot_email toml_data
  ; domain= Config.bot_domain toml_data
  ; app_id }

let github_mapping, gitlab_mapping = Config.make_mappings_table toml_data

let string_of_installation_tokens =
  Hashtbl.fold ~init:"" ~f:(fun ~key ~data acc ->
      acc ^ f "Owner: %s, token: %s,  expire at: %f\n" key (fst data) (snd data))

(* TODO: deprecate unsigned webhooks *)

let callback _conn req body =
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
      | Ok (_, JobEvent job_info) ->
          (fun () ->
            let gh_owner, gh_repo =
              github_repo_of_gitlab_url ~gitlab_mapping
                job_info.common_info.repo_url
            in
            action_as_github_app ~bot_info ~key ~app_id ~owner:gh_owner
              ~repo:gh_repo
              (job_action ~gitlab_mapping job_info))
          |> Lwt.async ;
          Server.respond_string ~status:`OK ~body:"Job event." ()
      | Ok (_, PipelineEvent pipeline_info) ->
          (fun () ->
            let owner, repo =
              github_repo_of_gitlab_project_path ~gitlab_mapping
                pipeline_info.project_path
            in
            action_as_github_app ~bot_info ~key ~app_id ~owner ~repo
              (pipeline_action ~gitlab_mapping pipeline_info))
          |> Lwt.async ;
          Server.respond_string ~status:`OK ~body:"Pipeline event." ()
      | Ok (_, UnsupportedEvent e) ->
          Server.respond_string ~status:`OK
            ~body:(f "Unsupported event %s." e)
            ()
      | Error ("Webhook password mismatch." as e) ->
          Stdio.print_string e ;
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
      | Ok (_, PushEvent {owner; repo; base_ref; commits_msg}) ->
          (fun () ->
            init_git_bare_repository ~bot_info
            >>= fun () ->
            action_as_github_app ~bot_info ~key ~app_id ~owner ~repo
              (push_action ~base_ref ~commits_msg))
          |> Lwt.async ;
          Server.respond_string ~status:`OK ~body:"Processing push event." ()
      | Ok (_, PullRequestUpdated (PullRequestClosed, pr_info)) ->
          (fun () ->
            init_git_bare_repository ~bot_info
            >>= fun () ->
            action_as_github_app ~bot_info ~key ~app_id
              ~owner:pr_info.issue.issue.owner ~repo:pr_info.issue.issue.repo
              (pull_request_closed_action ~gitlab_mapping ~github_mapping
                 pr_info))
          |> Lwt.async ;
          Server.respond_string ~status:`OK
            ~body:
              (f
                 "Pull request %s/%s#%d was closed: removing the branch from \
                  GitLab."
                 pr_info.issue.issue.owner pr_info.issue.issue.repo
                 pr_info.issue.issue.number)
            ()
      | Ok (signed, PullRequestUpdated (action, pr_info)) ->
          init_git_bare_repository ~bot_info
          >>= fun () ->
          action_as_github_app ~bot_info ~key ~app_id
            ~owner:pr_info.issue.issue.owner ~repo:pr_info.issue.issue.repo
            (pull_request_updated_action ~action ~pr_info ~gitlab_mapping
               ~github_mapping ~signed)
      | Ok (_, IssueClosed {issue}) ->
          (* TODO: only for projects that requested this feature *)
          (fun () ->
            action_as_github_app ~bot_info ~key ~app_id ~owner:issue.owner
              ~repo:issue.repo
              (adjust_milestone ~issue ~sleep_time:5.))
          |> Lwt.async ;
          Server.respond_string ~status:`OK
            ~body:
              (f "Issue %s/%s#%d was closed: checking its milestone."
                 issue.owner issue.repo issue.number)
            ()
      | Ok (_, RemovedFromProject ({issue= Some issue; column_id} as card)) ->
          (fun () ->
            action_as_github_app ~bot_info ~key ~app_id ~owner:issue.owner
              ~repo:issue.repo
              (project_action ~issue ~column_id))
          |> Lwt.async ;
          Server.respond_string ~status:`OK
            ~body:
              (f
                 "Issue or PR %s/%s#%d was removed from project column %d: \
                  checking if this was a backporting column."
                 issue.owner issue.repo issue.number card.column_id)
            ()
      | Ok (_, RemovedFromProject _) ->
          Server.respond_string ~status:`OK
            ~body:"Note card removed from project: nothing to do." ()
      | Ok (_, IssueOpened ({body= Some body} as issue_info)) ->
          let body = Helpers.trim_comments body in
          if
            string_match
              ~regexp:(f "@%s:? [Mm]inimize[^```]*```\\([^```]+\\)```" bot_name)
              body
          then
            (fun () ->
              init_git_bare_repository ~bot_info
              >>= fun () ->
              action_as_github_app ~bot_info ~key ~app_id
                ~owner:issue_info.issue.owner ~repo:issue_info.issue.repo
                (run_coq_minimizer ~script:(Str.matched_group 1 body)
                   ~comment_thread_id:issue_info.id
                   ~comment_author:issue_info.user ~owner:issue_info.issue.owner
                   ~repo:issue_info.issue.repo))
            |> Lwt.async ;
          Server.respond_string ~status:`OK ~body:"Handling minimization." ()
      | Ok (signed, CommentCreated comment_info) ->
          let body = Helpers.trim_comments comment_info.body in
          if
            string_match
              ~regexp:(f "@%s:? [Mm]inimize[^```]*```\\([^```]+\\)```" bot_name)
              body
          then (
            (fun () ->
              init_git_bare_repository ~bot_info
              >>= fun () ->
              action_as_github_app ~bot_info ~key ~app_id
                ~owner:comment_info.issue.issue.owner
                ~repo:comment_info.issue.issue.repo
                (run_coq_minimizer ~script:(Str.matched_group 1 body)
                   ~comment_thread_id:comment_info.issue.id
                   ~comment_author:comment_info.author
                   ~owner:comment_info.issue.issue.owner
                   ~repo:comment_info.issue.issue.repo))
            |> Lwt.async ;
            Server.respond_string ~status:`OK ~body:"Handling minimization." ()
            )
          else if
            string_match
              ~regexp:(f "@%s:? [Cc][Ii][- ][Mm]inimize" bot_name)
              body
          then (
            match comment_info.pull_request with
            | Some pull_request ->
                (fun () ->
                  (* XXX FIXME do we want this???
                     action_as_github_app ~bot_info ~key ~app_id
                       ~owner:comment_info.issue.issue.owner
                       ~repo:comment_info.issue.issue.repo
                  *)
                  minimize_failed_tests ~bot_info
                    ~owner:comment_info.issue.issue.owner
                    ~repo:comment_info.issue.issue.repo
                    ~base:pull_request.base.sha ~head:pull_request.head.sha
                    ~pr_number:(Some comment_info.issue.number)
                    ~head_pipeline_summary:None)
                |> Lwt.async ;
                Server.respond_string ~status:`OK
                  ~body:"Handling CI minimization." ()
            | None ->
                (fun () ->
                  (*XXX DO we need this? action_as_github_app ~bot_info ~key ~app_id
                    ~owner:comment_info.issue.issue.owner ~repo:comment_info.issue.issue.repo*)
                  GitHub_mutations.post_comment
                    ~id:
                      comment_info.id
                      (* XXX This was comment_thread_id, is id correct here? *)
                    ~message:
                      (f
                         "Hey @%s, you cannot run CI minimization on issues \
                          that are not pull requests."
                         comment_info.author)
                    ~bot_info
                  >>= GitHub_mutations.report_on_posting_comment)
                |> Lwt.async ;
                (* XXX IS this an Ok or a failure? *)
                Server.respond_string ~status:`OK
                  ~body:"Invalid request for CI minimization." () )
          else if
            string_match ~regexp:(f "@%s:? [Rr]un CI now" bot_name) body
            && comment_info.issue.pull_request
          then
            init_git_bare_repository ~bot_info
            >>= fun () ->
            action_as_github_app ~bot_info ~key ~app_id
              ~owner:comment_info.issue.issue.owner
              ~repo:comment_info.issue.issue.repo
              (run_ci_action ~comment_info ~gitlab_mapping ~github_mapping
                 ~signed)
          else if
            string_match ~regexp:(f "@%s:? [Mm]erge now" bot_name) body
            && comment_info.issue.pull_request
            && String.equal comment_info.issue.issue.owner "coq"
            && String.equal comment_info.issue.issue.repo "coq"
            && signed
          then (
            (fun () ->
              action_as_github_app ~bot_info ~key ~app_id
                ~owner:comment_info.issue.issue.owner
                ~repo:comment_info.issue.issue.repo
                (merge_pull_request_action comment_info))
            |> Lwt.async ;
            Server.respond_string ~status:`OK
              ~body:(f "Received a request to merge the PR.")
              () )
          else
            Server.respond_string ~status:`OK
              ~body:(f "Unhandled comment: %s" body)
              ()
      | Ok (signed, CheckRunReRequested {external_id}) ->
          if not signed then
            Server.respond_string ~status:(Code.status_of_code 401)
              ~body:"Request to rerun check run must be signed." ()
          else if String.is_empty external_id then
            Server.respond_string ~status:(Code.status_of_code 400)
              ~body:"Request to rerun check run but empty external ID." ()
          else (
            (fun () ->
              GitLab_mutations.generic_retry ~bot_info ~url_part:external_id)
            |> Lwt.async ;
            Server.respond_string ~status:`OK
              ~body:
                (f
                   "Received a request to re-run a job / pipeline (GitLab ID : \
                    %s)."
                   external_id)
              () )
      | Ok (_, UnsupportedEvent s) ->
          Server.respond_string ~status:`OK ~body:(f "No action taken: %s" s) ()
      | Ok _ ->
          Server.respond_string ~status:`OK
            ~body:"No action taken: event or action is not yet supported." ()
      | Error ("Webhook signed but with wrong signature." as e) ->
          Stdio.print_string e ;
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
  | _ ->
      Server.respond_not_found ()

let launch =
  Stdio.printf "Starting server.\n" ;
  let mode = `TCP (`Port port) in
  Server.create ~mode (Server.make ~callback ())

let () =
  Lwt.async_exception_hook :=
    fun exn ->
      Stdio.printf "Error: Unhandled exception: %s\n" (Exn.to_string exn)

(* RNG seeding: https://github.com/mirage/mirage-crypto#faq *)
let () = Mirage_crypto_rng_lwt.initialize ()

let () = Lwt_main.run launch
