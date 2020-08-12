open Actions
open Base
open Bot_components
open Cohttp
open Cohttp_lwt_unix
open Git_utils
open Lwt.Infix
open Utils

let toml_data = Config.toml_of_file (Sys.get_argv ()).(1)

let port = Config.port toml_data

let gitlab_access_token = Config.gitlab_access_token toml_data

let github_access_token = Config.github_access_token toml_data

let github_webhook_secret = Config.github_webhook_secret toml_data

let gitlab_webhook_secret = Config.gitlab_webhook_secret toml_data

let bot_name = Config.bot_name toml_data

let bot_info =
  { github_token= github_access_token
  ; gitlab_token= gitlab_access_token
  ; name= bot_name
  ; email= Config.bot_email toml_data bot_name
  ; domain= Config.bot_domain toml_data bot_name }

let github_mapping, gitlab_mapping = Config.make_mappings_table toml_data

let github_of_gitlab = Hashtbl.find gitlab_mapping

let gitlab_of_github = Hashtbl.find github_mapping

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
          job_action ~bot_info ~github_of_gitlab job_info ;
          Server.respond_string ~status:`OK ~body:"Job event." ()
      | Ok (_, PipelineEvent pipeline_info) ->
          (fun () -> pipeline_action ~bot_info ~github_of_gitlab pipeline_info)
          |> Lwt.async ;
          Server.respond_string ~status:`OK ~body:"Pipeline event." ()
      | Ok (_, UnsupportedEvent e) ->
          Server.respond_string ~status:`OK
            ~body:(f "Unsupported event %s." e)
            ()
      | Error s ->
          Server.respond_string ~status:(Code.status_of_code 400)
            ~body:(f "Error: %s" s) () )
  | "/push" | "/pull_request" (* legacy endpoints *) | "/github" -> (
      body
      >>= fun body ->
      match
        GitHub_subscriptions.receive_github ~secret:github_webhook_secret
          (Request.headers req) body
      with
      | Ok (_, GitHub_subscriptions.PushEvent {base_ref; commits_msg}) ->
          push_action ~base_ref ~commits_msg ~bot_info |> Lwt.async ;
          Server.respond_string ~status:`OK ~body:"Processing push event." ()
      | Ok
          ( _
          , GitHub_subscriptions.PullRequestUpdated (PullRequestClosed, pr_info)
          ) ->
          pull_request_closed ~bot_info ~gitlab_mapping ~github_mapping
            ~gitlab_of_github pr_info
          |> Lwt.async ;
          Server.respond_string ~status:`OK
            ~body:
              (f
                 "Pull request %s/%s#%d was closed: removing the branch from \
                  GitLab."
                 pr_info.issue.issue.owner pr_info.issue.issue.repo
                 pr_info.issue.issue.number)
            ()
      | Ok (signed, GitHub_subscriptions.PullRequestUpdated (action, pr_info))
        ->
          pull_request_updated_action ~action ~pr_info ~bot_info ~gitlab_mapping
            ~github_mapping ~gitlab_of_github ~signed
      | Ok (_, GitHub_subscriptions.IssueClosed {issue}) ->
          (* TODO: only for projects that requested this feature *)
          adjust_milestone ~issue ~sleep_time:5. ~bot_info |> Lwt.async ;
          Server.respond_string ~status:`OK
            ~body:
              (f "Issue %s/%s#%d was closed: checking its milestone."
                 issue.owner issue.repo issue.number)
            ()
      | Ok
          ( _
          , GitHub_subscriptions.RemovedFromProject
              ({issue= Some issue; column_id} as card) ) ->
          project_action ~issue ~column_id ~bot_info |> Lwt.async ;
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
              ~bot_info
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
              ~comment_thread_id:comment_info.issue.id
              ~comment_author:comment_info.author ~bot_info
            |> Lwt.async ;
            Server.respond_string ~status:`OK ~body:"Handling minimization." ()
            )
          else if
            string_match ~regexp:(f "@%s:? [Rr]un CI now" bot_name) body
            && comment_info.issue.pull_request
          then
            run_ci ~comment_info ~bot_info ~gitlab_mapping ~github_mapping
              ~gitlab_of_github ~signed
          else if
            string_match ~regexp:(f "@%s:? [Mm]erge now" bot_name) body
            && comment_info.issue.pull_request
            && String.equal comment_info.issue.issue.owner "coq"
            && String.equal comment_info.issue.issue.repo "coq"
            && signed
          then (
            (fun () -> merge_pull_request ~comment_info ~bot_info) |> Lwt.async ;
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
      body >>= fun body -> coq_bug_minimizer_results_action body ~bot_info
  | _ ->
      Server.respond_not_found ()

let launch =
  init_git_bare_repository ~bot_info
  >>= fun () ->
  Stdio.printf "Starting server.\n" ;
  let mode = `TCP (`Port port) in
  Server.create ~mode (Server.make ~callback ())

let () =
  Lwt.async_exception_hook :=
    fun exn -> Stdio.printf "Error: Unhandled exception: %s" (Exn.to_string exn)

let () = Lwt_main.run launch
