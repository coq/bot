open Actions
open Base
open Bot_components
open Cohttp
open Cohttp_lwt_unix
open Git_utils
open Helpers
open Lwt.Infix

let toml_data = Config.toml_of_file (Sys.get_argv ()).(1)

let port = Config.port toml_data

let gitlab_access_token = Config.gitlab_access_token toml_data

let github_access_token = Config.github_access_token toml_data

let github_webhook_secret = Config.github_webhook_secret toml_data

let gitlab_webhook_secret = Config.gitlab_webhook_secret toml_data

let bot_name = Config.bot_name toml_data

let bot_info : Bot_components.Bot_info.t =
  { github_token= github_access_token
  ; gitlab_token= gitlab_access_token
  ; name= bot_name
  ; email= Config.bot_email toml_data
  ; domain= Config.bot_domain toml_data }

let key = Config.github_private_key

let app_id = Config.github_app_id toml_data

let github_mapping, gitlab_mapping = Config.make_mappings_table toml_data

let github_of_gitlab = Hashtbl.find gitlab_mapping

let gitlab_of_github = Hashtbl.find github_mapping

let installation_tokens : (string, string * float) Base.Hashtbl.t =
  match Hashtbl.of_alist (module String) [] with
  | `Duplicate_key _ ->
      raise (Failure "Duplicate key in config.")
  | `Ok t ->
      t

let github_repo_of_gitlab_project_path gitlab_project_path =
  let gitlab_full_name = gitlab_project_path in
  let repo_full_name =
    Option.value
      (github_of_gitlab gitlab_full_name)
      ~default:
        ( Stdio.printf "No correspondence found for GitLab repository %s.\n"
            gitlab_full_name ;
          gitlab_full_name )
  in
  match Str.split (Str.regexp "/") repo_full_name with
  | [owner_; repo_] ->
      (owner_, repo_)
  | _ ->
      (* Shouldn't happen *)
      ("owner", "repo")

let github_repo_of_gitlab_url gitlab_repo_url =
  let owner, repo =
    let repo_url = gitlab_repo_url in
    if not (string_match ~regexp:".*:\\(.*\\)/\\(.*\\).git" repo_url) then
      Stdio.printf "Could not match project name on repository url.\n" ;
    (Str.matched_group 1 repo_url, Str.matched_group 2 repo_url)
  in
  let repo_full_name = owner ^ "/" ^ repo in
  github_repo_of_gitlab_project_path repo_full_name

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
              github_repo_of_gitlab_url job_info.repo_url
            in
            action_as_github_app ~bot_info ~key ~app_id ~owner:gh_owner
              ~repo:gh_repo ~installation_tokens
              (job_action ~github_of_gitlab job_info))
          |> Lwt.async ;
          Server.respond_string ~status:`OK ~body:"Job event." ()
      | Ok (_, PipelineEvent pipeline_info) ->
          (fun () ->
            let owner, repo =
              github_repo_of_gitlab_project_path pipeline_info.project_path
            in
            action_as_github_app ~bot_info ~key ~app_id ~owner ~repo
              ~installation_tokens
              (pipeline_action ~github_of_gitlab pipeline_info))
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
              ~installation_tokens
              (push_action ~base_ref ~commits_msg))
          |> Lwt.async ;
          Server.respond_string ~status:`OK ~body:"Processing push event." ()
      | Ok (_, PullRequestUpdated (PullRequestClosed, pr_info)) ->
          (fun () ->
            init_git_bare_repository ~bot_info
            >>= fun () ->
            action_as_github_app ~bot_info ~key ~app_id
              ~owner:pr_info.issue.issue.owner ~repo:pr_info.issue.issue.repo
              ~installation_tokens
              (pull_request_closed_action ~gitlab_mapping ~github_mapping
                 ~gitlab_of_github pr_info))
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
            ~installation_tokens
            (pull_request_updated_action ~action ~pr_info ~gitlab_mapping
               ~github_mapping ~gitlab_of_github ~signed)
      | Ok (_, IssueClosed {issue}) ->
          (* TODO: only for projects that requested this feature *)
          (fun () ->
            action_as_github_app ~bot_info ~key ~app_id ~owner:issue.owner
              ~repo:issue.repo ~installation_tokens
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
              ~repo:issue.repo ~installation_tokens
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
                ~installation_tokens
                (run_coq_minimizer ~script:(Str.matched_group 1 body)
                   ~coq_minimizer_repo_token:bot_info.github_token
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
                ~repo:comment_info.issue.issue.repo ~installation_tokens
                (run_coq_minimizer ~script:(Str.matched_group 1 body)
                   ~coq_minimizer_repo_token:bot_info.github_token
                   ~comment_thread_id:comment_info.issue.id
                   ~comment_author:comment_info.author
                   ~owner:comment_info.issue.issue.owner
                   ~repo:comment_info.issue.issue.repo))
            |> Lwt.async ;
            Server.respond_string ~status:`OK ~body:"Handling minimization." ()
            )
          else if
            string_match ~regexp:(f "@%s:? [Rr]un CI now" bot_name) body
            && comment_info.issue.pull_request
          then
            init_git_bare_repository ~bot_info
            >>= fun () ->
            action_as_github_app ~bot_info ~key ~app_id
              ~owner:comment_info.issue.issue.owner
              ~repo:comment_info.issue.issue.repo ~installation_tokens
              (run_ci_action ~comment_info ~gitlab_mapping ~github_mapping
                 ~gitlab_of_github ~signed)
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
                ~repo:comment_info.issue.issue.repo ~installation_tokens
                (merge_pull_request_action ~comment_info))
            |> Lwt.async ;
            Server.respond_string ~status:`OK
              ~body:(f "Received a request to merge the PR.")
              () )
          else
            Server.respond_string ~status:`OK
              ~body:(f "Unhandled comment: %s." body)
              ()
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
      coq_bug_minimizer_results_action body ~bot_info ~key ~app_id
        ~coq_minimizer_repo_token:bot_info.github_token ~installation_tokens
  | _ ->
      Server.respond_not_found ()

let launch =
  Stdio.printf "Starting server.\n" ;
  let mode = `TCP (`Port port) in
  Server.create ~mode (Server.make ~callback ())

let () =
  Lwt.async_exception_hook :=
    fun exn -> Stdio.printf "Error: Unhandled exception: %s" (Exn.to_string exn)

(* RNG seeding: https://github.com/mirage/mirage-crypto#faq *)
let () = Mirage_crypto_rng_lwt.initialize ()

let () = Lwt_main.run launch
