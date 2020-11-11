open Base
open Bot_components
open Bot_components.Bot_info
open Bot_components.GitHub_types
open Helpers
open Lwt.Infix

let gitlab_repo ~bot_info ~owner ~name =
  f "https://oauth2:%s@gitlab.com/%s/%s.git" bot_info.gitlab_token owner name

let report_status command report code =
  Error (f "Command \"%s\" %s %d\n" command report code)

let gitlab_ref ~bot_info ~(issue : issue) ~github_mapping ~gitlab_mapping =
  let gh_repo = issue.owner ^ "/" ^ issue.repo in
  let open Lwt.Infix in
  (* First, we check our hashtable for a key named after the GitHub
     repository and return the associated GitLab repository. If the
     key is not found, we load the config file from the default branch.
     Last (backward-compatibility) we assume the GitLab and GitHub
     projects are named the same. *)
  ( match Hashtbl.find github_mapping gh_repo with
  | None -> (
      Stdio.printf "No correspondence found for GitHub repository %s/%s.\n"
        issue.owner issue.repo ;
      GitHub_queries.get_default_branch ~bot_info ~owner:issue.owner
        ~repo:issue.repo
      >>= function
      | Ok branch -> (
          GitHub_queries.get_file_content ~bot_info ~owner:issue.owner
            ~repo:issue.repo ~branch
            ~file_name:(f "%s.toml" bot_info.name)
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
        raise (Failure "Str.split")
  in
  ( {name= f "pr-%d" issue.number; repo_url= gitlab_repo ~owner ~name ~bot_info}
    : remote_ref_info )

let ( |&& ) command1 command2 = command1 ^ " && " ^ command2

let execute_cmd command =
  (*
  Lwt_io.printf "Executing command: %s\n" command
  >>= fun () ->
   *)
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

let git_fetch ?(force = true) remote_ref local_branch_name =
  f "git fetch -fu %s %s%s:refs/heads/%s" remote_ref.repo_url
    (if force then "+" else "")
    (Stdlib.Filename.quote remote_ref.name)
    (Stdlib.Filename.quote local_branch_name)

let git_push ?(force = true) ~remote_ref ~local_ref =
  f "git push %s %s%s:refs/heads/%s" remote_ref.repo_url
    (if force then " +" else " ")
    (Stdlib.Filename.quote local_ref)
    (Stdlib.Filename.quote remote_ref.name)

let git_delete ~remote_ref = git_push ~force:false ~remote_ref ~local_ref:""

let git_make_ancestor ~pr_title ~pr_number ~base head =
  f "./make_ancestor.sh %s %s %s %d"
    (Stdlib.Filename.quote base)
    (Stdlib.Filename.quote head)
    (Stdlib.Filename.quote pr_title)
    pr_number
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

let git_coq_bug_minimizer ~bot_info ~script ~comment_thread_id ~comment_author
    ~owner ~repo =
  f "./coq_bug_minimizer.sh '%s' %s %s %s %s %s %s %s" script comment_thread_id
    comment_author
    (get_token bot_info.github_token)
    bot_info.name bot_info.domain owner repo
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

let init_git_bare_repository ~bot_info =
  Stdio.printf "Initializing repository...\n" ;
  "git init --bare"
  |&& f "git config user.email \"%s\"" bot_info.email
  |&& f "git config user.name \"%s\"" bot_info.name
  |> execute_cmd
  >|= function
  | Ok _ ->
      Stdio.printf "Bare repository initialized.\n"
  | Error e ->
      Stdio.printf "%s.\n" e

let run_coq_minimizer ~bot_info ~coq_minimizer_repo_token ~script
    ~comment_thread_id ~comment_author ~owner ~repo =
  git_coq_bug_minimizer
    ~bot_info:{bot_info with github_token= coq_minimizer_repo_token}
    ~script ~comment_thread_id ~comment_author ~owner ~repo
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
      Lwt_io.printf "Error: %s\n" f
