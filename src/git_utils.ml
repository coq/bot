open Base
open Bot_components
open Bot_components.Bot_info
open Bot_components.GitHub_types
open Helpers
open Lwt.Infix
open Lwt.Syntax

let gitlab_repo ~bot_info ~gitlab_domain ~gitlab_full_name =
  gitlab_token bot_info gitlab_domain
  |> Result.map ~f:(fun token ->
         f "https://oauth2:%s@%s/%s.git" token gitlab_domain gitlab_full_name )

let report_status ?(mask = []) ?(stderr_content = "") command report code =
  let stderr =
    if String.is_empty stderr_content then "" else stderr_content ^ "\n"
  in
  Error
    (List.fold_left
       ~init:(f {|Command "%s" %s %d%s%s|} command report code "\n" stderr)
       ~f:(fun acc m -> Str.global_replace (Str.regexp_string m) "XXXXX" acc)
       mask )

let gitlab_ref ~bot_info ~(issue : issue) ~github_mapping ~gitlab_mapping =
  let default_gitlab_domain = "gitlab.com" in
  let gh_repo = issue.owner ^ "/" ^ issue.repo in
  let open Lwt.Infix in
  (* First, we check our hashtable for a key named after the GitHub
     repository and return the associated GitLab repository. If the
     key is not found, we load the config file from the default branch.
     Last (backward-compatibility) we assume the GitLab and GitHub
     projects are named the same. *)
  let default_value = (default_gitlab_domain, gh_repo) in
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
            ~file_name:(f "%s.toml" bot_info.github_name)
          >>= function
          | Ok (Some content) ->
              let gl_domain =
                Option.value
                  (Config.subkey_value
                     (Config.toml_of_string content)
                     "mapping" "gitlab_domain" )
                  ~default:default_gitlab_domain
              in
              let gl_repo =
                Option.value
                  (Config.subkey_value
                     (Config.toml_of_string content)
                     "mapping" "gitlab" )
                  ~default:gh_repo
              in
              ( match
                  Hashtbl.add gitlab_mapping
                    ~key:(gl_domain ^ "/" ^ gl_repo)
                    ~data:gh_repo
                with
              | `Duplicate ->
                  ()
              | `Ok ->
                  () ) ;
              ( match
                  Hashtbl.add github_mapping ~key:gh_repo
                    ~data:(gl_domain, gl_repo)
                with
              | `Duplicate ->
                  ()
              | `Ok ->
                  () ) ;
              Lwt.return (gl_domain, gl_repo)
          | _ ->
              Lwt.return default_value )
      | _ ->
          Lwt.return default_value )
  | Some r ->
      Lwt.return r )
  >|= fun (gitlab_domain, gitlab_full_name) ->
  gitlab_repo ~gitlab_domain ~gitlab_full_name ~bot_info
  |> Result.map ~f:(fun gl_repo ->
         {name= f "refs/heads/pr-%d" issue.number; repo_url= gl_repo} )

let ( |&& ) command1 command2 = command1 ^ " && " ^ command2

let execute_cmd ?(mask = []) command =
  Lwt_io.printf "Executing command: %s\n" command
  >>= fun () ->
  let process = Lwt_process.open_process_full (Lwt_process.shell command) in
  let stdout_pipe = copy_stream ~src:process#stdout ~dst:Lwt_io.stdout in
  let stderr_pipe = copy_stream ~src:process#stderr ~dst:Lwt_io.stderr in
  (* Capture stdout and stderr in parallel *)
  (* Wait for the process to finish *)
  let+ _stdout_content = stdout_pipe
  and+ stderr_content = stderr_pipe
  and+ status = process#status in
  match status with
  | Unix.WEXITED code ->
      if Int.equal code 0 then Ok ()
      else report_status ~mask ~stderr_content command "exited with status" code
  | Unix.WSIGNALED signal ->
      report_status ~mask command "was killed by signal number" signal
  | Unix.WSTOPPED signal ->
      report_status ~mask command "was stopped by signal number" signal

let git_fetch ?(force = true) remote_ref local_branch_name =
  f "git fetch --quiet -fu %s %s%s:%s" remote_ref.repo_url
    (if force then "+" else "")
    (Stdlib.Filename.quote remote_ref.name)
    (Stdlib.Filename.quote local_branch_name)

let git_push ?(force = true) ?(options = "") ~remote_ref ~local_ref () =
  f "git push %s %s%s:%s %s" remote_ref.repo_url
    (if force then " +" else " ")
    (Stdlib.Filename.quote local_ref)
    (Stdlib.Filename.quote remote_ref.name)
    options

let git_delete ~remote_ref = git_push ~force:false ~remote_ref ~local_ref:"" ()

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

let git_test_modified ~base ~head pattern =
  let command =
    f {|git diff %s...%s --name-only | grep "%s"|} base head pattern
  in
  Lwt_unix.system command
  >|= fun status ->
  match status with
  | Unix.WEXITED 0 ->
      Ok true (* file was modified *)
  | Unix.WEXITED 1 ->
      Ok false (* file was not modified *)
  | Unix.WEXITED code ->
      Error (f "%s exited with status %d." command code)
  | Unix.WSIGNALED signal ->
      Error (f "%s killed by signal %d." command signal)
  | Unix.WSTOPPED signal ->
      Error (f "%s stopped by signal %d." command signal)

let git_coq_bug_minimizer ~bot_info ~script ~comment_thread_id ~comment_author
    ~owner ~repo ~coq_version ~ocaml_version ~minimizer_extra_arguments =
  (* To push a new branch we need to identify as coqbot the GitHub
     user, who is a collaborator on the run-coq-bug-minimizer repo,
     not coqbot the GitHub App *)
  Stdlib.Filename.quote_command "./coq_bug_minimizer.sh"
    [ script
    ; GitHub_ID.to_string comment_thread_id
    ; comment_author
    ; bot_info.github_pat
    ; bot_info.github_name
    ; bot_info.domain
    ; owner
    ; repo
    ; coq_version
    ; ocaml_version
    ; minimizer_extra_arguments |> String.concat ~sep:" " ]
  |> execute_cmd ~mask:[bot_info.github_pat]

let git_run_ci_minimization ~bot_info ~comment_thread_id ~owner ~repo ~pr_number
    ~docker_image ~target ~ci_targets ~opam_switch ~failing_urls ~passing_urls ~base ~head
    ~minimizer_extra_arguments ~bug_file_name =
  (* To push a new branch we need to identify as coqbot the GitHub
     user, who is a collaborator on the run-coq-bug-minimizer repo,
     not coqbot the GitHub App *)
  ( [ GitHub_ID.to_string comment_thread_id
    ; bot_info.github_pat
    ; bot_info.github_name
    ; bot_info.domain
    ; owner
    ; repo
    ; pr_number
    ; docker_image
    ; target
    ; ci_targets |> String.concat ~sep:" "
    ; opam_switch
    ; failing_urls
    ; passing_urls
    ; base
    ; head
    ; minimizer_extra_arguments |> String.concat ~sep:" " ]
  @
  match bug_file_name with Some bug_file_name -> [bug_file_name] | None -> [] )
  |> Stdlib.Filename.quote_command "./run_ci_minimization.sh"
  |> execute_cmd ~mask:[bot_info.github_pat]

let init_git_bare_repository ~bot_info =
  let* () = Lwt_io.printl "Initializing repository..." in
  "git init --bare"
  |&& f {|git config user.email "%s"|} bot_info.email
  |&& f {|git config user.name "%s"|} bot_info.github_name
  |> execute_cmd ~mask:[bot_info.github_pat]
  >>= function
  | Ok _ ->
      Lwt_io.printl "Bare repository initialized."
  | Error e ->
      Lwt_io.printlf "Error while initializing bare repository: %s." e
