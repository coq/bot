open Base
open Bot_components
open Lwt

let f = Printf.sprintf

let string_match ~regexp string =
  try
    let _ = Str.search_forward (Str.regexp regexp) string 0 in
    true
  with Stdlib.Not_found -> false

let pr_from_branch branch =
  if string_match ~regexp:"^pr-\\([0-9]*\\)$" branch then
    (Some (Str.matched_group 1 branch |> Int.of_string), "pull request")
  else (None, "branch")

let first_line_of_string s =
  if string_match ~regexp:"\\(.*\\)\n" s then Str.matched_group 1 s else s

let remove_between s i j =
  String.sub ~pos:0 ~len:i s ^ String.sub s ~pos:j ~len:(String.length s - j)

let trim_comments comment =
  let rec aux comment begin_ in_comment =
    if not in_comment then
      try
        let begin_ = Str.search_forward (Str.regexp "<!--") comment 0 in
        aux comment begin_ true
      with Stdlib.Not_found -> comment
    else
      try
        let end_ = Str.search_forward (Str.regexp "-->") comment begin_ in
        aux (remove_between comment begin_ (end_ + 3)) 0 false
      with Stdlib.Not_found -> comment
  in
  aux comment 0 false

let action_with_new_installation_token ~bot_info ~key ~app_id ~owner ~repo
    ~(installation_tokens : (string, string * float) Base.Hashtbl.t) action =
  GitHub_app.get_installation_token ~bot_info ~key ~app_id ~owner ~repo
  >>= function
  | Ok (github_token, expiration_date) ->
      let _ =
        Hashtbl.add installation_tokens ~key:owner
          ~data:(github_token, expiration_date)
      in
      let bot_info : Bot_info.t = {bot_info with github_token} in
      action ~bot_info
  | Error _ ->
      action ~bot_info

let action_as_github_app ~bot_info ~key ~app_id ~owner ~repo
    ~installation_tokens action =
  (* Executes an action with an installation token if the repository has
     the GitHub app installed.
     Generates a new installation token if the existing one has expired. *)
  match Hashtbl.find installation_tokens owner with
  | Some (github_token, expiration_date) ->
      if Float.(expiration_date < Unix.time ()) then (
        Hashtbl.remove installation_tokens owner ;
        action_with_new_installation_token ~bot_info ~key ~app_id ~owner ~repo
          ~installation_tokens action )
      else
        let bot_info : Bot_info.t = {bot_info with github_token} in
        action ~bot_info
  | None ->
      action_with_new_installation_token ~bot_info ~key ~app_id ~owner ~repo
        ~installation_tokens action

let github_repo_of_gitlab_project_path ~github_of_gitlab gitlab_project_path =
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
      raise (Failure "Str.split")

let github_repo_of_gitlab_url ~github_of_gitlab gitlab_repo_url =
  let owner, repo =
    let repo_url = gitlab_repo_url in
    if not (string_match ~regexp:".*:\\(.*\\)/\\(.*\\).git" repo_url) then
      Stdio.printf "Could not match project name on repository url.\n" ;
    (Str.matched_group 1 repo_url, Str.matched_group 2 repo_url)
  in
  let repo_full_name = owner ^ "/" ^ repo in
  github_repo_of_gitlab_project_path ~github_of_gitlab repo_full_name
