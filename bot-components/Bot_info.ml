open Base

type t =
  { gitlab_instances: (string, string * string) Hashtbl.t
  ; github_pat: string
  ; github_install_token: string option
  ; github_name: string
  ; email: string
  ; domain: string
  ; app_id: int }

let github_token bot_info =
  match bot_info.github_install_token with
  | Some t ->
      t
  | None ->
      bot_info.github_pat

let gitlab_name_and_token bot_info gitlab_domain =
  match Hashtbl.find bot_info.gitlab_instances gitlab_domain with
  | Some t ->
      Ok t
  | None ->
      Error
        ( "I don't know about GitLab domain " ^ gitlab_domain
        ^ " (not in my configuration file)" )

let gitlab_token bot_info gitlab_domain =
  gitlab_name_and_token bot_info gitlab_domain |> Result.map ~f:snd
