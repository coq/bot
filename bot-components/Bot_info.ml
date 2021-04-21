type t =
  { gitlab_token: string
  ; github_pat: string
  ; github_install_token: string option
  ; name: string
  ; email: string
  ; domain: string
  ; app_id: int }

let github_token bot_info =
  match bot_info.github_install_token with
  | Some t ->
      t
  | None ->
      bot_info.github_pat
