type t =
  { gitlab_token: string
  ; github_pat: string
  ; github_install_token: string option
  ; name: string
  ; email: string
  ; domain: string
  ; app_id: int }

val github_token : t -> string
