type github_token = ACCESS_TOKEN of string | INSTALL_TOKEN of string

type t =
  { gitlab_token: string
  ; github_token: github_token
  ; name: string
  ; email: string
  ; domain: string
  ; app_id: int }

val get_token : github_token -> string
