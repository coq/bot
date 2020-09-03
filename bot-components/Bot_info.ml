type github_token = ACCESS_TOKEN of string | INSTALL_TOKEN of string

type t =
  { gitlab_token: string
  ; github_token: github_token
  ; name: string
  ; email: string
  ; domain: string
  ; app_id: int }

let get_token t = match t with ACCESS_TOKEN t | INSTALL_TOKEN t -> t
