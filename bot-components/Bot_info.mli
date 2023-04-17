type t =
  { gitlab_instances: (string, string * string) Base.Hashtbl.t
  ; github_pat: string
  ; github_install_token: string option
  ; github_name: string
  ; email: string
  ; domain: string
  ; app_id: int }

val github_token : t -> string

val gitlab_token : t -> string -> (string, string) Result.t

val gitlab_name_and_token : t -> string -> (string * string, string) Result.t
