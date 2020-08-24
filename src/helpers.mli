val f : ('a, unit, string) format -> 'a

val string_match : regexp:string -> string -> bool

val pr_from_branch : string -> int option * string

val first_line_of_string : string -> string

val remove_between : string -> int -> int -> string

val trim_comments : string -> string

val github_repo_of_gitlab_project_path :
  github_of_gitlab:(string -> string option) -> string -> string * string

val github_repo_of_gitlab_url :
  github_of_gitlab:(string -> string option) -> string -> string * string
