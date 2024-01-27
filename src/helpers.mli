val f : ('a, unit, string) format -> 'a

val code_wrap : string -> string
(** [code_wrap] = [f "```\n%s\n```"] *)

val string_match : regexp:string -> ?pos:int -> string -> bool

val fold_string_matches :
  regexp:string -> f:((unit -> 'a) -> 'a) -> init:'a -> ?pos:int -> string -> 'a

val map_string_matches : regexp:string -> f:(unit -> 'a) -> string -> 'a list

val iter_string_matches : regexp:string -> f:(unit -> unit) -> string -> unit

val pr_from_branch : string -> int option * string

val first_line_of_string : string -> string

val remove_between : string -> int -> int -> string

val trim_comments : string -> string

val strip_quoted_bot_name : github_bot_name:string -> string -> string

val github_repo_of_gitlab_project_path :
     gitlab_mapping:(string, string) Base.Hashtbl.t
  -> gitlab_domain:string
  -> gitlab_repo_full_name:string
  -> string * string

val parse_gitlab_repo_url :
  http_repo_url:string -> (string * string, string) result

val github_repo_of_gitlab_url :
     gitlab_mapping:(string, string) Base.Hashtbl.t
  -> http_repo_url:string
  -> (string * string, string) result

val download : uri:Uri.t -> string -> (unit, string) Lwt_result.t

val download_to :
  uri:Uri.t -> Lwt_io.output_channel -> (unit, string) Lwt_result.t
