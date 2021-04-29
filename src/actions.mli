open Bot_components

val job_action :
     bot_info:Bot_info.t
  -> GitLab_types.ci_common_info GitLab_types.job_info
  -> gitlab_mapping:(string, string) Base.Hashtbl.t
  -> unit Lwt.t

val pipeline_action :
     bot_info:Bot_info.t
  -> GitLab_types.pipeline_info
  -> gitlab_mapping:(string, string) Base.Hashtbl.t
  -> unit Lwt.t

val run_coq_minimizer :
     bot_info:Bot_components.Bot_info.t
  -> script:string
  -> comment_thread_id:string
  -> comment_author:string
  -> owner:string
  -> repo:string
  -> unit Lwt.t

val coq_bug_minimizer_results_action :
     bot_info:Bot_info.t
  -> ci:bool
  -> key:Mirage_crypto_pk.Rsa.priv
  -> app_id:int
  -> string
  -> (Cohttp.Response.t * Cohttp_lwt__Body.t) Lwt.t

val merge_pull_request_action :
  bot_info:Bot_info.t -> ?t:float -> GitHub_types.comment_info -> unit Lwt.t

val run_ci_action :
     bot_info:Bot_info.t
  -> comment_info:GitHub_types.comment_info
  -> gitlab_mapping:(string, string) Base.Hashtbl.t
  -> github_mapping:(string, string) Base.Hashtbl.t
  -> signed:bool
  -> (Cohttp.Response.t * Cohttp_lwt__Body.t) Lwt.t

val pull_request_closed_action :
     bot_info:Bot_info.t
  -> GitHub_types.issue_info GitHub_types.pull_request_info
  -> gitlab_mapping:(string, string) Base.Hashtbl.t
  -> github_mapping:(string, string) Base.Hashtbl.t
  -> unit Lwt.t

val pull_request_updated_action :
     bot_info:Bot_info.t
  -> action:GitHub_types.pull_request_action
  -> pr_info:GitHub_types.issue_info GitHub_types.pull_request_info
  -> gitlab_mapping:(string, string) Base.Hashtbl.t
  -> github_mapping:(string, string) Base.Hashtbl.t
  -> signed:bool
  -> (Cohttp.Response.t * Cohttp_lwt__.Body.t) Lwt.t

val adjust_milestone :
     bot_info:Bot_info.t
  -> issue:GitHub_types.issue
  -> sleep_time:float
  -> unit Lwt.t

val project_action :
  bot_info:Bot_info.t -> issue:GitHub_types.issue -> column_id:int -> unit Lwt.t

val push_action :
     bot_info:Bot_info.t
  -> base_ref:string
  -> commits_msg:string list
  -> unit Lwt.t

type ci_minimization_request =
  | Auto
  | RequestSuggested
  | RequestAll
  | RequestExplicit of string list

val minimize_failed_tests :
     bot_info:Bot_components.Bot_info.t
  -> owner:string
  -> repo:string
  -> pr_number:int option
  -> base:string
  -> head:string
  -> head_pipeline_summary:string option
  -> request:ci_minimization_request
  -> unit Lwt.t
