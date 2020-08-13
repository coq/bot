val job_action :
     bot_info:Bot_components.Bot_info.bot_info
  -> Bot_components.GitLab_types.job_info
  -> github_of_gitlab:(string -> string option)
  -> unit Lwt.t

val pipeline_action :
     bot_info:Bot_components.Bot_info.bot_info
  -> Bot_components.GitLab_types.pipeline_info
  -> github_of_gitlab:(string -> string option)
  -> unit Lwt.t

val coq_bug_minimizer_results_action :
     bot_info:Bot_components.Bot_info.bot_info
  -> string
  -> (Cohttp.Response.t * Cohttp_lwt__Body.t) Lwt.t

val merge_pull_request :
     bot_info:Bot_components.Bot_info.bot_info
  -> comment_info:Bot_components.GitHub_types.comment_info
  -> unit Lwt.t

val pull_request_updated :
     bot_info:Bot_components.Bot_info.bot_info
  -> Bot_components.GitHub_types.issue_info
     Bot_components.GitHub_types.pull_request_info
  -> unit
  -> gitlab_mapping:(string, string) Base.Hashtbl.t
  -> github_mapping:(string, string) Base.Hashtbl.t
  -> gitlab_of_github:(string -> string option)
  -> (unit, string) Lwt_result.t

val run_ci :
     bot_info:Bot_components.Bot_info.bot_info
  -> comment_info:Bot_components.GitHub_types.comment_info
  -> gitlab_mapping:(string, string) Base.Hashtbl.t
  -> github_mapping:(string, string) Base.Hashtbl.t
  -> gitlab_of_github:(string -> string option)
  -> signed:bool
  -> (Cohttp.Response.t * Cohttp_lwt__Body.t) Lwt.t

val pull_request_closed :
     bot_info:Bot_components.Bot_info.bot_info
  -> Bot_components.GitHub_types.issue_info
     Bot_components.GitHub_types.pull_request_info
  -> gitlab_mapping:(string, string) Base.Hashtbl.t
  -> github_mapping:(string, string) Base.Hashtbl.t
  -> gitlab_of_github:(string -> string option)
  -> unit Lwt.t

val pull_request_updated_action :
     bot_info:Bot_components.Bot_info.bot_info
  -> action:Bot_components.GitHub_types.pull_request_action
  -> pr_info:
       Bot_components.GitHub_types.issue_info
       Bot_components.GitHub_types.pull_request_info
  -> gitlab_mapping:(string, string) Base.Hashtbl.t
  -> github_mapping:(string, string) Base.Hashtbl.t
  -> gitlab_of_github:(string -> string option)
  -> signed:bool
  -> (Cohttp.Response.t * Cohttp_lwt__.Body.t) Lwt.t

val adjust_milestone :
     bot_info:Bot_components.Bot_info.bot_info
  -> issue:Bot_components.GitHub_types.issue
  -> sleep_time:float
  -> unit
  -> unit Lwt.t

val project_action :
     bot_info:Bot_components.Bot_info.bot_info
  -> issue:Bot_components.GitHub_types.issue
  -> column_id:int
  -> unit
  -> unit Lwt.t

val push_action :
     bot_info:Bot_components.Bot_info.bot_info
  -> base_ref:string
  -> commits_msg:string list
  -> unit
  -> unit Lwt.t

val repeat_request : string Lwt.t -> string Lwt.t

type build_failure = Warn | Retry | Ignore

val trace_action : repo_full_name:string -> string -> build_failure