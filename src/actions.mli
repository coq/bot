val job_action :
     Bot_components.GitLab_subscriptions.job_info
  -> bot_info:Bot_components.Utils.bot_info
  -> github_of_gitlab:(string -> string option)
  -> unit

val pipeline_action :
     Bot_components.GitLab_subscriptions.pipeline_info
  -> bot_info:Bot_components.Utils.bot_info
  -> github_of_gitlab:(string -> string option)
  -> unit Lwt.t

val coq_bug_minimizer_results_action :
     string
  -> bot_info:Bot_components.Utils.bot_info
  -> (Cohttp.Response.t * Cohttp_lwt__Body.t) Lwt.t

val merge_pull_request :
     comment_info:Bot_components.GitHub_subscriptions.comment_info
  -> bot_info:Bot_components.Utils.bot_info
  -> unit Lwt.t

val pull_request_updated :
     Bot_components.GitHub_subscriptions.issue_info
     Bot_components.GitHub_subscriptions.pull_request_info
  -> unit
  -> bot_info:Bot_components.Utils.bot_info
  -> gitlab_mapping:(string, string) Base.Hashtbl.t
  -> github_mapping:(string, string) Base.Hashtbl.t
  -> gitlab_of_github:(string -> string option)
  -> (unit, string) Lwt_result.t

val run_ci :
     comment_info:Bot_components.GitHub_subscriptions.comment_info
  -> bot_info:Bot_components.Utils.bot_info
  -> gitlab_mapping:(string, string) Base.Hashtbl.t
  -> github_mapping:(string, string) Base.Hashtbl.t
  -> gitlab_of_github:(string -> string option)
  -> signed:bool
  -> (Cohttp.Response.t * Cohttp_lwt__Body.t) Lwt.t

val pull_request_closed :
     Bot_components.GitHub_subscriptions.issue_info
     Bot_components.GitHub_subscriptions.pull_request_info
  -> unit
  -> bot_info:Bot_components.Utils.bot_info
  -> gitlab_mapping:(string, string) Base.Hashtbl.t
  -> github_mapping:(string, string) Base.Hashtbl.t
  -> gitlab_of_github:(string -> string option)
  -> unit Lwt.t

val pull_request_updated_action :
     action:Bot_components.GitHub_subscriptions.pull_request_action
  -> pr_info:
       Bot_components.GitHub_subscriptions.issue_info
       Bot_components.GitHub_subscriptions.pull_request_info
  -> bot_info:Bot_components.Utils.bot_info
  -> gitlab_mapping:(string, string) Base.Hashtbl.t
  -> github_mapping:(string, string) Base.Hashtbl.t
  -> gitlab_of_github:(string -> string option)
  -> signed:bool
  -> (Cohttp.Response.t * Cohttp_lwt__.Body.t) Lwt.t

val adjust_milestone :
     issue:Bot_components.GitHub_subscriptions.issue
  -> sleep_time:float
  -> bot_info:Bot_components.Utils.bot_info
  -> unit
  -> unit Lwt.t

val project_action :
     issue:Bot_components.GitHub_subscriptions.issue
  -> column_id:int
  -> bot_info:Bot_components.Utils.bot_info
  -> unit
  -> unit Lwt.t

val push_action :
     base_ref:string
  -> commits_msg:string list
  -> bot_info:Bot_components.Utils.bot_info
  -> unit
  -> unit Lwt.t

val repeat_request : string Lwt.t -> string Lwt.t

type build_failure = Warn | Retry | Ignore

val trace_action : repo_full_name:string -> string -> build_failure
