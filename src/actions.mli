val job_action :
     bot_info:Bot_components.Bot_info.t
  -> Bot_components.GitLab_types.job_info
  -> github_of_gitlab:(string -> string option)
  -> unit Lwt.t

val pipeline_action :
     bot_info:Bot_components.Bot_info.t
  -> Bot_components.GitLab_types.pipeline_info
  -> github_of_gitlab:(string -> string option)
  -> app_id:int
  -> unit Lwt.t

val coq_bug_minimizer_results_action :
     bot_info:Bot_components.Bot_info.t
  -> coq_minimizer_repo_token:Bot_components.Bot_info.github_token
  -> key:Mirage_crypto_pk.Rsa.priv
  -> app_id:int
  -> string
  -> (Cohttp.Response.t * Cohttp_lwt__Body.t) Lwt.t

val merge_pull_request_action :
     bot_info:Bot_components.Bot_info.t
  -> comment_info:Bot_components.GitHub_types.comment_info
  -> unit Lwt.t

val run_ci_action :
     bot_info:Bot_components.Bot_info.t
  -> comment_info:Bot_components.GitHub_types.comment_info
  -> gitlab_mapping:(string, string) Base.Hashtbl.t
  -> github_mapping:(string, string) Base.Hashtbl.t
  -> gitlab_of_github:(string -> string option)
  -> signed:bool
  -> (Cohttp.Response.t * Cohttp_lwt__Body.t) Lwt.t

val pull_request_closed_action :
     bot_info:Bot_components.Bot_info.t
  -> Bot_components.GitHub_types.issue_info
     Bot_components.GitHub_types.pull_request_info
  -> gitlab_mapping:(string, string) Base.Hashtbl.t
  -> github_mapping:(string, string) Base.Hashtbl.t
  -> gitlab_of_github:(string -> string option)
  -> unit Lwt.t

val pull_request_updated_action :
     bot_info:Bot_components.Bot_info.t
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
     bot_info:Bot_components.Bot_info.t
  -> issue:Bot_components.GitHub_types.issue
  -> sleep_time:float
  -> unit Lwt.t

val project_action :
     bot_info:Bot_components.Bot_info.t
  -> issue:Bot_components.GitHub_types.issue
  -> column_id:int
  -> unit Lwt.t

val push_action :
     bot_info:Bot_components.Bot_info.t
  -> base_ref:string
  -> commits_msg:string list
  -> unit Lwt.t
