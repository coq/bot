val gitlab_ref :
     bot_info:Bot_components.Bot_info.t
  -> issue:Bot_components.GitHub_types.issue
  -> github_mapping:(string, string) Base.Hashtbl.t
  -> gitlab_mapping:(string, string) Base.Hashtbl.t
  -> Bot_components.GitHub_types.remote_ref_info Lwt.t

val ( |&& ) : string -> string -> string

val execute_cmd : string -> (unit, string) result Lwt.t

val git_fetch :
  ?force:bool -> Bot_components.GitHub_types.remote_ref_info -> string -> string

val git_push :
     ?force:bool
  -> remote_ref:Bot_components.GitHub_types.remote_ref_info
  -> local_ref:string
  -> string

val git_delete :
  remote_ref:Bot_components.GitHub_types.remote_ref_info -> string

val git_make_ancestor :
     pr_title:string
  -> pr_number:int
  -> base:string
  -> string
  -> (bool, string) result Lwt.t

val init_git_bare_repository : bot_info:Bot_components.Bot_info.t -> unit Lwt.t

val git_coq_bug_minimizer :
     bot_info:Bot_components.Bot_info.t
  -> script:string
  -> comment_thread_id:string
  -> comment_author:string
  -> owner:string
  -> repo:string
  -> (unit, string) result Lwt.t

val git_run_ci_minimization :
     bot_info:Bot_components.Bot_info.t
  -> comment_thread_id:string
  -> owner:string
  -> repo:string
  -> docker_image:string
  -> target:string
  -> opam_switch:string
  -> failing_urls:string
  -> passing_urls:string
  -> base:string
  -> head:string
  -> (unit, string) result Lwt.t
