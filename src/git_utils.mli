val gitlab_repo :
  bot_info:Bot_components.Bot_info.t -> gitlab_full_name:string -> string

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
  -> ?options:string
  -> remote_ref:Bot_components.GitHub_types.remote_ref_info
  -> local_ref:string
  -> unit
  -> string

val git_delete :
  remote_ref:Bot_components.GitHub_types.remote_ref_info -> string

val git_make_ancestor :
     pr_title:string
  -> pr_number:int
  -> base:string
  -> string
  -> (bool, string) result Lwt.t

val git_test_modified :
  base:string -> head:string -> string -> (bool, string) result Lwt.t

val init_git_bare_repository : bot_info:Bot_components.Bot_info.t -> unit Lwt.t

val git_coq_bug_minimizer :
     bot_info:Bot_components.Bot_info.t
  -> script:string
  -> comment_thread_id:Bot_components.GitHub_ID.t
  -> comment_author:string
  -> owner:string
  -> repo:string
  -> coq_version:string
  -> ocaml_version:string
  -> minimizer_extra_arguments:string list
  -> (unit, string) result Lwt.t

val git_run_ci_minimization :
     bot_info:Bot_components.Bot_info.t
  -> comment_thread_id:Bot_components.GitHub_ID.t
  -> owner:string
  -> repo:string
  -> pr_number:string
  -> docker_image:string
  -> target:string
  -> opam_switch:string
  -> failing_urls:string
  -> passing_urls:string
  -> base:string
  -> head:string
  -> minimizer_extra_arguments:string list
  -> bug_file_name:string option
  -> (unit, string) result Lwt.t
