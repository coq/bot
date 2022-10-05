open GitHub_types

val mv_card_to_column :
  bot_info:Bot_info.t -> mv_card_to_column_input -> unit Lwt.t

val post_comment :
     bot_info:Bot_info.t
  -> id:GitHub_ID.t
  -> message:string
  -> (string, string) result Lwt.t

val report_on_posting_comment : (string, string) result -> unit Lwt.t

val close_pull_request : bot_info:Bot_info.t -> pr_id:GitHub_ID.t -> unit Lwt.t

val merge_pull_request :
     bot_info:Bot_info.t
  -> ?merge_method:merge_method
  -> ?commit_headline:string
  -> ?commit_body:string
  -> pr_id:GitHub_ID.t
  -> unit
  -> unit Lwt.t

val create_check_run :
     bot_info:Bot_info.t
  -> ?conclusion:check_conclusion
  -> name:string
  -> repo_id:GitHub_ID.t
  -> head_sha:string
  -> status:check_run_status
  -> details_url:string
  -> title:string
  -> ?text:string
  -> summary:string
  -> ?external_id:string
  -> unit
  -> (string, string) result Lwt.t

val update_check_run :
     bot_info:Bot_info.t
  -> check_run_id:GitHub_ID.t
  -> repo_id:GitHub_ID.t
  -> conclusion:check_conclusion
  -> ?details_url:string
  -> title:string
  -> ?text:string
  -> summary:string
  -> unit
  -> unit Lwt.t

val reflect_pull_request_milestone :
  bot_info:Bot_info.t -> issue_closer_info -> unit Lwt.t

val add_labels :
     bot_info:Bot_info.t
  -> labels:GitHub_ID.t list
  -> pr_id:GitHub_ID.t
  -> unit Lwt.t

val remove_labels :
     bot_info:Bot_info.t
  -> labels:GitHub_ID.t list
  -> pr_id:GitHub_ID.t
  -> unit Lwt.t

val update_milestone : bot_info:Bot_info.t -> string -> issue -> unit Lwt.t

val remove_milestone : bot_info:Bot_info.t -> issue -> unit Lwt.t

val send_status_check :
     bot_info:Bot_info.t
  -> repo_full_name:string
  -> commit:string
  -> state:string
  -> url:string
  -> context:string
  -> description:string
  -> unit Lwt.t

val add_pr_to_column :
  bot_info:Bot_info.t -> pr_id:int -> column_id:int -> unit Lwt.t
