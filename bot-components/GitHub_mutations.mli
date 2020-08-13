val mv_card_to_column :
     bot_info:Bot_info.t
  -> GitHub_types.mv_card_to_column_input
  -> unit Lwt.t

val post_comment :
     bot_info:Bot_info.t
  -> id:GitHub_types.id
  -> message:string
  -> unit Lwt.t

val merge_pull_request :
     bot_info:Bot_info.t
  -> ?merge_method:GitHub_types.merge_method
  -> ?commit_headline:string
  -> ?commit_body:string
  -> pr_id:GitHub_types.id
  -> unit Lwt.t

val reflect_pull_request_milestone :
  bot_info:Bot_info.t -> GitHub_types.issue_closer_info -> unit Lwt.t

val add_rebase_label :
  bot_info:Bot_info.t -> GitHub_types.issue -> unit Lwt.t

val remove_rebase_label :
  bot_info:Bot_info.t -> GitHub_types.issue -> unit Lwt.t

val update_milestone :
  bot_info:Bot_info.t -> string -> GitHub_types.issue -> unit Lwt.t

val remove_milestone :
  bot_info:Bot_info.t -> GitHub_types.issue -> unit Lwt.t

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
