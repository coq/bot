val mv_card_to_column :
     bot_info:Utils.bot_info
  -> GitHub_queries.mv_card_to_column_input
  -> unit Lwt.t

val post_comment :
  bot_info:Utils.bot_info -> id:string -> message:string -> unit Lwt.t

val merge_pull_request :
     ?merge_method:[< `MERGE | `REBASE | `SQUASH]
  -> ?commit_headline:string
  -> ?commit_body:string
  -> bot_info:Utils.bot_info
  -> pr_id:string
  -> unit Lwt.t

val reflect_pull_request_milestone :
  bot_info:Utils.bot_info -> GitHub_queries.issue_closer_info -> unit Lwt.t

val add_rebase_label :
  GitHub_subscriptions.issue -> bot_info:Utils.bot_info -> unit Lwt.t

val remove_rebase_label :
  GitHub_subscriptions.issue -> bot_info:Utils.bot_info -> unit Lwt.t

val update_milestone :
  string -> GitHub_subscriptions.issue -> bot_info:Utils.bot_info -> unit Lwt.t

val remove_milestone :
  GitHub_subscriptions.issue -> bot_info:Utils.bot_info -> unit Lwt.t

val send_status_check :
     repo_full_name:string
  -> commit:string
  -> state:string
  -> url:string
  -> context:string
  -> description:string
  -> bot_info:Utils.bot_info
  -> unit Lwt.t

val add_pr_to_column : int -> int -> bot_info:Utils.bot_info -> unit Lwt.t
