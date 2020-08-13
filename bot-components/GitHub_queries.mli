val pull_request_milestone_and_cards :
     bot_info:Bot_info.bot_info
  -> owner:string
  -> repo:string
  -> number:int
  -> ( GitHub_types.project_card list * GitHub_GraphQL.milestone option
     , string )
     result
     Lwt.t

val backported_pr_info :
     bot_info:Bot_info.bot_info
  -> int
  -> string
  -> GitHub_types.mv_card_to_column_input option Lwt.t

val get_pull_request_id_and_milestone :
     bot_info:Bot_info.bot_info
  -> owner:string
  -> repo:string
  -> number:int
  -> ((string * int * GitHub_types.full_backport_info) option, string) result
     Lwt.t

val get_team_membership :
     bot_info:Bot_info.bot_info
  -> org:string
  -> team:string
  -> user:string
  -> (bool, string) result Lwt.t

val get_pull_request_refs :
     bot_info:Bot_info.bot_info
  -> owner:string
  -> repo:string
  -> number:int
  -> (string GitHub_types.pull_request_info, string) result Lwt.t

val get_pull_request_reviews_refs :
     bot_info:Bot_info.bot_info
  -> owner:string
  -> repo:string
  -> number:int
  -> (GitHub_types.pull_request_reviews_info, string) result Lwt.t

val get_file_content :
     bot_info:Bot_info.bot_info
  -> owner:string
  -> repo:string
  -> branch:string
  -> file_name:string
  -> (string option, string) result Lwt.t

val get_default_branch :
     bot_info:Bot_info.bot_info
  -> owner:string
  -> repo:string
  -> (string, string) result Lwt.t

val get_issue_closer_info :
     bot_info:Bot_info.bot_info
  -> GitHub_types.issue
  -> (GitHub_types.issue_closer_info GitHub_types.closed_by, string) result
     Lwt.t

val get_status_check :
     repo_full_name:string
  -> commit:string
  -> context:string
  -> bot_info:Bot_info.bot_info
  -> bool Lwt.t

val get_cards_in_column :
  int -> bot_info:Bot_info.bot_info -> (string * int) list Lwt.t
