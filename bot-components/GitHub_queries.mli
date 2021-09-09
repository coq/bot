open GitHub_GraphQL
open GitHub_types

val get_pull_request_milestone_and_cards :
     bot_info:Bot_info.t
  -> owner:string
  -> repo:string
  -> number:int
  -> (project_card list * milestone option, string) result Lwt.t

val extract_backport_info :
  bot_info:Bot_info.t -> string -> full_backport_info option

val get_backported_pr_info :
     bot_info:Bot_info.t
  -> int
  -> string
  -> (mv_card_to_column_input option, string) result Lwt.t

val get_pull_request_id_and_milestone :
     bot_info:Bot_info.t
  -> owner:string
  -> repo:string
  -> number:int
  -> ((ID.t * int * full_backport_info) option, string) result Lwt.t

val get_milestone_merged_prs :
     bot_info:Bot_info.t
  -> owner:string
  -> repo:string
  -> number:int
  -> (pr_id_and_cards list, string) result Lwt.t

val get_team_membership :
     bot_info:Bot_info.t
  -> org:string
  -> team:string
  -> user:string
  -> (bool, string) result Lwt.t

val get_pull_request_refs :
     bot_info:Bot_info.t
  -> owner:string
  -> repo:string
  -> number:int
  -> (ID.t pull_request_info, string) result Lwt.t

val get_pull_request_reviews_refs :
     bot_info:Bot_info.t
  -> owner:string
  -> repo:string
  -> number:int
  -> (pull_request_reviews_info, string) result Lwt.t

val get_file_content :
     bot_info:Bot_info.t
  -> owner:string
  -> repo:string
  -> branch:string
  -> file_name:string
  -> (string option, string) result Lwt.t

val get_default_branch :
     bot_info:Bot_info.t
  -> owner:string
  -> repo:string
  -> (string, string) result Lwt.t

val get_issue_closer_info :
     bot_info:Bot_info.t
  -> issue
  -> (issue_closer_info closed_by, string) result Lwt.t

val get_repository_id :
     bot_info:Bot_info.t
  -> owner:string
  -> repo:string
  -> (ID.t, string) result Lwt.t

val get_status_check :
     bot_info:Bot_info.t
  -> owner:string
  -> repo:string
  -> commit:string
  -> context:string
  -> (bool, string) result Lwt.t

(* N.B. the [bool option] is [None] for in progress and [Some success_status] for finished *)
(* TODO: Should we use a type for this instead of [bool option]? *)
type base_and_head_checks_info =
  { pr_id: ID.t
  ; base_checks: (check_tab_info * bool option, string * string) result list
  ; head_checks: (check_tab_info * bool option, string * string) result list
  ; draft: bool
  ; labels: string list }

val get_base_and_head_checks :
     bot_info:Bot_info.t
  -> owner:string
  -> repo:string
  -> pr_number:int
  -> base:string
  -> head:string
  -> (base_and_head_checks_info, string) result Lwt.t

val get_cards_in_column :
  int -> bot_info:Bot_info.t -> ((string * int) list, string) result Lwt.t

val get_open_pull_requests_with_label :
     bot_info:Bot_info.t
  -> owner:string
  -> repo:string
  -> label:string
  -> ((ID.t * int) list, string) result Lwt.t

val get_label :
     bot_info:Bot_info.t
  -> owner:string
  -> repo:string
  -> label:string
  -> (ID.t option, string) result Lwt.t

val get_pull_request_label_timeline :
     bot_info:Bot_info.t
  -> owner:string
  -> repo:string
  -> pr_number:int
  -> ((bool * string * float) list, string) result Lwt.t

val get_pull_request_labels :
     bot_info:Bot_info.t
  -> owner:string
  -> repo:string
  -> pr_number:int
  -> (string list, string) result Lwt.t
