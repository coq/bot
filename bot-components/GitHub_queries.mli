val send_graphql_query :
     bot_info:Utils.bot_info
  -> < parse: Yojson.Basic.t -> 'a
     ; query: string
     ; variables: Yojson.Basic.t
     ; .. >
  -> ('a, string) result Lwt.t

type backport_info =
  {backport_to: string; request_inclusion_column: int; backported_column: int}

type full_backport_info =
  {backport_info: backport_info list; rejected_milestone: string}

val get_backport_info :
  bot_info:Utils.bot_info -> string -> full_backport_info option

type project_card =
  { id: GitHub_GraphQL.id
  ; column: GitHub_GraphQL.project_column option
  ; columns: GitHub_GraphQL.project_column list }

val pull_request_milestone_and_cards :
     bot_info:Utils.bot_info
  -> owner:string
  -> repo:string
  -> number:int
  -> (project_card list * GitHub_GraphQL.milestone option, string) result Lwt.t

type mv_card_to_column_input =
  {card_id: GitHub_GraphQL.id; column_id: GitHub_GraphQL.id}

val backported_pr_info :
     bot_info:Utils.bot_info
  -> int
  -> string
  -> mv_card_to_column_input option Lwt.t

val get_pull_request_id_and_milestone :
     bot_info:Utils.bot_info
  -> owner:string
  -> repo:string
  -> number:int
  -> ((string * int * full_backport_info) option, string) result Lwt.t

val get_team_membership :
     bot_info:Utils.bot_info
  -> org:string
  -> team:string
  -> user:string
  -> (bool, string) result Lwt.t

val get_pull_request_refs :
     bot_info:Utils.bot_info
  -> owner:string
  -> repo:string
  -> number:int
  -> (string GitHub_subscriptions.pull_request_info, string) result Lwt.t

val get_pull_request_reviews_refs :
     bot_info:Utils.bot_info
  -> owner:string
  -> repo:string
  -> number:int
  -> (GitHub_subscriptions.pull_request_reviews_info, string) result Lwt.t

val get_file_content :
     bot_info:Utils.bot_info
  -> owner:string
  -> repo:string
  -> branch:string
  -> file_name:string
  -> (string option, string) result Lwt.t

val get_default_branch :
     bot_info:Utils.bot_info
  -> owner:string
  -> repo:string
  -> (string, string) result Lwt.t

type closer_info =
  {pull_request_id: GitHub_GraphQL.id; milestone_id: GitHub_GraphQL.id option}

type 'a closed_by =
  | ClosedByPullRequest of 'a
  | ClosedByCommit
  (* Only used when commit is not associated to a PR *)
  | ClosedByOther

type issue_closer_info =
  { issue_id: GitHub_GraphQL.id
  ; milestone_id: GitHub_GraphQL.id option
  ; closer: closer_info }

val get_issue_closer_info :
     bot_info:Utils.bot_info
  -> GitHub_subscriptions.issue
  -> (issue_closer_info closed_by, string) result Lwt.t

val get_status_check :
     repo_full_name:string
  -> commit:string
  -> context:string
  -> bot_info:Utils.bot_info
  -> bool Lwt.t

val get_cards_in_column :
  int -> bot_info:Utils.bot_info -> (string * int) list Lwt.t
