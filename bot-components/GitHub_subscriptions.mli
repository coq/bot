type issue = {owner: string; repo: string; number: int}

type comment = {id: string; author: string; created_by_email: bool}

type issue_info =
  { issue: issue
  ; title: string
  ; id: string
  ; user: string
  ; labels: string list
  ; milestoned: bool
  ; pull_request: bool
  ; body: string option
  ; assignees: string list }

type remote_ref_info = {repo_url: string; name: string}

type commit_info = {branch: remote_ref_info; sha: string}

type review_decision = CHANGES_REQUESTED | APPROVED | REVIEW_REQUIRED | NONE

type pull_request_action =
  | PullRequestOpened
  | PullRequestClosed
  | PullRequestReopened
  | PullRequestSynchronized

type 'a pull_request_info =
  { issue: 'a
  ; base: commit_info
  ; head: commit_info
  ; merged: bool
  ; last_commit_message: string option }

type pull_request_reviews_info =
  { baseRef: string
  ; files: string list
  ; approved_reviews: string list
  ; comment_reviews: string list
  ; review_decision: review_decision
  ; last_comments: comment list }

type project_card = {issue: issue option; column_id: int}

type comment_info =
  { body: string
  ; author: string
  ; pull_request: issue_info pull_request_info option
  ; issue: issue_info
  ; review_comment: bool
  ; id: string }

type check_run_info = {id: int; node_id: string; url: string}

type push_info = {base_ref: string; commits_msg: string list}

type msg =
  | NoOp of string
  | IssueOpened of issue_info
  | IssueClosed of issue_info
  | RemovedFromProject of project_card
  | PullRequestUpdated of pull_request_action * issue_info pull_request_info
  | BranchCreated of remote_ref_info
  | TagCreated of remote_ref_info
  | CommentCreated of comment_info
  | CheckRunCreated of check_run_info
  | PushEvent of push_info

val issue_info_of_json :
  ?issue_json:Yojson.Basic.t -> Yojson.Basic.t -> issue_info

val commit_info_of_json : Yojson.Basic.t -> commit_info

val pull_request_info_of_json : Yojson.Basic.t -> issue_info pull_request_info

val project_card_of_json : Yojson.Basic.t -> (project_card, string) result

val comment_info_of_json :
  ?review_comment:bool -> Yojson.Basic.t -> comment_info

val check_run_info_of_json : Yojson.Basic.t -> check_run_info

val push_event_info_of_json : Yojson.Basic.t -> push_info

val receive_github :
  secret:string -> Cohttp.Header.t -> string -> (bool * msg, string) result
