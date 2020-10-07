type id = string

type repository_info = {id: int; node_id: string; owner: string; name: string}

type milestone = {title: string; description: string option}

type project_column = {id: string; databaseId: int option}

type merge_method = MERGE | REBASE | SQUASH

type backport_info =
  {backport_to: string; request_inclusion_column: int; backported_column: int}

type full_backport_info =
  {backport_info: backport_info list; rejected_milestone: string}

type project_card =
  {id: id; column: project_column option; columns: project_column list}

type mv_card_to_column_input = {card_id: id; column_id: id}

type closer_info = {pull_request_id: id; milestone_id: id option}

type 'a closed_by =
  | ClosedByPullRequest of 'a
  | ClosedByCommit
  (* Only used when commit is not associated to a PR *)
  | ClosedByOther
  | NoCloseEvent

type issue_closer_info =
  {issue_id: id; milestone_id: id option; closer: closer_info}

type issue = {owner: string; repo: string; number: int}

type comment = {id: id; author: string; created_by_email: bool}

type issue_info =
  { issue: issue
  ; title: string
  ; number: int
  ; id: id
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

type project_card_issue = {issue: issue option; column_id: int}

type comment_info =
  { body: string
  ; author: string
  ; pull_request: issue_info pull_request_info option
  ; issue: issue_info
  ; review_comment: bool
  ; id: id }

type push_info =
  {owner: string; repo: string; base_ref: string; commits_msg: string list}

type check_run_status = COMPLETED | IN_PROGRESS | QUEUED

type check_conclusion =
  | ACTION_REQUIRED
  | CANCELLED
  | FAILURE
  | NEUTRAL
  | SKIPPED
  | STALE
  | SUCCESS
  | TIMED_OUT

type check_suite_info =
  {id: int; node_id: id; head_sha: string; status: check_run_status}

type check_run_info =
  { id: int
  ; node_id: id
  ; head_sha: string
  ; status: check_run_status
  ; check_suite_info: check_suite_info
  ; repository_info: repository_info
  ; external_id: string }

type check_run =
  { id: int
  ; node_id: id
  ; head_sha: string
  ; name: string
  ; status: string
  ; url: string
  ; title: string
  ; text: string }

type check_tab_info = {name: string; summary: string option; text: string option}
