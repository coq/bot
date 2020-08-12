type id = string

type merge_method = MERGE | REBASE | SQUASH

type backport_info =
  {backport_to: string; request_inclusion_column: int; backported_column: int}

type full_backport_info =
  {backport_info: backport_info list; rejected_milestone: string}

type project_card =
  { id: id
  ; column: GitHub_GraphQL.project_column option
  ; columns: GitHub_GraphQL.project_column list }

type mv_card_to_column_input = {card_id: id; column_id: id}

type closer_info = {pull_request_id: id; milestone_id: id option}

type 'a closed_by =
  | ClosedByPullRequest of 'a
  | ClosedByCommit
  (* Only used when commit is not associated to a PR *)
  | ClosedByOther

type issue_closer_info =
  {issue_id: id; milestone_id: id option; closer: closer_info}

type issue = {owner: string; repo: string; number: int}

type comment = {id: id; author: string; created_by_email: bool}

type issue_info =
  { issue: issue
  ; title: string
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

type check_run_info = {id: int; node_id: id; url: string}

type push_info = {base_ref: string; commits_msg: string list}
