open GitHub_types

type msg =
  | IssueOpened of issue_info
  | IssueClosed of issue_info
  | RemovedFromProject of project_card_issue
  | PullRequestUpdated of pull_request_action * issue_info pull_request_info
  | BranchCreated of remote_ref_info
  | TagCreated of remote_ref_info
  | CommentCreated of comment_info
  | CheckRunCreated of check_run_info
  | CheckRunUpdated of check_run_info
  | CheckRunReRequested of check_run_info
  | CheckSuiteCreated of check_suite_info
  | CheckSuiteRequested of check_suite_info
  | PushEvent of push_info
  | UnsupportedEvent of string

val receive_github :
  secret:string -> Cohttp.Header.t -> string -> (bool * msg, string) result
