type msg =
  | IssueOpened of GitHub_types.issue_info
  | IssueClosed of GitHub_types.issue_info
  | RemovedFromProject of GitHub_types.project_card_issue
  | PullRequestUpdated of
      GitHub_types.pull_request_action
      * GitHub_types.issue_info GitHub_types.pull_request_info
  | BranchCreated of GitHub_types.remote_ref_info
  | TagCreated of GitHub_types.remote_ref_info
  | CommentCreated of GitHub_types.comment_info
  | CheckRunCreated of GitHub_types.check_run_info
  | PushEvent of GitHub_types.push_info
  | UnsupportedEvent of string

val receive_github :
  secret:string -> Cohttp.Header.t -> string -> (bool * msg, string) result
