val issue_info_of_json :
  ?issue_json:Yojson.Basic.t -> Yojson.Basic.t -> GitHub_types.issue_info

val commit_info_of_json : Yojson.Basic.t -> GitHub_types.commit_info

val pull_request_info_of_json :
  Yojson.Basic.t -> GitHub_types.issue_info GitHub_types.pull_request_info

val project_card_of_json :
  Yojson.Basic.t -> (GitHub_types.project_card_issue, string) result

val comment_info_of_json :
  ?review_comment:bool -> Yojson.Basic.t -> GitHub_types.comment_info

val check_run_info_of_json : Yojson.Basic.t -> GitHub_types.check_run_info

val push_event_info_of_json : Yojson.Basic.t -> GitHub_types.push_info

val receive_github :
     secret:string
  -> Cohttp.Header.t
  -> string
  -> (bool * GitHub_types.msg, string) result
