val pr_from_branch : string -> int option * string

val job_info_of_json : Yojson.Basic.t -> GitLab_types.job_info

val pipeline_info_of_json : Yojson.Basic.t -> GitLab_types.pipeline_info

val receive_gitlab :
     secret:string
  -> Cohttp.Header.t
  -> string
  -> (bool * GitLab_types.msg, string) result
