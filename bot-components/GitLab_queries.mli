val get_build_trace :
     bot_info:Bot_info.t
  -> gitlab_domain:string
  -> project_id:int
  -> build_id:int
  -> (string, string) Lwt_result.t

val get_retry_nb :
     bot_info:Bot_info.t
  -> gitlab_domain:string
  -> full_name:string
  -> build_id:int
  -> build_name:string
  -> (int, string) Lwt_result.t
