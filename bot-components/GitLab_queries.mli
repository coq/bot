val get_build_trace :
  bot_info:Bot_info.t -> project_id:int -> build_id:int -> string Lwt.t

val get_retry_nb :
     bot_info:Bot_info.t
  -> full_name:string
  -> build_id:int
  -> build_name:string
  -> (int, string) Lwt_result.t
