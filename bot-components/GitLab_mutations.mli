val retry_job :
  bot_info:Bot_info.t -> project_id:int -> build_id:int -> unit Lwt.t

val generic_retry : bot_info:Bot_info.t -> url_part:string -> unit Lwt.t

val play_job :
     bot_info:Bot_info.t
  -> project_id:int
  -> build_id:int
  -> ?key_value_pairs:(string * string) list
  -> unit
  -> unit Lwt.t
