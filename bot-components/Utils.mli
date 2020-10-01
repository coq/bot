val f : ('a, unit, string) format -> 'a

val string_match : regexp:string -> string -> bool

val headers : bot_info:Bot_info.t -> (string * string) list -> Cohttp.Header.t

val print_response : Cohttp.Response.t * Cohttp_lwt.Body.t -> unit Lwt.t

val send_request :
     bot_info:Bot_info.t
  -> body:Cohttp_lwt.Body.t
  -> uri:Uri.t
  -> (string * string) list
  -> unit Lwt.t

val project_api_preview_header : (string * string) list

val app_api_preview_header : (string * string) list

val generic_get :
     bot_info:Bot_info.t
  -> string
  -> ?header_list:(string * string) list
  -> (Yojson.Basic.t -> 'a)
  -> ('a, string) result Lwt.t
