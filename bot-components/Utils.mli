val f : ('a, unit, string) format -> 'a

val string_match : regexp:string -> string -> bool

val headers : (string * string) list -> string -> Cohttp.Header.t

val print_response : Cohttp.Response.t * Cohttp_lwt.Body.t -> unit Lwt.t

val send_request :
     body:Cohttp_lwt.Body.t
  -> uri:Uri.t
  -> (string * string) list
  -> string
  -> unit Lwt.t

val project_api_preview_header : (string * string) list

val app_api_preview_header : (string * string) list

val api_json_header : (string * string) list

val github_header : Bot_info.t -> (string * string) list

val generic_get :
     bot_info:Bot_info.t
  -> string
  -> ?header_list:(string * string) list
  -> (Yojson.Basic.t -> 'a)
  -> ('a, string) result Lwt.t

val generic_get_zip :
     bot_info:Bot_info.t
  -> string
  -> ?header_list:(string * string) list
  -> ((Zip.entry * string) list -> 'a)
  -> ('a, string * string * string * string) result Lwt.t
