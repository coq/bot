type bot_info =
  { gitlab_token: string
  ; github_token: string
  ; name: string
  ; email: string
  ; domain: string }

val f : ('a, unit, string) format -> 'a

val string_match : regexp:string -> string -> bool

val print_response : Cohttp.Response.t * Cohttp_lwt.Body.t -> unit Lwt.t

val headers : bot_info:bot_info -> (string * string) list -> Cohttp.Header.t

val send_request :
     bot_info:bot_info
  -> body:Cohttp_lwt.Body.t
  -> uri:Uri.t
  -> (string * string) list
  -> unit Lwt.t

val get_backport_info :
  bot_info:bot_info -> string -> GitHub_types.full_backport_info option

val project_api_preview_header : (string * string) list

val generic_get :
     bot_info:bot_info
  -> string
  -> ?header_list:(string * string) list
  -> default:'a
  -> (Yojson.Basic.t -> 'a)
  -> 'a Lwt.t

val remove_between : string -> int -> int -> string

val trim_comments : string -> string
