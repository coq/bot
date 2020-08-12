type bot_info =
  { gitlab_token: string
  ; github_token: string
  ; name: string
  ; email: string
  ; domain: string }

val f : ('a, unit, string) format -> 'a

val string_match : regexp:string -> string -> bool

val print_response : Cohttp.Response.t * Cohttp_lwt.Body.t -> unit Lwt.t

val headers : (string * string) list -> bot_info:bot_info -> Cohttp.Header.t

val send_request :
     body:Cohttp_lwt.Body.t
  -> uri:Uri.t
  -> bot_info:bot_info
  -> (string * string) list
  -> unit Lwt.t

val project_api_preview_header : (string * string) list

val generic_get :
     string
  -> ?header_list:(string * string) list
  -> default:'a
  -> (Yojson.Basic.t -> 'a)
  -> bot_info:bot_info
  -> 'a Lwt.t

val remove_between : string -> int -> int -> string

val trim_comments : string -> string
