type api = GitHub | GitLab of string

val send_graphql_query :
     bot_info:Bot_info.t
  -> ?extra_headers:(string * string) list
  -> api:api
  -> query:string
  -> parse:(Yojson.Basic.t -> 'a)
  -> Yojson.Basic.t
  -> ('a, string) result Lwt.t
