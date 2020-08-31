val send_graphql_query :
     bot_info:Bot_info.t
  -> ?extra_headers:(string * string) list
  -> < parse: Yojson.Basic.t -> 'a
     ; query: string
     ; variables: Yojson.Basic.t
     ; .. >
  -> ('a, string) result Lwt.t
