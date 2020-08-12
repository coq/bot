val send_graphql_query :
     bot_info:Utils.bot_info
  -> < parse: Yojson.Basic.t -> 'a
     ; query: string
     ; variables: Yojson.Basic.t
     ; .. >
  -> ('a, string) result Lwt.t
