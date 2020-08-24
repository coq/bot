open Bot_components

val installation_tokens : (string, string * float) Base.Hashtbl.t

val action_as_github_app :
     bot_info:Bot_info.t
  -> key:Mirage_crypto_pk.Rsa.priv
  -> app_id:int
  -> owner:string
  -> repo:string
  -> (bot_info:Bot_info.t -> 'a Lwt.t)
  -> 'a Lwt.t
