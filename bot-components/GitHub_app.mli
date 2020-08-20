val get_installation_token :
     bot_info:Bot_info.t
  -> key:Mirage_crypto_pk.Rsa.priv
  -> app_id:int
  -> owner:string
  -> repo:string
  -> (string, string) result Lwt.t

val get_installation_token_org :
     bot_info:Bot_info.t
  -> key:Mirage_crypto_pk.Rsa.priv
  -> app_id:int
  -> org:string
  -> (string, string) result Lwt.t
