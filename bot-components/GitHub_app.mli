val get_installation_token :
     bot_info:Bot_info.t
  -> key:Mirage_crypto_pk.Rsa.priv
  -> app_id:int
  -> install_id:int
  -> (string * float, string) result Lwt.t

val get_installations :
     bot_info:Bot_info.t
  -> key:Mirage_crypto_pk.Rsa.priv
  -> app_id:int
  -> ((string * int) list, string) result Lwt.t
