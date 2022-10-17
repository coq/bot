val toml_of_file : string -> Toml.Types.table

val toml_of_string : string -> Toml.Types.table

val subkey_value : Toml.Types.table -> string -> string -> string option

val string_of_mapping : (string, string) Base.Hashtbl.t -> string

val port : Toml.Types.table -> int

val gitlab_access_token : Toml.Types.table -> string

val github_access_token : Toml.Types.table -> string

val github_webhook_secret : Toml.Types.table -> string

val gitlab_webhook_secret : Toml.Types.table -> string

val daily_schedule_secret : Toml.Types.table -> string

val bot_name : Toml.Types.table -> string

val bot_domain : Toml.Types.table -> string

val bot_email : Toml.Types.table -> string

val github_app_id : Toml.Types.table -> int

val github_private_key : unit -> Mirage_crypto_pk.Rsa.priv

val make_mappings_table :
     Toml.Types.value Toml.Types.Table.t
  -> (string, string) Base.Hashtbl.t * (string, string) Base.Hashtbl.t
