val toml_of_file : string -> TomlTypes.table

val toml_of_string : string -> TomlTypes.table

val subkey_value : TomlTypes.table -> string -> string -> string option

val string_of_mapping : (string, string) Base.Hashtbl.t -> string

val port : TomlTypes.table -> int

val gitlab_access_token : TomlTypes.table -> string

val github_access_token : TomlTypes.table -> string

val github_webhook_secret : TomlTypes.table -> string

val gitlab_webhook_secret : TomlTypes.table -> string

val bot_name : TomlTypes.table -> string

val bot_domain : TomlTypes.table -> string

val bot_email : TomlTypes.table -> string

val make_mappings_table :
     TomlTypes.value TomlTypes.Table.t
  -> (string, string) Base.Hashtbl.t * (string, string) Base.Hashtbl.t
