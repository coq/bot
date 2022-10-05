type t

val to_string : t -> string

val of_string : string -> t

val of_json : Yojson.Basic.t -> t

val equal : t -> t -> bool
