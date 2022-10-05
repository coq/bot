type t = ID of string

let to_string (ID s) = s

let of_string s = ID s

let of_json json = ID (Yojson.Basic.Util.to_string json)

let equal (ID a) (ID b) = String.equal a b
