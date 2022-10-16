(** Bench result table together with optional line ccount *)
module Table : sig
  type t = {table: string; count: int option}
end

(** Bench results *)
type t =
  { summary_table: Table.t option
  ; slow_table: Table.t option
  ; fast_table: Table.t option
  ; slow_table_pdiff: Table.t option
  ; fast_table_pdiff: Table.t option }

val bench_text : t -> string
(** Generate the markdown for the text results as would appear in the GitHub
  sumamry *)

val bench_comment_text :
  gitlab_url:string -> check_url:(string, 'a) result -> t -> string
(** Generate the markdown for the text results as would appear in the GitHub
  comment *)
