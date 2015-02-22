(* In Text, all the exposed offsets (e.g., in length, sub, &c.) are in
 * number of codes, even if internally it might use byte offsets. *)

type t

val length: t -> int

(* [from_string s] checks the UTF8 validity of [s] and returns text if it
 * is valid. [to_string t] is the inverse. *)
val from_string: string -> t
val to_string: t -> string

(* [sub t cursor] is the text included in the range described by the
 * cursor. *)
val sub: t -> Cursor.t -> t

val uchar: t -> int -> Uutf.uchar

(* gives the offset of the [n]th newline character *)
val newline: t -> int -> int

(* [hook f init t s l] folds [f] over chunks of strings that represent the
 * substring of [t] starting at code [s] and [l] codes long. The arguments of
 * [f] are in bytes.
 *
 * TODO: this is potentially confusing specs (interleaving bytes and codes,
 * maybe use type aliases. *)
val hook :
	?o:int -> ?l:int ->
	f:('a -> string -> int -> int -> 'a) -> acc:'a ->
	t ->
	'a
