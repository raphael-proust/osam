(* In Text, all the exposed offsets (e.g., in length, sub, &c.) are in
 * number of codes, even if internally it might use byte offsets. *)

type t

(* [empty] is a text with 0 codepoints inside. *)
val empty: t

(* [from_string s] checks the UTF8 validity of [s] and returns text if it
 * is valid. [to_string t] is the inverse. *)
val from_string: string -> t option
val to_string: t -> string

(* [sub t cursor] is the text included in the range described by the
 * cursor. *)
val sub: t -> Cursor.t -> t

(* [legnth t] is the number of codepoints in [t]. *)
val length: t -> int

(* [change t (c,r)] modifies [t]. It replaces the part described by [c]
 * with the content of [r]. *)
val change: t -> (Cursor.t * t) -> t

(* [hook f init t s l] folds [f] over chunks of strings that represent the
 * substring of [t] starting at code [s] and [l] codes long. The arguments of
 * [f] are in bytes.
 *
 * TODO: this is potentially confusing specs (interleaving bytes and codes,
 * maybe use type aliases. *)
val hook: ('a -> string -> int -> int -> 'a) -> 'a -> t -> int -> int -> 'a
