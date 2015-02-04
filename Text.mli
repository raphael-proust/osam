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

(* [nth t i] is the ith codepoint of [t] *)
val nth: t -> int -> Uutf.uchar
(* [legnth t] is the number of codepoints in [t]. *)
val length: t -> int

(* [change t (c,r)] modifies [t]. It replaces the part described by [c]
 * with the content of [r]. *)
val change: t -> (Cursor.t * t) -> t

(*TODO: let fold take a subpart of the string (so that we can regexp)
 * also let fold go backwards (so that we can backwards regexp).*)
(* [fold t a f] folds over all the codepoints of [t] using [f] with the
 * initial input [a]. *)
val fold: ('a -> Uutf.uchar -> 'a) -> 'a -> t -> 'a

(* [cat ts] is the concatenation of all the texts in [ts]. *)
val cat: t list -> t

