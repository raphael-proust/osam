
(* a cursor describes a range of characters (like the dot in acme) *)
type t

val mk_point: int -> t
val mk_relative: start:int -> offset:int -> t
val mk_absolute: start:int -> end_:int -> t

val range: t -> t -> t

val relative: t -> (int * int)
val absolute: t -> (int * int)
val start: t -> int
val end_: t -> int
val length: t -> int

val shift: t -> int -> t
val shift_end: t -> int -> t
(* [extend_start t o] moves the start extremity of the cursor [t] by
 * offset [o]. NB: If [o] is negative, the result is larger than [t]. *)
val shift_start: t -> int -> t

val split_relative: t -> int -> t * t
val split_absolute: t -> int -> t * t

val overlap: t -> t -> bool
val contains: t -> int -> bool

