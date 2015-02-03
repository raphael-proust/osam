
type t
val mk: Cursor.t -> Text.t -> t
val shift: t -> int -> t
exception Conflict

(*intertwines apply and shift correctly*)
val apply_batch: ?merge:(t -> t -> t) -> t list -> Text.t -> Text.t
