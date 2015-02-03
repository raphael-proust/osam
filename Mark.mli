(*TODO: make several Marks possible (especially using more than unit) *)
type t = unit
type env
val empty: env
val put: env -> t -> Cursor.t -> env
val get: env -> t -> Cursor.t option
