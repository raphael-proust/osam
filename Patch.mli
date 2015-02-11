
type t
val mk: Cursor.t -> Text.t -> t
val shift: int -> t -> t

val sort: ?merge:(t -> t -> t) -> t list -> t list
val hook:
	?o:int -> ?l:int ->
	f:('a -> string -> int -> int -> 'a) ->
	acc:'a ->
	Text.t ->
	t list ->
	'a
