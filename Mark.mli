
module type S = sig
	type t
	type env
	val empty: env
	val put: env -> t -> Cursor.t -> env
	val get: env -> t -> Cursor.t option
	val parse: string -> int -> (t * int)
end

module Unit : S with type t = unit
module LowerChar : S with type t = char
