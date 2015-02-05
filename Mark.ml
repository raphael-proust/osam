
module type S = sig
	type t
	type env
	val empty: env
	val put: env -> t -> Cursor.t -> env
	val get: env -> t -> Cursor.t option
	val parse: string -> int -> (t * int)
end

module Unit = struct
	type t = unit
	type env = Cursor.t option
	let empty = None
	let put _ () c = Some c
	let get e () = e
	let parse _ p = ((), p)
end
module LowerChar = struct

	let rec insert e (k:int) c = match e with
		| [] -> [(k,c)]
		| ((kk, _) as kkcc) :: e as ee ->
			if k = kk then
				(k,c)::e
			else if kk > k then
				(k,c)::ee
			else
				kkcc::(insert e k c)
	let rec lookup e k = match e with
		| [] -> None
		| (kk, cc) :: e ->
			if k = kk then
				Some cc
			else if k > kk then
				None
			else
				lookup e k

	type t = char
	type env = (int * Cursor.t) list
	let empty = []
	let put e k c =
		let k = Char.code k - Char.code 'a' in
		if k < 0 || k >= 26 then
			raise (Invalid_argument "Out of bounds")
		else
			insert e k c
	let get e k =
		let k = Char.code k - Char.code 'a' in
		if k < 0 || k >= 26 then
			raise (Invalid_argument "Out of bounds")
		else
			 lookup e k
	let parse s p =
		if p >= String.length s then
			raise (Invalid_argument "Out of bounds")
		else
			let c = String.get s p in
			if c < 'a' || 'z' < c then
				raise (Invalid_argument "Out of bounds")
			else
				(c, p+1)
end
