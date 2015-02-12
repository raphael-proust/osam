(*TODO optimise regexps to work on bytes and allow byte folding *)
module DSL = struct
	type t =
		| Epsilon
		| Point of Uutf.uchar
		| Choice of t list
		| Sequence of t list
		| Loop of t

	let point u = Point u
	let seq ts = Sequence ts
	let alt ts = Choice ts
	let question t = Choice [t ; Epsilon]
	let star t = Loop t
	let plus t = Sequence [t ; Loop t]

	let parse s p = failwith "TODO: Regexp.parse"
end

module type S = sig
	type t
	val compile: DSL.t -> t
	val has_match: t -> Text.t -> Cursor.t -> bool
	val next_match: t -> Text.t -> Cursor.t -> Cursor.t option
	val prev_match: t -> Text.t -> Cursor.t -> Cursor.t option
	val all_matches: t -> Text.t -> Cursor.t -> Cursor.t list
end

module Interp = struct
	type t = DSL.t =
		| Epsilon
		| Point of Uutf.uchar
		| Choice of t list
		| Sequence of t list
		| Loop of t

	let compile t = t

	let next_match t txt c =
		let rec loop cur = function
			| Epsilon -> Some cur
			| Point u ->
				if failwith "TODO: get uchar at (Cursor.end_ cur)" = u then
					Some (Cursor.shift_end cur 1)
				else
					None
			| Choice ts -> begin
				match ts with
				| [] -> None
				| t::ts -> match loop cur t with
					| Some _ as c -> c
					| None -> loop cur (Choice ts)
			end
			| Sequence ts -> begin
				match ts with
				| [] -> Some cur
				| t::ts -> match loop cur t with
					| Some c -> loop cur (Sequence ts)
					| None -> None
			end
			| Loop t -> begin
				match loop cur t with
				| None -> Some cur
				| Some cur -> loop cur (Loop t)
			end
		in
		let rec looploop start =
			if start = Cursor.end_ c then
				None
			else
				match loop (Cursor.mk_point start) t with
				| None -> looploop (start + 1)
				| Some c -> Some c
		in
		looploop (Cursor.start c)
				

	let prev_match _ _ _ = failwith "TODO"

	let all_matches t txt c =
		let rec loop acc off =
			if off = Cursor.end_ c then
				acc
			else
				match next_match t txt Cursor.(mk_absolute off (end_ c)) with
				| None -> acc
				| Some c -> loop (c::acc) (Cursor.end_ c)
		in
		loop [] (Cursor.start c)

	let has_match t txt c =
		(* This is not optimal: we can abort early instead of being greedy. *)
		match next_match t txt c with
		| None -> false
		| Some _ -> true

end

module NFA = struct
	type t

	(* Compile strategy:
		* transform [Point u] into a sequence of bytes
		* there's only byte, choice, seq, and loop: make an byte-automata
		* use the `hook` to pass bytes directly, instead of uchars.
	*)
	let compile _ = failwith "TODO"
	let has_match _ _ _ = failwith "TODO"
	let next_match _ _ _ = failwith "TODO"
	let prev_match _ _ _ = failwith "TODO"
	let all_matches _ _ _ = failwith "TODO"
end
module DFA = struct
	type t
	let compile _ = failwith "TODO"
	let has_match _ _ _ = failwith "TODO"
	let next_match _ _ _ = failwith "TODO"
	let prev_match _ _ _ = failwith "TODO"
	let all_matches _ _ _ = failwith "TODO"
end
