(*TODO optimise regexps to work on bytes and allow byte folding *)
module DSL = struct
	type t =
		| Epsilon
		| Byte of char
		| Choice of t list
		| Sequence of t list
		| Loop of t

	let point b =
		failwith "TODO: emit the UTF8 bytes and make a sequence"
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

module NFA = struct
	type t
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
