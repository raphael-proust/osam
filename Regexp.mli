
module DSL: sig
	type t
	val point: Uutf.uchar -> t
	val seq: t list -> t
	val alt: t list -> t
	val question: t -> t
	val star: t -> t
	val plus: t -> t
	val parse: string -> int -> (t * int)
end

module type S = sig
	type t
	val compile: DSL.t -> t
	val has_match: t -> Text.t -> Cursor.t -> bool
	val next_match: t -> Text.t -> Cursor.t -> Cursor.t option
	val prev_match: t -> Text.t -> Cursor.t -> Cursor.t option
	val all_matches: t -> Text.t -> Cursor.t -> Cursor.t list
end

module Interp : S
module NFA : S
module DFA : S
