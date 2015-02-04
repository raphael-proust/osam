type nfa
type dfa
val compile: nfa -> dfa
val has_match: dfa -> Text.t -> Cursor.t -> bool
val next_match: dfa -> Text.t -> Cursor.t -> Cursor.t option
val prev_match: dfa -> Text.t -> Cursor.t -> Cursor.t option
val all_matches: dfa -> Text.t -> Cursor.t -> Cursor.t list
module DSL: sig
	val point: Text.Codepoint.t -> nfa
	val cat: nfa list -> nfa
	val alt: nfa list -> nfa
	val star: nfa -> nfa
	val question: nfa -> nfa
	val plus: nfa -> nfa
end
