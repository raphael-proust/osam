(*TODO optimise regexps to work on bytes and allow byte folding *)
type nfa
type dfa
let compile _ = failwith "TODO"
let has_match _ _ _ = failwith "TODO"
let next_match _ _ _ = failwith "TODO"
let prev_match _ _ _ = failwith "TODO"
let all_matches _ _ _ = failwith "TODO"
module DSL = struct
	let point _ = failwith "TODO"
	let cat _ = failwith "TODO"
	let alt _ = failwith "TODO"
	let star _ = failwith "TODO"
	let question _ = failwith "TODO"
	let plus _ = failwith "TODO"
end
