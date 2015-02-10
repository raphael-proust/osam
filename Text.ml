(*TODO:
	* functorise over code-points so that we can provide a ASCII optimised
	* version.
	*
	* Provide hooks for the regexp engine (need to be able to safely pass the
	* raw strings to the matching engine).
	*
	* Provide an alternative implementation with a string for the whole text
	* and an index info on the side (with byte and code addresses of \n
	* characters and other such info to speed up random access)
 *)

(* TODO: rewrite. We do not need any of the complexity in this implementation.
 * In particular, we are only ever dealing with one string that is our input.
 * So here is the plan:
 *
 * Have the text represented as one string with an index
 * The index contains offsets of \n characters and of every n-th code
 * Simply emit patches as a result of an action
 * Provide a string-folding function over (patch list * text)
 *)

type address = {
	bytes: int;
	codes: int;
}

type t = {
	content: string;
	start: address;
	end_: address;
	milestones: address list;
	newlines: address list;
	malformeds: address list;
}

let hook f acc s o l = failwith "TODO"
let sub t c = failwith "TODO"
let from_string s =
	let (newlines, milestones, end_, malformeds) = failwith "TODO" in
	{
		content = s;
		start = {bytes=0; codes=0;};
		end_;
		milestones;
		newlines;
		malformeds;
	}
let to_string t = failwith "TODO"
