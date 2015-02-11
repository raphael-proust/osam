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
let zero = {
	bytes = 0;
	codes = 0;
}

(* TODO: make the lists into trees so we can do bisections. Possibly, use an
 * array for milestones for O(1) access.
 * TODO: make it possible to sift through both newlines and milestones. *)
(* INVARIANT: milestones, newlines, and malformed are sorted. *)
type t = {
	content: string;
	(* These are absolute in the string. *)
	start: address;
	end_: address;

	(* These are relative to start *)
	milestones: address list;
	newlines: address list;
	malformeds: address list;
}

exception Early_Return_Byte of int
let byte t o =
	let rec find_milestone candidate = function
		| [] -> candidate
		| m::ms ->
			if m.codes > o then
				candidate
			else
				find_milestone m ms
	in
	let find m =
		try
			Uutf.String.fold_utf_8
				(* See https://github.com/dbuenzli/uutf/pull/6
				~pos:(start.bytes + m.bytes)
				*)
				(fun oo b _ ->
					if oo = o then
						raise (Early_Return_Byte b)
					else begin
						assert (oo < o);
						oo+1
					end
				)
				(* See https://github.com/dbuenzli/uutf/pull/6
				m.codes
				*) (- t.start.codes)
				t.content
		with
		| Early_Return_Byte b -> b
	in
	find (find_milestone zero t.milestones)

let hook ?o ?l ~f ~acc s = failwith "TODO"
let sub t c =
	let patch_addresses _ = failwith "TODO" in
	{t with
	start = (let codes = Cursor.start c in {codes; bytes = byte t codes;});
	end_ = (let codes = Cursor.end_ c in {codes; bytes = byte t codes;});
	milestones = patch_addresses t.milestones;
	newlines = patch_addresses t.newlines;
	malformeds = patch_addresses t.malformeds;
	}
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
let to_string t =
	String.sub t.content t.start.bytes (t.end_.bytes - t.start.bytes)
