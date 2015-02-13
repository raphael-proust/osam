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

let length t = t.end_.codes - t.start.codes

let byte t o =
	if o > (t.end_.codes - t.start.codes) then
		raise (Invalid_argument "Code out of bound")
	else
	let rec find_milestone candidate = function
		| [] -> candidate
		| m::ms ->
			if m.codes > o then
				candidate
			else
				find_milestone m ms
	in
	let find m =
		let decoder = Uutf.decoder ~encoding:`UTF_8 `Manual in
		let () =
			let start = t.start.bytes + m.bytes in
			let len = t.end_.bytes - start in
			Uutf.Manual.src decoder t.content start len
		in
		let rec loop o =
			if o = 0 then
				Uutf.decoder_byte_count decoder
			else
				match Uutf.decode decoder with
				| `End -> assert false
				| `Await ->
					Uutf.Manual.src decoder "" 0 0;
					loop o
				| `Malformed _ | `Uchar _ -> loop (o-1)
		in
		loop (o - m.codes) + m.bytes
	in
	find (find_milestone zero t.milestones)

exception Early_Return_Uchar of Uutf.uchar
let uchar t o =
	(* TODO: make into a reasonable performance.. easy with [fold] taking pos
	 * and len. *)
	try
		let _:int =
			Uutf.String.fold_utf_8
				(fun o _ u ->
					if o = 0 then
						match u with
						| `Uchar u -> raise (Early_Return_Uchar u)
						| `Malformed m -> failwith "TODO: error mgmt"
					else
						o - 1
				)
				o
				t.content
		in
		raise (Invalid_argument "Code beyond end of text")
	with
		| Early_Return_Uchar u -> u

let newline t n =
	(*TODO: error management*)
	(*TODO: we return the code offset which is probably later turned into a
	 * byte offset. Expose an abstract address and have [offset] and [newline]
	 * resolve to it. *)
	(List.nth t.newlines n).codes

let hook ?(o=0) ?l ~f ~acc t =
	let l = match l with
		| None -> (t.end_.codes - t.start.codes) - o
		| Some l ->
			if l > (t.end_.codes - t.start.codes) - o then
				raise (Invalid_argument "Code out of bound")
			else
				l
	in
	let o = byte t o in
	let l = (byte t l) - o in
	f acc t.content o l

let sub t c =
	if Cursor.end_ c > t.end_.codes then
		raise (Invalid_argument "Code out of bound")
	else
	let start =
		let codes = Cursor.start c in
		{codes; bytes = byte t codes;}
	in
	let end_ =
		let codes = Cursor.end_ c in
		{codes; bytes = byte t codes;}
	in
	let patch_addresses a =
		List.map
			(fun a ->
				{bytes = a.bytes - (start.bytes - t.start.bytes);
				codes = a.codes - (start.codes - t.start.codes);
				}
			)
			(List.filter
				(fun {codes} -> Cursor.contains c codes)
				a
			)
	in
	{t with
	start;
	end_;
	milestones = patch_addresses t.milestones;
	newlines = patch_addresses t.newlines;
	malformeds = patch_addresses t.malformeds;
	}

(* TODO: allow add offset and length optional arguments *)
let from_string s =
	let (codes, rev_newlines, rev_milestones, rev_malformeds) =
		Uutf.String.fold_utf_8
			(fun (codes, newlines, milestones, malformeds) bytes u ->
				let milestones =
					(* TODO: find a good value for that *)
					if codes mod 256 = 0 then
						{codes; bytes;}::milestones
					else
						milestones
				in
				match u with
				| `Malformed _ ->
					(codes+1,
					newlines,
					milestones,
					{codes; bytes;}::malformeds)
				| `Uchar u ->
					(* TODO: support CR (\r) and NEL *)
					if u = Char.code '\n' then
						(codes+1,
						{codes; bytes;}::newlines,
						milestones,
						malformeds)
					else
						(codes+1,
						newlines,
						milestones,
						malformeds)
			)
			(0, [], [], [])
			s
	in
	{
		content = s;
		start = {bytes=0; codes=0;};
		end_ = {codes = codes; bytes = String.length s;};
		milestones = List.rev rev_milestones;
		newlines = List.rev rev_newlines;
		malformeds = List.rev rev_malformeds;
	}
let to_string t =
	if t.start.bytes >= String.length t.content then
		""
	else
		String.sub t.content t.start.bytes (t.end_.bytes - t.start.bytes)


let test () =
	let ab = from_string "abcdefghijklmnopqrstuvwxyz" in
	let abn = from_string "a\nb\nc\nd\ne\nf\ng\nh\ni\nj\nk\nl\nm\nn\no\np\nq\nr\ns\nt\nu\nv\nw\nx\ny\nz\n" in
	let u = from_string "⋄⎕" in

	let print t =
		Printf.printf "%s [%d(%d),%d(%d)]: "
			t.content
			t.start.codes t.start.bytes
			t.end_.codes t.end_.bytes;
		Printf.printf "%s\n%!"
			(to_string t)
	in

	print ab;
	print (sub ab (Cursor.mk_absolute 0 0));
	print (sub ab (Cursor.mk_absolute 0 1));
	print (sub ab (Cursor.mk_absolute 1 1));
	print (sub ab (Cursor.mk_absolute 0 26));
	print (sub ab (Cursor.mk_absolute 2 3));

	print abn;
	print (sub abn (Cursor.mk_absolute 0 (newline abn 0)));
	print (sub abn (Cursor.mk_absolute (newline abn 0) (newline abn 1)));
	print (sub abn (Cursor.mk_absolute 0 (newline abn 1)));
	print (sub abn (Cursor.mk_absolute 0 (newline abn 2)));

	print u;
	print (sub u (Cursor.mk_absolute 0 0));
	print (sub u (Cursor.mk_absolute 0 1));
	print (sub u (Cursor.mk_absolute 0 2));
	print (sub u (Cursor.mk_absolute 1 2));
(* 	print (sub u (Cursor.mk_absolute 2 2)); *)

	()

