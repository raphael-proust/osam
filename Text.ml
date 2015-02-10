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

type address = {
	bytes: int;
	codes: int;
}

module Chunk = struct

type t = {
	(* Underlying content, very probably shared *)
	content: string;

	(* Address information *)
	offset: address;
	extent: address;

	(* Other information *)

	newlines: int;
}
let codes c = c.extent.codes
let bytes c = c.extent.bytes
let decoder c =
	let d = Uutf.decoder ~encoding:`UTF_8 `Manual in
	Uutf.Manual.src d c.content c.offset.bytes c.extent.bytes;
	d

let byte_offset c o =
	if o < 0 || c.extent.codes <= o then
		raise (Invalid_argument "out of bound")
	else
		let d = decoder c in
		let rec loop () =
			if Uutf.decoder_count d = o then
				Uutf.decoder_byte_count d
			else begin
				ignore (Uutf.decode d);
				loop ()
			end
		in
		loop ()

let sub cursor c =
	if c.offset.codes + c.extent.codes <= Cursor.end_ cursor then
		raise (Invalid_argument "out of bound")
	else
		{
			content = c.content;
			offset = {
				bytes = byte_offset c (Cursor.start cursor);
				codes = c.offset.codes + Cursor.start cursor;
			};
			extent = {
				bytes = byte_offset c (Cursor.end_ cursor);
				codes = Cursor.length cursor;
			};
			newlines =
				max 0 (c.newlines - (c.extent.codes - Cursor.end_ cursor));
		}


let hook f acc c s l =
	if c.extent.codes < s + l then
		raise (Invalid_argument "out of bound")
	else
		let ss = byte_offset c s in
		let ee = byte_offset c (s + l) in
		f acc c.content (c.offset.bytes + ss) (ee - ss)

end
	

(* We use ropes of chunks. *)
type split = {
	content: t list;
	bytes: int;
	codes: int;
}
and t =
	| Empty
	| Leaf of Chunk.t
	| Split of split

let append t c = match t with
	| Empty -> Leaf c
	| Leaf l ->
		Split {
			content = [Leaf l; Leaf c];
			bytes = Chunk.(bytes l + bytes c);
			codes = Chunk.(codes l + codes c);
		}
	| Split s ->
		(* TODO: find a balancing scheme, experiment and benchmark *)
		Split {
			content = s.content @ [Leaf c];
			bytes = s.bytes + Chunk.bytes c;
			codes = s.codes + Chunk.codes c;
		}


let codes = function
	| Empty -> 0
	| Leaf c -> Chunk.codes c
	| Split s -> s.codes
let length = codes (*for export purposes*)
let bytes = function
	| Empty -> 0
	| Leaf c -> Chunk.bytes c
	| Split s -> s.bytes

let empty = Empty
exception Malformed
let from_string s =
	try
		let zero = {bytes=0; codes=0} in
		let (r, offset, extent, newlines) =
			Uutf.String.fold_utf_8
				(fun (r, offset, extent, newlines) i c ->
					match c with
					| `Malformed _ -> failwith "TODO: error mgmt"
					| `Uchar u ->
						if newlines > 0 && u <> (Char.code '\n') then
							(*break*)
							((append
								r
								{Chunk.content=s; offset; extent; newlines}
							 ),
							 {bytes = i;
							  codes = offset.codes + extent.codes + 1;
							 },
							 zero,
							 0)
						else if u = (Char.code '\n') then
							(r,
							 offset,
							 {bytes = i - offset.bytes;
							  codes = extent.codes + 1
							 },
							 newlines + 1)
						else
							(r,
							 offset,
							 {bytes = i - offset.bytes;
							  codes = extent.codes + 1
							 },
							 newlines)
				)
				(Empty, zero, zero, 0)
				s
		in
		Some (append r {Chunk.content=s; offset; extent; newlines})
	with
	| Malformed -> None

let rec sub t c =
	if Cursor.start c < 0 || Cursor.end_ c > codes t then
		raise (Invalid_argument "Out of bounds")
	else match t with
	| Empty -> assert false
	| Leaf chunk ->
		Leaf (Chunk.sub c chunk)
	| Split s -> match s.content with
	| [] -> assert false
	| t::ts ->
		if Cursor.end_ c < codes t then
			sub t c
		else if codes t < Cursor.start c then
			let s = {
				content = ts;
				codes = s.codes - codes t;
				bytes = s.bytes - bytes t;
			}
			in
			sub (Split s) (Cursor.shift c (~- (codes t)))
		else
			(*TODO: make chunks using the base strings and build a Split*)
			failwith "TODO: this is harder"

let change t (c,r) = failwith "TODO"

let rec hook f acc t s l =
	if codes t < l then
		raise (Invalid_argument "Out of bound")
	else match t with
	| Empty -> assert (l = 0); acc
	| Leaf c -> Chunk.hook f acc c s l
	| Split ts ->
		let (acc, _, _) = List.fold_left
			(fun (acc, s, l) t ->
				if l = 0 then
					(* the hook is past *)
					(acc, 0, 0)
				else if codes t < s then
					(* the hook is for further away *)
					(acc, s - codes t, l)
				else if s + l <= codes t then
					(* the hook is included in the current chunk *)
					let acc = hook f acc t s l in
					(acc, 0, 0)
				else (* codes t < s + l *)
					(* the hook intersect witht the current chunk and goes
					 * over *)
					let acc = hook f acc t s (codes t - s) in
					(acc, 0, l - (codes t - s))
			)
			(acc, s, l)
			ts.content
		in
		acc

let to_string s =
	let b = Buffer.create (bytes s) in
	hook (fun () s o l -> Buffer.add_substring b s o l) () s 0 (codes s);
	Buffer.contents b

