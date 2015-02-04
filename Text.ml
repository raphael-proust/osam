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

	(*TODO: newlines:int; ascii:bool; *)
}
let codes c = c.extent.codes
let bytes c = c.extent.bytes
let decoder c =
	let d = Uutf.decoder ~encoding:`UTF_8 `Manual in
	Uutf.Manual.src d c.content c.offset.bytes c.extent.bytes

let sub cursor c =
	if c.offset.codes + c.extent.codes <= Cursor.end_ cursor then
		raise (Invalid_argument "out of bound")
	else
		{
			content = c.content;
			offset = {
				bytes = failwith "TODO";
				codes = c.offset.codes + Cursor.start cursor;
			};
			extent = {
				bytes = failwith "TODO";
				codes = Cursor.length cursor;
			};
		}


let hook f acc c s l =
	if c.extent.bytes < s + l then
		raise (Invalid_argument "out of bound")
	else
		f acc c.content (c.offset.bytes + s) l

end
	

(* We use ropes of chunks *)
type split = {
	content: t list;
	bytes: int;
	codes: int;
}
and t =
	| Empty
	| Leaf of Chunk.t
	| Split of split

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
		let cs =
			Uutf.String.fold_utf_8
				(fun _ _ _ -> failwith "TODO: split into chunks")
				Empty
				s
		in
		Some cs
	with
	| Malformed -> None

let to_string s = failwith "TODO"

let rec sub t c =
	if Cursor.start c < 0 || Cursor.end_ c > codes t then
		failwith "TODO: error management"
	else match t with
	| Empty -> assert false
	| Leaf chunk ->
		Leaf (Chunk.sub c chunk)
	| Split s -> match s.content with
	| [] -> failwith "TODO: error management"
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
	if bytes t < l then
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
				else if bytes t < s then
					(* the hook is for further away *)
					(acc, s - bytes t, l)
				else if s + l <= bytes t then
					let acc = hook f acc t s l in
					(acc, 0, 0)
				else (* bytes t < s + l *)
					let acc = hook f acc t s (bytes t - s) in
					(acc, 0, l - (bytes t - s))
			)
			(acc, s, l)
			ts.content
		in
		acc

