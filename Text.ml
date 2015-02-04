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

	(* The number of newline characters in the chunk. Note that newlines
	 * are only ever at the end of the chunk *)
	newlines: int;
	ascii: bool;
}
let codes c = c.extent.codes
let bytes c = c.extent.bytes
let decoder c =
	let d = Uutf.decoder ~encoding:`UTF_8 `Manual in
	Uutf.Manual.src d c.content c.offset.bytes c.extent.bytes

let nth : int -> t -> Uutf.uchar = failwith "TODO"
let iter : (Uutf.uchar -> unit) -> t -> unit = failwith "TODO"
let fold : ('a -> Uutf.uchar -> 'a) -> 'a -> t -> 'a = fun _ _ _ -> failwith "TODO"
let sub : Cursor.t -> t -> t = failwith "TODO"
let split : int -> t -> t = failwith "TODO"

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

let rec nth t o =
	if o < 0 || codes t < o then
		failwith "TODO: error management"
	else match t with
	| Empty -> assert false
	| Leaf c ->
		Chunk.nth o c
	| Split s -> match s.content with
	| [] -> failwith "TODO: error management"
	| t::ts ->
		if o < codes t then
			nth t o
		else
			let s = {
				content = ts;
				codes = s.codes - codes t;
				bytes = s.bytes - bytes t;
			}
			in
			nth (Split s) (o - codes t)

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


let rec fold f acc = function
	| Empty -> acc
	| Leaf c ->
		Chunk.fold f acc c
	| Split {content} ->
		List.fold_left (fold f) acc content

let cat _ = failwith "TODO"

let change t (c,r) = failwith "TODO"

