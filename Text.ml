(*TODO:
	* functorise over code-points so that we can provide a ASCII optimised
	* version.
	*
	* Get Cursor into a distinct file because it is unrelated to text.
	*
	* Provide hooks for the regexp engine (need to be able to safely pass the
	* raw strings to the matching engine).
 *)


module Codepoint : sig
	type t
	type ts = private string
	val width: t -> int (* returns between 1 and 6 *)
	val of_int: int -> t option
	val read: ts -> int -> (t * int) option
	val read_exn: ts -> int -> (t * int)
	val validate: string -> (ts * int) option
end = struct
	(* This assumes a well-formed (i.e. UTF8 valid) underlying string.*)

	let ordered i1 i2 i3 = i1 <= i2 && i2 <= i3

	(* [w?] tests a single byte*)
	let s1 b = ordered 0b00000000 b 0b01111111
	let s2 b = ordered 0b11000000 b 0b11011111
	let s3 b = ordered 0b11100000 b 0b11101111
	let s4 b = ordered 0b11110000 b 0b11110111
	let s5 b = ordered 0b11111000 b 0b11111011
	let s6 b = ordered 0b11111100 b 0b11111101
	let sc b = ordered 0b10000000 b 0b10111111

	type t = int
	type ts = string

	let width t =
		if 0x0 <= t && t <= 0x7F then
			1
		else if 0x80 <= t && t <= 0x7FF then
			2
		else if 0x800 <= t && t <= 0xFFFF then
			3
		else if 0x10000 <= t && t <= 0x1FFFFF then
			4
		else if 0x200000 <= t && t <= 0x3FFFFFF then
			5
		else if 0x4000000 <= t && t <= 0x7FFFFFFF then
			6
		else
			failwith "TODO: error mgmt"
	let of_int i = failwith "TODO"

	let read_exn s o =
		(*TODO: error management*)
		(* assumes o is the offset of the first byte of a codepoint *)
		let byte s o = Char.code (String.get s o) in
		let c = byte s o in
		if s1 c then
			(c, o+1)
		else if s2 c then
			let c2 = byte s (o+1) in
			assert (sc c2);
			(((c land 0b11111) lsl 5) lor (c2 land 0b111111),
			o+2)
		else if s3 c then
			let c2 = byte s (o+1) in
			let c3 = byte s (o+2) in
			assert (sc c2 && sc c3);
			(((c land 0b11111) lsl 10)
			lor ((c2 land 0b111111) lsl 5)
			lor (c3 land 0b111111),
			o+3)
		else if s4 c then
			let c2 = byte s (o+1) in
			let c3 = byte s (o+2) in
			let c4 = byte s (o+3) in
			assert (sc c2 && sc c3 && sc c4);
			(((c land 0b11111) lsl 15)
			lor ((c2 land 0b111111) lsl 10)
			lor ((c3 land 0b111111) lsl 5)
			lor (c4 land 0b111111),
			o+4)
		else if s5 c then
			let c2 = byte s (o+1) in
			let c3 = byte s (o+2) in
			let c4 = byte s (o+3) in
			let c5 = byte s (o+4) in
			assert (sc c2 && sc c3 && sc c4 && sc c5);
			(((c land 0b11111) lsl 20)
			lor ((c2 land 0b111111) lsl 15)
			lor ((c3 land 0b111111) lsl 10)
			lor ((c4 land 0b111111) lsl 5)
			lor (c5 land 0b111111),
			o+5)
		else if s6 c then
			let c2 = byte s (o+1) in
			let c3 = byte s (o+2) in
			let c4 = byte s (o+3) in
			let c5 = byte s (o+4) in
			let c6 = byte s (o+5) in
			assert (sc c2 && sc c3 && sc c4 && sc c5 && sc c6);
			(((c land 0b11111) lsl 25)
			lor ((c2 land 0b111111) lsl 15)
			lor ((c3 land 0b111111) lsl 15)
			lor ((c4 land 0b111111) lsl 10)
			lor ((c5 land 0b111111) lsl 5)
			lor (c6 land 0b111111),
			o+6)
		else
			assert false

	let read s o = failwith "TODO"

	let validate s = failwith "TODO"

end


(* We do not expose that, it is port of the internal representation *)
module Chunk : sig
	type t
	val bytes: t -> int
	val codes: t -> int
	val from_string: string -> t option
	val to_string: t -> string
	val apply_on_substring: (string -> int -> int -> 'a) -> t -> 'a
	val sub: t -> Cursor.t -> t
	val fold: ('a -> Codepoint.t -> 'a) -> 'a -> t -> 'a
	val iter: (Codepoint.t -> unit) -> t -> unit
	val nth: t -> int -> (Codepoint.t * int)
	
end = struct

	(* An address points to a position in a string. It has a byte offset (for
	 * access to the underlying string) and a code offset (so as to remember
	 * how many code points there are before the point).
	 *)
	type address = {
		bytes: int;
		codes: int;
	}

	(* A chunk is a string with start and end markers. Only the substring
	 * between the start and end_ shall ever be accessed.*)
	type t = {
		content: Codepoint.ts; (*might be shared*)
		offset: address;
		extent: address;
	}

	let bytes c = c.extent.bytes
	let codes c = c.extent.codes

	let from_string s = match Codepoint.validate s with
		| None -> None
		| Some (content, codes) ->
			Some {
				content;
				offset={codes=0; bytes=0;};
				extent={codes; bytes=String.length s};
			}
	let to_string t =
		String.sub (t.content:>string) t.offset.bytes t.extent.bytes
	let apply_on_substring f t =
		f (t.content:>string) t.offset.bytes t.extent.bytes


	(*TODO? factorise nth, fold, and iter (using local exception for fast
	 * return?*)

	(* In the following functions, the following parameters have the following
	 * use:
	 * [b] is the byte address tracking our scanning
	 * [o] is the number of codes we want to go through
	 *) 

	let rec fold f acc s b o =
		if o = 0 then
			let (c, _) = Codepoint.read_exn s b in
			f acc c
		else
			let (c, b) = Codepoint.read_exn s b in
			fold f (f acc c) s b (o - 1)
	let fold f acc c =
		fold f acc c.content c.offset.bytes c.extent.codes

	let rec nth s b o =
		if o = 0 then
			let (c, b) = Codepoint.read_exn s b in
			(c, b)
		else
			let (_, b) = Codepoint.read_exn s b in
			nth s b (o - 1)
	let nth t o =
		if o < 0 || o < t.extent.codes then
			failwith "TODO: error management"
		else
			nth t.content t.offset.bytes o

	let rec iter f s b o =
		if o = 0 then
			let (c, _) = Codepoint.read_exn s b in
			f c
		else
			let (c, b) = Codepoint.read_exn s b in
			f c;
			iter f s b (o - 1)
	let iter f c =
		iter f c.content c.offset.bytes c.extent.codes

	let sub chunk cursor =
		if Cursor.end_ cursor > chunk.extent.codes then
			failwith "TODO: error mgmt"
		else
			let (_, bstart) = nth chunk (Cursor.start cursor) in
			(*TODO: make a nth_after to avoid scanning the prefix twice. *)
			let (_, bend) = nth chunk (Cursor.end_ cursor) in
			{
				content = chunk.content;
				offset = {
					bytes = chunk.offset.bytes + bstart;
					codes = chunk.offset.codes + Cursor.start cursor;
				};
				extent = {
					bytes = chunk.offset.bytes + bend;
					codes = chunk.offset.codes + Cursor.end_ cursor;
				};
			}

end

(* We do not expose that, it is port of the internal representation *)
module Rope = struct

	type split = {
		content: t list;
		bytes: int;
		codes: int;
	}
	and t =
		| Leaf of Chunk.t
		| Split of split

	let codes = function
		| Leaf c -> Chunk.codes c
		| Split {codes} -> codes
	let bytes = function
		| Leaf c -> Chunk.bytes c
		| Split {bytes} -> bytes

	let empty =
		match Chunk.from_string "" with
		| None -> assert false
		| Some c -> Leaf c

	let from_string s =
		match Chunk.from_string s with
		| Some c -> Some (Leaf c)
		| None -> None
	let rec to_string buff off = function
		| Leaf c ->
			Chunk.apply_on_substring (Buffer.add_substring buff) c;
			off + Chunk.bytes c
		| Split {content} ->
			List.fold_left (to_string buff) off content
	let to_string = function
		| Leaf c ->
			Chunk.to_string c
		| Split {bytes} as t  ->
			let b = Buffer.create bytes in
			let _:int = to_string b 0 t in
			Buffer.contents b


	let rec nth t o =
		if o < 0 || codes t < o then
			failwith "TODO: error management"
		else match t with
		| Leaf c ->
			Chunk.nth c o
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
		| Leaf chunk ->
			Leaf (Chunk.sub chunk c)
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
		| Leaf c ->
			Chunk.fold f acc c
		| Split {content} ->
			List.fold_left (fold f) acc content

	let cat _ = failwith "TODO"


end


	type t = Rope.t
	let length = Rope.codes
	let empty = Rope.empty
	let from_string = Rope.from_string
	let to_string = Rope.to_string
	let nth = Rope.nth
	let sub = Rope.sub
	let fold = Rope.fold
	let cat = Rope.cat
	


