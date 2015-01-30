
module Codepoint : sig
	type t
	val width: t -> int (* returns between 1 and 6 *)
	val of_char: char -> t
	val of_int: int -> t

	(* [read_in_string s o] evaluates to the character stored in [s] at (byte)
	 * offset [o] and the (byte) offset of the next character. *)
	val read_in_string: string -> int -> (t * int)
	(*TODO: properties (lowercase, uppercase, scripts, &c.) using uucp*)
end = struct
	type t = int
	let width c =
		if 0x0 <= c && c <= 0x7F then
			1
		else if 0x80 <= c && c <= 0x7FF then
			2
		else if 0x800 <= c && c <= 0xFFFF then
			3
		else if 0x10000 <= c && c <= 0x1FFFFF then
			4
		else if 0x200000 <= c && c <= 0x3FFFFFF then
			5
		else if 0x4000000 <= c && c <= 0x7FFFFFFF then
			6
		else
			failwith "TODO: error mgmt"
	let of_char c = Char.code c (*ASCII is included as is in UTF8*)
	let of_int i = i

	let read_in_string s o =
		(* assumes o is the offset of the first byte of a codepoint *)
		let c = Char.code (String.get s o) in
		if 0b0 <= c && c <= 0b01111111 then
			(c, o+1)
		else if 0b11000000 <= c && c <= 0b11011111 then
			let c2 = Char.code (String.get s (o+1)) in
			assert (0b10000000 <= c2 && c2 <= 0b10111111);
			(((c land 0b11111) lsl 5) lor (c2 land 0b111111),
			o+2)
		else if 0b11100000 <= c && c <= 0b11101111 then
			let c2 = Char.code (String.get s (o+1)) in
			let c3 = Char.code (String.get s (o+2)) in
			assert (0b10000000 <= c2 && c2 <= 0b10111111);
			assert (0b10000000 <= c3 && c3 <= 0b10111111);
			(((c land 0b11111) lsl 10)
			lor ((c2 land 0b111111) lsl 5)
			lor (c3 land 0b111111),
			o+3)
		else if 0b11110000 <= c && c <= 0b11110111 then
			let c2 = Char.code (String.get s (o+1)) in
			let c3 = Char.code (String.get s (o+2)) in
			let c4 = Char.code (String.get s (o+3)) in
			assert (0b10000000 <= c2 && c2 <= 0b10111111);
			assert (0b10000000 <= c3 && c3 <= 0b10111111);
			assert (0b10000000 <= c4 && c4 <= 0b10111111);
			(((c land 0b11111) lsl 15)
			lor ((c2 land 0b111111) lsl 10)
			lor ((c3 land 0b111111) lsl 5)
			lor (c4 land 0b111111),
			o+4)
		else if 0b11111000 <= c && c <= 0b11111011 then
			let c2 = Char.code (String.get s (o+1)) in
			let c3 = Char.code (String.get s (o+2)) in
			let c4 = Char.code (String.get s (o+3)) in
			let c5 = Char.code (String.get s (o+4)) in
			assert (0b10000000 <= c2 && c2 <= 0b10111111);
			assert (0b10000000 <= c3 && c3 <= 0b10111111);
			assert (0b10000000 <= c4 && c4 <= 0b10111111);
			assert (0b10000000 <= c5 && c5 <= 0b10111111);
			(((c land 0b11111) lsl 20)
			lor ((c2 land 0b111111) lsl 15)
			lor ((c3 land 0b111111) lsl 10)
			lor ((c4 land 0b111111) lsl 5)
			lor (c5 land 0b111111),
			o+5)
		else if 0b11111100 <= c && c <= 0b11111101 then
			let c2 = Char.code (String.get s (o+1)) in
			let c3 = Char.code (String.get s (o+2)) in
			let c4 = Char.code (String.get s (o+3)) in
			let c5 = Char.code (String.get s (o+4)) in
			let c6 = Char.code (String.get s (o+5)) in
			assert (0b10000000 <= c2 && c2 <= 0b10111111);
			assert (0b10000000 <= c3 && c3 <= 0b10111111);
			assert (0b10000000 <= c4 && c4 <= 0b10111111);
			assert (0b10000000 <= c5 && c5 <= 0b10111111);
			assert (0b10000000 <= c6 && c6 <= 0b10111111);
			(((c land 0b11111) lsl 25)
			lor ((c2 land 0b111111) lsl 15)
			lor ((c3 land 0b111111) lsl 15)
			lor ((c4 land 0b111111) lsl 10)
			lor ((c5 land 0b111111) lsl 5)
			lor (c6 land 0b111111),
			o+6)
		else
			failwith "TODO error management"

end

module Cursor : sig
	(* a cursor describes a range of characters (like the dot in acme) *)
	type t

	val mk_relative: start:int -> offset:int -> t
	val mk_absolute: start:int -> end_:int -> t

	val relative: t -> (int * int)
	val absolute: t -> (int * int)
	val start: t -> int
	val end_: t -> int
	val length: t -> int

	val shift: t -> int -> t
	val overlap: t -> t -> bool
end = struct
	(*TODO: deal with edge cases: negative lengths, negative positions, &c. *)
	type t = int * int

	let mk_relative ~start ~offset = (start, start+offset)
	let mk_absolute ~start ~end_ = (start, end_)

	let relative (start, end_) = (start, end_ - start)
	let absolute t = t
	let start (s, _) = s
	let end_ (_, e) = e
	let length (s, e) = e - s

	let shift (s,e) o = (s+o, e+o)
	let overlap (s1, e1) (s2, e2) =
		(*TODO? unit tests?*)
		(s1 < s2 && s2 < e1)
		|| (s2 < s1 && s1 < e2)
end

module Text : sig
	(* In Text, all the exposed offsets (e.g., in length, sub, &c.) even if
	 * internally it might use byte offsets. *)

	type t

	(* [legnth t] is the number of codepoints in [t]. *)
	val length: t -> int

	(* [empty] is a text with 0 codepoints inside. *)
	val empty: t

	(* [from_string s] checks the UTF8 validity of [s] and returns text if it
	 * is valid. [to_string t] is the inverse. *)
	val from_string: string -> t option
	val to_string: t -> string

	(* [nth i t] is the ith codepoint of [t]. If [i] is bigger or equal to
	 * [length t] or lower than [0] then it fails. *)
	val nth: int -> t -> Codepoint.t

	(* [sub t cursor] is the text included in the range described by the
	 * cursor. *)
	val sub: t -> Cursor.t -> t

	(* [fold t a f] folds over all the codepoints of [t] using [f] with the
	 * initial input [a]. *)
	val fold: t -> 'a -> ('a -> Codepoint.t -> 'a) -> 'a

	(* [cat ts] is the concatenation of all the texts in [ts]. *)
	val cat: t list -> t

end = struct
	type t
	let length _ = failwith "TODO"

	let empty = failwith "TODO"
	let from_string _ = failwith "TODO"
	let to_string _ = failwith "TODO"

	let nth _ _ = failwith "TODO"
	let sub _ _ = failwith "TODO"
	let fold _ _ _ = failwith "TODO"
	let cat _ = failwith "TODO"


end

(*TODO: functorise by Text *)
module Patch : sig

	type t = (Cursor.t * Text.t)
	val apply: Text.t -> t -> Text.t
	val shift: t -> int -> t
	exception Conflict

	(*intertwines apply and shift correctly*)
	val apply_batch: ?merge:(t -> t -> t) -> t list -> Text.t -> Text.t

end = struct

	type t = (Cursor.t * Text.t)
	let apply _ _ = failwith "TODO"
	let effect (c, t) = Text.length t - Cursor.length c
	let shift (c,t) o = (Cursor.shift c o, t)
	let conflict (c1,_) (c2, _) = Cursor.overlap c1 c2
	exception Conflict

	let apply_batch ?(merge = (fun _ _ -> raise Conflict)) cs text =
		match cs with
		| [] -> text
		| _ :: _ as cs ->
			(* We first sort the patches by initial offset so it is easy to
			 * detect conflicts and keep track of relative positions. *)
			let cs =
				let cs = List.sort (fun (s1,_) (s2,_) -> compare s1 s2) cs in
				(*These can't fail becuase sort does not remove any element.*)
				let c = List.hd cs in
				let cs = List.tl cs in
				let (cs_rev, last) = List.fold_left
					(fun (cs_rev, last) c ->
						if conflict last c then
							(cs_rev, merge last c)
						else
							(last :: cs_rev, c)
					)
					([], c)
					cs
				in
				List.rev (last :: cs_rev)
			in
			let (text, _, _) =
				List.fold_left
					(* text is the text as is being modified,
					 * last is the highest offset affected,
					 * offset is the correction to apply to patches
					 *)
					(fun (text, last, offset) ((c, _) as p) ->
						let p = shift p offset in
						let text = apply text p in
						let offset = offset + effect p in
						(text, Cursor.end_ c, offset)
					)
					(text, 0, 0)
					cs
			in
			text


end


module Marks : sig
	(*TODO: make several Marks possible (especially using more than unit) *)
	type t = unit
	type env
	val empty: env
	val put: env -> t -> Cursor.t -> env
	val get: env -> t -> Cursor.t option
end = struct
	type t = unit
	type env = Cursor.t option
	let empty = None
	let put _ () c = Some c
	let get e () = e
end

module Regexp : sig
	type nfa
	type dfa
	val compile: nfa -> dfa
	val has_match: dfa -> Text.t -> Cursor.t -> bool
	val next_match: dfa -> Text.t -> Cursor.t -> Cursor.t option
	val all_matches: dfa -> Text.t -> Cursor.t -> Cursor.t list
	module DSL: sig
		val point: Codepoint.t -> nfa
		val cat: nfa list -> nfa
		val alt: nfa list -> nfa
		val star: nfa -> nfa
		val question: nfa -> nfa
		val plus: nfa -> nfa
	end
end = struct
	type nfa
	type dfa
	let compile _ = failwith "TODO"
	let has_match _ _ _ = failwith "TODO"
	let next_match _ _ _ = failwith "TODO"
	let all_matches _ _ _ = failwith "TODO"
	module DSL = struct
		let point _ = failwith "TODO"
		let cat _ = failwith "TODO"
		let alt _ = failwith "TODO"
		let star _ = failwith "TODO"
		let question _ = failwith "TODO"
		let plus _ = failwith "TODO"
	end
end

module Command : sig
	(*TODO: allow for return status and asynchronous execution*)
	type t
	type status
	val build: string -> t
	val run: ?stdin:string -> ?stdout:Buffer.t -> t -> status
end = struct
	type t = string
	type status = unit (*TODO: allow for lwt, functorize Action over Cmd*)
	let build s = s
	let run ?stdin ?stdout t = failwith "TODO"
end

(*TODO: functorise over marks and text, provide both UTF8 and ASCII text*)
module Actions : sig

type substitutee
type substitutor

type addr =
	(** The current range. *)
	| Dot
	(** Absolute number of characters *)
	| Offset of int
	(** Absolute number of newline characters *)
	| Line of int
	(** The last line *)
	| LastLine
	(** A previously set mark *)
	| Mark of Marks.t
	(** The range of characters of the second address, evaluated at the end of
	 * the first. *)
	| Plus of addr * addr
	(** The range of characters of the second address, evaluated in reverse at
	 * the begining of the first. *)
	| Minus of addr * addr
	(** The range of characters covering both addresses and the gap in between
	 * if any. *)
	| Comma of addr * addr
	(** The range of characters covering both addresses and the gap in between
	 * if any. The second address is evaluated at the end of the first. *)
	| Semicolon of addr * addr
	(** The first match of the Regexp.nfa. *)
	| ForwardRe of Regexp.nfa
	(** The first match of the Regexp.nfa, backward. *)
	| BackwardRe of Regexp.nfa


(** Actions: ways to change the text. *)
type action =

	(** Dispatch executes several actions in a row. The first action is
	 * executed, the offsets are patched, and the next action is run, until
	 * there are none. *)
	| Dispatch of action list

	(** For: execute the action for each of the matches of the regexp in the
	 * current range. Actions are executed with the range set to the regexp
	 * match. *)
	| For of Regexp.nfa * action
	(** Rof: execute the action for each of the gap between the matches of the
	 * regexp in the current range. The gaps include the selection between
	 * the start of the dot and the first match as well as between the last
	 * match and the end of the dot -- even if these ranges are empty. *)
	| Rof of Regexp.nfa * action
	(** If: execute the action if there is a substring that matches the
	 * regexp in the current range. *)
	| If of Regexp.nfa * action
	(** Ifnot: execute the action if there is *no* substring that matches the
	 * regexp in the current range. *)
	| Ifnot of Regexp.nfa * action

	(** Append: add text at the end of the current range. *)
	| Append of Text.t
	(** Insert: add text at the begining of the current range. *)
	| Insert of Text.t
	(** Replace: replace all the current range by the given text. The action
	 * `Replace t` is equivalent to `Dispatch [Delete; Insert t]`. *)
	| Replace of Text.t

	(** Substitute: Similar to `Dispatch [For re; Replace t]` but with support
	 * for numbered groups. *)
	| Substitute of substitutee * substitutor

	(** Delete: removes the characters in range. *)
	| Delete
	(** Move: move the characters in range at the given address. If the given
	 * address covers more than 0 characters, they are replaced. *)
	| Move of addr
	(** Copy: like remove, but keeps the range intact (except if the target
	 * address is within the current range. *)
	| Copy of addr

	(** PipeOut: send the character in the current range into the stdin of the
	 * given system command. *)
	| PipeOut of Command.t
	(** PipeIn: replace the characters in the current range by the stdout of
	 * the given system command. *)
	| PipeIn of Command.t
	(** Pipe: filter the current range of characters through the given system
	 * command. *)
	| Pipe of Command.t

	(** Sets the given mark to address the current selection. *)
	| SetMark of Marks.t

val parse: string -> addr * action

val run:
	(* Pass the text to be edited *)
	text:Text.t ->
	(* Some context *)
	dot:Cursor.t -> marks:Marks.env ->
	(* A target range *)
	addr ->
	(* And the action to be executed *)
	action ->
	(* We return a patch rather than modifying the text *)
	(Patch.t list * Marks.env)

end = struct

type substitutee
type substitutor

(** Addresses: addresses describe ranges in the document *)
type addr =
	| Dot
	| Offset of int
	| Line of int
	| LastLine
	| Mark of Marks.t
	| Plus of addr * addr
	| Minus of addr * addr
	| Comma of addr * addr
	| Semicolon of addr * addr
	| ForwardRe of Regexp.nfa
	| BackwardRe of Regexp.nfa

type action =
	| Dispatch of action list
	| For of Regexp.nfa * action
	| Rof of Regexp.nfa * action
	| If of Regexp.nfa * action
	| Ifnot of Regexp.nfa * action
	| Append of Text.t
	| Insert of Text.t
	| Replace of Text.t
	| Substitute of substitutee * substitutor
	| Delete
	| Move of addr
	| Copy of addr
	| PipeOut of Command.t
	| PipeIn of Command.t
	| Pipe of Command.t
	| SetMark of Marks.t


let rec address text dot marks = function
	| Dot -> dot
	| Offset i -> Cursor.mk_relative ~start:i ~offset:0
	| Line _ -> failwith "TODO"
	| LastLine -> failwith "TODO"
	| Mark m -> begin
		match Marks.get marks m with
		| Some c -> c
		| None -> failwith "TODO: error management"
	end
	| Plus (a1, a2) -> failwith "TODO"
	| Minus (a1, a2) -> failwith "TODO"
	| Comma (a1, a2) -> failwith "TODO"
	| Semicolon (a1, a2) -> failwith "TODO"
	| ForwardRe re -> failwith "TODO"
	| BackwardRe re -> failwith "TODO"

(* TODO: we need to be able to parse regexps to parse actions. Thus we need to
 * functorise over Regexp (which gives the opportunity to support several
 * regexp style (plan9, vim, perl, &c.)). *)
let parse _ = failwith "TODO"

let reverse_dots range dots =
	let s = Cursor.start range in
	let e = Cursor.end_ range in
	let (tods, next_s) =
		List.fold_left
			(fun (tods, next_s) dot ->
				let s = Cursor.start dot in
				let e = Cursor.end_ dot in
				assert (next_s <= s);
				(Cursor.mk_absolute next_s s :: tods, e)
			)
			([], s)
			dots
	in
	assert (next_s <= e);
	let tods = Cursor.mk_absolute next_s  e :: tods in
	List.rev tods

(*We don't pass dot because we expect the caller to use Text.sub
 * Will have to be changed to accomodate for Move and Copy. *)
let rec action text (dot:Cursor.t) marks = function
	| Dispatch acts ->
		let (patchess, marks) =
			List.fold_left (fun (patchess, marks) act ->
				let (patches, marks) = action text dot marks act in
				(patches :: patchess, marks)
				)
				([], marks)
				acts
		in
		(List.flatten patchess, marks)
	| For (re, act) ->
		let dots = Regexp.(all_matches (compile re) text dot) in
		fold_over_dots text dots marks act
	| Rof (re, act) ->
		let dots = Regexp.(all_matches (compile re) text dot) in
		let dots = reverse_dots dot dots in
		fold_over_dots text dots marks act
	| If (re, act) ->
		if Regexp.(has_match (compile re) text dot) then
			action text dot marks act
		else
			([], marks)
	| Ifnot (re, act) ->
		if Regexp.(has_match (compile re) text dot) then
			([], marks)
		else
			action text dot marks act

	| Append t ->
		let e = Cursor.end_ dot in
		([(Cursor.mk_absolute e e, t)], marks)
	| Insert t ->
		let s = Cursor.start dot in
		([(Cursor.mk_absolute s s, t)], marks)
	| Replace t ->
		([(dot , t)], marks)
	| Substitute (se, sr) -> failwith "TODO"
	| Delete ->
		([(dot , Text.empty)], marks)

	| Move a ->
		let target = address text dot marks a in
		([(target, Text.sub text dot); (dot, Text.empty)], marks)
	| Copy a ->
		let target = address text dot marks a in
		([target, Text.sub text dot], marks)

	(*TODO: manage stdout and such *)
	| PipeOut cmd ->
		let (_ : Command.status) =
			Command.run ~stdin:(Text.to_string text) cmd
		in
		([], marks)
	| PipeIn cmd ->
		let b = Buffer.create 100 in
		let (_ : Command.status) = Command.run ~stdout:b cmd in
		let res = match Text.from_string (Buffer.contents b) with
			| None -> Text.empty
			| Some t -> t
		in
		([dot, res], marks)
	| Pipe cmd ->
		let b = Buffer.create 100 in
		let (_ : Command.status) =
			Command.run
				~stdin:(Text.to_string text)
				~stdout:b
				cmd
		in
		let res = match Text.from_string (Buffer.contents b) with
			| None -> Text.empty
			| Some t -> t
		in
		([dot, res], marks)

	| SetMark m ->
		([], Marks.put marks m dot)

and fold_over_dots text dots marks act =
	let (patchess, marks) =
		List.fold_left (fun (patchess, marks) dot ->
			let (patches, marks) = action text dot marks act in
			(patches :: patchess, marks)
			)
			([], marks)
			dots
	in
	(List.flatten patchess, marks)

let run ~text ~dot ~marks addr act =
	let dot = address text dot marks addr in
	let (patches, marks) = action text dot marks act in
	(patches, marks)

end
