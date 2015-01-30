
module Codepoint : sig
	type t
	val width: t -> int (* returns between 1 and 6 *)
	val of_char: char -> t
	val of_int: int -> t
	(*TODO: properties (lowercase, uppercase, scripts, &c.) using uucp*)
end = struct
	type t
	let width _ = failwith "TODO"
	let of_char _ = failwith "TODO"
	let of_int _ = failwith "TODO"
end

module Cursor : sig
	(* a cursor describes a range of characters (like the dot in acme) *)
	type t = int * int
	val shift: t -> int -> t
end = struct
	type t = int * int
	let shift (s,e) o = (s+o, e+o)
end

module Text : sig
	type t
	val length: t -> int

	val empty: t
	val from_string: string -> t
	val to_string: t -> string

	val nth: int -> t -> Codepoint.t
	val sub: t -> Cursor.t -> t
	val fold: t -> 'a -> ('a -> Codepoint.t -> 'a) -> 'a
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

	(*intertwines apply and shift correctly*)
	val apply_batch: t list -> Text.t -> Text.t

end = struct

	type t = (Cursor.t * Text.t)
	let apply _ _ = failwith "TODO"
	let shift (c,t) o = (Cursor.shift c o, t)

	let apply_batch cs text =
		(* We first sort the patches by initial offset so it is easy to detect
		 * conflicts and keep track of relative positions. *)
		let cs = List.sort (fun (s1,_) (s2,_) -> compare s1 s2) cs in
		let (text, _, _) =
			List.fold_left
				(* text is the text as is being modified,
				 * last is the highest offset affected,
				 * corr is the correction to apply to patches
				 *)
				(fun (text, last, corr) p ->
					let p = shift p corr in
					let (s,e), t = p in
					if s < last then
						(*TODO: we probably want a first pass on the list with a
						 * caller-provided merge function. In the simplest case,
						 * merge raises an exception that the caller catches. *)
						failwith "TODO: error management, the patches collide"
					else
						let t = apply t p in
						let corr = corr - (e - s) + Text.length t in
						(t, e, corr)
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
	| Offset i -> (i, i)
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

let reverse_dots (s,e) dots =
	let (tods, next_s) =
		List.fold_left
			(fun (tods, next_s) (s,e) ->
				assert (next_s <= s);
				((next_s, s) :: tods, e)
			)
			([], s)
			dots
	in
	assert (next_s <= e);
	let tods = (next_s, e) :: tods in
	List.rev tods

(*We don't pass dot because we expect the caller to use Text.sub
 * Will have to be changed to accomodate for Move and Copy. *)
let rec action text dot marks = function
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
		let e = Text.length text in
		([((e,e), t)], marks)
	| Insert t ->
		([((0,0), t)], marks)
	| Replace t ->
		([((0, Text.length text) , t)], marks)
	| Substitute (se, sr) -> failwith "TODO"
	| Delete ->
		([((0, Text.length text) , Text.empty)], marks)

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
		let res = Text.from_string (Buffer.contents b) in
		([(0, Text.length text), res], marks)
	| Pipe cmd ->
		let b = Buffer.create 100 in
		let (_ : Command.status) =
			Command.run
				~stdin:(Text.to_string text)
				~stdout:b
				cmd
		in
		let res = Text.from_string (Buffer.contents b) in
		([(0, Text.length text), res], marks)

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
	let (start, _) as dot = address text dot marks addr in
	let (patches, marks) = action text dot marks act in
	(patches, marks)

end
