(*TODO: Here are list of things to imporve once there is a minimal working
 * version.
 *
 * - make more abstract and semantically sound Address constructs
 * - optimise regexps to work on bytes and allow byte folding
 *)

module Regexp : sig
	type nfa
	type dfa
	val compile: nfa -> dfa
	val has_match: dfa -> Text.t -> Cursor.t -> bool
	val next_match: dfa -> Text.t -> Cursor.t -> Cursor.t option
	val prev_match: dfa -> Text.t -> Cursor.t -> Cursor.t option
	val all_matches: dfa -> Text.t -> Cursor.t -> Cursor.t list
	module DSL: sig
		val point: Text.Codepoint.t -> nfa
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
	| Mark of Mark.t
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
	| SetMark of Mark.t

val parse: string -> addr * action

val run:
	(* Pass the text to be edited *)
	text:Text.t ->
	(* Some context *)
	dot:Cursor.t -> marks:Mark.env ->
	(* A target range *)
	addr ->
	(* And the action to be executed *)
	action ->
	(* We return a patch rather than modifying the text *)
	(Patch.t list * Mark.env)

end = struct

type substitutee
type substitutor

(*TODO: find less ad-hoc more abstract constructs for address.
 * Absolute: Offset, Dot, Mark
 * Relative: Start/End of current resolution, Continue from current
 *   resolution, Set the dot to current resolution and continue with current
 *   resolution, &c.
 *
 * Provide a translation from the classic sam/acme constructors and the more
 * abstract ones.
 *)

(** Addresses: addresses describe ranges in the document *)
type addr =
	| Dot
	| Offset of int
	| Line of int
	| LastLine
	| Mark of Mark.t
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
	| SetMark of Mark.t


let rec address text dot marks start reverse = function
	(* [text] is the text in which we are trying to resolve the address.
	 * [dot] is the current dot
	 * [marks] is the mark environment
	 * [start] is the position relative to which we are resolving the address
	 * [reverse] is the direction we are searching to
	 *
	 * Note that: some address constructors are absolute (and thus ignore
	 * [start] while other are relative (and use [start]). Also note that
	 * [dot] and [start] are unrelated. *)
	| Dot -> dot
	| Offset i -> Cursor.mk_relative ~start:i ~offset:0
	| Line _ -> failwith "TODO"
	| LastLine -> failwith "TODO"
	| Mark m -> begin
		match Mark.get marks m with
		| Some c -> c
		| None -> failwith "TODO: error management"
	end
	| Plus (a1, a2) ->
		let c = address text dot marks start reverse a1 in
		address text dot marks (Cursor.end_ c) reverse a2
	| Minus (a1, a2) ->
		let c = address text dot marks start reverse a1 in
		address text dot marks (Cursor.start c) (not reverse) a2
	| Comma (a1, a2) ->
		let c1 = address text dot marks start reverse a1 in
		let c2 = address text dot marks start reverse a2 in
		Cursor.range c1 c2
	| Semicolon (a1, a2) ->
		let c1 = address text dot marks start reverse a1 in
		let c2 = address text c1 marks (Cursor.end_ c1) reverse a2 in
		Cursor.range c1 c2
	| ForwardRe re -> begin
		let dfa = Regexp.compile re in
		match Regexp.next_match dfa text (Cursor.mk_absolute start start) with
		| None -> failwith "TODO: error mgmt"
		| Some c -> c
	end
	| BackwardRe re -> begin
		let dfa = Regexp.compile re in
		match Regexp.prev_match dfa text (Cursor.mk_absolute start start) with
		| None -> failwith "TODO: error mgmt"
		| Some c -> c
	end

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
		([Patch.mk (Cursor.mk_absolute e e) t], marks)
	| Insert t ->
		let s = Cursor.start dot in
		([Patch.mk (Cursor.mk_absolute s s) t], marks)
	| Replace t ->
		([Patch.mk dot t], marks)
	| Substitute (se, sr) -> failwith "TODO"
	| Delete ->
		([Patch.mk dot Text.empty], marks)

	| Move a ->
		let target = address text dot marks (Cursor.start dot) false a in
		([Patch.mk target (Text.sub text dot); Patch.mk dot Text.empty],
		marks)
	| Copy a ->
		let target = address text dot marks (Cursor.start dot) false a in
		([Patch.mk target (Text.sub text dot)], marks)

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
		([Patch.mk dot res], marks)
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
		([Patch.mk dot res], marks)

	| SetMark m ->
		([], Mark.put marks m dot)

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
	let dot = address text dot marks 0 false addr in
	let (patches, marks) = action text dot marks act in
	(patches, marks)

end
