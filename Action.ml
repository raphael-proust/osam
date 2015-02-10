(*TODO: find less ad-hoc more abstract constructs for address.
 * Absolute: Offset, Dot, Mark
 * Relative: Start/End of current resolution, Continue from current
 *   resolution, Set the dot to current resolution and continue with current
 *   resolution, &c.
 *
 * Provide a translation from the classic sam/acme constructors and the more
 * abstract ones.
 *)

module Mark = Mark.Unit
module Re = Regexp.DFA

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
	| ForwardRe of Regexp.DSL.t
	| BackwardRe of Regexp.DSL.t

type substitutee
type substitutor

type action =
	| Dispatch of action list
	| For of Regexp.DSL.t * action
	| Rof of Regexp.DSL.t * action
	| If of Regexp.DSL.t * action
	| Ifnot of Regexp.DSL.t * action
	| Append of Text.t
	| Insert of Text.t
	| Replace of Text.t
	| Substitute of substitutee * substitutor
	| Delete
	| Move of addr
	| Copy of addr
	| PipeOut of Syscmd.t
	| PipeIn of Syscmd.t
	| Pipe of Syscmd.t
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
	| Line _ -> failwith "TODO: line addresses"
	| LastLine -> failwith "TODO: LastLine address"
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
		let dfa = Re.compile re in
		match Re.next_match dfa text (Cursor.mk_absolute start start) with
		| None -> failwith "TODO: error mgmt"
		| Some c -> c
	end
	| BackwardRe re -> begin
		let dfa = Re.compile re in
		match Re.prev_match dfa text (Cursor.mk_absolute start start) with
		| None -> failwith "TODO: error mgmt"
		| Some c -> c
	end

(* TODO: we need to be able to parse regexps and marks to parse actions. Thus
 * we need to
 * functorise over Regexp (which gives the opportunity to support several
 * regexp style (plan9, vim, perl, &c.)). *)
let parse _ = failwith "TODO: Action.parse"

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
		let dots = Re.(all_matches (compile re) text dot) in
		fold_over_dots text dots marks act
	| Rof (re, act) ->
		let dots = Re.(all_matches (compile re) text dot) in
		let dots = reverse_dots dot dots in
		fold_over_dots text dots marks act
	| If (re, act) ->
		if Re.(has_match (compile re) text dot) then
			action text dot marks act
		else
			([], marks)
	| Ifnot (re, act) ->
		if Re.(has_match (compile re) text dot) then
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
	| Substitute (se, sr) -> failwith "TODO: Substitute Action"
	| Delete ->
		let empty = Text.from_string "" in
		([Patch.mk dot empty], marks)

	| Move a ->
		let empty = Text.from_string "" in
		let target = address text dot marks (Cursor.start dot) false a in
		([Patch.mk target (Text.sub text dot); Patch.mk dot empty],
		marks)
	| Copy a ->
		let target = address text dot marks (Cursor.start dot) false a in
		([Patch.mk target (Text.sub text dot)], marks)

	(*TODO: manage stdout and such *)
	| PipeOut cmd ->
		let (_ : Syscmd.status) =
			Syscmd.run ~stdin:(Text.to_string text) cmd
		in
		([], marks)
	| PipeIn cmd ->
		let b = Buffer.create 100 in
		let (_ : Syscmd.status) = Syscmd.run ~stdout:b cmd in
		let res = Text.from_string (Buffer.contents b) in
		([Patch.mk dot res], marks)
	| Pipe cmd ->
		let b = Buffer.create 100 in
		let (_ : Syscmd.status) =
			Syscmd.run
				~stdin:(Text.to_string text)
				~stdout:b
				cmd
		in
		let res = Text.from_string (Buffer.contents b) in
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

