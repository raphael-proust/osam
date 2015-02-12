(*TODO: functorise over marks and text, provide both UTF8 and ASCII text*)
module Mark : Mark.S

type addr =
	(** The current range. *)
	| Dot
	(** Absolute number of characters *)
	| Offset of int
	(** The range between the [n-1]th and [n]th newline character. The [n-1]th
	 * newline char is the start of the text. *)
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
	(** The first match of the Regexp *)
	| ForwardRe of Regexp.DSL.t
	(** The first match of the Regexp backward. *)
	| BackwardRe of Regexp.DSL.t


(*make a separate module for substitution*)
type substitutee
type substitutor

(** Actions: ways to change the text. *)
type action =

	(** Dispatch executes several actions in a row. The first action is
	 * executed, the offsets are patched, and the next action is run, until
	 * there are none. *)
	| Dispatch of action list

	(** For: execute the action for each of the matches of the regexp in the
	 * current range. Actions are executed with the range set to the regexp
	 * match. *)
	| For of Regexp.DSL.t * action
	(** Rof: execute the action for each of the gap between the matches of the
	 * regexp in the current range. The gaps include the selection between
	 * the start of the dot and the first match as well as between the last
	 * match and the end of the dot -- even if these ranges are empty. *)
	| Rof of Regexp.DSL.t * action
	(** If: execute the action if there is a substring that matches the
	 * regexp in the current range. *)
	| If of Regexp.DSL.t * action
	(** Ifnot: execute the action if there is *no* substring that matches the
	 * regexp in the current range. *)
	| Ifnot of Regexp.DSL.t * action

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
	| PipeOut of Syscmd.t
	(** PipeIn: replace the characters in the current range by the stdout of
	 * the given system command. *)
	| PipeIn of Syscmd.t
	(** Pipe: filter the current range of characters through the given system
	 * command. *)
	| Pipe of Syscmd.t

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

