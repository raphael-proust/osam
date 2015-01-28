
module Codepoint : sig
    type t
    val width: t -> int (* returns between 1 and 6 *)
    val of_char: char -> t
    val of_int: int -> t
    (*TODO: properties (lowercase, uppercase, scripts, &c.)*)
end = struct
    type t
    let width _ = failwith "TODO"
    let of_char _ = failwith "TODO"
    let of_int _ = failwith "TODO"
end

module Text : sig
    type t
    type offset = int
    type cursor = offset * offset
    type patch = (cursor * t) option

    val shift: cursor -> offset -> cursor

    val nth: offset -> t -> Codepoint.t
    val sub: t -> cursor -> t
    val fold: t -> 'a -> ('a -> Codepoint.t -> 'a) -> 'a
    val length: t -> offset
    val empty: t
    val cat: t list -> t

    val apply: t -> patch -> t
    val compose: patch list -> patch

    val get: in_channel -> t
    val put: t -> out_channel -> unit
end = struct
    type t
    type offset = int
    type cursor = offset * offset
    type patch = (cursor * t) option

    let shift _ _ = failwith "TODO"

    let nth _ _ = failwith "TODO"
    let sub _ _ = failwith "TODO"
    let fold _ _ _ = failwith "TODO"
    let length _ = failwith "TODO"
    let empty = failwith "TODO"
    let cat _ = failwith "TODO"

    let apply _ _ = failwith "TODO"
    let compose _ = failwith "TODO"

    let get _ = failwith "TODO"
    let put _ _ = failwith "TODO"

end

module Marks : sig
    (*TODO: make several Marks possible (especially using more than unit) *)
    type t = unit
    type env
    val empty: env
    val put: env -> t -> Text.cursor -> env
    val get: env -> t -> Text.cursor option
end = struct
    type t = unit
    type env = Text.cursor option
    let empty = None
    let put _ () c = Some c
    let get e () = e
end

module Regexp : sig
    type nfa
    type dfa
    val compile: nfa -> dfa
    val has_match: dfa -> Text.t -> bool
    val next_match: dfa -> Text.t -> Text.cursor option
    val all_matches: dfa -> Text.t -> Text.cursor list
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
    let has_match _ _ = failwith "TODO"
    let next_match _ _ = failwith "TODO"
    let all_matches _ _ = failwith "TODO"
    module DSL = struct
        let point _ = failwith "TODO"
        let cat _ = failwith "TODO"
        let alt _ = failwith "TODO"
        let star _ = failwith "TODO"
        let question _ = failwith "TODO"
        let plus _ = failwith "TODO"
    end
end

(*TODO: functorise over marks and text, provide both UTF8 and ASCII text*)
module Actions : sig

    type syscmd
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

	(** For: execute the action for each of the matches of the Regexp.nfa in the
	 * current range. Actions are executed with the range set to the Regexp.nfa
	 * match. *)
	| For of Regexp.nfa * action
	(** Rof: execute the action for each of the gap between the matches of the
	 * Regexp.nfa in the current range. *)
	| Rof of Regexp.nfa * action
	(** If: execute the action if there is a substring that matches the
	 * Regexp.nfa in the current range. *)
	| If of Regexp.nfa * action
	(** Ifnot: execute the action if there is *no* substring that matches the
	 * Regexp.nfa in the current range. *)
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
	| PipeOut of syscmd
	(** PipeIn: replace the characters in the current range by the stdout of
	 * the given system command. *)
	| PipeIn of syscmd
	(** Pipe: filter the current range of characters through the given system
	 * command. *)
	| Pipe of syscmd

	(** Sets the given mark to address the current selection. *)
	| SetMark of Marks.t

	(* TODO: some form of yank and paste? *)
    type t = addr * action
end = struct

    type syscmd
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
	| PipeOut of syscmd
	| PipeIn of syscmd
	| Pipe of syscmd
	| SetMark of Marks.t

type t =
	addr * action

end

module Driver : sig
    val run:
        (* Pass the text to be edited *)
        text:Text.t ->
        (* Some context *)
        dot:Text.cursor -> marks:Marks.env ->
        (* And the action to be executed *)
        Actions.t ->
        (* We return a patch rather than modifying the text *)
        Text.patch
end = struct

    open Actions

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

(*We don't pass dot because we expect the caller to use Text.sub
 * Will have to be changed to accomodate for Move and Copy. *)
let rec action text = function
	| Dispatch acts ->
        (*NOTE: we use map (not fold) which means that SetMark doesn't affect
         * the other actions. Currently, SetMark is nop, need fixing. *)
        List.map (action text) acts
        |> Text.compose
	| For (re, act) ->
        let dfa = Regexp.compile re in
        Regexp.all_matches dfa text
        |> List.map (Text.sub text)
        |> List.map (fun t -> action t act)
        |> Text.compose
	| Rof (re, act) ->
        let dfa = Regexp.compile re in
        Regexp.all_matches dfa text
        |> (fun _ -> failwith "TODO: inverse all the cursors")
        |> List.map (Text.sub text)
        |> List.map (fun t -> action t act)
        |> Text.compose
	| If (re, act) ->
        let dfa = Regexp.compile re in
        if Regexp.has_match dfa text then
            action text act
        else
            None
	| Ifnot (re, act) ->
        let dfa = Regexp.compile re in
        if Regexp.has_match dfa text then
            None
        else
            action text act
	| Append t ->
        let e = Text.length text in
        Some ((e,e), t)
	| Insert t ->
        Some ((0,0), t)
	| Replace t ->
        Some ((0, Text.length text) , t)
	| Substitute (se, sr) -> failwith "TODO"
	| Delete ->
        Some ((0, Text.length text) , Text.empty)
	| Move a -> failwith "TODO"
	| Copy a -> failwith "TODO"
	| PipeOut cmd -> failwith "TODO"
	| PipeIn cmd -> failwith "TODO"
	| Pipe cmd -> failwith "TODO"
	| SetMark m -> failwith "TODO"

let run ~text ~dot ~marks (addr, act) =
    let dot = address text dot marks addr in
    action (Text.sub text dot) act


end
