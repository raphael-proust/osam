
type t = (Cursor.t * Text.t)
let mk c t = (c, t)

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
					let text = Text.change text p in
					let offset = offset + effect p in
					(text, Cursor.end_ c, offset)
				)
				(text, 0, 0)
				cs
		in
		text

