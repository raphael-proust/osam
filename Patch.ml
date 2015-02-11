
type t = (Cursor.t * Text.t)
let mk c t = (c, t)
let shift o (c,t) = (Cursor.shift c o, t)
let effect (c, t) = Text.length t - Cursor.length c

let merge_fail (_:t) (_:t) :t = raise (Invalid_argument "Conflicting patches")

let sort ?(merge=merge_fail) patches =
	let compare (c1, _) (c2, _) = Cursor.start c2 - Cursor.start c1 in
	let patches = List.sort compare patches in
	let rec loop rev_acc ((clast, _) as last) = function
		| [] -> last :: rev_acc
		| ((c,_) as patch)::patches ->
			if Cursor.overlap c clast then
				loop rev_acc (merge last patch) patches
			else
				loop (last :: rev_acc) patch patches
	in
	match patches with
	| [] -> []
	| p::ps -> List.rev (loop [] p ps)

let hook ?(o=0) ?l ~f ~acc txt patches =
	let l = match l with
		| None -> Text.length txt - o
		| Some l ->
			if l > Text.length txt - o then
				raise (Invalid_argument "Code out of bound")
			else
				l
	in
	let rec loop acc offset patches =
		if offset = o + l then
			acc
		else
			match patches with
			| [] ->
				Text.hook ~o:offset ~l:(l - (offset - o)) ~f ~acc txt
			| ((cur,repl) as p)::patches ->
				(* FIXME: need to make sure we don't go over `l` *)
				let acc =
					Text.hook ~o:offset ~l:(Cursor.start cur - o) ~f ~acc txt
				in
				let acc = Text.hook ~f ~acc repl in
				let patches = List.map (shift (effect p)) patches in
				loop acc (Cursor.end_ cur) patches
	in
	loop acc o patches
