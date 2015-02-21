
type t = (Cursor.t * Text.t)
let mk c t = (c, t)
let shift o (c,t) = (Cursor.shift c o, t)
let effect (c, t) = Text.length t - Cursor.length c

let merge_fail (_:t) (_:t) :t = raise (Invalid_argument "Conflicting patches")

let sort ?(merge=merge_fail) patches =
	let compare (c1,_) (c2,_) = compare (Cursor.start c1) (Cursor.start c2) in
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
	let total_length =
		Text.length txt
		+ (List.fold_left (+) 0 (List.map effect patches))
		- o
	in
	let l = match l with
		| None -> total_length
		| Some l ->
			if l > total_length then
				raise (Invalid_argument "Code out of bound")
			else
				l
	in
	let rec loop acc offset_p offset_t patches =
		if offset_p = o + l then
			acc
		else
			match patches with
			| [] ->
				Text.hook ~o:offset_t ~l:(l - offset_p) ~f ~acc txt
			| ((cur,repl) as p)::patches ->
				(* FIXME: This is more complicated, we need an offset to track
				 * progress on the patched text and one on the original text.
				 * *)
				let acc =
					Text.hook
						~o:offset_t ~l:(Cursor.start cur - offset_p)
						~f ~acc txt
				in
				let acc = Text.hook ~f ~acc repl in
				let patches = List.map (shift (effect p)) patches in
				let offset_p =
					(offset_p
					+ (Cursor.start cur - offset_p)
					+ Text.length repl)
				in
				loop acc offset_p (Cursor.end_ cur) patches
	in
	let get_offset_t _ _ = 0 (*FIXME!*) in
	loop acc o (get_offset_t o patches) patches

let print (c,t) =
	let (s,e) = Cursor.absolute c in
	"((" ^ string_of_int s ^ "," ^ string_of_int e ^ "), "
	^ Text.to_string t ^ ")"
