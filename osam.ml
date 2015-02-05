
let () =
	let patches, _ =
		Action.run
			~text:Text.empty
			~dot:(Cursor.mk_point 0)
			~marks:Action.Mark.empty
			(Action.Offset 0)
			(Action.Replace Text.empty)
	in
	let res = Patch.apply_batch patches Text.empty in
	let s = Text.to_string res in
	print_string s




