
let () = Printexc.record_backtrace true

let () =
	let text = Text.from_string "foobarbaz" in
	let dot = Cursor.mk_point 0 in
	let marks = Action.Mark.empty in
	let re = Regexp.DSL.(
		seq [
			point (Char.code 'b');
			point (Char.code 'a');
			alt [
				point (Char.code 'z');
				point (Char.code 'r');
			];
		])
	in
	let addr = Action.(Comma (Offset 0, Offset 9)) in
	let pre = Text.from_string "[" in
	let post = Text.from_string "]" in
	let act = Action.(For(re, Dispatch [Insert pre; Append post])) in

	let (patches, _) =
		Action.run ~text ~dot ~marks
			addr act
	in

	let () =
		Patch.hook
			~f:(fun () -> output_substring stdout)
			~acc:()
			text
			patches
	in

	let () = flush_all () in

	exit 0

