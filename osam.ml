
let () = Printexc.record_backtrace true

let () = Text.test ()

let () =
	let text = Text.from_string "foobarbaz" in
	let dot = Cursor.mk_relative 0 9 in
	let marks = Action.Mark.empty in


	let test name expect addr act =
		print_endline name;
		print_endline expect;
		let (patches, _) = Action.run ~text ~dot ~marks addr act in
		let patches = Patch.sort patches in
		let () =
			Patch.hook
				~f:(fun () -> output_substring stdout)
				~acc:()
				text
				patches
		in
		print_newline ()
	in

	(* TEST1! Status: fail *)
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

	let () = test "TEST1" "foo[bar][baz]" addr act in

	(* TEST2! *)
	let addr = Action.Dot in
	let pre = Text.from_string "[" in
	let post = Text.from_string "]" in
	let act = Action.(Dispatch [Insert pre; Append post]) in

	let () = test "TEST2" "[foobarbaz]" addr act in


	let () = flush_all () in
	exit 0

