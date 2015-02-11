
(* INVARIANT: the start (lhs) is lower or equal to the end (rhs).
 * INVARIANT: the start is positive or zero.
 *)
type t = int * int

(*TODO? factorise constructors?*)
let mk_point i =
	if i < 0 then
		failwith "TODO: err mgmt"
	else
		(i, i)
let mk_relative ~start ~offset =
	if start < 0 then
		failwith "TODO: err mgmt"
	else if 0 <= offset then
		(start, start+offset)
	else
		(start + offset, start)
let mk_absolute ~start ~end_ =
	if start < 0 then
		failwith "TODO: err mgmt"
	else if start <= end_ then
		(start, end_)
	else
		(end_, start)

let range (s1,e1) (s2,e2) =
	(min s1 s2, max e1 e2)

let relative (start, end_) = (start, end_ - start)
let absolute t = t
let start (s, _) = s
let end_ (_, e) = e
let length (s, e) = e - s

(* TODO: replace by constructor to make sure the INVARIANT is safe. *)
let shift (s,e) o = (s+o, e+o)
let shift_end (s,e) o =
	if s <= e+o then
		(s, e+o)
	else
		(e+o, s)
let shift_start (s,e) o =
	if s+o <= e then
		(s+o, e)
	else
		(e, s+o)

let split_relative ((start, end_) as t) i =
	if i < 0 then
		((start, start), t)
	else if end_ - start <= i then
		(t, (end_, end_))
	else
		((start, start+i), (start+i, end_))
let split_absolute ((start, end_) as t) i =
	if i <= start then
		((start, start), t)
	else if end_ <= i then
		(t, (end_, end_))
	else
		((start, i), (i, end_))


let overlap (s1, e1) (s2, e2) =
	(s1 <= s2 && s2 < e1)
	|| (s2 <= s1 && s1 < e2)

let contains (s, e) i =
	(s <= i && i < e)
