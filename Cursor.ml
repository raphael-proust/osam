
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

(*Note: we manually inline the constructors here. Be careful about the
 * INVARIANT. *)
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

let overlap (s1, e1) (s2, e2) =
	(s1 <= s2 && s2 < e1)
	|| (s2 <= s1 && s1 < e2)

