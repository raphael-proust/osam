type t = unit
type env = Cursor.t option
let empty = None
let put _ () c = Some c
let get e () = e
