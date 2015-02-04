type t = string
type status = unit (*TODO: allow for lwt, functorize Action over Cmd*)
let build s = s
let run ?stdin ?stdout t = failwith "TODO"
