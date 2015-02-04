(*TODO: allow for return status and asynchronous execution*)
type t
type status
val build: string -> t
val run: ?stdin:string -> ?stdout:Buffer.t -> t -> status
