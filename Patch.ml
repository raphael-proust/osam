
type t = (Cursor.t * Text.t)
let mk c t = (c, t)
let shift (c,t) o = (Cursor.shift c o, t)
