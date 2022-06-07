open Prims
type 'a st = Prims.int -> ('a * Prims.int)
let (x : Prims.int) = Prims.int_zero
let (read_and_increment : Prims.int st) =
  fun x1 -> (x1, (x1 + Prims.int_one))
let (incr : Prims.int -> Prims.int) = fun x1 -> x1 + Prims.int_one