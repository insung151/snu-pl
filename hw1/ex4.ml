(* 2016-16322 황인성 *)

type crazy2 = NIL 
  | ZERO of crazy2 
  | ONE of crazy2 
  | MONE of crazy2 

let rec crazy2val: crazy2 -> int = fun c -> 
    match c with
    | ZERO c1 -> crazy2val(c1) * 2
    | ONE c1 -> crazy2val(c1) * 2 + 1
    | MONE c1 -> crazy2val(c1) * 2 - 1
    | NIL -> 0
