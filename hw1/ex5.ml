(* 2016-16322 황인성 *)

type crazy2 = NIL 
  | ZERO of crazy2 
  | ONE of crazy2 
  | MONE of crazy2 

let rec crazy2add: crazy2 * crazy2 -> crazy2 = fun (c1, c2) -> 
  match c1, c2 with
  | ZERO e1, ZERO e2 -> ZERO(crazy2add(e1, e2))
  | MONE e1, ONE e2 -> ZERO(crazy2add(e1, e2))
  | ONE e1, MONE e2 -> ZERO(crazy2add(e1, e2))
  | ONE e1, ONE e2 -> ZERO(crazy2add(crazy2add(e1, e2), ONE(NIL)))
  | MONE e1, MONE e2 -> ZERO(crazy2add(crazy2add(e1, e2), MONE(NIL)))
  | NIL, c2 -> c2
  | c1, NIL -> c1
  | ZERO e1, ONE e2 -> ONE(crazy2add(e1, e2))
  | ONE e1, ZERO e2 -> ONE(crazy2add(e1, e2))
  | ZERO e1, MONE e2 -> MONE(crazy2add(e1, e2))
  | MONE e1, ZERO e2 -> MONE(crazy2add(e1, e2))
