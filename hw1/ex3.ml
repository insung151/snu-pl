(* 2016-16322 황인성 *)

type formula = TRUE
  | FALSE
  | NOT of formula
  | ANDALSO of formula * formula
  | ORELSE of formula * formula
  | IMPLY of formula * formula
  | LESS of expr * expr
and expr = NUM of int
  | PLUS of expr * expr
  | MINUS of expr * expr


let rec eval: formula -> bool = fun (f) -> 
  let rec calc: expr -> int = fun e ->
    match e with
    | PLUS (e1, e2) ->  calc(e1) + calc(e2)
    | MINUS (e1, e2) -> calc(e1) - calc(e2)
    | NUM e1 -> e1
  in 
  match f with
  | TRUE -> true
  | FALSE -> false
  | NOT f1 -> not (eval(f1))
  | ANDALSO (f1, f2) -> eval(f1) && eval(f2)
  | ORELSE (f1, f2) -> eval(f1) || eval(f2)
  | IMPLY (f1, f2) -> not (eval (f1) == true && eval (f2) == false)
  | LESS (e1, e2) -> calc(MINUS (e1, e2 )) < 0
