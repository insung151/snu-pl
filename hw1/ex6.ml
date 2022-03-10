(* 2016-16322 황인성 *)

type expr = NUM of int
  | PLUS of expr * expr
  | MINUS of expr * expr
  | MULT of expr * expr
  | DIVIDE of expr * expr
  | MAX of expr list

let rec eval : expr -> int = fun e ->
  match e with
  | NUM i -> i
  | PLUS (e1, e2) -> eval(e1) + eval(e2)
  | MINUS (e1, e2) -> eval(e1) - eval(e2)
  | MULT (e1, e2) -> eval(e1) * eval(e2)
  | DIVIDE (e1, e2) -> eval(e1) / eval(e2)
  | MAX [] -> 0
  | MAX (head::[]) -> eval(head)
  | MAX (first::second::tail) -> 
    if eval(first) > eval(second) then
      eval(MAX (first::tail))
    else 
      eval(MAX (second::tail))
