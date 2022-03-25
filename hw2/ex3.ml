(* 2016-16322 황인성 *)

type ae = CONST of int
  | VAR of string
  | POWER of string * int
  | TIMES of ae list
  | SUM of ae list

  exception InvalidArgument

  let rec diff: ae * string -> ae = fun (e, s) ->
    match e with
    | CONST i -> CONST 0
    | VAR v -> 
      if v = s then
        CONST 1
      else
        CONST 0
    | POWER (v, i) ->
      if v = s then
        TIMES ((CONST i)::POWER(v, i - 1)::[])
      else
        CONST 0
    | TIMES [] -> raise InvalidArgumentx
    | TIMES (head::[]) -> diff(head, s)
    | TIMES (head::tail) ->
      SUM ((TIMES (diff(head, s)::(TIMES tail)::[]))::(TIMES (head::diff(TIMES tail, s)::[]))::[])
    | SUM [] -> raise InvalidArgument
    | SUM (head::[]) -> 
      diff(head, s)
    | SUM (head::tail) -> 
      SUM (diff(head, s)::diff(SUM tail, s)::[])
