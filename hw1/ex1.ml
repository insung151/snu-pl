(* 2016-16322 황인성 *)

let rec merge : int list * int list -> int list = fun (x, y) ->
  match x, y with
  | x, [] -> x
  | [], y -> y
  |xhead::xtail, yhead::ytail -> 
    if xhead > yhead then
      xhead::merge (xtail, y)
    else
      yhead::merge (x, ytail)