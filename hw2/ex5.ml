(* 2016-16322 황인성 *)

exception NOMOVE of string


type item = string

type tree = LEAF of item
  | NODE of tree list

type zipper = TOP
  | HAND of tree list * zipper * tree list

type location = LOC of tree * zipper

let goLeft loc = match loc with
  | LOC(t, TOP) -> raise (NOMOVE "left of top")
  | LOC(t, HAND(l::left, up, right)) -> LOC(l, HAND(left, up, t::right))
  | LOC(t, HAND([], up, right)) -> raise (NOMOVE "left of first")

let goRight loc = match loc with
  | LOC(t, TOP) -> raise (NOMOVE "left of top")
  | LOC(t, HAND(left, up, l::right)) -> LOC(l, HAND(t::left, up, right))
  | LOC(t, HAND(left, up, [])) -> raise (NOMOVE "left of first")

let rec goUp loc = match loc with
| LOC(t, TOP) -> raise (NOMOVE "left of top")
| LOC(t, HAND(left, up, right)) -> LOC(NODE (List.rev(left)@(t::right)), up)

let rec goDown loc = match loc with
| LOC(LEAF _, z) -> raise (NOMOVE "")
| LOC(NODE (head::tail), h) -> LOC(head, HAND([], h,tail))
| LOC(NODE [], h) -> raise (NOMOVE "")