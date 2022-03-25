(* 2016-16322 황인성 *)

type heap = EMPTY | NODE of rank * value * heap * heap 
and rank = int
and value = int

let rank h = match h with
  | EMPTY -> -1
  | NODE(r, _, _, _) -> r


exception EmptyHeap

let shake (x,lh,rh) = if (rank lh) >= (rank rh)
	then NODE(rank rh + 1, x, lh, rh)
	else NODE(rank lh + 1, x, rh, lh)


let rec merge: heap * heap -> heap = fun (h1, h2) ->
  match h1, h2 with
  | EMPTY, b -> b
  | a, EMPTY -> a
  | NODE (arank, av, al, ar), NODE(brank, bv, bl, br) ->
    if (av > bv) then
      shake(bv, h1, merge(bl, br))
    else 
      shake(av, h2, merge(al, ar))
    


let insert(x, h) = merge(h, NODE(0, x, EMPTY, EMPTY))

let findMin h = match h with
  | EMPTY -> raise EmptyHeap
  | NODE(_, x, _, _) -> x

  let deleteMin h = match h with
  | EMPTY -> raise EmptyHeap
  | NODE(_, x, lh, rh) -> merge(lh,rh)
