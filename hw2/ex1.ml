(* 2016-16322 황인성 *)

let rec iter: (int * ('a -> 'a)) -> 'a -> 'a = fun (n, f) ->
  if n == 0 then
    fun x -> x
  else
    fun x -> iter(n-1, f)(f(x))
