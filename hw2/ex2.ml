(* 2016-16322 황인성 *)
}
type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
  | Poland | Portugal | Italy | Germany | Norway | Sweden | England
  | Argentina

type tourna = LEAF of team
  | NODE of tourna * tourna

let rec parenize: tourna -> string = fun t ->
  match t with
  | LEAF x -> 
    (match x with
    | Korea -> "Korea"   | France -> "France" | Usa -> "Usa"   | Brazil -> "Brazil" | Japan -> "Japan"
    | Nigeria -> "Nigeria"  | Cameroon -> "Cameroon"    | Poland -> "Poland"    | Portugal -> "Portugal"
    | Italy -> "Italy"  | Germany -> "Germany"  | Norway -> "Norway"    | Sweden -> "Sweden"
    | England -> "England"  | Argentina -> "Argentina")
  | NODE (t1, t2) ->
    "(" ^ parenize(t1) ^ " " ^ parenize(t2) ^ ")"
