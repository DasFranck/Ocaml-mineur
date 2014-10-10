(* 0.1 Egalite  *)
let rec are_equal x y = match (x,y) with 
  |([],[]) -> true
  |(a::b,c::d) when a = c -> are_equal b d
  |_ -> false;;


(* 0.2 Concatenation *)
let rec append x y = match (x,y) with
  |([],_) -> y
  |(_,[]) -> x
  |(e::l,_) -> e::(append l y);;


(* 0.3 Inversion *)
let reverse a =
  let rec rec_reverse a b = match a with
    |[] -> b
    |e::l -> rec_reverse l (e::b)
  in
      rec_reverse a [];;


(* 0.4 Impression *)
let rec impress a = match a with
    |[] -> print_newline (); print_newline ();
    |e::l -> (print_char e; impress l);; 
