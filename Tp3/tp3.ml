let rec are_equal x y = match (x,y) with 
  |([],[]) -> true
  |(a::b,c::d) when a = c -> are_equal b d
  |_ -> false;;

let rec append x y = match (x,y) with
  |([],_) -> y
  |(_,[]) -> x
  |(e::l,_) -> e::(append l y);;
