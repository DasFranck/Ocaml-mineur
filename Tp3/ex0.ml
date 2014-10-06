let rec are_equal x y = match (x,y) with 
  |([],[]) -> true
  |(a::b,c::d) when a = c -> are_equal b d
  |_ -> false;;

let rec append x y = match (x,y) with
  |([],_) -> y
  |(_,[]) -> x
  |(e::l,_) -> e::(append l y);;

let reverse a =
    let rec rec_reverse a b = match a with
            |[] -> b
            |e::l -> rec_reverse l (e::b)
     in
       rec_reverse a [];;

let rec impress a = match a with
            |[] -> ()
            |e::l ->
                begin  
                  print_char e;
                  print_newline ();
                  impress l
                end;;
