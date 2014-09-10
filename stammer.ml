(* Deps : mirror.ml, abba.ml*)
let stammer x  = let ba = string_of_int(mirror x) in
    let abba = string_of_int (abba x) in
    let ab = string_of_int x in 
    int_of_string (ba^ab^abba);;
