(* Deps : mirror.ml *)
let abba x = let ba = mirror x in
    let ab = (string_of_int x) in
    let ba = (string_of_int ba)in
        int_of_string (ab^ba);;
