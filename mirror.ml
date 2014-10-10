let mirror x = let nombre = (string_of_int x) in
    let nom = nombre.[0] in
    let bre = nombre.[1] in
    let noms = String.make 1 nom in
    let bres = String.make 1 bre in
    int_of_string (bres^noms);
