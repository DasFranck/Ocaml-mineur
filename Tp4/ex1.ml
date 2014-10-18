(* Prerequis *)
let rec flat x = match x with
  |[]                 -> []
  |[]::l              -> (flat l)
  |(a::b)::l          -> a ::(flat (b::l))

(* 1.1 Compte *)
let rec cell_count lst = match lst with
  |[]                   -> 0
  |e::l when e=0 || e=1 -> e+(cell_count l)
  |_ -> failwith "Erreur : liste incorrecte";;


(* 1.2 Re-Compte *)
let rec remaining x = cell_count(flat x);;
