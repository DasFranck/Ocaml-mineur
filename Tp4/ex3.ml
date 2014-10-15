(* Prerequis *)


(* 3.1 Cellule *)
let get_cell (x,y) board = 
  let rec gc1 x n board = match board with
    |[] -> failwith "Erreur, liste vide"
    |e::l when n = x -> e
    |e::l            -> gc1 x (n+1) l
  in

  let gc2 y n board = match board with
    |[] -> failwith "Erreur, liste vide (2)"
    |e::l when n = y -> e
    |e::l            -> gc1 y (n+1) l
  in
   
    gc2 y 1 (gc1 x 1 board);;


(* 3.2 Remplacement *)
let replace_cell value (x,y) board = match 


(* 3.3 *)


(* 3.4 *)
