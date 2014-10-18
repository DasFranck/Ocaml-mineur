(* Prerequis *)
open Random;;
self_init();;

let gcell (x,y) board = 
  let rec gc1 y n board = match board with
    |[]              -> -5
    |e::l when n = y -> e
    |e::l            -> gc1 y (n+1) l
  in

  let rec gc2 x y n board = match board with
    |[]              -> []
    |e::l when n = x -> [gc1 y 1 e]
    |e::l            -> gc2 x y (n+1) l
  in
     gc2 (x-1) y 0 board;;

let rec flat x = match x with
  |[]                 -> []
  |[]::l              -> (flat l)
  |(a::b)::l          -> a ::(flat (b::l))


(* 3.1 Cellule *)
let get_cell (x,y) board = 
  let rec gc1 x n board = match board with
    |[]              -> failwith "Erreur, liste vide (1)"
    |e::l when n = x -> e
    |e::l            -> gc1 x (n+1) l
  in

  let gc2 y n board = match board with
    |[]              -> failwith "Erreur, liste vide (2)"
    |e::l when n = y -> e
    |e::l            -> gc1 y (n+1) l
  in
     gc2 y 1 (gc1 x 1 board);;


(* 3.2 Remplacement *)
let replace_cell value (x,y) board = 
  let rec sizefac board n = match board with
    |[]    -> 0
    |e::[] -> n+1
    |e::l  -> sizefac l (n+1)
   in
  let size = sizefac board 0
   in
  let rec grcy (y,brd) = match (y,brd) with
    |(n,e::l) when y>size || y<=0 -> failwith "Error (1)"
    |(_,[])               -> []
    |(1,e::l) when y = 1  -> value::l
    |(n,e::l) when y = n  -> e::(grcy ((n-1),l))
    |_                    -> failwith "Error (2)"
  in
  let rec grcx (x,brd) = match (x,brd) with
    |(n,e::l) when x>size || x<=0 -> failwith "Error (3)"
    |(_, [])              -> []
    |(1,e::l) when x = 1  -> (grcy (y,e))::l
    |(n,e::l) when x = n  -> e::(grcx ((n-1),l))
    |_                    -> failwith "Error (4)" 
  in 
     grcx (x,board);;


(* 3.3 Donneur de vie *)
let seed_life board n =
  let rec sizefac board n = match board with
    |[]    -> 0
    |e::[] -> n+1
    |e::l  -> sizefac l (n+1)
  in
  let rec sl n c board =
    let x = ((Random.int c)+1) and
        y = ((Random.int c)+1) 
     in
    let size = sizefac board 0
     in
    if n>(size*size) then failwith "n est superieur au nombre de cases" 
    else  if n > 0 then
            if (get_cell (x,y) board) = 0
              then sl (n-1) c (replace_cell 1 (x,y) board)
            else   sl  n    c  board
          else board

 in sl n (sizefac board 0) board;;


(* 3.4 Voisinage *)
let get_cell_neighborhood (x,y) board = 
  let rec killbill x = match x with
    |[]               -> []
    |e::l when e = -5 -> killbill l
    |e::l             -> e::(killbill l)
   in
  let rec sizefac board n = match board with
    |[]        -> 0
    |e::[]     -> n+1
    |e::l      -> sizefac l (n+1)
   in
  let size = sizefac board 0
   in
  if x > size || y > size || x<=0 || y<=0 then failwith "Error (5)"
  else
  killbill  (flat [gcell ((x-1),(y+1)) board; gcell (x  ,(y+1)) board; gcell ((x+1),(y+1)) board;
                   gcell ((x-1),(y  )) board                         ; gcell ((x+1),y    ) board;
                   gcell ((x-1),(y-1)) board; gcell (x  ,(y-1)) board; gcell ((x+1),(y-1)) board]);;
