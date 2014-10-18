(* Prerequis *)
#load "unix.cma";;
open Unix;;

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

let replace_cell value (x,y) board = 
  let rec grcy (y,brd) = match (y,brd) with
    |(_,[])             -> []
    |(1,e::l) when y = 1 -> value::l
    |(n,e::l) when y = n -> e::(grcy ((n-1),l))
    |_                   -> failwith "Error (1)"
   in
  let rec grcx (x,brd) = match (x,brd) with
    |(_, [])             -> []
    |(1,e::l) when x = 1 -> (grcy (y,e))::l
    |(n,e::l) when x = n -> e::(grcx ((n-1),l))
    |_                   -> failwith "Error (2)" 
   in 
      grcx (x,board);;

let rec flat x = match x with
  |[]                 -> []
  |[]::l              -> (flat l)
  |(a::b)::l          -> a ::(flat (b::l))

let rec cell_count lst = match lst with
  |[]                   -> 0
  |e::l when e=0 || e=1 -> e+(cell_count l)
  |_ -> failwith "Erreur : liste incorrecte";;

let rec remaining x = cell_count(flat x);;

let get_cell_neighborhood (x,y) board = 
  let rec killbill x = match x with
    |[]               -> []
    |e::l when e = -5 -> killbill l
    |e::l             -> e::(killbill l)
   in
   killbill  (flat [gcell ((x-1),(y+1)) board; gcell (x  ,(y+1)) board; gcell ((x+1),(y+1)) board;
                    gcell ((x-1),(y  )) board                         ; gcell ((x+1),y    ) board;
                    gcell ((x-1),(y-1)) board; gcell (x  ,(y-1)) board; gcell ((x+1),(y-1)) board]);;


(* 4.1 Iteration *)
let iterate board =
  let rec sizefac board n = match board with
    |[]    -> 0
    |e::[] -> n+1
    |e::l  -> sizefac l (n+1)
   in
  let size = sizefac board 0
   in
  let numneigh (x,y) board  = cell_count (get_cell_neighborhood (x,y) board)
   in
  let changethemall (x,y) board final =    
    if (get_cell (x,y) board = 1) then if (numneigh (x,y) board < 2) ||(numneigh (x,y) board > 3) then replace_cell 0 (x,y) final
                                       else final

(* 4.2 Jouer *)                                             
    else if (numneigh (x,y) board = 3) then replace_cell 1 (x,y) final
         else final
   in 
  let rec boardy (x,y) board final = match (x,y) with
    |(x,y) when ((x = size) && (y = size)) -> changethemall (x,y) board final
    |(x,y) when  (y = size)                -> boardy (x+1,1) board (changethemall (x,y) board final)
    |(x,y)                                 -> boardy (x,y+1) board (changethemall (x,y) board final)

  in 
     boardy (1,1) board board;;

let rec play board =
  let again = remaining board
   in
  match again with 
    |0     ->  draw_board board 50
    |again ->  draw_board board 50; Unix.sleep 1; play (iterate board);;
