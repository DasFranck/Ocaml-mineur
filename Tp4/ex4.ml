(* Prerequis *)
#load "unix.cma";;
open Unix;;
#load "graphics.cma" ;;
open Graphics;;
open_graph "" ;;

let rec flat x = match x with
  |[]                 -> []
  |[]::l              -> (flat l)
  |(a::b)::l          -> a ::(flat (b::l))

let rec cell_count lst = match lst with
  |[]                   -> 0
  |e::l when e=0 || e=1 -> e+(cell_count l)
  |_ -> failwith "Erreur : liste incorrecte";;

let rec remaining x = cell_count(flat x);;

let rec drsq x y size color =
  set_color color;
  match size with
    |size when size = 0 -> ()
    |_ -> draw_rect x y size size;
          drsq x y (size-1) color;;

let draw_square (x,y) size = drsq x y size black;;  

let rec draw_fill_square (x,y) size color =
  set_color color;
  draw_rect x y size size;
  drsq x y (size-1) color;;

let draw_cell (x,y) size cell = match cell with
  |0 -> draw_square (x,y) size;
        draw_fill_square (x+1,y+1) (size-2) white 
  |1 -> draw_square (x,y) size 
  |_ -> failwith "Statue de la cellule invalide";;

let draw_board board size =
  let rec drborde a x y = match a with
    |(e::l)::m -> draw_cell ((x*size),(y*size)) size e;
                  drborde (l::m) (x+1) y
    |[]::m     -> drborde m 0 (y-1)
    |[]        -> ()
  in

  let rec tbsize x = match x with
    |[]   -> 0
    |e::l -> 1+(tbsize l)
  
  in drborde board 0 ((tbsize board)-1);;

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
    else if (numneigh (x,y) board = 3) then replace_cell 1 (x,y) final
         else final
   in 
  let rec boardy (x,y) board final = match (x,y) with
    |(x,y) when ((x = size) && (y = size)) -> changethemall (x,y) board final
    |(x,y) when  (y = size)                -> boardy (x+1,1) board (changethemall (x,y) board final)
    |(x,y)                                 -> boardy (x,y+1) board (changethemall (x,y) board final)
   in 
     boardy (1,1) board board;;

	 
(* 4.2 Jouer *)    	 
let rec play board =
  let again = remaining board
   in
  match again with 
    |0     ->  draw_board board 50
    |again ->  draw_board board 50; Unix.sleep 1; play (iterate board);;

(* ATTENTION : Toutes les fonctions ne marchent correctement que si le plateau de jeu est carré!!! *)
