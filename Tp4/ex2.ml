(*Prerequis *)
#load "graphics.cma" ;;
open Graphics;;
open_graph "" ;;

let rec drsq x y size color =
  set_color color;
  match size with
    |size when size = 0 -> ()
    |_ -> draw_rect x y size size;
          drsq x y (size-1) color;;

(* 2.1 Carre *)
let draw_square (x,y) size = drsq x y size black;;  


(* 2.2 Carre - encore *)
let rec draw_fill_square (x,y) size color =
  set_color color;
  draw_rect x y size size;
  drsq x y (size-1) color;;


(* 2.3 Cellule *)
let draw_cell (x,y) size cell = match cell with
  |0 -> draw_square (x,y) size;
        draw_fill_square (x+1,y+1) (size-2) white 
  |1 -> draw_square (x,y) size 
  |_ -> failwith "Statue de la cellule invalide";;


(* 2.4 Plateau *)
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
