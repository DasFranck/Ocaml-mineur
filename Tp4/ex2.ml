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
 |0 -> draw_square (x,y) size
 |1 -> draw_fill_square (x,y) size green
 |_ -> failwith "Statue de la cellule invalide";;

(* 2.4 Plateau *)
let draw_board board size =
  let drli

  let rec lslist board size (xloc,yloc) = match board with
     |a::l -> drli 
     |_ ->  
  

