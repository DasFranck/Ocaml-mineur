type 'a bundle = 
  | Empty
  | Item of 'a * 'a bundle;;


(* 0.1 Vide *)
let empty_bundle = Empty;;


(* 0.2 Est Vide ? *)
let is_empty x = match x with 
  |Empty -> true
  |_     -> false;;


(* 0.3 Constructeur *)
let cons x y = Item(y, x);;


(* 0.4 Le premier *)
let head humble = match humble with
  |Item(x, y) -> x
  |_          -> failwith "Head failed: empty bundle";;

(* 0.5 Le reste *)
let tail humble = match humble with
  |Item(x, y) -> y
  |_          -> failwith "Tail failed: empty tail";;
