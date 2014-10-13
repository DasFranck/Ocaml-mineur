(* Prerequis *)
open Random;;
self_init();;

let rec append x y = match x with
  |[]   -> y
  |e::l -> e::(append l y);; 


(* 0.1 Liste *)
let rec gen_list n = match n with
  | n when n = 0 -> []
  | _ -> 0::(gen_list (n-1));;


(* 0.2 aleatoire *)
let rec gen_rand_list n = match n with
  | n when n = 0 -> []
  | _ -> (int(2))::(gen_rand_list (n-1));;


(* 0.3 Liste de Listes *)
let gen_board n = let rec genb n x = match n with
  | n when n = 0 -> []
  | _ -> append [(gen_list x)] (genb (n-1) x)
  in genb n n;;


(* 0.4 Liste de Listes aleatoire *)
let gen_rand_board n = let rec genb n x = match n with
  | n when n = 0 -> []
  | _ -> append [(gen_rand_list x)] (genb (n-1) x)
  in genb n n;;
