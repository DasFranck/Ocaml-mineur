(* Prerequis *)
type boolean =
  | True | False
  | Var of string
  | Not of boolean
  | And of boolean * boolean
  | Or  of boolean * boolean;;

let rec append x y = match x with
  |[]   -> y
  |e::l -> e::(append l y);; 

(* 1.1 Valeur ? *)
let rec value str edeu = match edeu with
  |[]                    -> failwith "Erreur"
  |


(* 1.2 Extraction *)
let rec extract bll =
  let extr bll lst = match bll with
    |True |False -> []
    |Var x -> [x]
    |Not x -> extract x
    |Or (x,y)  -> append (append (extract x) lst) (extract y)
    |And (x,y) -> append (append (extract x) lst) (extract y)
   in (extr bll []);;

(* 1.3 Generateur *)
