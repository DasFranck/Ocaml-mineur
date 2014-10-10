(* Prerequis pour 2.1 *)
let rec append x y = match x with
  |[] -> y
  |e::l -> e::(append l y);; 
  
 let rec letter_to_morse x = match x with
  |'A'|'a' -> ['.';'-']
  |'B'|'b' -> ['-';'.';'.';'.']
  |'C'|'c' -> ['-';'.';'-';'.']
  |'D'|'d' -> ['-';'.';'.']
  |'E'|'e' -> ['.']
  |'F'|'f' -> ['.';'.';'-';'.']
  |'G'|'g' -> ['-';'-';'.']
  |'H'|'h' -> ['.';'.';'.';'.']
  |'I'|'i' -> ['.';'.']
  |'J'|'j' -> ['.';'-';'-';'-']
  |'K'|'k' -> ['-';'.';'-']
  |'L'|'l' -> ['.';'-';'.';'.']
  |'M'|'m' -> ['-';'-']
  |'N'|'n' -> ['-';'.']
  |'O'|'o' -> ['-';'-';'-']
  |'P'|'p' -> ['.';'-';'-';'.']
  |'Q'|'q' -> ['-';'-';'.';'-']
  |'R'|'r' -> ['.';'-';'.']
  |'S'|'s' -> ['.';'.';'.']
  |'T'|'t' -> ['-']
  |'U'|'u' -> ['.';'.';'-']
  |'V'|'v' -> ['.';'.';'.';'-']
  |'W'|'w' -> ['.';'-';'-']
  |'X'|'x' -> ['-';'.';'.';'-']
  |'Y'|'y' -> ['-';'.';'-';'-']
  |'Z'|'z' -> ['-';'-';'.';'.']
  |_       -> [];;

(* 2.1 Conversion *)
let word_to_morse x = let rec wtm w l =
  match l with
  |[] -> w
  |m::l -> wtm (append w (letter_to_morse m)) l
    in
       wtm [] x;;

(* 2.2 Une autre facon de voir les choses *)
let rec to_single_list x = match x with
  |[] -> []
  |[]::l when l <> [] -> ' '::(to_single_list l)
  |[]::l when l = [] -> (to_single_list l)
  |(a::b)::l -> a ::(to_single_list (b::l))
  |_ -> failwith "Error";

(* 2.3 Impression *)
let impress_morse x =
  let word = word_to_morse x
  let rec impmor = match x with
    |[] -> ()
    |e::l -> begin
               print_char a ;
               print
  in
    impmor word;;
