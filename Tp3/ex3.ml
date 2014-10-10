(* Prerequis*)
let rec append x y = match x with
  |[] -> y
  |e::l -> e::(append l y);; 
  
 let rec alphanum_to_morse x = match x with
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
  |'0'     -> ['-';'-';'-';'-';'-']
  |'1'     -> ['.';'-';'-';'-';'-']
  |'2'     -> ['.';'.';'-';'-';'-']
  |'3'     -> ['.';'.';'.';'-';'-']
  |'4'     -> ['.';'.';'.';'.';'-']
  |'5'     -> ['.';'.';'.';'.';'.']
  |'6'     -> ['-';'.';'.';'.';'.']
  |'7'     -> ['-';'-';'.';'.';'.']
  |'8'     -> ['-';'-';'-';'.';'.']
  |'9'     -> ['-';'-';'-';'-';'.']
  |_       -> [];;

(* 3.1 Conversion (encore) *)
let sentence_to_morse x = let rec wtm w l = match l with
  |[]   -> w
  |m::l -> wtm (append w [alphanum_to_morse m]) l
    in
                          let rec stw s l = match l with
  |[]   -> s
  |m::l -> stw (append s [wtm [] m]) l
    in 
       stw [] x;;


(* 3.2 char list list ... *)
let sentence_to_single_list x = let rec tsl x = match x with
      |[]                 -> []
      |[]::l when l <> [] -> ' '::(tsl l)
      |[]::l when l = []  -> tsl l 
      |(a::b)::l          -> a ::(tsl (b::l))
      |_                  -> failwith "Error"
    in
                               let rec stsl l = match l with
      |[]                 -> []
      |a::l          -> append (append (tsl a)  (['/'])) (stsl l)
    in
       stsl x;;


(* 3.3 Sans escales *)
let to_single_morse x = let rec wtm w l = match l with
  |[]   -> w
  |m::l -> wtm (append w [alphanum_to_morse m]) l
    in
       let rec tsl x = match x with
             |[] -> []
             |[]::l when l <> [] -> ' '::(tsl l)
             |[]::l when l = [] -> (tsl l)
             |(a::b)::l -> a ::(tsl (b::l))
             |_ -> failwith "Error"
      in tsl (wtm [] x);;

let latin_sentence_to_single x = sentence_to_single_list (sentence_to_morse x);;
