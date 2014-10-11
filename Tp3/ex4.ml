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
  |'/'     -> ['/']
  |_       -> [];;

  
(* 4. Encodez moi tout ca! *)
let latin_to_morse str =
  let len = String.length str in
  let str = str^" " in 
    let rec strlist str l n = match n with
      |n when n<0           -> l
      |n when str.[n] = ' ' -> strlist str ('/'::l) (n-1)                 
      |_                    -> strlist str (str.[n]::l) (n-1) 
    in

    let rec lstmorse w l = match l with
      |[]   -> w
      |m::l -> lstmorse (append w [alphanum_to_morse m]) l 
    in

    let rec fuse l = match l with
      |[]                                  -> []
      |a::b::c::d when b = ['/']           -> fuse ((append (append a b) c)::d)
      |a::b::c    when b = ['/'] && c = [] -> fuse ((append a b )::c)
      |a::b                                -> a:: (fuse b)
    in

    let rec tsl x = match x with
      |[]                 -> []
      |[]::l when l <> [] -> ' '::(tsl l)
      |[]::l when l = []  -> tsl l
      |(a::b)::l          -> a ::(tsl (b::l))
      |_ -> failwith "Error" 
    in
   
    let rec liststr x =  match x with
      |[] -> ""
      |e::l -> (String.make 1 e)^(liststr l) 
    in
     
      liststr (tsl (fuse (lstmorse [] (strlist str [] len))));;
