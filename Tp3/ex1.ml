(* 1.1 Validite *)
let rec is_morse x = match x with
  |[] -> true
  |e::l when (e = '-' || e = '.') -> is_morse l
  |_ -> false;;


(* 1.2 Conversion *)
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
