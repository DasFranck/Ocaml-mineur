(* Prerequis *)
let rec reverse a =
    let rec rec_reverse a b = match a with
      |[]   -> b
      |e::l -> rec_reverse l (e::b)
    in
        rec_reverse a [];;

let rec append x y = match x with
  |[]    -> y
  |e::l  -> e::(append l y);;

let morse_to_alphanum x = match x with
  | ['.';'-']            -> 'A'
  | ['-';'.';'.';'.']    -> 'B'
  | ['-';'.';'-';'.']    -> 'C'
  | ['-';'.';'.']        -> 'D'
  | ['.']                -> 'E'
  | ['.';'.';'-';'.']    -> 'F'
  | ['-';'-';'.']        -> 'G'
  | ['.';'.';'.';'.']    -> 'H'
  | ['.';'.']            -> 'I'
  | ['.';'-';'-';'-']    -> 'J'
  | ['-';'.';'-']        -> 'K'
  | ['.';'-';'.';'.']    -> 'L'
  | ['-';'-']            -> 'M'
  | ['-';'.']            -> 'N'
  | ['-';'-';'-']        -> 'O'
  | ['.';'-';'-';'.']    -> 'P'
  | ['-';'-';'.';'-']    -> 'Q'
  | ['.';'-';'.']        -> 'R'
  | ['.';'.';'.']        -> 'S'
  | ['-']                -> 'T'
  | ['.';'.';'-']        -> 'U'
  | ['.';'.';'.';'-']    -> 'V'
  | ['.';'-';'-']        -> 'W' 
  | ['-';'.';'.';'-']    -> 'X'
  | ['-';'.';'-';'-']    -> 'Y'
  | ['-';'-';'.';'.']    -> 'X'
  | ['-';'-';'-';'-';'-']-> '0'
  | ['.';'-';'-';'-';'-']-> '1'
  | ['.';'.';'-';'-';'-']-> '2'
  | ['.';'.';'.';'-';'-']-> '3'
  | ['.';'.';'.';'.';'-']-> '4'
  | ['.';'.';'.';'.';'.']-> '5'
  | ['-';'.';'.';'.';'.']-> '6'
  | ['-';'-';'.';'.';'.']-> '7'
  | ['-';'-';'-';'.';'.']-> '8'
  | ['-';'-';'-';'-';'.']-> '9'
  | ['/']                -> ' '
  | []                   -> '%'
  | _                    -> '#' 

(* 5. decodez moi tout ca! *)
let morse_to_latin mrs =
  let len = (String.length mrs) in
  let mrs = mrs^" " in
    let rec mrstolst mrs l n = match n with
      |n when n<0             -> l 
      |n when (mrs.[n] = ' ') -> mrstolst mrs ('#'::l) (n-1)
      |_                      -> mrstolst mrs (mrs.[n]::l) (n-1)
    in
   
    let rec lstto2lst lst = match lst with
      |[]                           -> lst
      |e::l when e = '#' || e = '/' -> [] 
      |e::l                         -> append [e] (lstto2lst l)
    in
     
    let rec dlo lst mst nst = match lst with
      |[]                   -> nst
      |e::l when e = '#'    -> dlo l [] (append [lstto2lst mst] nst)
      |a::e::l when e = '/' -> dlo l [] (append (append [['/']] [lstto2lst (append mst [a])]) nst)
      |e::l                 -> dlo l (append mst [e]) nst
    in

    let rec morlst l = match l with
      |[]   -> ""
      |m::l -> (String.make 1 (morse_to_alphanum m))^(morlst l)
    in

    let rec clean lst l n = match n with
      |n when n<0                               -> l
      |n when (lst.[n] = '%')                   -> clean lst l (n-1)
      |n when n = 1 -> clean lst l (n-1)
      |_                                        -> clean lst (l^(String.make 1 lst.[n])) (n-1)

    in clean (morlst (dlo (mrstolst mrs [] len) [] [[]])) "" (String.length (morlst (dlo (mrstolst mrs [] len) [] [[]])) - 1) ;;
