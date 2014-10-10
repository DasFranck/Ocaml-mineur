(* Calcule si une année x est bisextile ou non *) 

let bisextile x =
     (x mod 4 = 0 || x mod 400 = 0) && (x mod 100 <> 0 || x mod 400 = 0 ) ;;
