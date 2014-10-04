#load "graphics.cma" ;;
open Graphics;;
open_graph "" ;;
(*qqqqq/*)
open Random ;;
self_init() ;;

let rec montagne n x y z t =
    match n with
        |0 -> 
            begin
              moveto x y;
              lineto z t;
            end

        |n when n>0 ->
            begin
              let mid=(x+z)/2 and h = (y+t)/2 + int(abs(z-x)/5 + 20) in
              montagne (n-1) x y mid h;
              montagne (n-1) mid h z t
            end 
        |_ -> invalid_arg "n doit etre un entier positif" ;;


let rec dragon n x y z t =
    match n with
        |1 -> begin
                moveto x y;
                lineto z t;
              end
        |_ ->
              let u = (x + z + t - y) / 2
              and v = (y + t - z + x) / 2
              in
                 begin
                   dragon (n-1) x y u v;
                   dragon (n-1) z t u v;
                 end ;;
