#load "graphics.cma" ;;
open Graphics;;
open_graph "" ;;

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