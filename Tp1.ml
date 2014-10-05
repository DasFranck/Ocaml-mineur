#load "graphics.cma" ;;
open Graphics;;
open_graph "" ;;

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

let rec sierpinski n x y z t =
    match n with
        |n when n = 0 -> print_string
        |n when n > 0-> begin
                moveto x y;
                lineto z t;
                moveto x y;
                lineto (((z-x)/2)+x) (int_of_float(float_of_int(z-x)*.((sqrt 3.)/.2.))+ y);
                moveto z t;
                lineto (((z-x)/2)+x) (int_of_float(float_of_int(z-x)*.((sqrt 3.)/.2.))+ y);
                sierpinski (n-1) x y ((x+z)/2) t;
                sierpinski (n-1) ((x+z)/2) y z t;
                sierpinski (n-1) (((z-x)/4)+x) (int_of_float((float_of_int(z-x)*.((sqrt 3.)/.2.))/.2.)+ y) (((z-x)*3/4)+ x) (int_of_float((float_of_int(z-x)*.((sqrt 3.)/.2.))/.2.)+ y) ;
              end ;;
