#load "graphics.cma" ;;
open Graphics;;
open_graph "" ;;

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
              end
		|_ -> failwith "Error." ;;

(* La fonction est expérimentale et il y a un léger décalage à cause des int. *)