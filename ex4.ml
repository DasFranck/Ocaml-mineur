let ex4 =   let a = (x / 1000) in
            let b = (x mod 1000) / 100 in
            let c = (x mod 100) / 10 in
            let d = (x mod 10) in
            let x_fl = (float_of_int x) in
            let x_sqrt = (sqrt x_fl) in
            let x_flrt = (int_of_float x_sqrt) in
                (a = b) && (c = d) && ((x / x_flrt) = x_flrt);;
