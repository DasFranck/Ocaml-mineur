let setint x = let h = x/3600 in
               let m = (x mod 3600) / 60 in
               let s = x mod 60 in
               (h * 10000) + (m * 100) + s;; 
