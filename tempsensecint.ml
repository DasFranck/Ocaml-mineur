let tesint x = let hs = (x / 10000) * 3600 in
               let ms = ((x mod 10000) / 100) * 60 in
               let s  = x mod 100 in
                    hs + ms + s;; 
