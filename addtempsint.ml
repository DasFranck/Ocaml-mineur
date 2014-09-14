let addtint ha ma sa hb mb sb = let s = (sa + sb) mod 60 in
                                let m = ((ma + mb) mod 60) + ((sa + sb) / 60) in
                                let h = ((ha + hb) mod 24) + ((ma + mb) / 60) in
                                let j = (ha + hb) / 24 in
                              
                                  j * 1000000 + h * 10000 + m * 100 + s;;
