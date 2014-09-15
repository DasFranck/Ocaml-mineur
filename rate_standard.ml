let rate_standard x = match x with
                     | x when x < 500  -> 4.6
                     | x when x < 1000 -> 5.9
                     | x when x < 2000 -> 6.5
                     | x when x < 3000 -> 7.2
                     | _ -> -1. ;; (* -1. signifie une erreur *)
