let rate_express x = match x with
                     | x when x < 500  -> 9.1
                     | x when x < 1000 -> 11.
                     | x when x < 2000 -> 13.5
                     | x when x < 3000 -> 14.2
                     | _ -> -1. ;; (* -1. signifie une erreur *)
