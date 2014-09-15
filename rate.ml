let rate str x = if str = "eco" then match x with
                                        | x when x < 500  -> 3.4
                                        | x when x < 1000 -> 4.6
                                        | x when x < 2000 -> 5.1
                                        | x when x < 3000 -> 6.9
                                        | _ -> invalid_arg "x doit etre inferieur a 3000g"

               else if str = "standard" then match x with
                                                  | x when x < 500  -> 4.6
                                                  | x when x < 1000 -> 5.9
                                                  | x when x < 2000 -> 6.5
                                                  | x when x < 3000 -> 7.2
                                                  | _ -> invalid_arg "x doit etre inferieur a 3000g"

                    else if str = "express" then match x with
                                                      | x when x < 500  -> 9.1
                                                      | x when x < 1000 -> 11.
                                                      | x when x < 2000 -> 13.5
                                                      | x when x < 3000 -> 14.2
                                                      | _ -> invalid_arg "x doit etre inferieur a 3000g"
                         
                         else invalid_arg "La classe n'a pas ete comprise, choisissez entre eco, standard ou express";;       
