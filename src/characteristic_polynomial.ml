(* Module to evaluate the charateristic polynomial of a set *)

open FiniteField

module CharPoly =
  functor (F : FINITEFIELD) ->
struct
  open F

  type element = t
  type set = element list


  (* Evaluate charateristic polynomial in field F *)
  let evalCharPoly (s : set) (point : element) =
    let rec loop l acc = 
      match l with
      | [] -> acc
      | x :: xs -> 
        let acc' = (x -: point) *: acc in
        loop xs acc' 
    in
    loop s one


  (* Quotient of two evaluations, in F *)
  let quotient (chi_1 : element) (chi_2 : element) =
    chi_1 /: chi_2

end
