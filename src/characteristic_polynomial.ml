(* Module to evaluate the charateristic polynomial of a set *)

open FiniteField

module CharPoly = 
  functor (F : FINITEFIELD) -> 
     struct
        type element = F.t
        type set = F.t list
      
        (* Evaluate charateristic polynomial in field F *)
        let evalCharPoly (s : set) (point : element) = 
            let factors = List.map (F.min point) s in
            List.fold_left F.mult F.one factors 

        (* Quotient of two evaluations, in F *)
        let quotient (chi_1 : element) (chi_2 : element) =
            F.div chi_1 chi_2

     end 

