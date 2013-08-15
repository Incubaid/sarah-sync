(* Module to perform Chien search *)

open FiniteField

module Chien =
  functor  (F : FINITEFIELD) ->
struct
  type element = F.t 
  type polynom = element array (* Coefficients *)
  type roots = element list    (* Roots *)

  (* Chien-search algorithm. Gammas overwrite the coefficient-array.
     Coefficients should be ordered according to ascendig powers of x. *)
  let chienSearch (coeffs : polynom) = 
    let roots = ref ([] : roots) in
    let foundRoot root =
      roots := root :: !roots ;
    in
    if coeffs.(0) = F.zero then foundRoot F.zero ;
    let q = F.q in
    let updateGamma j _ =  
      coeffs.(j) <- F.mult coeffs.(j) (F.exp F.primEl j)
    in 
    let element = ref (F.one) in    
    for i = 0 to q - 2 do
      let sum = Array.fold_left F.plus F.zero coeffs in
      if sum = F.zero then foundRoot !element ;
      element := F.mult !element F.primEl ;
      Array.iteri updateGamma coeffs
    done ;
    !roots

  (* Chien-search update. Counts roots, so a the procedure can terminate when all roots are found. *)
  let chienSearch2 (coeffs : polynom) = 
    let t = Array.length coeffs - 1 in (* Degree of the polynom *)
    let roots = ref ([] : roots) in
    let foundRoot root =
      roots := root :: !roots ;
    in
    if coeffs.(0) = F.zero then foundRoot F.zero ;
    let q = F.q in
    let updateGamma j _ =  
      coeffs.(j) <- F.mult coeffs.(j) (F.exp F.primEl j)
    in 
    let element = ref (F.one) in   
    let counter = ref 0 in
    while ( List.length !roots <> t && !counter < q - 1) do
      let sum = Array.fold_left F.plus F.zero coeffs in
      if sum = F.zero then foundRoot !element ;
      element := F.mult !element F.primEl ;
      Array.iteri updateGamma coeffs ;
      counter := succ (!counter)
    done ;
    !roots
      
end
