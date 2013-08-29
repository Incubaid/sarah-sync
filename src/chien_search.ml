(* Module to perform Chien search *)

open FiniteField

module Chien =
  functor  (F : FINITEFIELD) ->
struct
  type element = F.t
  type polynom = element array (* Coefficients *)
  type roots = element list    (* Roots *)

  (* Extra function *)
  let is_zero (pol : polynom) =
    List.for_all ((=) F.zero) (Array.to_list pol)


  (* Degree of a polynomial. Highest coefficient different from zero *)
  let get_degree (pol : polynom) =
    let rec aux i =
      if (pol.(i) <> F.zero || i = 0)
      then i
      else aux (i-1)
    in
    if is_zero pol
    then 0
    else aux (Array.length pol - 1)


  (* Chien-search algorithm. Gammas overwrite the coefficient-array.
     Coefficients should be ordered according to ascending powers of x. *)
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
   (* let t = Array.length coeffs - 1 in *)(* Degree of the polynom *)
    let t = get_degree coeffs in
    let () = Printf.printf "Starting Chien search. Polynomial is of degree %i.\n%!" t in
    let roots = ref ([] : roots) in
    let foundRoot root =
      let () = Printf.printf "Found root. %i already found.\n%!" (List.length !roots) in
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
