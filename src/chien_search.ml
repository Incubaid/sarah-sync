(* Module to perform Chien search *)

open FiniteField
open Polynom

module Chien =
  functor  (F : FINITEFIELD) ->
struct
  open F

  type element = t
  type polynom = element array (* Coefficients *)
  type roots = element list    (* Roots *)

  module P = Polynom(F)


  (* Chien-search algorithm.
     Coefficients should be ordered according to ascending powers of x.
     Counts roots, so the procedure can terminate when all roots are found. *)
  let chienSearch (coeffs : polynom) =
    let t = P.get_degree coeffs in
    let cst = coeffs.(0) in 
    let () = Printf.printf "Starting Chien search. Polynomial is of degree %i.\n%!" t in
    let roots = ref ([] : roots) in
    let foundRoot root =
      let () = Printf.printf "Found root. %i already found.\n%!" (List.length !roots) in
      roots := root :: !roots ;
    in
    if coeffs.(0) =: zero then foundRoot zero ;
    let element = ref one in
    let counter = ref 0 in
    while ( List.length !roots <> t && !counter < q - 1) do
      let rec loop i acc prev_pow= 
        if i > t
        then acc
        else 
          begin
            let pow = prev_pow *: !element in
            let term = coeffs.(i) *: pow in
            let acc' = acc +: term in
            loop (i + 1) acc' pow
          end
      in 
      let sum = loop 1 cst one in
      if sum =: zero then foundRoot !element ;
      element := !element *: primEl ;
      counter := succ (!counter)
    done ;
    !roots

end
