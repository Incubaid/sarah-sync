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


  (* Synthetic division of a polynomial by (x-a) *)
  let synth_div (pol : polynom) (a : element) =
    let t = P.get_degree pol in
    let rec loop i el prev = 
      if i = -1
      then ()
      else
        begin
          let res = el +: (prev *: a) in
          let el' = pol.(i) in
          let () = pol.(i) <- res in
          loop (i - 1) el' res
        end
    in
    loop (t - 1) pol.(t) zero;
    pol.(t) <- zero


  (* Chien-search algorithm. Determines multiplicity of roots.
     Coefficients should be ordered according to ascending powers of x.
     Counts roots, so the procedure can terminate when all roots are found. *)
  let chienSearch (coeffs : polynom) =
    let t = P.get_degree coeffs in
    let () = Printf.printf "Starting Chien search. Polynomial is of degree %i.\n%!" t in
    let roots = ref ([] : roots) in
    let foundRoot root =
      roots := root :: !roots ;
      Printf.printf "Found root. %i already found.\n%!" (List.length !roots)
    in
    if coeffs.(0) =: zero then foundRoot zero ;
    let element = ref one in
    let counter = ref 0 in
    while ( List.length !roots <> t && !counter < q - 1) do
      let rec loop i acc prev_pow= 
        if i > (P.get_degree coeffs)
        then acc
        else 
          begin
            let pow = prev_pow *: !element in
            let term = coeffs.(i) *: pow in
            let acc' = acc +: term in
            loop (i + 1) acc' pow
          end
      in 
      let sum = loop 1 coeffs.(0) one in
      if sum =: zero 
      then
        begin
          foundRoot !element ;
          synth_div coeffs !element
        end
      else
        begin
          element := !element *: primEl ;
          counter := succ (!counter)
        end
    done ;
    !roots

end
