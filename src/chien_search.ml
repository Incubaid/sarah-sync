(* Module to perform Chien search.
   Chien search is an algorithm to find roots of a given polynomial over a finite field. *)

open FiniteField
open Polynom

module Chien =
  functor  (F : FINITEFIELD) ->
struct
  open F

  module P = Polynom(F)

  type element = P.element
  type polynom = P.polynom     (* Coefficients ordered according to ascending powers of x. *)


  (* Synthetic division of a polynomial by (x-a). *)
  let synth_div (pol, t : polynom) (a : element) =
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
    pol.(t) <- zero ;
    P.proper_degree (pol, t - 1)


  (* Chien-search algorithm.
     After a root r has been detected, the polynomial is divided by (x - r) and the root is checked again.
     This allows us to determine the multiplicity of the roots.
     The roots are counted, so the procedure can terminate when all roots have been found. *)
  let chien_search ((coeffs, deg) as pol : polynom) =
    let rts_init =
      if coeffs.(0) =: zero
      then [zero]
      else []
    in
    let rec find el rts k ((coeffs, t) as p) =
      if List.length rts == deg || k == q
      then rts   (* All roots have been found *)
      else
        begin
          let rec loop i acc prev_pow =
            if i > t
            then acc
            else
              begin
                let pow = prev_pow *: el in
                let term = coeffs.(i) *: pow in
                let acc' = acc +: term in
                loop (i + 1) acc' pow
              end
          in
          let sum = loop 1 coeffs.(0) one in
          let rts' , el', k' , p'=
            if sum =: zero
            then
              begin
                let p' = synth_div p el in
                el :: rts, el, k, p'
              end
            else
              begin
                rts, el *: primEl, (k + 1), p
              end
          in
          find el' rts' k' p'
        end
    in
    find one rts_init 0 pol

end
