(* Set reconciliation *)

open FiniteField
open Characteristic_polynomial
open Interpolation
open Chien_search

module SetReconciliation =
  functor  (F : FINITEFIELD) ->
struct
  type element = F.t
  type set = element list

  module CharPoly = CharPoly(F)
  module InterPol = Interpolation(F)

  module Chien = Chien(F)

  (* Evaluation of the characteristic polynomials. Returns the ratios. *)
  let evalCharPols (chi_1 : element list) (s2 : set) (evalPts : element list) =
    let chi_2 = List.map (CharPoly.evalCharPoly s2) evalPts in
    let ratVals = List.map2 CharPoly.quotient chi_1 chi_2 in
    ratVals

  (* Interpolation *)
  let interpolation (m1 : int) (chi_1 : element list) (s2 : set) (evalPts : element list) =
    let m = List.length evalPts in
    let delta = m1 - List.length s2 in
    let ratVals = evalCharPols chi_1 s2 evalPts in
    InterPol.interpolate evalPts ratVals m delta

  (* Reconciliation. Ensure that the roots in numerator and denominator are all different. *)
  let reconcile (m1 : int) (chi_1 : element list) (set2 : set) (evalPts : element list) =
    if List.length evalPts = 0
    then [],[]   (* No evaluation points *)
    else
      begin
        let () = Printf.printf "Performing interpolation.\n%!" in
        let cfsNum, cfsDenom = interpolation m1 chi_1 set2 evalPts in
        let roots_num = List.sort compare (Chien.chienSearch2 cfsNum) in
        let roots_denom = List.sort compare (Chien.chienSearch2 cfsDenom) in
        let () = Printf.printf "Extracting proper roots.\n%!" in
        let rec find_proper_els list1 list2 proper1 proper2 =
          match list1, list2 with
          | (x::xs), (y::ys) ->
            if x = y
            then find_proper_els xs ys proper1 proper2
            else
              begin
                if x < y
                then find_proper_els xs (y::ys) (x::proper1) proper2
                else find_proper_els (x::xs) ys proper1 (y::proper2)
              end
          | rest1, rest2  -> (List.append rest1 proper1 , List.append rest2 proper2)
        in
        find_proper_els roots_num roots_denom [] []
      end




end
