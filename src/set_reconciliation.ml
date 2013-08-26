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


  (* Evaluate a polynom in a given point *)
  let evaluatePol (coeffs : element array) (pnt : element) =
    let terms = Array.mapi (fun i coeff ->
      F.mult coeff (F.exp pnt i)) coeffs
    in
    Array.fold_left F.plus F.zero terms

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
    then []   (* No evaluation points *)
    else
      begin
        let () = Printf.printf "Performing interpolation.\n%!" in
        let cfsNum, cfsDenom = interpolation m1 chi_1 set2 evalPts in
        let roots_num = List.sort compare (Chien.chienSearch2 cfsNum) in
        let () = Printf.printf "Extracting proper roots.\n%!" in
        let is_proper_root root = 
          let eval = evaluatePol cfsDenom root in
          eval <> F.zero 
        in
        List.filter is_proper_root roots_num

      end




end
