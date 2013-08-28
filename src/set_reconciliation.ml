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

  (* Evaluate a polynom in a given point, using Horner's rule *)
  let evaluate_pol (coeffs : element array) (pnt : element) =
    let rec loop i acc =
      if i = -1
      then acc
      else
        begin
          let acc' = F.plus (F.mult acc pnt) coeffs.(i) in
          loop (i - 1) acc'
        end
    in
    loop (Array.length coeffs - 1) F.zero


  (* Evaluation of the characteristic polynomials. Returns the ratios. *)
  let evalCharPols (chi_1 : element list) (s2 : set) (evalPts : element list) =
    let chi_2 = List.map (CharPoly.evalCharPoly s2) evalPts in
    let ratVals = List.map2 CharPoly.quotient chi_1 chi_2 in
    ratVals

  (* Alternative to the function above: calculates the ratios of the sampled values of the characteristic polynomials directly from the sets. *)
  let get_rational_values (s1 : set) (s2 : set) (eval_pts : element list) =
    let chi_1 = List.map (CharPoly.evalCharPoly s1) eval_pts in
    let chi_2 = List.map (CharPoly.evalCharPoly s2) eval_pts in
    let rat_vals = List.map2 CharPoly.quotient chi_1 chi_2 in
    rat_vals

  (* Interpolation *)
  let interpolation (m1 : int) (chi_1 : element list) (s2 : set) (evalPts : element list) =
    let m = List.length evalPts in
    let delta = m1 - List.length s2 in
    let ratVals = evalCharPols chi_1 s2 evalPts in
    InterPol.interpolate evalPts ratVals m delta


  (* Interpolation: alternative for the function above *)
  let interpolation_B eval_pts rat_vals delta =
    let m = List.length rat_vals in
    InterPol.interpolate eval_pts rat_vals m delta


  (* Reconciliation. Ensure that the roots in numerator and denominator are all different. *)
  let reconcile cfs_num cfs_denom =
    let roots_num = Chien.chienSearch2 cfs_num in
    let () = Printf.printf "Extracting proper roots.\n%!" in
    let is_proper_root root =
      let eval = evaluate_pol cfs_denom root in
      eval <> F.zero
    in
    let result = List.filter is_proper_root roots_num in
    Printf.printf "Removed %i root(s).\n%!" (List.length roots_num - List.length result) ;
    result



end
