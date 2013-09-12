(* Set reconciliation *)

open FiniteField
open Characteristic_polynomial
open Interpolation
open Chien_search
open Find_roots
open Polynom

module SetReconciliation =
  functor  (F : FINITEFIELD) ->
struct
  open F

  type element = t
  type set = element list

  module CharPoly = CharPoly(F)
  module InterPol = Interpolation(F)

  module Chien = Chien(F)
  module BTA = Root_finder(F)
  module P = Polynom(F)


  (* Evaluation of the characteristic polynomials. Returns the ratios. *)
  let eval_char_pols (chi_1 : element list) (s2 : set) (evalPts : element list) =
    let chi_2 = List.map (CharPoly.eval_char_poly s2) evalPts in
    let ratVals = List.map2 CharPoly.quotient chi_1 chi_2 in
    ratVals


  (* Alternative to the function above: calculates the ratios of the sampled values of the characteristic polynomials directly from the sets. *)
  let get_rational_values (s1 : set) (s2 : set) (eval_pts : element list) =
    let chi_1 = List.map (CharPoly.eval_char_poly s1) eval_pts in
    let chi_2 = List.map (CharPoly.eval_char_poly s2) eval_pts in
    let rat_vals = List.map2 CharPoly.quotient chi_1 chi_2 in
    rat_vals


  (* Interpolation *)
  let interpolation (m1 : int) (chi_1 : element list) (s2 : set) (evalPts : element list) =
    let m = List.length evalPts in
    let delta = m1 - List.length s2 in
    let ratVals = eval_char_pols chi_1 s2 evalPts in
    InterPol.interpolate evalPts ratVals m delta


  (* Interpolation: alternative for the function above.
     Use this one when the rational values are already available. *)
  let interpolation_B eval_pts rat_vals delta =
    let m = List.length rat_vals in
    InterPol.interpolate eval_pts rat_vals m delta


  (* Extracting proper elements of two lists.
     Used to find proper roots of numerator and denominator. *)
  let rec find_proper_els list1 list2 proper =
      match list1, list2 with
      | (x::xs), (y::ys) ->
        if x = y
            then find_proper_els xs ys proper
        else
          begin
        if x < y
        then find_proper_els xs (y::ys) (x::proper)
        else find_proper_els (x::xs) ys proper
          end
      | rest1, rest2  -> List.append rest1 proper


  (* Reconciliation, with Chien search to find the roots.
     Ensures that the roots in numerator and denominator are all different. *)
  let reconcile cfs_num cfs_denom =
    let roots_num = Chien.chien_search cfs_num in
    let roots_denom = Chien.chien_search cfs_denom in
    find_proper_els roots_num roots_denom []


  (* Reconciliation, with BTA to find the roots.
     Ensures that the roots in numerator and denominator are all different. *)
  let reconcile_BTA cfs_num cfs_denom =
    let roots_num = List.sort compare (BTA.roots cfs_num) in
    let roots_denom = List.sort compare (BTA.roots cfs_denom) in
    find_proper_els roots_num roots_denom []


end
