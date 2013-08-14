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
  let evalCharPols (s1 : set) (s2 : set) (evalPts : element list) =
    let chi_1 = List.map (CharPoly.evalCharPoly s1) evalPts in
    let chi_2 = List.map (CharPoly.evalCharPoly s2) evalPts in
    let ratVals = List.map2 CharPoly.quotient chi_1 chi_2 in
    ratVals

  (* Interpolation *)
  let interpolation (s1 : set) (s2 : set) (evalPts : element list) =
    let m = List.length evalPts in
    let delta = List.length s1 - List.length s2 in
    let ratVals = evalCharPols s1 s2 evalPts in
    InterPol.interpolate evalPts ratVals m delta
      
  (* Reconciliation *)
  let reconcile (s1 : set) (s2 : set) (evalPts : element list) =
    let cfsNum, cfsDenom = interpolation s1 s2 evalPts in
    ( Chien.chienSearch2 cfsNum , Chien.chienSearch2 cfsDenom )
      
end 
