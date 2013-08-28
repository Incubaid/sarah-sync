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

  (* NEW 28/8 Evaluate a polynom in a given point, using Horner's rule *)
  let evaluate_pol_B (coeffs : element array) (pnt : element) =
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

  (* NEW VERSION OF THE FUNCTION ABOVE. Returns the ratios of the characteristic polynomials. *)
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

  (* NEW Interpolation *)
  let interpolation_B eval_pts rat_vals delta =
    let m = List.length rat_vals in
    InterPol.interpolate eval_pts rat_vals m delta

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

  (* NEW 28/8 Reconciliation. Uses the old method to extract proper roots. *)
  let reconcile_old (m1 : int) (chi_1 : element list) (set2 : set) (evalPts : element list) =
    if List.length evalPts = 0
    then []   (* No evaluation points *)
    else
      begin
        let () = Printf.printf "Performing interpolation.\n%!" in
        let cfsNum, cfsDenom = interpolation m1 chi_1 set2 evalPts in
        let roots_num = List.sort compare (Chien.chienSearch2 cfsNum) in
	    let roots_denom = List.sort compare (Chien.chienSearch2 cfsDenom) in
        let () = Printf.printf "Extracting proper roots.\n%!" in
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
	    in 
	    find_proper_els roots_num roots_denom []
      end


  (* NEW reconciliation. Ensure that the roots in numerator and denominator are all different. *)
  let reconcile_B cfs_num cfs_denom =
    let roots_num = Chien.chienSearch2 cfs_num in
    let () = Printf.printf "Extracting proper roots.\n%!" in
    let is_proper_root root = 
      let eval = evaluate_pol_B cfs_denom root in
      eval <> F.zero 
    in
    let result = List.filter is_proper_root roots_num in
    Printf.printf "Removed %i root(s).\n%!" (List.length roots_num - List.length result) ;
    result


  (* NEW reconciliation. 28/8, uses the old method to extract proper roots. *)
  let reconcile_C cfs_num cfs_denom =
    let roots_num = List.sort compare (Chien.chienSearch2 cfs_num) in
    let roots_denom = List.sort compare (Chien.chienSearch2 cfs_denom) in
    let () = Printf.printf "Extracting proper roots.\n%!" in
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
	in 
    let result = find_proper_els roots_num roots_denom [] in
    Printf.printf "Removed %i root(s).\n%!" (List.length roots_num - List.length result) ;
    result


end
