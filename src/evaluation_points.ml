(* Module to determine the number of evaluation points needed in the reconciliation *)

open FiniteField
open Set_reconciliation

module EvaluationPts = 
  functor (F : FINITEFIELD) ->
struct

  type element = F.t

  exception Unable_to_find_good_m
    
  module S = SetReconciliation(F)

  (* Produce a given number of evaluation points. These will be: [q-1, q-2, ...] *)
  let evalPts m = 
    let q = int_of_float (2. ** (float_of_int F.w)) in
    let rec getPoints start len =
      if len = 0
      then []
      else F.wrap start :: (getPoints (start - 1) (len - 1))
    in
    getPoints (q - 1) m

  (* Determine extra evaluation points, used in the function to determine an upper bound on m. 
     m: number of evaluation points in the algorithm, k: number of extra points requested *) 
  let extraEvalPts m k =             
    let q = int_of_float (2. ** (float_of_int F.w)) in
    let rec getPoints start len =
      if len = 0
      then []
      else F.wrap start :: (getPoints (start - 1) (len - 1))
    in
    getPoints (q - m - 1) k

  (* Evaluate a polynom in a given point *)
  let evaluatePol (coeffs : element array) (pnt : element) = 
    let terms = Array.mapi (fun i coeff ->
      F.mult coeff (F.exp pnt i)) coeffs
    in
    Array.fold_left F.plus F.zero terms
      
  (* Determine a close upper bound on the number of evaluation points needed by the reconciliation algorithm *)
  let findM (s1 : S.set) (s2 : S.set) =
    let delta = List.length s1 - List.length s2 in
    let initMin = 1 in
    let initMax = List.length s1 + List.length s2 in
    let rec search min max onceFound prevFound = 
      if max < min
      then raise Unable_to_find_good_m
      else 
	begin
	  let mid =
	    let mid' = (min + max) / 2 in 
	    if (mid' - delta) mod 2 <> 0   (* Ensure same parity *)
	    then mid' + 1
	    else mid'
	  in 
	  if mid = max
	  then (if onceFound then prevFound else raise Unable_to_find_good_m )
	  else 
	    begin
	      let pts = evalPts mid in
	      let cfsNum, cfsDenom = S.interpolation s1 s2 pts in
	      let k = 1 in        (* CHANGE THIS *)
	      let extraPts = extraEvalPts mid k in
	      let actualVals = S.evalCharPols s1 s2 extraPts in
	      let ourNumVals = List.map (evaluatePol cfsNum) extraPts in
	      let ourDenomVals = List.map (evaluatePol cfsDenom) extraPts in
	      let ourVals = List.map2 F.div ourNumVals ourDenomVals in
	      let ok = List.for_all2 F.eq actualVals ourVals in
	      if ok
	      then 
		let max' = mid in
		let onceFound' = true in
		let prevFound' = mid in
		search min max' onceFound' prevFound'
	      else 
		if onceFound
		then prevFound    (* a previously found solution *)
		else 
		  let min' = mid in
		  search min' max onceFound prevFound
	    end 
	end 
    in 
    search initMin initMax false 0
end
