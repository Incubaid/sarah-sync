open Camljerasure

open OurFiniteField
open F_16
open F_128
open FiniteField
open Matrices
open Characteristic_polynomial
open Chien_search
open Interpolation
open Set_reconciliation



(*-TESTS------------------------------------------------------------*)

module F = TestFiniteField

(* Printing arrays *)
let print_array arr =
  let print_element el = print_int el ; print_string " " in
  Array.iter print_element arr ;
  print_newline ();;

(* Printing lists *)
let print_list list = 
  let print_el el = F.print el ; print_string " " in
  List.iter print_el list ;
  print_newline ();;


(* Testing Interpolation *)
(*module Int = Interpolation(TestFiniteField)

let ks = [ F.one ; 2 ; 3 ; 4 ; 5 ] ;;
let fs = [ F.one ; 3 ; 2 ; 9 ; 1] ;;

let m = List.length ks in
let delta = 1 in
let system, d1 , d2 = Int.constructSystem ks fs m delta in
print_string "Testing interpolation\n" ;
print_string "System:\n" ;
Array.iter print_array system ;
let ps, qs = Int.interpolate ks fs m delta in
print_string "Solution:\n" ;
print_array ps ;
print_array qs ;; *)


(* Testing Set reconciliation *)
module S = SetReconciliation(F_128.TestFiniteField)
module F7 = F_128.TestFiniteField

let set1 = [1 ; 2 ; 9 ; 12 ; 33] ;;
let set2 = [1 ; 2 ; 9 ; 10 ; 12 ; 28] ;;

let set1B = [3 ; 45 ; 60 ; 1] ;;
let set2B = [9 ; 3 ; 1 ; 60 ; 55 ; 90] ;;

let set1C = [3 ; 99; 8; 10] ;;
let set2C = [1 ; 2 ; 99 ; 32 ; 6; 8; 10; 66];;

let set1D = [4; 88; 9; 24];;
let set2D = [9; 11; 88; 21] ;;



let evalPts m = 
  let q = int_of_float (2. ** (float_of_int F7.w)) in
  let rec getPoints start len =
    if len = 0
    then []
    else start :: (getPoints (start - 1) (len - 1))
  in
  getPoints (q - 1) m ;;





(* ---- BEGIN Bepalen aantal evalutatiepunten ---- *)



(* Bepaal de extra evaluatiepunten.
   m: aantal evaluatiepunten in het algoritme
   k: aantal extra punten *) 
let extraEvalPts m k =             
  let q = int_of_float (2. ** (float_of_int F7.w)) in
  let rec getPoints start len =
    if len = 0
    then []
    else start :: (getPoints (start - 1) (len - 1))
  in
  getPoints (q - m - 1) k ;;

(* Evalueer polynoom in punt *)
let evaluatePol (coeffs : F7.t array) pnt = 
  let terms = Array.mapi (fun i coeff ->
                           F7.mult coeff (F7.exp pnt i)) coeffs
  in
  Array.fold_left F.plus F.zero terms
                        
exception No_good_m                          

let findM (s1 : S.set) (s2 : S.set) =
  let delta = List.length s1 - List.length s2 in
  let initMin = 1 in
  let initMax = List.length s1 + List.length s2 in
  let rec search min max onceFound prevFound = 
    if max < min
    then raise No_good_m
    else begin
      let mid =
	let mid' = (min + max) / 2 in 
	if (mid' - delta) mod 2 <> 0   (* Ensure same parity *)
	then mid' + 1
	else mid'
      in 
      if mid = max
      then (if onceFound then prevFound else raise No_good_m )
      else begin
	let pts = evalPts mid in
	let cfsNum, cfsDenom = S.interpolation s1 s2 pts in
	let k = 1 in        (* AANPASSEN *)
	let extraPts = extraEvalPts mid k in
	let actualVals = S.evalCharPols s1 s2 extraPts in
	let ourNumVals = List.map (evaluatePol cfsNum) extraPts in
	let ourDenomVals = List.map (evaluatePol cfsDenom) extraPts in
	let ourVals = List.map2 F7.div ourNumVals ourDenomVals in
	let ok = List.for_all2 F7.eq actualVals ourVals in
	if ok
	then 
	  let max' = mid in
	  let onceFound' = true in
	  let prevFound' = mid in
          search min max' onceFound' prevFound'
	else 
	  if onceFound
	  then prevFound    (* Een vroegere oplossing *)
	  else 
	    let min' = mid in
	    search min' max onceFound prevFound
      end 
    end 
  in 
  search initMin initMax false F7.zero ;;



(* ---- EINDE Bepalen aantal evalutatiepunten ---- *)


let () = print_string "Testing Set Reconc\n" in
let m = findM set1 set2 in
let mB = findM set1B set2B in
let mC = findM set1C set2C in
let mD = findM set1D set2D in
let pts = evalPts m in
let ptsB = evalPts mB  in
let ptsC = evalPts mC  in
let ptsD = evalPts mD  in
let (sols , extra) = S.reconcile set1 set2 pts in
let (solsB , extraB) = S.reconcile set1B set2B ptsB in
let (solsC , extraC) = S.reconcile set1C set2C ptsC in
let (solsD , extraD) = S.reconcile set1D set2D ptsD in
print_string "Set 1: " ; print_list set1 ;
print_string "Set 2: " ; print_list set2 ;
print_string "Only in set 1: " ; print_list sols ;
print_string "Only in set 2: " ; print_list extra ;
print_string "Number of evaluation points: " ; print_int m ;
print_newline () ;
print_string "Set 1B: " ; print_list set1B ;
print_string "Set 2B: " ; print_list set2B ;
print_string "Only in set 1B: " ; print_list solsB ;
print_string "Only in set 2B: " ; print_list extraB ;
print_string "Number of evaluation points: " ; print_int mB ;
print_newline () ;
print_string "Set 1C: " ; print_list set1C ;
print_string "Set 2C: " ; print_list set2C ;
print_string "Only in set 1C: " ; print_list solsC ;
print_string "Only in set 2C: " ; print_list extraC ;
print_string "Number of evaluation points: " ; print_int mC ;
print_newline () ;
print_string "Set 1D: " ; print_list set1D ;
print_string "Set 2D: " ; print_list set2D ;
print_string "Only in set 1D: " ; print_list solsD ;
print_string "Only in set 2D: " ; print_list extraD ;
print_string "Number of evaluation points: " ; print_int mD ;
print_newline () 
