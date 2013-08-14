(* Tests for set reconciliation *)

open OUnit
open F_128
open F_1024
open Set_reconciliation
open Evaluation_points

module F1 = F_128.TestFiniteField
module S1 = SetReconciliation(F1)
module EP1 = EvaluationPts(F1)

module F2 = F_1024.TestFiniteField
module S2 = SetReconciliation(F2)
module EP2 = EvaluationPts(F2)


let test_set_reconc () =
  let tests = [      (* (set1 , set2 , delta1, delta2) *)
    ( [1] , [2] ,[1] , [2]);
    ( [1 ; 2 ; 9 ; 12 ; 33], [1 ; 2 ; 9 ; 10 ; 12 ; 28], [33], [10; 28] ) ;
    ( [3 ; 45 ; 60 ; 1], [9 ; 3 ; 1 ; 60 ; 55 ; 90], [45], [9; 55; 90] ) ;
    ( [1 ; 2 ; 99 ; 32 ; 6; 8; 10; 66], [3 ; 99; 8; 10], [1; 2; 6; 32; 66], [3] ) ;
    ( [4; 88; 9; 24] , [9; 11; 88; 21] , [4; 24] ,[11; 21] ) ;
  ] 
  in 
  let test_one (set1, set2, only_set1, only_set2) =
    let m = EP1.findM set1 set2 in
    let pts = EP1.evalPts m in
    let (sols, extra) = S1.reconcile set1 set2 pts in
    OUnit.assert_equal (List.sort compare sols) (List.sort compare only_set1) ; 
    OUnit.assert_equal (List.sort compare extra) (List.sort compare only_set2)
  in 
  List.iter test_one tests


let test_set_reconc_bigger () =
  let tests = [      (* (set1 , set2 , delta1, delta2) *)
    ( [314 ; 315 ; 316 ; 317 ; 318; 319 ; 320 ; 321 ; 322 ; 323; 324 ; 325 ; 326 ; 327 ; 328 ; 329 ; 330] , 
      [314 ; 200 ; 201; 203 ; 204 ; 205 ; 206 ; 207 ; 208 ; 209 ; 210 ;211; 212 ; 213 ; 214 ;215 ; 216] ,
      [315 ; 316 ; 317 ; 318; 319 ; 320 ; 321 ; 322 ; 323; 324 ; 325 ; 326 ; 327 ; 328 ; 329 ; 330], 
      [ 200 ; 201; 203 ; 204 ; 205 ; 206 ; 207 ; 208 ; 209 ; 210 ;211; 212 ; 213 ; 214 ;215 ; 216] ) ;
    ( [1 ; 2 ; 3 ; 4 ; 5 ; 6 ; 7 ; 8 ; 9 ; 10 ; 11 ; 12 ; 13; 14; 15 ; 16; 17 ; 18 ; 19 ; 20] , 
      [1 ; 3 ; 5 ; 7 ; 9 ; 11 ; 13 ; 15 ; 17 ; 19 ; 21 ; 23 ; 25 ; 27 ; 29 ; 31 ; 33; 35; 37] , 
      [2 ; 4 ; 6 ; 8 ; 10 ; 12 ; 14 ; 16 ; 18 ; 20], 
      [21 ; 23 ; 25 ; 27 ; 29 ; 31 ; 33; 35 ; 37] ) ;
    ( [1; 2; 3; 99; 105; 219; 220; 221; 222; 223; 224; 225; 226; 227; 228; 229; 230] , 
      [3; 55; 56; 57 ; 58 ; 59 ; 60 ; 61 ; 62; 220; 221; 222; 223; 224; 225; 226; 227; 228; 229; 230] , 
      [1; 2; 99; 105; 219], 
      [55 ; 56 ; 57 ; 58 ; 59 ; 60 ; 61 ; 62 ] );
   (  [1; 2; 3; 4; 5 ; 6 ; 7 ; 8 ; 9 ; 10 ; 11 ; 12 ; 13 ;14 ; 15 ; 16 ;17 ; 18 ;19 ;20 ; 21 ; 22 ; 23 ; 24 ;25 ;26 ;27 ;28 ; 29 ;30] , 
      [1; 2; 3; 4; 5 ; 6 ; 7 ; 8 ; 9 ; 10 ; 314 ; 315 ; 316 ; 317 ; 318; 319 ; 320 ; 321 ; 322 ; 323; 324 ; 325 ; 326 ; 327 ; 328 ; 329 ; 330 ] , 
      [11 ; 12 ; 13 ;14 ; 15 ; 16 ;17 ; 18 ;19 ;20 ; 21 ; 22 ; 23 ; 24 ;25 ;26 ;27 ;28 ; 29 ;30 ], 
      [314 ; 315 ; 316 ; 317 ; 318; 319 ; 320 ; 321 ; 322 ; 323; 324 ; 325 ; 326 ; 327 ; 328 ; 329 ; 330] ) ;
 (*  ( [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20; 21; 22; 23; 24; 25; 26 ; 27; 28; 29; 30] ,  (* System no solution *)
      [60 ; 61; 62 ; 63 ; 64 ; 65 ; 66 ; 67 ; 68 ; 69 ;70 ; 71 ; 72 ; 73 ; 74 ; 75 ; 76 ; 77 ; 78 ; 79 ; 80; 81; 82; 83; 84; 85; 86; 87; 88; 89; 90] , 
      [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20; 21; 22; 23; 24; 25; 26 ; 27; 28; 29; 30], 
      [60 ; 61; 62 ; 63 ; 64 ; 65 ; 66 ; 67 ; 68 ; 69 ;70 ; 71 ; 72 ; 73 ; 74 ; 75 ; 76 ; 77 ; 78 ; 79 ; 80; 81; 82; 83; 84; 85; 86; 87; 88; 89; 90 ] ) *)
    ( [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20] ,
      [60 ; 61; 62 ; 63 ; 64 ; 65 ; 66 ; 67 ; 68 ; 69 ;70 ; 71 ; 72 ; 73 ; 74 ; 75 ; 76 ; 77 ; 78 ; 79 ; 80] , 
      [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20], 
      [60 ; 61; 62 ; 63 ; 64 ; 65 ; 66 ; 67 ; 68 ; 69 ;70 ; 71 ; 72 ; 73 ; 74 ; 75 ; 76 ; 77 ; 78 ; 79 ; 80 ] )
  ] 
  in 
  let test_one (set1, set2, only_set1, only_set2) =
    let m = EP2.findM set1 set2 in
    let pts = EP2.evalPts m in
    let (sols, extra) = S2.reconcile set1 set2 pts in
    let msgNum = Printf.sprintf "Numerator: found %i elements, needed %i.\n" (List.length sols) (List.length only_set1) in
    let msgDenom = Printf.sprintf "Denominator: found %i elements, needed %i.\n" (List.length extra) (List.length only_set2) in
    OUnit.assert_equal (List.sort compare sols) (List.sort compare only_set1)  ~msg:msgNum ; 
    OUnit.assert_equal (List.sort compare extra) (List.sort compare only_set2) ~msg:msgDenom 
  in 
  List.iter test_one tests
    

