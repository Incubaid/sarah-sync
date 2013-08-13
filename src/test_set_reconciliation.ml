(* Tests for set reconciliation *)

open OUnit
open F_128
open Set_reconciliation
open Finite_field


module S = SetReconciliation(TestFiniteField)
module F7 = TestFiniteField

let test_set_reconc () =
  let tests = [      (* (set1 , set2 , delta1, delta2) *)
    ( [1 ; 2 ; 9 ; 12 ; 33], [1 ; 2 ; 9 ; 10 ; 12 ; 28], [33], [10; 28] ) ;
    ( [3 ; 45 ; 60 ; 1], [9 ; 3 ; 1 ; 60 ; 55 ; 90], [45], [9; 55; 90] ) ;
    ( [1 ; 2 ; 99 ; 32 ; 6; 8; 10; 66], [3 ; 99; 8; 10], [1; 2; 6; 32; 66], [3] ) ;
    ( [4; 88; 9; 24] , [9; 11; 88; 21] , [4; 24] ,[11; 21] )
  ] 
  in 
  let test_one (set1, set2, only_set1, only_set2) =
    let m = findM set1 set2 in
    let pts = evalPts m in
    let (sols, extra) = S.reconcile set1 set2 pts in
    OUnit.assert_equal (List.sort compare sols) (List.sort compare only_set1) ; 
    OUnit.assert_equal (List.sort compare extra) (List.sort compare only_set2)
  in 
  List.iter test_one tests
  

let suite = "Set Reconciliation" >::: [ "test_set_reconciliation" >:: test_set_reconc ]

let _ = run_test_tt_main suite
