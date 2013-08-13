(* Testen met OUnit *)
open OUnit

open Test_chien


let suite = "Set Reconciliation" >::: [ "test_chien_search" >:: test_chien_search]

let _ = run_test_tt_main suite
 
