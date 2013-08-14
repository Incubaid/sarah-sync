(* Testing with OUnit *)
open OUnit

open Test_chien
open Test_set_reconciliation

let suite = "Set Reconciliation" >::: [ "test_chien_search" >:: test_chien_search ;
                                        "test_set_reconciliation" >:: test_set_reconc ;
                                        "test_set_reconciliation_bigger" >:: test_set_reconc_bigger]

let _ = run_test_tt_main suite
 


