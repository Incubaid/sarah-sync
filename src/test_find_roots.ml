(* Tests for find_roots.ml *)

open OUnit
open FiniteField
open Find_roots

module Field = FiniteField.Make(struct
  let w = 13
end)

module RF = Root_finder(Field)

let test_roots () =
  let open Field in
  let twentyfive = (wrap 5) *: (wrap 5) in
  let twelve = (wrap 9) +: (wrap 3) in
  let twentyseven = (wrap 9) *: (wrap 3) in
  let thirteen = twelve +: one in
  let thirtynine = twentyseven +: twelve in
  let fortyfive = (wrap 5) *: (wrap 9) in
  let fourteen = (wrap 5) +: (wrap 9) in
  let tests = [
    [|one ; zero ; one|] , [ one ; one], "test1"  ;
    [|one ; zero ; zero|] , [], "test2"   ;
    [|zero ; one ; zero ; one|] ,[zero ; one ; one], "test3"  ;
    [|zero ; wrap 5 ; one|], [zero ; wrap 5], "test4" ;
    [|zero ; twentyfive ; zero ; one |] , [zero ; wrap 5 ; wrap 5], "test5" ;
    [|zero ; twentyseven ; twelve ; one|] , [zero ; wrap 3 ; wrap 9], "test6" ;
    [|twentyseven ; thirtynine; thirteen ; one|] , [wrap 1 ; wrap 3 ; wrap 9], "test7" ;
    [|fortyfive *: (wrap 2); fourteen *: (wrap 2) ; wrap 2|] , [wrap 5 ; wrap 9], "test8"
  ]
  in
  let test_one (poly, rts, id) =
    Printf.printf "Performing %s.\n%!" id ;
    let answer = RF.roots poly in
    let msg = Printf.sprintf "Incorrect roots." in
    OUnit.assert_equal (List.sort compare answer) (List.sort compare rts) ~msg
  in
  List.iter test_one tests


let suite = "Set Reconciliation" >::: [ "test_roots" >:: test_roots ]

let _ = run_test_tt_main suite
