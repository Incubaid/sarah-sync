(* Tests for find_roots.ml *)

open OUnit
open FiniteField
open Find_roots

module Field = FiniteField.Make(struct
  let w = 10
end)

module RF = Root_finder(Field)

let test_roots () =
  let open Field in
  let twentyfive = mult (wrap 5) (wrap 5) in
  let twelve = plus (wrap 9) (wrap 3) in
  let twentyseven = mult (wrap 9) (wrap 3) in
  let thirteen = plus twelve one in
  let thirtynine = plus twentyseven twelve in
  let fortyfive = mult (wrap 5) (wrap 9) in
  let fourteen =  plus (wrap 5) (wrap 9) in
  let tests = [
    [|one ; zero ; one|] , [ one ; one]  ;
    [|one ; zero ; zero|] , []   ;
    [|zero ; one ; zero ; one|] ,[zero ; one ; one]  ;
    [|zero ; wrap 5 ; one|], [zero ; wrap 5] ;
    [|zero ; twentyfive ; zero ; one |] , [zero ; wrap 5 ; wrap 5] ;
    [|zero ; twentyseven ; twelve ; one|] , [zero ; wrap 3 ; wrap 9] ;
    [|twentyseven ; thirtynine; thirteen ; one|] , [wrap 1 ; wrap 3 ; wrap 9] ;
    [|mult fortyfive (wrap 2); mult fourteen (wrap 2) ; wrap 2|] , [wrap 5 ; wrap 9]
  ]
  in
  let test_one (poly, rts) =
    let answer = RF.roots poly in
    let msg = Printf.sprintf "Incorrect roots." in
    OUnit.assert_equal (List.sort compare answer) (List.sort compare rts) ~msg
  in
  List.iter test_one tests


let suite = "Set Reconciliation" >::: [ "test_roots" >:: test_roots ]

let _ = run_test_tt_main suite
