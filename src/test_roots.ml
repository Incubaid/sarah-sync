(* Tests to verify whether Chien and BTA always find the same roots *)

open OUnit
open FiniteField
open Chien_search
open Find_roots

module Field = GF1024

module C = Chien(Field)
module BTA = Root_finder(Field)

let test_compare () =
  let open Field in
  let twentyfive = (wrap 5) *: (wrap 5) in
  let twelve = (wrap 9) +: (wrap 3) in
  let twentyseven = (wrap 9) *: (wrap 3) in
  let thirteen = twelve +: one in
  let thirtynine = twentyseven +: twelve in
  let fortyfive = (wrap 5) *: (wrap 9) in
  let fourteen =  (wrap 5) +: (wrap 9) in
  let tests = [
    [|one ; zero ; one|] , "test1"  ;
    [|one ; zero ; zero|] , "test2" ;
    [|zero ; one ; zero ; one|] , "test3" ;
    [|zero ; wrap 5 ; one|] , "test4" ;
    [|zero ; twentyfive ; zero ; one |] , "test5" ;
    [|zero ; twentyseven ; twelve ; one|] , "test6" ;
    [|twentyseven ; thirtynine; thirteen ; one|] , "test7" ;
    [|fortyfive *: (wrap 2); fourteen *: (wrap 2) ; wrap 2|] , "test8" ;
    [|wrap 47 ; wrap 668 ; wrap 829 ; wrap 192 ; wrap 752 ; wrap 240 ; wrap 202 ; one|] , "test9"
  ]
  in
  let test_one (poly, id) =
    let pol_copy = Array.copy poly in   (* Chien modifies the array *)
    let ch = C.chienSearch poly in
    let bta = BTA.roots pol_copy in
    let msg = Printf.sprintf "Roots found by Chien and BTA do not match in %s." id in
    OUnit.assert_equal (List.sort compare ch) (List.sort compare bta) ~msg
  in
  List.iter test_one tests
