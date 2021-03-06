(* Tests to verify whether Chien and BTA always find the same roots *)

open OUnit
open FiniteField
open Chien_search
open Find_roots

module Field = FiniteField.Make(struct
   let w = 13
end)

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
    ([|one ; zero ; one|], 2) , "test1"  ;
    ([|one ; zero ; zero|], 0) , "test2" ;
    ([|zero ; one ; zero ; one|], 3) , "test3" ;
    ([|zero ; wrap 5 ; one|], 2) , "test4" ;
    ([|zero ; twentyfive ; zero ; one |], 3) , "test5" ;
    ([|zero ; twentyseven ; twelve ; one|], 3) , "test6" ;
    ([|twentyseven ; thirtynine; thirteen ; one|], 3) , "test7" ;
    ([|fortyfive *: (wrap 2); fourteen *: (wrap 2) ; wrap 2|], 2) , "test8" ;
    ([|wrap 47 ; wrap 668 ; wrap 829 ; wrap 192 ; wrap 752 ; wrap 240 ; wrap 202 ; one|], 7) , "test9" ;
    ([|zero ; wrap 5; one ; zero ; one|], 4), "test10"
  ]
  in

  let test_one ((p,t) as poly, id) =
    let pol_copy = (Array.copy p, t) in   (* Chien modifies the array *)

    let st1 = Unix.gettimeofday () in
    let ch = C.chien_search poly in
    let sp1 = Unix.gettimeofday () in

    let st2 = Unix.gettimeofday () in
    let bta = BTA.roots pol_copy in
    let sp2 = Unix.gettimeofday () in

    let msg = Printf.sprintf "Roots found by Chien and BTA do not match in %s." id in
    Printf.printf "Time chien: %f\n%!" (sp1 -. st1) ;
    Printf.printf "Time bta: %f\n%!" (sp2 -. st2) ;
    OUnit.assert_equal (List.sort compare ch) (List.sort compare bta) ~msg
  in
  List.iter test_one tests
