(* Tests for Chien Search *)

open OUnit
open FiniteField
open Chien_search

module C = Chien(GF16)

let test_chien_search () = 
  let twentyfive = GF16.mult (GF16.wrap 5) (GF16.wrap 5) in
  let twelve = GF16.plus (GF16.wrap 9) (GF16.wrap 3) in
  let twentyseven = GF16.mult (GF16.wrap 9) (GF16.wrap 3) in
  let tests = [
    ( [|GF16.zero ; GF16.one ; GF16.one|] , [GF16.zero ; GF16.one] ) ;
    ( [|GF16.one ; GF16.zero ; GF16.zero|] , [] ) ;
    ( [|GF16.zero ; GF16.one ; GF16.zero ; GF16.one|] ,[GF16.zero ; GF16.one] ) ;
    ( [|GF16.zero ; GF16.wrap 5 ; GF16.one|], [GF16.zero ; GF16.wrap 5]) ;
    ( [|GF16.zero ; twentyfive ; GF16.zero ; GF16.one |] , [GF16.zero ; GF16.wrap 5] ) ;
    ( [|GF16.zero ; twentyseven ; twelve ; GF16.one|] , [GF16.zero ; GF16.wrap 3 ; GF16.wrap 9])
  ]
  in 
  let test_one (poly, roots) =
    let answer = C.chienSearch2 poly in
    OUnit.assert_equal (List.sort compare answer) (List.sort compare roots)
  in
  List.iter test_one tests

