(* Tests for Chien Search *)

open OUnit
open F_16
open Chien_search

module F = TestFiniteField
module C = Chien(TestFiniteField)

let test_chien_search () = 
  let twentyfive = F.mult 5 5 in
  let twelve = F.plus 9 3 in
  let twentyseven = F.mult 9 3 in
  let tests = [
    ( [|F.zero ; F.one ; F.one|] , [0 ; 1] ) ;
    ( [|F.one ; F.zero ; F.zero|] , [] ) ;
    ( [|F.zero ; F.one ; F.zero ; F.one|] ,[0 ; 1] ) ;
    ( [| F.zero ; 5 ; F.one|], [0 ; 5]) ;
    ( [|F.zero ; twentyfive ; F.zero ; F.one |] , [0 ; 5] ) ;
    ( [|F.zero ; twentyseven ; twelve ; F.one|] , [0 ; 3 ; 9])
  ]
  in 
  let test_one (poly, roots) =
    let answer = C.chienSearch2 poly in
    OUnit.assert_equal (List.sort compare answer) (List.sort compare roots)
  in
  List.iter test_one tests
