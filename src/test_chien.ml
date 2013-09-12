(* Tests for Chien Search *)

open OUnit
open FiniteField
open Chien_search

module Field = FiniteField.Make(struct
  let w = 16
end)

module C = Chien(Field)

let test_chien_search () =
  let open Field in
  let twentyfive = (wrap 5) *: (wrap 5) in
  let twelve = (wrap 9) +: (wrap 3) in
  let twentyseven = (wrap 9) *: (wrap 3) in
  let thirteen = twelve +: one in
  let thirtynine = twentyseven +: twelve in
  let fortyfive = (wrap 5) *: (wrap 9) in
  let fourteen =  (wrap 5) +: (wrap 9) in
  let tests = [
    ([|one ; zero ; one|], 2 ) , [ one ; one ]  ;
    ([|one ; zero ; zero|], 0) , []   ;
    ([|zero ; one ; zero ; one|], 3) ,[zero ; one ; one ]  ;
    ([|zero ; wrap 5 ; one|], 2), [zero ; wrap 5] ;
    ([|zero ; twentyfive ; zero ; one |], 3) , [zero ; wrap 5 ; wrap 5] ;
    ([|zero ; twentyseven ; twelve ; one|], 3) , [zero ; wrap 3 ; wrap 9] ;
    ([|twentyseven ; thirtynine; thirteen ; one|], 3) , [wrap 1 ; wrap 3 ; wrap 9] ;
    ([|fortyfive *: (wrap 2); fourteen *: (wrap 2) ; wrap 2|], 2) , [wrap 5 ; wrap 9]
  ]
  in
  let test_one (poly, roots) =
    let answer = C.chienSearch poly in
    let msg = Printf.sprintf "Incorrect roots." in
    OUnit.assert_equal (List.sort compare answer) (List.sort compare roots) ~msg
  in
  List.iter test_one tests
