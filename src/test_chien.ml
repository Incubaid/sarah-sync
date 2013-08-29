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
  let twentyfive = mult (wrap 5) (wrap 5) in
  let twelve = plus (wrap 9) (wrap 3) in
  let twentyseven = mult (wrap 9) (wrap 3) in
  let thirteen = plus twelve one in
  let thirtynine = plus twentyseven twelve in
  let fortyfive = mult (wrap 5) (wrap 9) in
  let fourteen =  plus (wrap 5) (wrap 9) in
  let tests = [
    [|one ; zero ; one|] , [ one ]  ;
    [|one ; zero ; zero|] , []   ;
    [|zero ; one ; zero ; one|] ,[zero ; one ]  ; 
    [|zero ; wrap 5 ; one|], [zero ; wrap 5] ;
    [|zero ; twentyfive ; zero ; one |] , [zero ; wrap 5] ; 
    [|zero ; twentyseven ; twelve ; one|] , [zero ; wrap 3 ; wrap 9] ;
    [|twentyseven ; thirtynine; thirteen ; one|] , [wrap 1 ; wrap 3 ; wrap 9] ;
    [|mult fortyfive (wrap 2); mult fourteen (wrap 2) ; wrap 2|] , [wrap 5 ; wrap 9]
  ]
  in 
  let test_one (poly, roots) =
    let answer = C.chienSearch2 poly in
    let msg = Printf.sprintf "Incorrect roots." in
    OUnit.assert_equal (List.sort compare answer) (List.sort compare roots) ~msg
  in
  List.iter test_one tests

