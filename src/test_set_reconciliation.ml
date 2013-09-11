(* Tests for set reconciliation *)

open OUnit
open FiniteField
open Set_reconciliation
open Evaluation_points

module S1 = SetReconciliation(GF128)
module EP1 = EvaluationPts(GF128)

module S2 = SetReconciliation(GF1024)
module EP2 = EvaluationPts(GF1024)


let test_set_reconc () =
  let tests = [      (* (set1 , set2 , delta1, identifier) *)
    ( [1] , [2] , [1], "test1" );
    ( [1 ; 2 ; 9 ; 12 ; 33], [1 ; 2 ; 9 ; 10 ; 12 ; 28], [33], "test2" ) ;
    ( [3 ; 45 ; 60 ; 1], [9 ; 3 ; 1 ; 60 ; 55 ; 90], [45], "test3" ) ;
    ( [1 ; 2 ; 99 ; 32 ; 6; 8; 10; 66], [3 ; 99; 8; 10], [1; 2; 6; 32; 66], "test4" ) ;
    ( [4; 88; 9; 24] , [9; 11; 88; 21] , [4; 24] , "test5") ;
  ]
  in
  let test_one (set1, set2, only_set1, id) =
    let () = Printf.printf "============================================\n%!" in
    let () = Printf.printf "Performing %s\n%!" id in

    let size_1 = List.length set1 in
    let size_2 = List.length set2 in
    let delta = size_1 - size_2 in
    let init_max, k = EP1.get_max_vals size_1 size_2 in
    let eval_pts = EP1.evalPts init_max in
    let rat_vals = S1.get_rational_values set1 set2 eval_pts in
    let extra_pts = EP1.extraEvalPts k in
    let actual_vals = S1.get_rational_values set1 set2 extra_pts in
    let m, ((cfs_num, dN) as c_n), ((cfs_denom, dD) as c_d) = EP1.findM delta rat_vals actual_vals init_max eval_pts extra_pts in
    let () = Printf.printf "DECIDED TO USE: m = %i.\n%!" m in

    (* Copy of coefficients, because Chien overwrites the arrays *)
    let cfs_num_cp = (Array.copy cfs_num, dN) in
    let cfs_denom_cp = (Array.copy cfs_denom, dD) in


    (* Test with Chien search *)
    let () = Printf.printf "Testing Chien\n%!" in
    let sols_ch =
      if m = 0
      then []
      else S1.reconcile c_n c_d
    in
    let msg = Printf.sprintf "Numerator: found %i elements, needed %i in %s\n" (List.length sols_ch) (List.length only_set1) id in
    OUnit.assert_equal (List.sort compare sols_ch) (List.sort compare only_set1) ~msg ;


    (* Test with BTA *)
    let () = Printf.printf "Testing BTA\n%!" in
    let sols_BTA =
      if m = 0
      then []
      else S1.reconcile_BTA cfs_num_cp cfs_denom_cp
    in
    let msg_b = Printf.sprintf "Numerator: found %i elements, needed %i in %s\n" (List.length sols_BTA) (List.length only_set1) id in
    OUnit.assert_equal (List.sort compare sols_BTA) (List.sort compare only_set1) ~msg:msg_b

  in
  let open GF128 in
  List.iter test_one
    ( List.map
        (fun (a,b,c,d) ->
          (List.map wrap a, List.map wrap b, List.map wrap c, d)
        )
        tests )


let test_set_reconc_bigger () =
  let tests = [      (* (set1 , set2 , delta1, identifier) *)
    ( [314 ; 315 ; 316 ; 317 ; 318; 319 ; 320 ; 321 ; 322 ; 323; 324 ; 325 ; 326 ; 327 ; 328 ; 329 ; 330] ,
      [314 ; 200 ; 201; 203 ; 204 ; 205 ; 206 ; 207 ; 208 ; 209 ; 210 ;211; 212 ; 213 ; 214 ;215 ; 216] ,
      [315 ; 316 ; 317 ; 318; 319 ; 320 ; 321 ; 322 ; 323; 324 ; 325 ; 326 ; 327 ; 328 ; 329 ; 330],
      "test_1b" ) ;
    ( [1 ; 2 ; 3 ; 4 ; 5 ; 6 ; 7 ; 8 ; 9 ; 10 ; 11 ; 12 ; 13; 14; 15 ; 16; 17 ; 18 ; 19 ; 20] ,
      [1 ; 3 ; 5 ; 7 ; 9 ; 11 ; 13 ; 15 ; 17 ; 19 ; 21 ; 23 ; 25 ; 27 ; 29 ; 31 ; 33; 35; 37] ,
      [2 ; 4 ; 6 ; 8 ; 10 ; 12 ; 14 ; 16 ; 18 ; 20],
      "test_2b" ) ;
    ( [1; 2; 3; 99; 105; 219; 220; 221; 222; 223; 224; 225; 226; 227; 228; 229; 230] ,
      [3; 55; 56; 57 ; 58 ; 59 ; 60 ; 61 ; 62; 220; 221; 222; 223; 224; 225; 226; 227; 228; 229; 230] ,
      [1; 2; 99; 105; 219],
      "test3b" );
    ( [1; 2; 3; 4; 5 ; 6 ; 7 ; 8 ; 9 ; 10 ; 11 ; 12 ; 13 ;14 ; 15 ; 16 ;17 ; 18 ;19 ;20 ; 21 ; 22 ; 23 ; 24 ;25 ;26 ;27 ;28 ; 29 ;30] ,
      [1; 2; 3; 4; 5 ; 6 ; 7 ; 8 ; 9 ; 10 ; 314 ; 315 ; 316 ; 317 ; 318; 319 ; 320 ; 321 ; 322 ; 323; 324 ; 325 ; 326 ; 327 ; 328 ; 329 ; 330 ] ,
      [11 ; 12 ; 13 ;14 ; 15 ; 16 ;17 ; 18 ;19 ;20 ; 21 ; 22 ; 23 ; 24 ;25 ;26 ;27 ;28 ; 29 ;30 ],
      "test4b" ) ;
    ( [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20; 21; 22; 23; 24; 25; 26 ; 27; 28; 29; 30] ,
      [60 ; 61; 62 ; 63 ; 64 ; 65 ; 66 ; 67 ; 68 ; 69 ;70 ; 71 ; 72 ; 73 ; 74 ; 75 ; 76 ; 77 ; 78 ; 79 ; 80; 81; 82; 83; 84; 85; 86; 87; 88; 89; 90] ,
      [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20; 21; 22; 23; 24; 25; 26 ; 27; 28; 29; 30],
      "test5b" ) ;
    ( [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20] ,
      [60 ; 61; 62 ; 63 ; 64 ; 65 ; 66 ; 67 ; 68 ; 69 ;70 ; 71 ; 72 ; 73 ; 74 ; 75 ; 76 ; 77 ; 78 ; 79 ; 80] ,
      [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20] ,
      "test6b" ) ;
    ( [1],
      [1],
      [],
      "test7b" )
  ]
  in
  let test_one (set1, set2, only_set1, id) =
    let () = Printf.printf "============================================\n%!" in
    let () = Printf.printf "Performing %s\n%!" id in


    let size_1 = List.length set1 in
    let size_2 = List.length set2 in
    let delta = size_1 - size_2 in
    let init_max, k = EP2.get_max_vals size_1 size_2 in
    let eval_pts = EP2.evalPts init_max in
    let rat_vals = S2.get_rational_values set1 set2 eval_pts in
    let extra_pts = EP2.extraEvalPts k in
    let actual_vals = S2.get_rational_values set1 set2 extra_pts in
    let m, ((cfs_num, dN) as c_n), ((cfs_denom, dD) as c_d) = EP2.findM delta rat_vals actual_vals init_max eval_pts extra_pts in
    let () = Printf.printf "DECIDED TO USE: m = %i.\n%!" m in

    (* Copy of coefficients, because Chien overwrites the arrays *)
    let cfs_num_cp = (Array.copy cfs_num, dN) in
    let cfs_denom_cp = (Array.copy cfs_denom, dD) in

    (* Test with Chien search *)
    let () = Printf.printf "Testing Chien\n%!" in
    let sols_ch =
      if m = 0
      then []
      else S2.reconcile c_n c_d
    in
    let msg = Printf.sprintf "Numerator: found %i elements, needed %i in %s\n" (List.length sols_ch) (List.length only_set1) id in
    OUnit.assert_equal (List.sort compare sols_ch) (List.sort compare only_set1) ~msg ;

    (* Test with BTA *)
    let () = Printf.printf "Testing BTA\n%!" in
     let sols_BTA =
      if m = 0
      then []
      else S2.reconcile_BTA cfs_num_cp cfs_denom_cp
    in
    let msg_b = Printf.sprintf "Numerator: found %i elements, needed %i in %s\n" (List.length sols_BTA) (List.length only_set1) id in
    OUnit.assert_equal (List.sort compare sols_BTA) (List.sort compare only_set1) ~msg:msg_b

  in
  let open GF1024 in
  List.iter test_one
    ( List.map
        (fun (a,b,c, d) ->
          (List.map wrap a, List.map wrap b, List.map wrap c, d)
        )
        tests )
