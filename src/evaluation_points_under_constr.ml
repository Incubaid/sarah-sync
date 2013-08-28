(* Module to determine the number of evaluation points needed in the reconciliation *)

open FiniteField
open Set_reconciliation

module EvaluationPts =
  functor (F : FINITEFIELD) ->
struct

  type element = F.t

  exception Unable_to_find_good_m

  module S = SetReconciliation(F)

  (* Produce a given number of evaluation points. These will be: [q-1, q-2, ...] *)
  let evalPts m =
    let q = F.q in
    let rec getPoints start len =
      if len = 0
      then []
      else F.wrap start :: (getPoints (start - 1) (len - 1))
    in
    getPoints (q - 1) m

  (* Determine extra evaluation points, used in the function to determine an upper bound on m.
     k: number of extra points requested. Point will be: [2^(w-1) + 1, 2^(w-1) + 2, ...] *)
  let extraEvalPts k =
    let first = (1 lsl (F.w - 1)) + 1 in
    let rec getPoints start len =
      if len = 0
      then []
      else F.wrap start :: (getPoints (start + 1) (len - 1))
    in
    getPoints first k 


  (* Determine a close upper bound on the number of evaluation points needed by the reconciliation algorithm *)
  let findM_old (s1 : S.set) (s2 : S.set) =
    let m1 = List.length s1 in
    let delta = m1 - List.length s2 in
    let initMin =
      if abs delta = 0
      then 1
      else abs delta
    in
    let initMax = m1 + List.length s2 in
    let rec search min max previous_min =
      if max < min
      then raise Unable_to_find_good_m
      else
        begin
          if min = max
          then min
          else
            begin
              let good_min =
                if (min - delta) land 1 <> 0 (* Ensure same parity *)
                then min + 1
                else min
              in
              if good_min = max
              then good_min
              else 
                begin
                  let mid =
                    let mid' = (min + max) / 2 in
                    if (mid' - delta) land 1 <> 0   (* Ensure same parity *)
                    then mid' + 1
                    else mid'
                  in
                  let pts = evalPts good_min in
                  try
                    let () = Printf.printf "Considering m: %i.\n%!" good_min in
                    let chi_1 = List.map (S.CharPoly.evalCharPoly s1) pts in
                    let cfsNum, cfsDenom = S.interpolation m1 chi_1 s2 pts in
                    let k = 1 in        (* CHANGE THIS *)
                    (*let extraPts = extraEvalPts good_min k in *)
                    let extraPts = extraEvalPts k in
                    let actual_chi_1 = List.map (S.CharPoly.evalCharPoly s1) extraPts in (* !!!! *)
                    let actualVals = S.evalCharPols actual_chi_1 s2 extraPts in
                    let ourNumVals = List.map (S.evaluate_pol cfsNum) extraPts in
                    let ourDenomVals = List.map (S.evaluate_pol cfsDenom) extraPts in
                    let ourVals = List.map2 F.div ourNumVals ourDenomVals in
                    let ok = List.for_all2 F.eq actualVals ourVals in
                    if ok
                    then
                      begin
                        if previous_min = -1 (* Only in first step *)
                        then good_min
                        else
                          begin
                            let new_min = (previous_min + good_min) / 2 in
                            let new_max = good_min in
                            search new_min new_max previous_min
                          end
                      end
                    else
                      begin
                        let new_min = mid in
                        let new_previous = good_min in
                        search new_min max new_previous
                      end
                  with S.InterPol.M.System_no_solution ->
                    let new_min = mid in
                    let new_previous = good_min in
                    search new_min max new_previous
                end
            end
        end
    in
    search initMin initMax (-1)
 


  (* Get the first n elements of a list *)
  let take n list =
    let rec loop acc i =
      if i = n
      then acc
      else 
        begin
          let acc' = (List.nth list i) :: acc in
          loop acc' (i + 1)
        end
    in
    List.rev (loop [] 0)


  (* New version 27/8 *)
  let findM_old_2 (s1 : S.set) (s2 : S.set) =
    let m1 = List.length s1 in
    let delta = m1 - List.length s2 in
    let initMin =
      if abs delta = 0
      then 1
      else abs delta
    in
    let initMax = m1 + List.length s2 in
    let pts = evalPts initMax in
    let chi_1 = List.map (S.CharPoly.evalCharPoly s1) pts in
    (*let chi_2 = List.map (S.CharPoly.evalCharPoly s2) pts in *)
    let k = 1 in                  (* CHANGE THIS *)
    let extraPts = extraEvalPts k in
    let actual_chi_1 = List.map (S.CharPoly.evalCharPoly s1) extraPts in (* !!!! *)
    let actualVals = S.evalCharPols actual_chi_1 s2 extraPts in
    let rec search min max previous_min =
      if max < min
      then raise Unable_to_find_good_m
      else
        begin
          if min = max
          then min
          else
            begin
              let good_min =
                if (min - delta) land 1 <> 0 (* Ensure same parity *)
                then min + 1
                else min
              in
              if good_min = max
              then good_min
              else 
                begin
                  let mid =
                    let mid' = (min + max) / 2 in
                    if (mid' - delta) land 1 <> 0   (* Ensure same parity *)
                    then mid' + 1
                    else mid'
                  in
                  try
                    let () = Printf.printf "Considering m: %i.\n%!" good_min in
                    let used_chi_1 = take good_min chi_1 in
                    let cfsNum, cfsDenom = S.interpolation m1 used_chi_1 s2 (take good_min pts) in
                    let ourNumVals = List.map (S.evaluate_pol cfsNum) extraPts in
                    let ourDenomVals = List.map (S.evaluate_pol cfsDenom) extraPts in
                    let ourVals = List.map2 F.div ourNumVals ourDenomVals in
                    let ok = List.for_all2 F.eq actualVals ourVals in
                    if ok
                    then
                      begin
                        if previous_min = -1 (* Only in first step *)
                        then good_min
                        else
                          begin
                            let new_min = (previous_min + good_min) / 2 in
                            let new_max = good_min in
                            search new_min new_max previous_min
                          end
                      end
                    else
                      begin
                        let new_min = mid in
                        let new_previous = good_min in
                        search new_min max new_previous
                      end
                  with S.InterPol.M.System_no_solution ->
                    let new_min = mid in
                    let new_previous = good_min in
                    search new_min max new_previous
                end
            end
        end
    in
    search initMin initMax (-1)



  (* Determine the maximal value of m and the maximum number of extra evaluation points*)
  let get_max_vals size_1 size_2 =
    let k = 1 in     (* CHANGE THIS *) 
    (size_1 + size_2, k)

  (* Even newer 27/8 *)
  (* Determine an upper bound on the number of sample points *)
  let findM delta rat_vals actual_vals init_max eval_pts extra_pts =
    let init_min =
      if abs delta = 0
      then 1
      else abs delta
    in
    let rec search min max =
      if max < min
      then raise Unable_to_find_good_m
      else
        begin
          if min = max
          then 
            begin
              let () = Printf.printf "Min is %i. Max is %i\n%!" min max in
              let cfs_num, cfs_denom = S.interpolation_B (take min eval_pts) (take min rat_vals) delta in
              min, cfs_num, cfs_denom
            end
          else
            begin
              let good_min =
                if (min - delta) land 1 <> 0 (* Ensure same parity *)
                then min + 1
                else min
              in
              if good_min = max
              then 
                begin
                  let () = Printf.printf "Goodmin is %i. Max is %i\n%!" good_min max in
                  let cfs_num, cfs_denom = S.interpolation_B (take good_min eval_pts) (take good_min rat_vals) delta in
                  good_min, cfs_num, cfs_denom
                end
              else 
                begin
                  let mid =
                    (* let mid' = (min + max) / 2 in
                    if (mid' - delta) land 1 <> 0   (* Ensure same parity *)
                    then mid' + 1
                    else mid' *)
                    let product = float_of_int (min * max) in
                    int_of_float (ceil (sqrt product))
                  in
                  try
                    let () = Printf.printf "Considering m: %i. Max is %i\n%!" good_min max in
                    let cfs_num, cfs_denom = S.interpolation_B (take good_min eval_pts) (take good_min rat_vals) delta in
                    let ourNumVals = List.map (S.evaluate_pol cfs_num) extra_pts in
                    let ourDenomVals = List.map (S.evaluate_pol cfs_denom) extra_pts in
                    let our_vals = List.map2 F.div ourNumVals ourDenomVals in
                    let ok = List.for_all2 F.eq actual_vals our_vals in
                    if ok
                    then
                      (good_min , cfs_num , cfs_denom)
                    else
                      begin
                        let new_min = mid in
                        search new_min max
                      end
                  with S.InterPol.M.System_no_solution ->
                    let new_min = mid in
                    search new_min max
                end
            end
        end
    in
    search init_min init_max

end
