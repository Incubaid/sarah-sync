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


  (* Get the first n elements of a list *)
  let take n list =
    if List.length list < n
    then failwith "Invalid subset-length in method take."
    else
      begin
        let rec loop curr acc i =
          match curr with
          | [] -> acc
          | x :: xs ->
            if i = n
            then acc
            else 
              begin
                let acc' = x :: acc in
                loop xs acc' (i + 1)
              end
        in
        List.rev (loop list [] 0)
      end

  (* Determine the maximal value of m and the maximum number of extra evaluation points *)
  let get_max_vals size_1 size_2 =
    let k = 1 in     (* CHANGE THIS *)
    (size_1 + size_2, k)


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
                    (*let mid' = (min + max) / 2 in
                    if (mid' - delta) land 1 <> 0   (* Ensure same parity *)
                    then mid' + 1
                    else mid'*)
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

  (* Determine a close upper bound on the number of evaluation points needed by the reconciliation algorithm
     size_1: size of the first set
     chi1: evaluation of the characteristic polynomial of the first set in all sample points
     extra1: evaluation of the characteristic polynomial of the first set in all extra points
     s2: second set *)
  let findM_server size_1 chi1 extra1 (s2 : S.set) =
    let delta = size_1 - List.length s2 in
    let initMin =
      if abs delta = 0
      then 1
      else abs delta
    in
    let initMax = List.length chi1 in
    let rec search min max =
      if max < min
      then raise Unable_to_find_good_m
      else
        begin
          if min = max
          then
            begin
              let pts = evalPts min in
              let cfs_num, cfs_denom = S.interpolation size_1 (take min chi1) s2 pts in
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
                  let pts = evalPts good_min in
                  let cfs_num, cfs_denom = S.interpolation size_1 (take good_min chi1) s2 pts in
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
                  let pts = evalPts good_min in
                  try
                    let () = Printf.printf "Considering m: %i.\n%!" good_min in
                    let used_chi1 = take good_min chi1 in         (* Get the sample points we need*)
                    let cfs_num, cfs_denom = S.interpolation size_1 used_chi1 s2 pts in
                    let k = 1 in                                  (* CHANGE THIS *)
                    let extraPts = extraEvalPts k in
                    let actual_chi_1 = take k extra1 in
                    let actualVals = S.evalCharPols actual_chi_1 s2 extraPts in
                    let ourNumVals = List.map (S.evaluate_pol cfs_num) extraPts in
                    let ourDenomVals = List.map (S.evaluate_pol cfs_denom) extraPts in
                    let ourVals = List.map2 F.div ourNumVals ourDenomVals in
                    let ok = List.for_all2 F.eq actualVals ourVals in
                    if ok
                    then
                      (good_min, cfs_num, cfs_denom)
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
    search initMin initMax


end
