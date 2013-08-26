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
     k: number of extra points requested *)
  let extraEvalPts k =
    let first = (1 lsl (F.w - 1)) + 1 in
    let rec getPoints start len =
      if len = 0
      then []
      else F.wrap start :: (getPoints (start + 1) (len - 1))
    in
    getPoints first k 


  (* Determine the maximal value of m and the maximum number of extra evaluation points*)
  let get_max_vals size_1 size_2 =
    let k = 1 in     (* CHANGE THIS *) 
    (size_1 + size_2, k)


  (* Determine a close upper bound on the number of evaluation points needed by the reconciliation algorithm *)
  (*let findM (s1 : S.set) (s2 : S.set) =
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
                    let extraPts = extraEvalPts good_min k in
                    let actual_chi_1 = List.map (S.CharPoly.evalCharPoly s1) extraPts in (* !!!! *)
                    let actualVals = S.evalCharPols actual_chi_1 s2 extraPts in
                    let ourNumVals = List.map (S.evaluatePol cfsNum) extraPts in
                    let ourDenomVals = List.map (S.evaluatePol cfsDenom) extraPts in
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
    search initMin initMax (-1) *)

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

  (* Determine a close upper bound on the number of evaluation points needed by the reconciliation algorithm 
     size_1: size of the first set
     chi1: evaluation of the characteristic polynomial of the first set in all sample points
     extra1: evaluation of the characteristic polynomial of the first set in all extra points
     s2: second set *)
  let findM size_1 chi1 extra1 (s2 : S.set) =
    let delta = size_1 - List.length s2 in
    let initMin =
      if abs delta = 0
      then 1
      else abs delta
    in
    let initMax = List.length chi1 in
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
                    (*let chi_1 = List.map (S.CharPoly.evalCharPoly s1) pts in *)
                    let used_chi1 = take good_min chi1 in         (* Get the sample points we need*)  
                    let cfsNum, cfsDenom = S.interpolation size_1 used_chi1 s2 pts in
                    let k = 1 in        (* CHANGE THIS *)
                    let extraPts = extraEvalPts k in
                    let actual_chi_1 = take k extra1 in (* !!!! *)
                    let actualVals = S.evalCharPols actual_chi_1 s2 extraPts in
                    let ourNumVals = List.map (S.evaluatePol cfsNum) extraPts in
                    let ourDenomVals = List.map (S.evaluatePol cfsDenom) extraPts in
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

end