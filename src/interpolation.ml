(* Interpolation *)

open FiniteField
open Matrices

module Interpolation =
  functor  (F : FINITEFIELD) ->
struct
  open F

  type element = t

  exception Delta_and_m_different_parity

  module M = Matrix(F)

  (* System for the interpolation.
     m = number of evalution points,  delta = |set1| - |set2| *)
  let constructSystem (points : element list) (values : element list) m delta =
    let d1 = (m + delta) / 2 in
    let d2 = (m - delta) / 2 in
    if d1 + d2 <> m
    then raise Delta_and_m_different_parity
    else
      begin
        let makeRow i =
          Array.init (m + 1)
            (fun j ->
              if j < d1
              then exp (List.nth points i) j
              else
                begin
                  if j < m
                  then (List.nth values i) *: (exp (List.nth points i) (j - d1))
                  else ( (List.nth values i) *: (exp ( List.nth points i) d2) ) -:
                    ( exp (List.nth points i) d1 )
                end
            )
        in
        ( Array.init m makeRow , d1 , d2)
      end

  (* Actual interpolation. Solves the system with Gaussian elimination. *)
  let interpolate (points : element list) (values : element list) m delta =
    let () = Printf.printf "Constructing system for interpolation.\n%!" in
    let system, d1, d2 = constructSystem points values m delta in
    let () = Printf.printf "Solving system for interpolation.\n%!" in
    let solution =  M.solveSystem system in
    let cfsNum = Array.init (d1 + 1)
      ( fun i ->
        if i < d1
        then solution.(i)
        else one ) in    (* Coefficients numerator *)
    let cfsDenom = Array.init (d2 + 1)
      ( fun i ->
        if i < d2
        then solution.(d1 + i)
        else one ) in    (* Coefficients denominator *)
    cfsNum , cfsDenom

end
