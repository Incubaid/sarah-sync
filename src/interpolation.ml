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
        let sys = Array.make_matrix m (m + 1) one in
        let rec loop pts vals r =
          match (pts, vals) with
          | [], []  -> ()
          | x :: xs, f :: fs  ->
            let rec fill_in c1 c2 curr =
              if d2 <= d1
              then 
                begin
                  if c2 < d1 + d2   (* Not yet last column *)
                  then 
                    begin
                      let next = curr *: x in
                      let () = sys.(r).(c1) <- curr in
                      let () = sys.(r).(c2) <- f *: curr in
                      fill_in (c1 + 1) (c2 + 1) next
                    end
                  else 
                    begin
                      if c2 = d1 + d2 then sys.(r).(d1 + d2) <- f *: curr ; (* Intermediate value last column *)
                      if c1 < d1
                      then 
                        begin
                          let next = curr *: x in
                          let () = sys.(r).(c1) <- curr in
                          fill_in (c1 + 1) (c2 + 1) next
                        end
                      else
                        sys.(r).(d1 + d2) <- sys.(r).(d1 + d2) -: curr
                    end
                end
              else 
                begin
                  if c1 < d1   (* Not yet last column *)
                  then 
                    begin
                      let next = curr *: x in
                      let () = sys.(r).(c1) <- curr in
                      let () = sys.(r).(c2) <- f *: curr in
                      fill_in (c1 + 1) (c2 + 1) next
                    end
                  else 
                    begin
                      if c1 = d1 then sys.(r).(d1 + d2) <- curr ; (* Intermediate value last column *)
                      if c2 < d1 + d2
                      then 
                        begin
                          let next = curr *: x in
                          let () = sys.(r).(c2) <- f *: curr in
                          fill_in (c1 + 1) (c2 + 1) next
                        end
                      else
                        sys.(r).(d1 + d2) <- (f *: curr) -: sys.(r).(d1 + d2)
                    end

                end
            in
            fill_in 0 d1 one ;
            loop xs fs (r + 1)
          | _, _ -> failwith "Different number of points and values."
        in
        loop points values 0 ;
        ( sys , d1 , d2)
      end


  (* Actual interpolation. Solves the system with Gaussian elimination. *)
  let interpolate (points : element list) (values : element list) m delta =
    let () = Printf.printf "Constructing system for interpolation.\n%!" in
    let sys, d1, d2 = constructSystem points values m delta in
    let () = Printf.printf "Solving system for interpolation.\n%!" in
    let solution =  M.solveSystem sys in
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
