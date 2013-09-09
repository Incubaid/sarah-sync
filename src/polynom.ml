(* Basic operations on polynomials *)

open FiniteField

module Polynom =
  functor  (F : FINITEFIELD) ->
struct
  open F

  type element = t
  type polynom = element array (* Coeffs, ordered according to increasing powers of x *)

  (* Verify whether a polynom is zero. Also returns the degree. *)
  let is_zero ?start (pol : polynom)  =
    let m = Array.length pol in
    let rec loop i =
      if i = 0
      then pol.(i) =: zero , i
      else
        begin
          if pol.(i) =: zero
          then loop (i - 1)
          else false, i
        end
    in
    match start with
    | Some s -> loop s
    | None -> loop (m - 1)


  (* Degree of a polynomial. Highest coefficient different from zero *)
  let get_degree (pol : polynom) =
    let _, d = is_zero pol in
    d


  (* Evaluate a polynom in a given point, using Horner's rule *)
  let evaluate_pol (pol : polynom) (pnt : element) =
    if pnt =: zero
    then zero
    else 
      begin
        let rec loop i acc =
          if i = -1
          then acc
          else
            begin
              let acc' = (acc *: pnt) +: pol.(i) in
              loop (i - 1) acc'
            end
        in
        loop (get_degree pol) zero
      end

end
