(* Computing the greatest common divisor of two polynomials with coefficients in a finite field *)

open FiniteField
open Polynom

module Gcd =
  functor (F : FINITEFIELD) ->
struct
  open F

  module P = Polynom(F)

  type element = P.element
  type polynom = P.polynom (* Coefficients of polynomial, sorted according to ascending powers of x. *)


  (* Euclidean division of two polynomials.
     Degree of the first polynomial should be larger than that of the second.*)
  let divide (num, d_n : polynom) (denom, deg_d : polynom) =
    let lead_d = denom.(deg_d) in
    if deg_d = 0    (* Denominator is a constant *)
    then
      begin
        let c = denom.(0) in
        let rem = [| zero |] in
        if c =: one
        then (num, d_n) , (rem, 0)
        else
          begin
            Array.iteri (fun i el -> (num.(i) <- el /: c)) num;
            (num, d_n), (rem, 0)
          end
      end
    else
      begin
        let num_full = Array.copy num, d_n in
        let rec loop quot =
          let (num', deg_n) as num_full = P.proper_degree num_full in
          if deg_n < deg_d
          then (Array.of_list quot, d_n - deg_d), num_full
          else
            begin
              let diff = deg_n - deg_d in
              let q = num'.(deg_n) /: lead_d in
              for i = diff to deg_n do
                num'.(i) <- num'.(i) -: ( q *: denom.(i - diff) )
              done;

              let quot' =
                let (_, new_deg) as num_full = P.proper_degree num_full in
                let extra_zrs =
                  if new_deg < deg_d   (* Division done *)
                  then 0
                  else deg_n - new_deg - 1 in
                let rec extra i res =
                  if i = 0
                  then res
                  else extra (i - 1) (zero :: res)
                in
                extra extra_zrs (q :: quot)
              in

              loop quot'
            end
        in
        loop []
      end


  (* Make a polynom monic, by dividing out the leading coefficient *)
  let make_monic (pol, d : polynom) =
    let leading = pol.(d) in
    if leading =: one
    then ()
    else
        Array.iteri
          (fun i el ->
            pol.(i) <- el /: leading
          ) pol


  (* Find the unique monic gcd of two polynomials *)
  let gcd ((pol_a, deg_a) as p_a : polynom) ((pol_b, deg_b) as p_b : polynom) =
    let rec aux p1 p2 =
      let b, _ = P.is_zero p2 in
      if b
      then p1
      else
        begin
          let _, rest = divide p1 p2 in
          aux p2 rest
        end
    in
    let result =
      if deg_a < deg_b
      then aux p_b p_a
      else aux p_a p_b
    in
    make_monic result ;
    result

end
