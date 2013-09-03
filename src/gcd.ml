(* Computing the greatest common divisor of two polynomials with coefficients in a finite field *)

open FiniteField
open Polynom

module Gcd =
  functor (F : FINITEFIELD) ->
struct
  open F

  type element = t
  type polynom = element array (* Coefficients of polynomial, sorted according to ascending powers of x. *)

  module P = Polynom(F)


  (* Euclidean division of two polynomials.
     Degree of the first polynomial should be larger than that of the second.*)
  let divide (num : polynom) (denom : polynom) =
    let deg_d = P.get_degree denom in
    let lead_d = denom.(deg_d) in
    if deg_d = 0    (* Denominator is a constant *)
    then
      begin
        let c = denom.(0) in
        let rem = [| zero |] in
        if c =: one
        then num , rem
        else
          begin
            Array.iteri (fun i el -> (num.(i) <- el /: c)) num;
            num, rem
          end
      end
    else
      begin
        let num' = Array.copy num in
        let rec loop quot =
          let deg_n = P.get_degree num' in
          if deg_n < deg_d
          then (Array.of_list quot), num'
          else
            begin
              let diff = deg_n - deg_d in
              let q = num'.(deg_n) /: lead_d in
              for i = diff to deg_n do
                num'.(i) <- num'.(i) -: ( q *: denom.(i - diff) )
              done;

              let quot' =
                let new_deg = P.get_degree num' in
                let extra_zrs = 
                  if new_deg < deg_d   (* Division done *)
                  then 0 
                  else deg_n - (P.get_degree num') - 1 in
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
  let make_monic (pol : polynom) =
    let leading = pol.(P.get_degree pol) in
    if leading =: one
    then ()
    else 
        Array.iteri
          (fun i el ->
            pol.(i) <- el /: leading
          )pol


  (* Find the unique monic gcd of two polynomials *)
  let gcd (pol_a : polynom) (pol_b : polynom) =
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
    let deg_a = P.get_degree pol_a in
    let deg_b = P.get_degree pol_b in
    let result =
      if deg_a < deg_b
      then aux pol_b pol_a
      else aux pol_a pol_b
    in
    make_monic result ;
    result


end
