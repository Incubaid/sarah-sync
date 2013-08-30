(* Computing the greatest common divisor of two polynomials with coefficients in a finite field *)

open FiniteField

module Gcd =
  functor (F : FINITEFIELD) ->
struct
  type element = F.t
  type polynom = element array (* Coefficients of polynomial, sorted according to ascending powers of x. *)


  (* Verify whether a polynom is zero. Also returns the degree. *)
  let is_zero (pol : polynom) =
    let m = Array.length pol in
    let rec loop b i =
      if b && i >= 0
      then
        begin
          let b' = b && (F.eq pol.(i) F.zero) in
          loop b' (i - 1)
        end
      else b, (i + 1) (* i + 1: last index that held a zero *)
    in
    loop true (m - 1)


  (* Degree of a polynomial. Highest coefficient different from zero *)
  let get_degree (pol : polynom) =
    let _, d = is_zero pol in
    d


  (* FIRST VERSION Euclidean division of two polynomials.
     Degree of the first polynomial should be larger than or equal to that of the second. *)
(*  let divide (pol_a : polynom) (pol_b : polynom) =
    let degree_a = get_degree pol_a in
    let degree_b = get_degree pol_b in
    let el = pol_b.(degree_b) in
    let remainder = Array.copy pol_a in
    let quotient = Array.make (degree_a - degree_b + 1) F.zero in
    let count_a = ref degree_a in
    while !count_a >= degree_b do
      let quot = F.div remainder.(!count_a) el in
      let adapt_remainder i el =
        if ( i < !count_a - degree_b || i > !count_a  - 1  )
        then ()
        else
          begin
            let result = F.min el (F.mult quot pol_b.(i - (!count_a - degree_b))) in
            remainder.(i) <- result
          end
      in
      Array.iteri adapt_remainder remainder ;
      quotient.(!count_a - degree_b) <- quot ;
      count_a := pred (!count_a)
    done ;
    ( quotient, Array.sub remainder 0 degree_b ) *)


  (* SECOND VERSION Euclidean division of two polynomials.
     Degree of the first polynomial should be larger than that of the second.*)
  let divide (num : polynom) (denom : polynom) =
    let deg_d = get_degree denom in
    let lead_d = denom.(deg_d) in
    if deg_d = 0    (* Denominator is a constant *)
    then
      begin
        let c = denom.(0) in
        let rem = [| F.zero |] in
        if F.eq c F.one
        then num , rem
        else
          begin
            Array.iteri (fun i el -> (num.(i) <- F.div el c)) num;
            num, rem
          end
      end
    else
      begin
        let num' = Array.copy num in
        let rec loop quot =
          let deg_n = get_degree num' in
          if deg_n < deg_d
          then (Array.of_list quot), num'
          else
            begin
              let diff = deg_n - deg_d in
              let q = F.div num'.(deg_n) lead_d in
              let quot' = q :: quot in
              for i = diff to deg_n do
                num'.(i) <- F.min num'.(i) ( F.mult q denom.(i - diff) )
              done;
              loop quot'
            end
        in
        loop []
      end


  (* Make a polynom monic, by dividing out the leading coefficient *)
  let make_monic (pol : polynom) =
    let leading = pol.(get_degree pol) in
    Array.iteri
      (fun i el ->
        pol.(i) <- F.div el leading
      ) pol


  (* Find the unique monic gcd of two polynomials *)
  let gcd (pol_a : polynom) (pol_b : polynom) =
    let rec aux p1 p2 =
      let b, _ = is_zero p2 in
      if b
      then p1
      else
        begin
          let _, rest = divide p1 p2 in
          aux p2 rest
        end
    in
    let deg_a = get_degree pol_a in
    let deg_b = get_degree pol_b in
    let result =
      if deg_a < deg_b
      then aux pol_b pol_a
      else aux pol_a pol_b
    in
    make_monic result ;
    result


end
