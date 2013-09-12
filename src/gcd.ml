(* Computing the greatest common divisor of two polynomials with coefficients in a finite field *)

open FiniteField
open Polynom

module Gcd =
  functor (F : FINITEFIELD) ->
struct
  open F

  module P = Polynom(F)

  type element = P.element
  type polynom = P.polynom     (* Coefficients of polynomial, sorted according to ascending powers of x. *)


  (* Euclidean division of two polynomials.
     Degree of the first polynomial should be larger than that of the second.*)
  let divide (num, d_n : polynom) (denom, deg_d : polynom) =
    let lead_d = denom.(deg_d) in
    if deg_d = 0    (* Denominator is a constant *)
    then
      begin
        let rem = [| zero |] in
        if lead_d =: one
        then (num, d_n) , (rem, 0)
        else
          begin
               Array.iteri (fun i el -> (num.(i) <- el /: lead_d)) num;
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


  (* Get coefficient. Used in the division of a trace function by a polynomial. *)
  let get_coeff c quot (denom, deg_d : polynom) =
    let m = List.length quot in
    let rec get curr i q_list =    (* i = current position in the quotient *)
      if i = m
      then curr
      else
        begin
          match q_list with
          | q :: qs ->
            let pos = deg_d - 1 - i in
            if pos < 0
            then curr
            else
              begin
                let curr' = curr -: (q *: denom.(pos)) in
                get curr' (i + 1) qs
              end
          | [] -> failwith "Should not happen."
        end
    in
    get c 0 quot


  (* Get coefficient of the remainder. Used in the division of a trace function by a polynomial. *)
  let get_coeff_rem c quot (denom, deg_d : polynom) offset=
    let m = List.length quot in
    let rec get curr i q_list =    (* i = current position in the quotient *)
      if i = m
      then curr
      else
        begin
          match q_list with
          | q :: qs ->
            let pos = deg_d - 1 - i - offset in
            if pos < 0
            then curr
            else
              begin
                let curr' = curr -: (q *: denom.(pos)) in
                get curr' (i + 1) qs
              end
          | [] -> failwith "Should not happen."
        end
    in
    get c 0 quot


  (* Verify whether a number is a power of 2 *)
  let pow_of_two n highest_pow =
    let rec loop pow =
      if n == (1 lsl pow)
      then pow
      else 
        if n > (1 lsl pow)
        then -1
        else loop (pow - 1)
    in
    loop highest_pow


  (* Euclidean division of a trace function by a polynomial.
     Degree of the polynomial will be at least one *)
  let divide_tr_num trace (denom, deg_d as d: polynom) =
    let deg_tr = 1 lsl (w - 1) in
    let lead_d = denom.(deg_d) in
    let deg_quot = deg_tr - deg_d in
    let rec loop_q i quot =
      if i > deg_quot
      then quot
      else
        begin
          let ind_tr = deg_tr - i in
          let c =
            let pow = pow_of_two ind_tr (w - 1) in
            if pow != -1
            then trace.(pow)
            else zero
          in
          let coeff = get_coeff c quot d in
          let q = coeff /: lead_d in
          let quot' = q :: quot in
          loop_q (i + 1) quot'
        end
    in
    let quot = loop_q 0 [] in
    let rec loop_r i rem =
      if i < 0
      then rem
      else
        begin
          let c =
            let pow = pow_of_two i (w - 1) in
            if pow != -1
            then trace.(pow)
            else zero
          in
          let offset = deg_d - 1 - i in
          let coeff = get_coeff_rem c quot d offset in
          let rem' = coeff :: rem in
          loop_r (i - 1) rem'
        end
    in

    let rem = Array.of_list (loop_r (deg_d - 1) []) in
    P.proper_degree (rem, deg_d - 1)


  (* Euclidean division of a polynomial by a trace function.*)
  let divide_tr_denom (num, d_n : polynom) trace =
    let lead_d = trace.(w - 1) in
    let deg_d = 1 lsl (w - 1) in
    let num_full = Array.copy num, d_n in
        let rec loop quot =
          let (num', deg_n) as num_full = P.proper_degree num_full in
          if deg_n < deg_d
          then num_full
          else
            begin
              let diff = deg_n - deg_d in
              let q = num'.(deg_n) /: lead_d in
              for i = diff to deg_n do
                let pos = i - diff in
                let pow = pow_of_two pos (w - 1) in
                if pow == -1
                then ()
                else  num'.(i) <- num'.(i) -: ( q *: trace.(pow) )
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



  (* Find the unique monic gcd of a polynomial and the trace-function.
     The polynomial will never be a constant*)
  let gcd_with_trace ((pol, deg) as p : polynom) trace =
    if deg = 0
    then failwith "Constant pol. (should not happen)"
    else
      begin
        (* Handle the trace function.
        Takes two steps when the polynomial is of higher degree than the trace.*)
        let deg_tr = 1 lsl (w - 1) in
        let p1_init, p2_init =
          if deg_tr >= deg
          then
            begin
              let rem = divide_tr_num trace p in
              p, rem
            end
          else
            begin
              let rem = divide_tr_denom p trace in
              let b, _ = P.is_zero rem in
              if b
              then p, rem
              else
                begin
                  let rem2 = divide_tr_num trace rem in
                  rem, rem2
                end
            end
        in

        (* Continue the divisions, all polynomials are regular (not trace functions) *)
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

        let result = aux p1_init p2_init in
        make_monic result ;
        result
      end


end
