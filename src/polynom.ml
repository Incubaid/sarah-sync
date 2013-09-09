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


(* ========================================================== *)
(* Verify equality of polynomials *)
  let equal_pols (pol1 : polynom) (pol2 : polynom) =
    let d1 = get_degree pol1 in
    let d2 = get_degree pol2 in
    let b = (d1 = d2) in
    let rec loop c i =
      if c && i <= d1
      then
        begin
          let c' = c && (pol1.(i) =: pol2.(i)) in
          loop c' (i + 1)
        end
      else c
    in
    loop b 0


  (* Verify whether polynomial is linear *)
  let is_linear (pol : polynom) =
    get_degree pol = 1


  (* Get the root from a monic linear polynomial *)
  let get_root (pol : polynom) =
    let leading = pol.(1) in
    if leading =: one
    then pol.(0)
    else pol.(0) /: pol.(1)


  (* Verify whether a polynomial has a zero root *)
  let has_zero_root (pol : polynom) =
    Array.length pol <> 0 && pol.(0) =: zero


  (* Extract zero root from the polynomial, i.e. divide the polynomial by x *)
  let divide_by_x (pol : polynom) =
    let d = get_degree pol in
    Array.sub pol 1 d

 (* Verify whether polynomial is of the form ax^2 + bx + c *)
  let is_quadratic pol =
    get_degree pol == 2


  (* Find roots of quadratic equation ax^2 + bx + c = 0 *)
  let quadratic pol =
    let a = pol.(2) in
    let b = pol.(1) in
    let c = pol.(0) in
    if b =: zero 
    then 
      begin    (* Double root sqrt(c/a) *) 
        let rec loop i acc =
          if i = w
          then acc
          else
            begin
              let acc' = square acc in
              loop (i + 1) acc'
            end
        in
        let root = loop 1 (c /: a) in
        [root; root]
      end
    else
      begin
        let delta = (a *: c) /: (b *: b) in
        if trace delta =: zero     (* Two different roots *)
        then
          begin
            let sol_1 =            (* s = k.delta^2 + (k+k^2).delta^4 +...+ (k+k^2+...+k^(2^(w-2))).delta^(2^(w-1)) *)
              let rec loop k_sum last_k pow acc i =
                if i = w
                then acc
                else
                  begin
                    let acc' = acc +: (k_sum *: pow) in
                    let pow' = square pow in
                    let last_k' = square last_k in
                    let k_sum' = k_sum +: last_k' in
                    loop k_sum' last_k' pow' acc' (i + 1)
                  end
              in
              loop F.k F.k (square delta) zero 1
            in
            let sol_2 = sol_1 +: one in
            let root_1 = (b /: a) *: sol_1 in
            let root_2 = (b /: a) *: sol_2 in
            [root_1 ; root_2]
          end
        else []
      end


end
