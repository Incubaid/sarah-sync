(* Finding roots of a polynomial in a finite field *)

open FiniteField
open Gcd

module Root_finder =
  functor (F : FINITEFIELD) ->
struct
  open F

  type element = t
  type polynom = element array

  module G = Gcd(F)


  (* Get the 'next' element from the basis (1, alpha, alpha^2, ..., alpha^(w-1))  *)
  let next_basis_el current =
    if current =: exp primEl (w - 1)
    then one
    else current *:  primEl


  (* Trace function evaluated in b.x . Tr(b.x) = (b.x) + (bx)^2 + ... (bx)^(2^(w-1)) *)
  let trace_shift b =
    let pol = Array.make ((1 lsl (w - 1)) + 1) zero in
    for i = 0 to w - 1 do
      pol.(1 lsl i) <- exp b (1 lsl i)
    done ;
    pol


  (* Verify equality of polynomials *)
  let equal_pols (pol1 : polynom) (pol2 : polynom) =
    let d1 = G.P.get_degree pol1 in
    let d2 = G.P.get_degree pol2 in
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
    G.P.get_degree pol = 1


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
    let d = G.P.get_degree pol in
    Array.sub pol 1 d


  (* Find all roots of a polynomial, using Tr(b.x). *)
  let roots (pol : polynom) =
    let rec aux rts p =
      Printf.printf "Polynomial has degree %i.\n%!" (G.P.get_degree p) ;
      if G.P.get_degree p = 0
      then rts
      else
        begin
          if is_linear p
          then (get_root p) :: rts
          else
            begin
              if has_zero_root p
              then
                begin
                  let p' = divide_by_x p in
                  let rts' = zero :: rts in
                  rts' @ (aux [] p')
                end
              else
                begin
                  let rec factorize f b =
                    Printf.printf "Computing gcd.\n%!" ;
                    let p1 = G.gcd f (trace_shift b) in
                    Printf.printf "Computing gcd done.\n%!" ;
                    if (G.P.get_degree p1 = 0 && p1.(0) =: one) || equal_pols p1 f   (* No good b *)
                    then
                      begin
                        let new_b = next_basis_el b  in
                        factorize f new_b
                      end
                    else
                      begin
                        let p2, _ = G.divide f p1 in
                        ( p1, p2 )
                      end
                  in
                  let p1, p2 = factorize p one in
                  rts @ (aux [] p1) @ (aux [] p2) ;
                end
            end
        end
    in
    aux [] pol


end
