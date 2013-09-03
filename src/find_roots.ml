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

  (* The largest basis element *)
  let largest = exp primEl (w - 1)

  (* Get the 'next' element from the basis (1, alpha, alpha^2, ..., alpha^(w-1)) *)
  let next_basis_el current =
    if current =: largest
    then one
    else current *:  primEl


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


  (* Trace function Tr (x) *)
  let init_trace () =
    let tr = Array.make ((1 lsl (w - 1)) + 1) zero in
    for i = 0 to w - 1 do
      tr.(1 lsl i) <- one
    done ;
    tr


  (* Update trace function to be evaluated in the next basis element.
     Returns this basis element, for future reference.*)
  let update_trace trace b =
    let b' = next_basis_el b in
    if b' = one
    then
      begin
        for i = 0 to w - 1 do
          trace.(1 lsl i) <- one
        done;
        b'
      end
    else
      begin
        let rec loop i pow =
          if i = w
          then ()
          else
            begin
              let pow' = square pow in
              let pos = 1 lsl i in
              trace.(pos) <- trace.(pos) *: pow ;
              loop (i + 1) pow'
            end
        in
        loop 0 primEl ;
        b'
      end


  (* Calculate Tr(el) *)
  let trace el =
    let rec loop acc i prev =
      if i = w
      then acc
      else
        begin
          let prev' = square prev in
          let acc' = acc +: prev' in
          loop acc' (i + 1) prev'
        end
    in
    loop el 1 el


  (* Find element k with Tr(k) = one *)
  let k =
    let rec loop curr =
      if trace curr =: one
      then curr
      else
        begin
          let curr' = curr *: primEl in
          loop curr'
        end
    in
    loop one


  (* Verify whether polynomial is of the form ax^2 + bx + c, with b not zero *)
  let is_quadratic pol =
    G.P.get_degree pol == 2 && pol.(1) != zero


  (* Find roots of quadratic equation ax^2 + bx + c = 0 *)
  let quadratic pol =
    let a = pol.(2) in
    let b = pol.(1) in
    let c = pol.(0) in
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
          loop k k (square delta) zero 1
        in
        let sol_2 = sol_1 +: one in
        let root_1 = (b /: a) *: sol_1 in
        let root_2 = (b /: a) *: sol_2 in
        [root_1 ; root_2]
      end
    else []


  (* Find all roots of a polynomial, using Tr(b.x). *)
  let roots (pol : polynom) =
    let rec aux rts p =
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
                if is_quadratic p
                then
                  begin
                    let roots = quadratic p in
                    roots @ rts
                  end
                else
                  begin
                    let trace = init_trace () in (* Tr(x) *)
                    let rec factorize f b =
                      Printf.printf "Computing gcd.\n%!" ;
                      let p1 = G.gcd f trace in
                      Printf.printf "Computing gcd done.\n%!" ;
                      if G.P.get_degree p1 = 0 || equal_pols p1 f   (* No good b *)
                      then
                        begin
                          if b =: largest      (* No b found *)
                          then ([|zero|] , [|zero|])
                          else 
                            begin
                              let b' = update_trace trace b  in
                              factorize f b'
                            end
                        end
                      else
                        begin
                          let p2, _ = G.divide f p1 in
                          ( p1, p2 )
                        end
                    in
                    let p1, p2 = factorize p one in
                    rts @ (aux [] p1) @ (aux [] p2)
                  end
            end
        end
    in
    aux [] pol


end
