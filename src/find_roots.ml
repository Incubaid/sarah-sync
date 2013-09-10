(* Finding roots of a polynomial in a finite field *)

open FiniteField
open Gcd

module Root_finder =
  functor (F : FINITEFIELD) ->
struct
  open F

  module G = Gcd(F)

  type element = G.P.element
  type polynom = G.P.polynom

  (* The largest basis element *)
  let largest = exp primEl (w - 1)


  (* Get the 'next' element from the basis (1, alpha, alpha^2, ..., alpha^(w-1)) *)
  let next_basis_el current =
    if current =: largest
    then one
    else current *:  primEl


  (* Trace function Tr (x) *)
  let init_trace () =
    let tr = Array.make ((1 lsl (w - 1)) + 1) zero in
    for i = 0 to w - 1 do
      tr.(1 lsl i) <- one
    done ;
    (tr, 1 lsl (w-1))


  (* Update trace function to be evaluated in the next basis element.
     Returns this basis element, for future reference.*)
  let update_trace (trace, _ : polynom) b =
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


  (* Find all roots of a polynomial, using Tr(b.x). *)
  let roots (pol : polynom) =
    let rec aux rts ((_, d) as p) =
      if d = 0
      then rts
      else
        begin
          if G.P.is_linear p
          then (G.P.get_root p) :: rts
          else
            begin
              if G.P.has_zero_root p
              then
                begin
                  let p' = G.P.divide_by_x p in
                  let rts' = zero :: rts in
                  rts' @ (aux [] p')
                end
              else
                if G.P.is_quadratic p
                then
                  begin
                    let roots = G.P.quadratic p in
                    roots @ rts
                  end
                else
                  begin
                    let trace = init_trace () in (* Tr(x) *)
                    let rec factorize f b =
                      let p1 = G.gcd f trace in
                      if G.P.get_degree p1 = 0 || G.P.equal_pols p1 f   (* No good b *)
                      then
                        begin
                          if b =: largest      (* No b found *)
                          then ([|zero|], 0) , ([|zero|] , 0)
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
