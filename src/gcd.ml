(* Computing the greatest common divisor of two polynomials with coefficients in a finite field *)

open FiniteField

module Gcd =
  functor (F : FINITEFIELD) ->
struct
  type element = F.t
  type polynom = element array (* Coefficients of polynomial, sorted according to ascending powers of x. *)


  (* Extra function *)
  let is_zero (pol : polynom) =
    List.for_all ((=) F.zero) (Array.to_list pol)

  (* Degree of a polynomial. Highest coefficient different from zero *)
  let get_degree (pol : polynom) =
    let rec aux i =
      if (pol.(i) <> F.zero || i = 0)
      then i
      else aux (i-1)
    in
    if is_zero pol
    then 0
    else aux (Array.length pol - 1)


  (* Euclidean division of two polynomials.
     Degree of the first polynomial should be larger than or equal to that of the second. *)
  let divide (pol_a : polynom) (pol_b : polynom) =
    let degree_a = get_degree pol_a in
    let degree_b = get_degree pol_b in
    let remainder = Array.copy pol_a in
    let quotient = Array.make (degree_a - degree_b + 1) F.zero in
    let count_a = ref degree_a in
    while !count_a >= degree_b do
      let quot = F.div remainder.(!count_a) pol_b.(degree_b) in
      let adapt_remainder i el =
        if ( i < !count_a - degree_b || i > !count_a - 1 )
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
    ( quotient, Array.sub remainder 0 degree_b )      (* Return the remainder *)


  (* Make a polynom monic, by dividing out the leading coefficient *)
  let make_monic (pol : polynom) =
    let leading = pol.(get_degree pol) in
    Array.map (fun el -> F.div el leading) pol


  (* Find the unique monic gcd of two polynomials *)
  let gcd (pol_a : polynom) (pol_b : polynom) =
    let rec aux p1 p2 =
      if is_zero p2
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
    make_monic result


end


(* Testing *)
(*
  module Field = struct
  type t = float
  let zero = 0.
  let one = 1.
  let plus =(+.)
  let min = (-.)
  let mult = ( *. )
  let div = (/.)
  let exp a i = a ** (float_of_int i)
  let primEl = 1.
  let print = print_float
  let eq = (=)
  let wrap = float_of_int
  let w = 0
  let q = 0
  end *)

module Field = GF128

module GC = Gcd(Field)

(*let pol1 =
  let pol1' = [|27 ; 10; 1|] in
  Array.map Field.wrap pol1'


  let pol2 =
  let pol2' = [|9 ; 1|] in
  Array.map Field.wrap pol2'  ;;

  let quot,r = GC.divide pol1 pol2 in
  let g = GC.gcd pol1 pol2 in
  Array.iter (fun el -> (Field.print el ; print_string " ")) quot ;
  print_newline () ;
  Array.iter (fun el -> (Field.print el ; print_string " ")) r ;
  print_newline () ;
  Array.iter (fun el -> (Field.print el ; print_string " ")) g ;
  print_newline () ; *)
