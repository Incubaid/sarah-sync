(* Module to ensure a polynomial is square-free *)

open FiniteField
open Gcd

module Square_Free = 
  functor (F : FINITEFIELD) ->
struct
  type element = F.t
  type polynom = element array

  module G = Gcd(F)

  (* Derivative of a polynom *)
  let derivative (pol : polynom) =
    Array.init (Array.length pol - 1) 
      (
	fun i ->
	F.mult (F.wrap (i+1)) pol.(i + 1)
      )

  (* Verify whether a polynomial is square-free. 
     This is the case when the gcd of the polynom and its derivative is 1.*)
  let is_square_free (pol : polynom) =
      let pol' = derivative pol in
      let g = G.gcd pol pol' in
      (G.get_degree g = 0) && (g.(0) = F.one)
    	
  (* Make polynomial square-free *)
  let square_free (pol : polynom) = 
    let aux f =
      let f' = derivative f in
      let g = G.gcd f f' in
      if (G.get_degree g = 0) && (g.(0) = F.one)
      then f
      else 
	begin
	  let q,r = G.divide f g in
	  q
	end
    in
    aux pol


end

(* Testing *)

(*module Field = GF128*)

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
end 

module SF = Square_Free(Field);;



(* let pol1 = [|Field.wrap 9 ; Field.wrap 24 ; Field.wrap 22 ; Field.wrap 8 ; Field.one|] in (* Dubbele wortel 1 en 3 *) *)
let pol1 = [|Field.wrap 1 ; Field.wrap 2 ; Field.wrap 1 |] in (* Dubbele wortel 1 *)
let d = SF.derivative pol1 in
let pol1' = SF.square_free pol1 in
Array.iter (fun el -> (Field.print el ; print_string " ")) pol1';
print_newline ();
Array.iter (fun el -> (Field.print el ; print_string " ")) d;
print_newline ()

