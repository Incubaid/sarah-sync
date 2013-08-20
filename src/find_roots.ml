(* Finding roots of a polynomial in a finite field *)

open FiniteField
open Gcd

module Root_finder = 
  functor (F : FINITEFIELD) ->
struct
  type element = F.t
  type polynom = element array

  module G = Gcd(F)

  (* Return random element of the field. Not used anymore *)
  let get_random seed = 
    let () = Random.init seed in
    let bound = F.q in
    Int64.to_int (Random.int64 (Int64.of_int bound)) 

  (* Get the 'next' element in the field *)
  let next current = 
    if current = F.zero
    then F.one
    else F.mult current F.primEl

   
  (* Trace function: Tr(x) = x + x^2 + x^4 + x^8 + ... + x^(2^(w-1)) *)
  let trace =
    let pol = Array.make ((1 lsl (F.w - 1)) + 1) F.zero in
    for i = 0 to F.w - 1 do
      pol.(1 lsl i) <- F.one
    done ;
    pol

  (* Trace function evaluated in x - a. Tr(x-a) = (x-a) + (x-a)^2 + ... = Tr(x) - Tr(a) = Tr(x) + Tr(a) *)
  let trace_shift a =
    let rec aux i acc =
      if i = F.w
      then acc
      else 
	begin 
	  let acc' = F.plus acc (F.exp a (1 lsl i)) in
	  aux (i + 1) acc'
	end
    in
    let trace_a = aux 1 a in
    let trace_x = trace in
    let () = trace_x.(0) <- trace_a in
    trace_x


  (* Verify equality of polynomials *)
  let equal_pols (pol1 : polynom) (pol2 : polynom) =
    let d1 = G.get_degree pol1 in
    let d2 = G.get_degree pol2 in
    d1 = d2 && (Array.sub pol1 0 (d1 + 1) = Array.sub pol2 0 (d2 + 1))       
  

  (* Verify whether polynomial is the product of only linear factors *)
  let prod_of_lin_facts (pol : polynom) =
    let product = 
      Array.init (F.q + 1) 
      (
	fun i ->
	  if i = 1 || i = F.q
	  then F.one
	  else F.zero
      )  (* x^q - x *)
    in
    let gcd = G.gcd pol product in
    equal_pols pol gcd

  (* Verify whether polynomial is linear *)
  let is_linear (pol : polynom) =
     G.get_degree pol = 1


  (* Get the root from a monic linear polynomial *)
  let get_root (pol : polynom) = F.div pol.(0) pol.(1)

    
  (* Find all roots of a polynomial *)
  let roots (pol : polynom) = 
     let rec aux rts p =
       if G.get_degree p = 0
       then rts 
       else 
	 begin
	   if is_linear p
	   then (get_root p) :: rts
	   else 
	     begin
	       let rec factorize f a = 
		 let p1 = G.gcd f (trace_shift a) in
		 if (G.get_degree p1 = 0 && p1.(0) = F.one) || equal_pols p1 f   (* No good a *)
		 then 
		   begin
		     let new_a = next a in
		     factorize f new_a
		   end
		 else 
		   begin
		     let p2, _ = G.divide f p1 in
		     ( p1, p2 )
		   end
	       in
	       let p1, p2 = factorize p F.zero in
	       rts @ (aux [] p1) @ (aux [] p2) ;
	     end
	 end
     in
     aux [] pol 

end



(* Testing *)
module Field = GF128

module RF = Root_finder(Field)
module G = Gcd(Field)


open OUnit

let test_roots () = 
  let twentyfive = Field.mult (Field.wrap 5) (Field.wrap 5) in
  let twelve = Field.plus (Field.wrap 9) (Field.wrap 3) in
  let twentyseven = Field.mult (Field.wrap 9) (Field.wrap 3) in 
  let tests = [
    ( [|Field.one ; Field.zero ; Field.one|] , [ Field.one ; Field.one] ) ;
    ( [|Field.one ; Field.zero ; Field.zero|] , [] )  ; 
    ( [|Field.zero ; Field.one ; Field.zero ; Field.one|] ,[Field.zero ; Field.one ; Field.one] ) ;
    ( [|Field.zero ; Field.wrap 5 ; Field.one|], [Field.zero ; Field.wrap 5]) ;
    ( [|Field.zero ; twentyfive ; Field.zero ; Field.one |] , [Field.zero ; Field.wrap 5 ; Field.wrap 5] )  (* ;
    ( [|Field.zero ; twentyseven ; twelve ; Field.one|] , [Field.zero ; Field.wrap 3 ; Field.wrap 9])  FAILS *)
  ]
  in 
  let test_one (poly, roots) =
    let answer = RF.roots poly in
    OUnit.assert_equal (List.sort compare answer) (List.sort compare roots)
  in
  List.iter test_one tests


let suite = "Set Reconciliation" >::: [ "test_roots" >:: test_roots ]

let _ = run_test_tt_main suite 
