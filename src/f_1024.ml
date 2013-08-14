(* Finite field with q = 2^10 *)

open Camljerasure

module TestFiniteField = 
struct
  type t = int
  let w = 10
  let plus = (lxor)
  let min = (lxor)
  let mult a b = Galois.single_multiply a b w
  let div a b =  Galois.single_divide a b w
  let primEl = 2
  let one = 1
  let zero = 0
  let exp a i = 
    if i = 0
    then one
    else 
      begin
	let allAs = Array.make i a in
	Array.fold_left mult one allAs
      end 
  let print = print_int
  let eq = (=)
  let wrap el = el
end