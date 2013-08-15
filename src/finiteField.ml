(* Finite fields of characteristic 2 *)

open Camljerasure

module type FIELD_PARAM = sig
  val w : int
end

module type F = sig
    type t
    val zero : t
    val one : t
    val plus : t -> t -> t
    val min : t -> t -> t
    val mult : t -> t -> t
    val div : t -> t-> t
    val exp : t -> int -> t   (* Exponentiation *)
    val primEl : t
    val print : t-> unit
    val eq : t-> t -> bool
    val wrap : int -> t
    val q : int
end

module type FINITEFIELD = sig
    include FIELD_PARAM
    include F
end

module Make(P:FIELD_PARAM): FINITEFIELD = struct
    type t = int
    let zero = 0
    let one = 1
    let plus = (lxor)
    let min = (lxor)
    let mult a b = Galois.single_multiply a b P.w
    let div a b = Galois.single_divide a b P.w
    let exp a i = 
      let rec loop acc j =
	if j = 0
	then acc 
	else loop (mult acc a) (j-1)
      in
      loop one i 
    let primEl = 2
    let print = print_int
    let eq = (=)
    let wrap el = el
    let w = P.w
    let q = (lsl) 1 P.w

end

(* Some finite fields *)
module GF16 = Make(struct
  let w = 4
end)

module GF128 = Make(struct
  let w = 7
end)

module GF256 = Make(struct
  let w = 8
end)

module GF512 = Make(struct
  let w = 9
end)

module GF1024 = Make(struct
  let w = 10
end)

module OurGF = Make(struct
  let w = 32
end)
