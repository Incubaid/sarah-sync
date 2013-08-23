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
  val compare : t -> t -> int
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
  let rec exp a i =
    if i = 0
    then one
    else 
      begin
        let square x = mult x x in
        if i land 1 = 0  (* Even *)
        then square (exp a (i/2))
        else mult (square (exp a (i/2))) a
      end
  let primEl = 2
  let print = print_int
  let eq = (=)
  let wrap el = el
  let w = P.w
  let compare = compare
  let q = 1 lsl P.w

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
