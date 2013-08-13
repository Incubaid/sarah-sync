(* Finite field interface *)

module type FINITEFIELD = 
  sig
    type t
    val w : int
    val plus : t -> t -> t
    val min : t -> t -> t
    val mult : t -> t -> t
    val div : t -> t -> t
    val exp : t -> int -> t   (* Exponentiation *)
    val primEl : t
    val zero : t
    val one : t
    val print : t -> unit
    val eq : t -> t -> bool
  end
