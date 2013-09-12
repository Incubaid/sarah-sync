(* Finite fields of characteristic 2 *)

open Camljerasure

module type FIELD_PARAM = sig
  val w : int
end

module type F = sig
  type t
  val zero : t
  val one : t
  val (+:) : t -> t -> t
  val (-:) : t -> t -> t
  val ( *: ) : t -> t -> t
  val (/:) : t -> t-> t
  val square : t -> t
  val exp : t -> int -> t   (* Exponentiation *)
  val trace : t -> t
  val primEl : t
  val k : t                 (* Element with trace(k) = one *)
  val print : t-> unit
  val (=:) : t-> t -> bool
  val wrap : int -> t
  val unwrap : t -> int
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
  let w = P.w
  let (=:) = (==) (* possible for ints *)
  let (+:) = (lxor)
  let (-:) = (lxor)
  let ( *: ) a b =
    if a =: zero || b =: zero
    then zero
    else Galois.single_multiply a b P.w
  let (/:) a b =
    if a =: zero
    then zero
    else Galois.single_divide a b P.w
  let square x = x *: x
  let rec exp a i =
    if i = 0
    then one
    else
      begin
        if i land 1 = 0  (* Even *)
        then exp (square a) (i/2)
        else (exp (square a) (i/2)) *: a
      end
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
  let primEl = 2
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
  let print = print_int
  let wrap el = el
  let unwrap el = el
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
