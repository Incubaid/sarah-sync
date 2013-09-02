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
  val primEl : t
  val print : t-> unit
  val (=:) : t-> t -> bool
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
  let (+:) = (lxor)
  let (-:) = (lxor)
  let ( *: ) a b = Galois.single_multiply a b P.w
  let (/:) a b = Galois.single_divide a b P.w
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
 (* let exp a i =
    let rec loop acc el j =
      if j = 0
      then acc
      else
        begin
          let sq = mult el el in
          let j' = j lsr 1 in
          let acc' =
            if j land 1 = 0 (* Even *)
            then acc
            else mult acc el
          in
          loop acc' sq j'
        end
    in
    loop one a i *)
  let primEl = 2
  let print = print_int
  let (=:) = (==) (* possible for ints *)
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
