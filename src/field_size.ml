(* Determine the required size of the finite field, based on the sizes of the two sets and the number of extra evaluation points. *)

let get_size s_1 s_2 k = 
  let required = 2 * (s_1 + s_2) + k in
  let rec find i =
    if (1 lsl i) > required
    then i
    else find (i + 1)
  in
  find 0
