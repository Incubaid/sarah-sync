(* Constructing set of elements in a finite field *)

open FiniteField

module Set_constructor =
  functor (F : FINITEFIELD) ->
struct

  (* Mapping to F_q met q = 2^w*)
  let map_to_field hash =
    let maxbits = F.w - 1 in
    let length = String.length (Printf.sprintf "%X" (1 lsl maxbits)) in
    let enough = String.sub hash (String.length hash - length) length in  (* e.g. 2^31 = 0x80000000, so 8 hex. digits of the hash will suffice *)
    let number = int_of_string ("0x" ^ enough) in
    let rec bin bin_rep current =
      if current = 0
      then bin_rep
      else
        let digit = string_of_int (current land 1) in
        let bin_rep' = digit ^ bin_rep in
        let current' = current lsr 1 in
        bin bin_rep' current'
    in
    if number <= (1 lsl maxbits)
    then F.wrap number
    else
      begin
        let binary = bin "" number in
        let element = "0b" ^ (String.sub binary (String.length binary - maxbits) maxbits) in
        F.wrap (int_of_string element)
      end

  (* Removing duplicate elements from a list *)
  let remove_duplicates l =
    let module S = Set.Make(F) in
    let set_of_list =
      List.fold_left
        (fun set el -> S.add el set)
        S.empty l
    in
    S.elements set_of_list

  (* Construct a set from a list of strings (hashes) *)
  let construct hashes =
    let set = List.map map_to_field hashes in
    remove_duplicates set


  (* Extend the info (hash, begin, size, file) with a mapping of the hash to the field *)
  let construct_full_info info =
    let set = ref [] in
    let extension (hash, start, size, file) =
      let el = map_to_field hash in
      set := el :: !set ;
      (el, hash, start, size, file)
    in
    let extended_info = List.map extension info in
    remove_duplicates !set, extended_info


end
