(* Interaction with database *)

open Lwt
open Camltc


let len_pos = 16
let len_hash = 32   (* Length of the hexadecimal representation of md5 *)


(* Construct the value that will be put in the database. "hash_2begin_posssizefile\n" *)
let construct_value hash_2 begin_pos size file =
  Printf.sprintf "%32s%016x%016x%S\n" hash_2 begin_pos size file


(* 'Decode' the information in the database *)
let decode value =
  Scanf.sscanf value "%32s%016x%016x%S" 
    (fun h_2 p s f -> h_2, (p, s, f))


(* Values of key as a list *)
let list_of_vals vals =
  let m = String.length vals in
  let rec aux pos =
    if pos == m
    then []
    else
      begin
        let pos' = String.index_from vals pos '\n' + 1 in
        let value = String.sub vals pos (pos' - pos - 1) in
        let info = decode value in
        info :: (aux pos')
      end
  in
  aux 0


(* Find keys pointing to a valid location and remove those that do not *)
let get_proper_keys ks db =
  let tot = Array.length ks in
  let rec loop acc i =
    if i = tot
    then acc
    else
      begin
        let key = ks.(i) in
        let vals = Bdb.get db key in
        let m = String.length vals in
        let rec aux pos =
          if pos == m
          then pos
          else
            begin
              let start_file = pos + (len_hash + 2 * len_pos) in
              let pos' = String.index_from vals pos '\n' + 1 in
              let len = pos' - start_file - 1 in
              let f = Scanf.sscanf (String.sub vals start_file len) "%S" (fun x -> x) in
              if Sys.file_exists f
              then pos
              else aux pos'
            end
        in
        let good_pos = aux 0 in
        let acc' =
          if good_pos == m
          then
            begin
              Bdb.out db key ;                                             (* Remove key from the database *)
              acc
            end
          else
            begin
              Bdb.put db key (String.sub vals good_pos (m - good_pos)) ;   (* Remove non-existing references *)
              key :: acc
            end
        in
        loop acc' (i + 1)
      end
  in
  loop [] 0


(* Get list of all the keys in the database *)
let keys db =
  Hotc.transaction db
    (
      fun db ->
        let ks = Bdb.range db None true None true (-1) in
        let proper = get_proper_keys ks db in
        Lwt.return proper
    )


(* Find the number of bindings of a given key in association list *)
let count_bindings a_list key = 
  let rec loop l c = 
    match l with 
    | [] -> c
    | (k,v) :: xs ->
      let c' =
        if k = key
        then c + 1
        else c
      in
      loop xs c'
  in
  loop a_list 0


(* Add extra information to database, if required. *)
let extra_loc key hash_2 begin_pos size file db =
  let value = construct_value hash_2 begin_pos size file in
  let curr_v = Bdb.get db key in
  let vals = list_of_vals curr_v in
  let c = count_bindings vals hash_2 in
  if c < 2
  then
    begin
      let new_v = curr_v ^ value in
      Bdb.put db key new_v
    end


(* Add information to the database.
   Key : hash.
   Value : "begin_possizefile\n" *)
let add hash hash_2 begin_pos size file db =
  Hotc.transaction db
    (
      fun db ->
        Lwt.catch
          (fun () ->
            Lwt.return (extra_loc hash hash_2 begin_pos size file db)
          )
          (function
          | e ->   (* Key not present yet *)
            let value = construct_value hash_2 begin_pos size file in
            let () = Bdb.put db hash value in
            Lwt.return ()
          )
    )


(* Get the first valid location corresponding to a given key and having the correct value of the second hash. 
   (begin_pos, end_pos, file) *)
let get_location key hash_2 db =
  Hotc.transaction db
    (
      fun db ->
        let vals = Bdb.get db key in
        let list = list_of_vals vals in
        let res = List.assoc hash_2 list in
        Lwt.return res                      (* Return the first location *)
    )
