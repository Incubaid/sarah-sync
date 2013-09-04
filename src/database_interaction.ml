(* Interaction with database *)

open Lwt
open Camltc


(* Converting integer to fixed-length string of length 64. *)
let convert_int n = 
  let s = string_of_int n in
  let extra = String.make (64 - String.length s) '*' in
  extra ^ s


(* Get list of all the keys in the database *)
let keys db =
  Hotc.transaction db
    (
      fun db ->
      let keys = Bdb.range db None true None true (-1) in
      Lwt.return (Array.to_list keys)
    )


(* Construct the value that will be put in the database. "begin_posssizefile" *)
let construct_value begin_pos size file =
  let begin_pos' = convert_int begin_pos in
  let size' = convert_int size in
  String.concat "" [ begin_pos' ; size' ; file ]


(* Add information to the database.
   Key : hash.
   Value : "begin_possizefile" *)
let add hash begin_pos size file db =
  let value = construct_value begin_pos size file in
  Hotc.transaction db
    (
      fun db ->
      let () = Bdb.put db hash value in
      Lwt.return ()
    )
  

(* 'Decode' the information in the database *)
let decode value =
  let begin_pos' = String.sub value 0 64 in
  let start1 = String.rindex begin_pos' '*' + 1 in
  let begin_pos = String.sub begin_pos' start1 (String.length begin_pos' - start1) in
  let size' = String.sub value 64 64 in
  let start2 = String.rindex size' '*' + 1 in
  let size = String.sub size' start2 (String.length size' - start2) in
  let file = String.sub value 128 (String.length value - 128) in
  int_of_string begin_pos, int_of_string size, file


(* Get the value corresponding to a given key. (begin_pos, end_pos, file) *)
let get_location key db =
  Hotc.transaction db
    (
      fun db ->
      let res = Bdb.get db key in
      Lwt.return (decode res)
    )
