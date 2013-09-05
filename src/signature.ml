(* Creating and accessing the signature of a file / set of files *)

open Lwt
open Camltc
open Database_interaction


(* Creation of the database *)
let get_new_name () = Filename.temp_file "signature" ".db"

let init_database () =
  let name = get_new_name () in
  Hotc.create name []


(* Interaction *)
let add_to_database (hash, hash_2, begin_pos, size, file) ~db =
  Database_interaction.add hash hash_2 begin_pos size file db

let get_location hash hash_2 db =
  Lwt.catch
    (fun () ->
      Database_interaction.get_location hash hash_2 db >>= fun loc ->
      Lwt.return (Some loc)
    )
    (function
    | e -> Lwt.return None
    )

let all_keys db =
  Database_interaction.keys db


(* Create signature and return the list of keys *)
let commit_info info_list db =
  Lwt_list.iter_s (add_to_database ~db) info_list >>= fun () ->
  all_keys db
