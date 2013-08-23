(* Creating and accessing the signature of a file / set of files *)

open Lwt
open Camltc
open Database_interaction
open Read_file


(* Creation of the database *)
let get_new_name () = Filename.temp_file "signature" ".db"

let database name = Lwt_main.run (Hotc.create name [])

let init_database () =
  let name = get_new_name () in
  database name
  

(* Interaction *)
let add_to_database (hash, begin_pos, size, file) ~db =
  Database_interaction.add hash begin_pos size file db

let get_location hash db =
  Database_interaction.get_location hash db 

let all_keys db =
  Database_interaction.keys db


(* Create signature and return the list of keys *)
let commit_info info_list db =
  let () = List.iter (add_to_database ~db) info_list in
  all_keys db ;;
  

(* Test *)
(*let hash = "test" in
let file = "/Test/test.ml" in
let start = 2 in
let size = 87 in
add_to_database (hash, start, size, file) ;
let (s, sz, f) = get_location hash in
let ks = all_keys () in
Printf.printf "%i, %i, %s\n%!" s sz f ;
List.iter (fun el -> Printf.printf "%s\n%!" el) ks *)
