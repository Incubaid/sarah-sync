(* Syncing of two files,  with database interaction.
   Size of the finite field is fixed beforehand. *)

open Read_file
open FiniteField
open Set_reconciliation
open Evaluation_points
open Construct_set
open Lwt
open Handle_interaction

open Sha1

module Syncing =
  functor (F : FINITEFIELD) ->
struct
  type filename = string

  module S = SetReconciliation(F)
  module EP = EvaluationPts(F)

  module SC = Set_constructor(F)


  (* Syncing *)
  let sync (client : filename) (server : filename) partition hash_function db =
    control_hash client hash_function >>= fun total_hash ->
    partition client hash_function >>= fun info_client ->
    partition server hash_function >>= fun info_server ->
    let set1, full_info_client = SC.construct_full_info info_client in
    Signature.commit_info info_server db >>= fun hashes_server ->
    let set2 = SC.construct hashes_server in
    Lwt_io.printlf "Looking for m.%!" >>= fun () ->
    let size_1 = List.length set1 in
    let size_2 = List.length set2 in
    let delta = size_1 - size_2 in
    let init_max, k = EP.get_max_vals size_1 size_2 in
    let eval_pts = EP.evalPts init_max in
    let rat_vals = S.get_rational_values set1 set2 eval_pts in
    let extra_pts = EP.extraEvalPts k in
    let actual_vals = S.get_rational_values set1 set2 extra_pts in
    let m, cfs_num, cfs_denom = EP.findM delta rat_vals actual_vals init_max eval_pts extra_pts in
    Lwt_io.printlf "Found m: %i.%!" m >>= fun () ->
    let to_send_by_1 =
      if m = 0
      then []
      else S.reconcile cfs_num cfs_denom
    in
    Lwt_io.printf "Reconciliation done. %i blocks of set1 are missing.\n%!" (List.length to_send_by_1) >>= fun () ->
    create_message to_send_by_1 full_info_client >>= fun message ->
    let complete_message = (message , total_hash) in
    Lwt.return (List.length to_send_by_1 , complete_message , info_client , hashes_server)


  exception Reconstruction_not_perfect


  (* Reconstruction *)
  let reconstruct (msg, hash) info_client hashes_server location hash_function nr_sent db =
    Lwt_io.printlf "Reconstruction.%!" >>= fun () ->
    let number = ref nr_sent in
    let current_pos = ref 0 in
    let decode m =
      match m with
      | Original orig ->  
        update_database db hash_function orig !current_pos location >>= fun new_pos ->
        current_pos := new_pos ;
        Lwt.return orig
      | Hash (hash, hash_2, i) ->
        Signature.get_location hash hash_2 db >>= fun opt ->
        match opt with
        | Some (begin_pos, size, file) -> 
          Signature.add_to_database (hash, hash_2, !current_pos, size, location) ~db >>= fun () ->
          let () = current_pos := !current_pos + size in
          get_block begin_pos size file
        | None ->
          number := succ !number ;
          let _,_, begin_pos, size, file = List.nth info_client i in
          get_block begin_pos size file >>= fun block ->
          update_database db hash_function block !current_pos location >>= fun new_pos ->
          current_pos := new_pos ;
          Lwt.return block
    in
    Lwt_list.map_s decode msg >>= fun content ->
    construct_file location content >>= fun () ->
    control_hash location hash_function >>= fun new_hash ->
    if new_hash <> hash
    then
      begin
        Lwt_io.printlf "Original hash: %s.\nObtained hash: %s.%!" hash new_hash >>= fun () ->
        raise Reconstruction_not_perfect
      end
    else Lwt_io.printlf "Reconstruction completed. %i blocks out of %i have been sent.%!" !number (List.length info_client)


  (* Sha1 hash function *)
  let sha1 l =
    Sha1.to_hex (Sha1.string l)


  (* Syncing by dividing into blocks *)
  let sync_with_blocks file1 file2 size hash_function location =
    Lwt_main.run 
      (
        Signature.init_database () >>= fun db ->
        sync file1 file2 (blocks ~size) hash_function db >>= fun (nr_sent, msg, prts1, l2) ->
        reconstruct msg prts1 l2 location hash_function nr_sent db
      )


  (* Syncing by partitioning on the words *)
  let sync_with_words file1 file2 hash_function location =
    Lwt_main.run 
      (
        Signature.init_database () >>= fun db ->
        sync file1 file2 words hash_function db >>= fun (nr_sent, msg, prts1, l2) ->
        reconstruct msg prts1 l2 location hash_function nr_sent db
      )


  (* Syncing by partitioning on whitespace *)
  let sync_with_whitespace file1 file2 size hash_function location =
    Lwt_main.run 
      (
        Signature.init_database () >>= fun db ->
        sync file1 file2 (blocks_using_whitespace ~size) hash_function db >>= fun (nr_sent, msg, prts1, l2) ->
        reconstruct msg prts1 l2 location hash_function nr_sent db
      )


  (* Syncing, by partitioning on the lines *)
  let sync_with_lines file1 file2 hash_function location =
    Lwt_main.run 
      (
        Signature.init_database () >>= fun db ->
        sync file1 file2 lines hash_function db >>= fun (nr_sent, msg, prts1, l2) ->
        reconstruct msg prts1 l2 location hash_function nr_sent db
      )

end


(* Tests. *)

module Field = FiniteField.Make(struct
  let w = 16
end)

module Sync = Syncing(Field) ;;


(* Testen voor fisher.txt *)
let outfile1 = "/home/spare/Documents/Output/test1" in
let () = Time.time (Sync.sync_with_words "/home/spare/Documents/FilesOmTeSyncen/old/fischer.txt" "/home/spare/Documents/FilesOmTeSyncen/new/fischer.txt" Sync.sha1) outfile1 in
let () = Printf.printf "========================================\n%!" in
let outfile2 = "/home/spare/Documents/Output/test2" in
let () = Time.time (Sync.sync_with_lines "/home/spare/Documents/FilesOmTeSyncen/old/fischer.txt" "/home/spare/Documents/FilesOmTeSyncen/new/fischer.txt" Sync.sha1) outfile2 in
let () = Printf.printf "========================================\n%!" in
let outfile3 = "/home/spare/Documents/Output/test3" in
let () = Time.time (Sync.sync_with_blocks "/home/spare/Documents/FilesOmTeSyncen/old/fischer.txt" "/home/spare/Documents/FilesOmTeSyncen/new/fischer.txt" 10 Sync.sha1) outfile3 in
let () = Printf.printf "========================================\n%!" in
let outfile4 = "/home/spare/Documents/Output/test4" in
let () = Time.time (Sync.sync_with_whitespace "/home/spare/Documents/FilesOmTeSyncen/old/fischer.txt" "/home/spare/Documents/FilesOmTeSyncen/new/fischer.txt" 10 Sync.sha1) outfile4 in
print_string "Done.\n" ;;


(* Testen voor big.bmp *)
let () = Printf.printf "========================================\n%!" in
let outfile = "/home/spare/Documents/Output/test_big" in
let () = Time.time (Sync.sync_with_blocks "/home/spare/Documents/FilesOmTeSyncen/old/big.bmp" "/home/spare/Documents/FilesOmTeSyncen/new/big.bmp" 4096 Sync.sha1) outfile in
print_string "Done.\n"
