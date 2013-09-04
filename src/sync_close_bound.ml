(* Syncing of two files *)

open Read_file
open FiniteField
open Set_reconciliation
open Evaluation_points
open Construct_set

open Sha1

module Syncing =
struct

  type filename = string
 
  (* Type of messages being sent *)
  type message = Hash of string * int    (* Hash and the block it identifies *)
                 | Original of string


  (* Displaying the message *)
  let print_message (m : message) =
    match m with
    | Hash (hash,_) -> print_string hash
    | Original orig -> print_string orig


  (* Control hash of an entire file *)
  let control_hash (file : filename) hash_function =
    let chan = open_in file in
    let as_string = Std.input_all chan in
    let hash = hash_function as_string in
    close_in chan ;
    hash


  (* Syncing *)
  let sync (client : filename) (server : filename) partition hash_function db =
    let k' = 1  in    (* CHANGE THIS *)
    let total_hash = control_hash client hash_function in
    let info_client = partition client hash_function in
    let info_server = partition server hash_function in  
    let f_w = Field_size.get_size (List.length info_client) (List.length info_server) k' in
    Printf.printf "Using w = %i\n%!" f_w ;
    let module F = FiniteField.Make(struct
      let w = f_w
    end)
    in
    let module S = SetReconciliation(F) in
    let module EP = EvaluationPts(F) in
    let module SC = Set_constructor(F) in
    let set1, full_info_client = SC.construct_full_info info_client in
    let hashes_server = Signature.commit_info info_server db in
    let set2 = SC.construct hashes_server in
    let () = Printf.printf "Looking for m.\n%!" in
    let size_1 = List.length set1 in
    let size_2 = List.length set2 in
    let delta = size_1 - size_2 in
    let init_max, k = EP.get_max_vals size_1 size_2 in
    let eval_pts = EP.evalPts init_max in
    let rat_vals = S.get_rational_values set1 set2 eval_pts in
    let extra_pts = EP.extraEvalPts k in
    let actual_vals = S.get_rational_values set1 set2 extra_pts in
    let m, cfs_num, cfs_denom = EP.findM delta rat_vals actual_vals init_max eval_pts extra_pts in
    let () = Printf.printf "Found m: %i.\n%!" m in
    let to_send_by_1 =
      if m = 0
      then []
      else S.reconcile cfs_num cfs_denom
    in
    let () = Printf.printf "Reconciliation done. %i blocks of set1 are missing.\n%!" (List.length to_send_by_1) in
    let element_or_original i (el, hash, begin_pos, size, file) =
      if List.mem el to_send_by_1
      then Original (get_block begin_pos size file)
      else Hash (hash, i)
    in
    let message = List.mapi element_or_original full_info_client in
    let complete_message = (message , total_hash) in
    (List.length to_send_by_1 , complete_message , info_client , hashes_server)




  (* Write the reconstructed file to a specified location *)
  let construct_file file contents =
    let chan = open_out file in
    let () = List.iter (output_string chan) contents in
    let () = flush chan in
    close_out chan


  exception Reconstruction_not_perfect


  (* Reconstruction *)
  let reconstruct (msg, hash) info_client hashes_server location hash_function nr_sent db =
    let () = Printf.printf "Reconstruction.\n%!" in
    let number = ref nr_sent in
    let decode m =
      match m with
      | Original orig -> orig
      | Hash (hash, i) ->
        let opt = Signature.get_location hash db in
        match opt with
        | Some (begin_pos, size, file) -> get_block begin_pos size file
        | None ->
          number := succ !number ;
          let _, begin_pos, size, file = List.nth info_client i in
          get_block begin_pos size file
    in
    let content = List.map decode msg in
    let () = construct_file location content in
    let new_hash = control_hash location hash_function in
    if new_hash <> hash
    then
      begin
        Printf.printf "Original hash: %s.\nObtained hash: %s.\n%!" hash new_hash;
        raise Reconstruction_not_perfect
      end
    else Printf.printf "Reconstruction completed. %i blocks out of %i have been sent.\n%!" !number (List.length info_client)


  (* Sha1 hash function *)
  let sha1 l =
    Sha1.to_hex (Sha1.string l)


  (* Syncing by dividing into blocks *)
  let sync_with_blocks file1 file2 size hash_function location =
    let db = Signature.init_database () in
    let nr_sent, msg, prts1, l2 = sync file1 file2 (blocks ~size) hash_function db in
    reconstruct msg prts1 l2 location hash_function nr_sent db


  (* Syncing by partitioning on the words *)
  let sync_with_words file1 file2 hash_function location =
    let db = Signature.init_database () in
    let nr_sent, msg, prts1, l2 = sync file1 file2 words hash_function db in
    reconstruct msg prts1 l2 location hash_function nr_sent db


  (* Syncing by partitioning on whitespace *)
  let sync_with_whitespace file1 file2 size hash_function location =
    let db = Signature.init_database () in
    let nr_sent, msg, prts1, l2 = sync file1 file2 (blocks_using_whitespace ~size) hash_function db in
    reconstruct msg prts1 l2 location hash_function nr_sent db


  (* Syncing, by partitioning on the lines *)
  let sync_with_lines file1 file2 hash_function location =
    let db = Signature.init_database () in
    let nr_sent, msg, prts1, l2 = sync file1 file2 lines hash_function db in
    reconstruct msg prts1 l2 location hash_function nr_sent db

end


(* ========== Tests ========== *)


module Sync = Syncing ;;


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
let () = Time.time (Sync.sync_with_blocks "/home/spare/Documents/FilesOmTeSyncen/old/big.bmp" "/home/spare/Documents/FilesOmTeSyncen/new/big.bmp" 4000 Sync.sha1) outfile in
print_string "Done.\n"
