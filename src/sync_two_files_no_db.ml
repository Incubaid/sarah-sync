(* Syncing of two files.
   Size of finite field is fixed beforehand.*)

open Read_file_old
open FiniteField
open Set_reconciliation
open Evaluation_points
open Construct_set

open Sha1

module Syncing =
  functor (F : FINITEFIELD) ->
struct
  type filename = string

  module S = SetReconciliation(F)
  module EP = EvaluationPts(F)

  module SC = Set_constructor(F)

  (* Type of messages being sent *)
  type message = Hash of string * int    (* Hash and the block it identifies *)
                 | Original of string


  (* Control hash of an entire file *)
  let control_hash (file : filename) hash_function =
    let chan = open_in file in
    let as_string = Std.input_all chan in
    let hash = hash_function as_string in
    close_in chan ;
    hash


  (* Syncing *)
  let sync (file1 : filename) (file2 : filename) partition hash_function =
    let total_hash = control_hash file1 hash_function in
    let parts1 = partition file1 in
    let parts2 = partition file2 in
    let l1, l2 = (List.map hash_function parts1 , List.map hash_function parts2) in
    let set1, set2 = (List.map SC.map_to_field l1 , List.map SC.map_to_field l2) in
    let assoc_list1 = List.combine set1 parts1 in
    let assoc_list2 = List.combine l2 parts2 in  (* Complete hashes as keys *)
    let set1_un = SC.remove_duplicates set1 in
    let set2_un = SC.remove_duplicates set2 in
    let size_1 = List.length set1_un in
    let size_2 = List.length set2_un in
    let delta = size_1 - size_2 in
    let init_max, k = EP.get_max_vals size_1 size_2 in
    let eval_pts = EP.eval_pts init_max in
    let rat_vals = S.get_rational_values set1_un set2_un eval_pts in
    let extra_pts = EP.extra_eval_pts k in
    let actual_vals = S.get_rational_values set1_un set2_un extra_pts in
    let m, cfs_num, cfs_denom = EP.findM delta rat_vals actual_vals init_max eval_pts extra_pts in
    let to_send_by_1 =
      if m = 0
      then []
      else S.reconcile cfs_num cfs_denom
    in
    let element_or_original i (el, orig) =
      if List.mem el to_send_by_1
      then Original orig
      else
        begin
          let orig_hash = List.nth l1 i in
          Hash (orig_hash, i)
        end
    in
    let message = List.mapi element_or_original assoc_list1 in
    let complete_message = (message , total_hash) in
    (List.length to_send_by_1 , complete_message , parts1 , assoc_list2)


  (* Write the reconstructed file to a specified location *)
  let construct_file file contents =
    let chan = open_out file in
    let () = List.iter (output_string chan) contents in
    let () = flush chan in
    close_out chan


  exception Reconstruction_not_perfect

  (* Reconstruction *)
 let reconstruct (msg, hash) parts1 assoc_list2 location hash_function nr_sent =
    let number = ref nr_sent in
    let decode m =
      match m with
      | Original orig -> orig
      | Hash (hash, i) ->
        try
          List.assoc hash assoc_list2
        with Not_found ->    (* Should be communicated back to the client, to acquire the original block. *)
          number := succ !number ;
          List.nth parts1 i
    in
    let content = List.map decode msg in
    let () = construct_file location content in
    let new_hash = control_hash location hash_function in
    if new_hash <> hash
    then
      begin
        Printf.printf "Reconstruction not correct. Original hash: %s.\nObtained hash: %s.\n%!" hash new_hash;
        raise Reconstruction_not_perfect
      end
    else Printf.printf "Reconstruction completed. %i blocks out of %i have been sent.\n%!" !number (List.length parts1)


  (* Sha1 hash function *)
  let sha1 l =
    Sha1.to_hex (Sha1.string l)


  (* Syncing by dividing into blocks *)
  let sync_with_blocks file1 file2 size hash_function location =
    let nr_sent, msg, prts1, l2 = sync file1 file2 (blocks ~size) hash_function in
    reconstruct msg prts1 l2 location hash_function nr_sent


  (* Syncing by partitioning on the words *)
  let sync_with_words file1 file2 hash_function location =
    let nr_sent, msg, prts1, l2 = sync file1 file2 words hash_function in
    reconstruct msg prts1 l2 location hash_function nr_sent


  (* Syncing by partitioning on whitespace *)
  let sync_with_whitespace file1 file2 size hash_function location =
    let nr_sent, msg, prts1, l2 = sync file1 file2 (blocks_using_whitespace ~size) hash_function in
    reconstruct msg prts1 l2 location hash_function nr_sent


  (* Syncing, by partitioning on the lines *)
  let sync_with_lines file1 file2 hash_function location =
    let nr_sent, msg, prts1, l2 = sync file1 file2 lines hash_function in
    reconstruct msg prts1 l2 location hash_function nr_sent

end
