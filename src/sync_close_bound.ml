(* Syncing of two files, with database interaction. 
   Size of the finite field is determined automatically. *)

open Read_file
open FiniteField
open Set_reconciliation
open Evaluation_points
open Construct_set
open Lwt

open Sha1

module Syncing =
struct

  type filename = string
 

  (* Type of messages being sent *)
  type message = Hash of string * int    (* Hash and the block it identifies *)
                 | Original of string


  (* Control hash of an entire file *)
  let control_hash (file : filename) hash_function =
    let hash ic =
      Lwt_io.read ic >>= fun s ->
      Lwt.return (hash_function s)
    in
    Lwt_io.with_file ~mode:Lwt_io.input file hash


  (* Function missing in Lwt_list *)
  let mapi f list =
    let rec loop acc i =
      function
      | [] -> Lwt.return (List.rev acc)
      |  x :: xs -> f i x >>= fun y ->
        loop (y :: acc) (i + 1) xs
    in
    loop [] 0 list


  (* Creating the message to send to the server *)
  let create_message to_send full_info_client =
    let element_or_original i (el, hash, begin_pos, size, file) =
      if List.mem el to_send
      then
        begin
          get_block begin_pos size file >>= fun s ->
          Lwt.return (Original s)
        end
      else
        begin
          Lwt.return (Hash (hash, i))
        end
    in
    mapi element_or_original full_info_client


  (* Syncing *)
  let sync (client : filename) (server : filename) partition hash_function db =
    let k' = 1  in    (* CHANGE THIS *)
    control_hash client hash_function >>= fun total_hash ->
    partition client hash_function >>= fun info_client ->
    partition server hash_function >>= fun info_server ->
    let f_w = Field_size.get_size (List.length info_client) (List.length info_server) k' in
    Lwt_io.printlf "Using w = %i%!" f_w  >>= fun () ->
    let module F = FiniteField.Make(struct
      let w = f_w
    end)
    in
    let module S = SetReconciliation(F) in
    let module EP = EvaluationPts(F) in
    let module SC = Set_constructor(F) in
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
    Lwt_io.printlf "Reconciliation done. %i blocks of set1 are missing.\n%!" (List.length to_send_by_1) >>= fun () ->
    create_message to_send_by_1 full_info_client >>=  fun message ->
    let complete_message = (message , total_hash) in
    Lwt.return (List.length to_send_by_1 , complete_message , info_client , hashes_server)


  (* Write the reconstructed file to a specified location *)
  let construct_file file content =
    Lwt_unix.openfile file [Lwt_unix.O_CREAT ; Lwt_unix.O_WRONLY] 0o640 >>= fun fd ->
    let loc_oc = Lwt_io.of_fd ~mode:Lwt_io.output fd in
    Lwt_list.iter_s (Lwt_io.write loc_oc) content


  exception Reconstruction_not_perfect


  (* Reconstruction *)
  let reconstruct (msg, hash) info_client hashes_server location hash_function nr_sent db =
    Lwt_io.printlf "Reconstruction.%!">>= fun () ->
    let number = ref nr_sent in
    let decode m =
      match m with
      | Original orig -> Lwt.return orig
      | Hash (hash, i) ->
        Signature.get_location hash db >>= fun opt ->
        match opt with
        | Some (begin_pos, size, file) -> get_block begin_pos size file
        | None ->
          number := succ !number ;
          let _, begin_pos, size, file = List.nth info_client i in
          get_block begin_pos size file
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
    else Lwt_io.printlf "Reconstruction completed. %i blocks out of %i have been sent.\n%!" !number (List.length info_client)


  (* Sha1 hash function *)
  let sha1 l =
    Sha1.to_hex (Sha1.string l)


  (* Syncing by dividing into blocks *)
  let sync_with_blocks file1 file2 size hash_function location =
    Lwt_main.run 
      (
        Signature.init_database () >>= fun db ->
        sync file1 file2 (blocks ~size) hash_function db >>= fun (nr_sent, msg, prts1,l2) ->
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
let () = Time.time (Sync.sync_with_blocks "/home/spare/Documents/FilesOmTeSyncen/old/big.bmp" "/home/spare/Documents/FilesOmTeSyncen/new/big.bmp" 4096 Sync.sha1) outfile in
print_string "Done.\n"
