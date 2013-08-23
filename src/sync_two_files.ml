(* Syncing of two files *)

open Read_file
open FiniteField
open Set_reconciliation
open Evaluation_points

open Sha1


module Syncing =
  functor (F : FINITEFIELD) ->
struct
  type filename = string

  module S = SetReconciliation(F)
  module EP = EvaluationPts(F)

  (* Removing duplicate elements from a list *)
  let remove_duplicates l =
    let module S = Set.Make(F) in
    let set_of_list =
      List.fold_left
        (fun set el -> S.add el set)
        S.empty l
    in
    S.elements set_of_list

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
    then number
    else
      begin
        let binary = bin "" number in
        let element = "0b" ^ (String.sub binary (String.length binary - maxbits) maxbits) in
        int_of_string element
      end


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
  let sync (file1 : filename) (file2 : filename) partition hash_function =
    let total_hash = control_hash file1 hash_function in
    let parts1 = partition file1 in
    let parts2 = partition file2 in
    let l1, l2 = (List.map hash_function parts1 , List.map hash_function parts2) in
    let els1, els2 = (List.map map_to_field l1 , List.map map_to_field l2) in
    let set1, set2 = (List.map F.wrap els1 , List.map F.wrap els2) in
    let assoc_list1 = List.combine set1 parts1 in
    let assoc_list2 = List.combine l2 parts2 in  (* Complete hashes as keys *)
    let set1_un = remove_duplicates set1 in
    let set2_un = remove_duplicates set2 in
    let () = Printf.printf "Looking for m.\n%!" in
    let m = EP.findM set1_un set2_un in
    let () = Printf.printf "Found m: %i.\n%!" m in
    let pts = EP.evalPts m in
    let m1 = List.length set1_un in
    let () = Printf.printf "Evaluating the characteristic polynomial of set1.\n%!" in
    let chi_1 = List.map (S.CharPoly.evalCharPoly set1_un) pts in
    let () = Printf.printf "Reconciling the sets.\n%!" in
    let to_send_by_1, extra_in_2 = S.reconcile m1 chi_1 set2_un pts in
    let () = Printf.printf "Reconciliation done. %i blocks of set1 are missing.\n%!" (List.length to_send_by_1) in
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
    let () = Printf.printf "Reconstruction.\n%!" in
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
        Printf.printf "Original hash: %s.\nObtained hash: %s.\n%!" hash new_hash;
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


(* Tests. *)

module Field = FiniteField.Make(struct
  let w = 16
end)

module Sync = Syncing(Field) ;;

(*let els1, els2, sols1, sols2, m, hash = Sync.sync_with_lines "/home/spare/Documents/FilesOmTeSyncen/old/fischer.txt" "/home/spare/Documents/FilesOmTeSyncen/new/fischer.txt" Sync.sha1 in *)
(* let els1, els2, sols1, sols2, m, hash = Sync.sync_with_blocks "/home/spare/Documents/FilesOmTeSyncen/old/fischer.txt" "/home/spare/Documents/FilesOmTeSyncen/new/fischer.txt" 100 Sync.sha1 in *)
(*let els1, els2, sols1, sols2, m, hash = Sync.sync_with_words "/home/spare/Documents/FilesOmTeSyncen/old/fischer.txt" "/home/spare/Documents/FilesOmTeSyncen/new/fischer.txt" Sync.sha1 in *)
(*print_string "\nRECONCILIATION\n";
List.iter (fun el -> (Field.print el ; print_string " ")) sols1;
print_newline ();
List.iter (fun el -> (Field.print el ; print_string " ")) sols2;
print_newline ();
print_int (List.length sols1); print_string " OF "; print_int (List.length els1); print_string " BLOCKS SENT.\n" ;
print_string "\nRESTORATION\n" ;
List.iter (fun el -> (Sync.print_message el)) m ;
print_newline () ;
let outfile = "/home/spare/Documents/Output/test1" in
Sync.reconstruct outfile (List.map Sync.get_string m) ; *)
(*print_string "\nVERIFICATION\n" ;
let new_hash = Sync.control_hash outfile Sync.sha1 in
print_string "Original: "; print_string hash ; print_newline () ;
print_string "Obtained: "; print_string new_hash ; print_newline () ;
print_string "These are " ;
if hash = new_hash
then print_string "the same.\n"
else print_string "not the same.\n" *)


let time f lastarg =
  let start = Unix.gettimeofday () in
  let res = f lastarg in
  let stop = Unix.gettimeofday () in
  Printf.printf "Execution time: %fs.\n%!" (stop -. start) ;
  res  ;;

(* Testen voor fisher.txt *)
let outfile1 = "/home/spare/Documents/Output/test1" in
let () = time (Sync.sync_with_words "/home/spare/Documents/FilesOmTeSyncen/old/fischer.txt" "/home/spare/Documents/FilesOmTeSyncen/new/fischer.txt" Sync.sha1) outfile1 in
let () = Printf.printf "========================================\n%!" in
let outfile2 = "/home/spare/Documents/Output/test2" in
let () = time (Sync.sync_with_lines "/home/spare/Documents/FilesOmTeSyncen/old/fischer.txt" "/home/spare/Documents/FilesOmTeSyncen/new/fischer.txt" Sync.sha1) outfile2 in
let () = Printf.printf "========================================\n%!" in
let outfile3 = "/home/spare/Documents/Output/test3" in
let () = time (Sync.sync_with_blocks "/home/spare/Documents/FilesOmTeSyncen/old/fischer.txt" "/home/spare/Documents/FilesOmTeSyncen/new/fischer.txt" 10 Sync.sha1) outfile3 in
let () = Printf.printf "========================================\n%!" in
let outfile4 = "/home/spare/Documents/Output/test4" in
let () = time (Sync.sync_with_whitespace "/home/spare/Documents/FilesOmTeSyncen/old/fischer.txt" "/home/spare/Documents/FilesOmTeSyncen/new/fischer.txt" 10 Sync.sha1) outfile4 in
print_string "Done.\n"


(* Testen voor big.bmp *)

(*module Field = FiniteField.Make(struct
  let w = 13
end)

module Sync = Syncing(Field) ;;

let outfile = "/home/spare/Documents/Output/test3" in
let () = time (Sync.sync_with_blocks "/home/spare/Documents/FilesOmTeSyncen/old/big.bmp" "/home/spare/Documents/FilesOmTeSyncen/new/big.bmp" 4000 Sync.sha1) outfile in
print_string "Done.\n" *)