(* Additional functions used in the interaction with the database and syncing *)

open Lwt
open Read_file

type filename = string

(* Type of messages being sent *)
type message = Hash of string * string * int    (* Hashes and the block they identify *)
               | Original of string


(* Control hash of an entire file *)
let control_hash (file : filename) hash_function =
  let hash ic =
    Lwt_io.read ic >>= fun s ->
    Lwt.return (hash_function s)
  in
  Lwt_io.with_file ~mode:Lwt_io.input file hash
 

(* Missing in Lwt_list *)
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
  let element_or_original i (el, hash, hash_2,  begin_pos, size, file) =
    if List.mem el to_send
    then
      begin
        get_block begin_pos size file >>= fun s ->
        Lwt.return (Original s)
      end
    else
      begin
        Lwt.return (Hash (hash, hash_2, i))
      end
  in
  mapi element_or_original full_info_client


(* Adds the location of a new block to the database and returns the new position in the file *)
let update_database db hash_function block begin_pos location =
  let hash_block = hash_function block in
  let hash_2 = md5 block in
  let size = String.length block in
  Signature.add_to_database (hash_block, hash_2, begin_pos, size, location) ~db >>= fun () ->
  Lwt.return (begin_pos + size)


(* Write the reconstructed file to a specified location *)
let construct_file file content =
  Lwt_unix.openfile file [Lwt_unix.O_CREAT ; Lwt_unix.O_WRONLY] 0o640 >>= fun fd ->
  let loc_oc = Lwt_io.of_fd ~mode:Lwt_io.output fd in
  Lwt_list.iter_s (Lwt_io.write loc_oc) content
