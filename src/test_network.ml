(* Testing the interaction with the network *)

open Read_file_network
open Lwt


(*
(* Fixing size of the field *)
open FiniteField
open Network_interaction
  
module Field = FiniteField.Make(struct
  let w = 16
end)

module N = Sync_with_network(Field) *)


(* Determine size of the field automatically *)
open Network_interaction_close_bound
module N = Sync_with_network


let host = "127.0.0.1"
let port = 9000
let soc = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0         (* Server socket maken *)
let addr = Unix.ADDR_INET (Unix.inet_addr_of_string host, port)   (* Address maken *)

let hash_function = function l ->
  Sha1.to_hex (Sha1.string l)


(* Test big *)
let partition_function =
  let size = 4096 in    (* Lwt_io.set_default_buffer_size voor vergroten buffer*)
  blocks ~size

let file_client = "/home/spare/Documents/FilesOmTeSyncen/old/big.bmp"
let file_server = "/home/spare/Documents/FilesOmTeSyncen/new/big.bmp"
let destination = "/home/spare/Documents/Output/netwerk_big" 


(* Test *)
let () =
  if Sys.argv.(1) = "server"
  then
    begin
      let db = Signature.init_database () in   (* Building database for testing *)
      Lwt_main.run 
        (
          partition_function file_server hash_function >>= fun info_server ->
          let _ = Signature.commit_info info_server db in
          N.server soc addr db hash_function
        )
    end
  else
    Time.time Lwt_main.run (N.sync_with_server addr file_client partition_function hash_function destination)

