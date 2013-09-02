(* Set reconciliation with network-interaction *)

open Lwt
open FiniteField
open Evaluation_points
open Set_reconciliation
open Construct_set
open Read_file_network

module Sync_with_network =
  functor (F : FINITEFIELD) ->
struct

  type filename = string 

  module S = SetReconciliation(F)
  module EP = EvaluationPts(F)
  module SC = Set_constructor(F)


  (* Control hash of an entire file *)
  let control_hash (file : filename) hash_function =
    let hash ic =
      Lwt_io.read ic >>= fun s ->
      Lwt.return (hash_function s)
    in
    Lwt_io.with_file ~mode:Lwt_io.input file hash


  (* ====== CLIENT ====== *)

  (* Type of messages being sent *)
  type message = Hash of string * int    (* Hash and the block it identifies *)
                 | Original of string


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


  (* Sending extra blocks *)
  let send_extra_blocks ic oc info_client =
    let rec loop () =
      Lwt_io.read_value ic >>= fun i ->
      if i = -1
      then
          Lwt.return ()
      else
        begin
          let _, begin_pos, size, file = List.nth info_client i in
          get_block begin_pos size file >>= fun block ->            (* Acquire the block *)
          Lwt_io.write_value oc block >>= fun () ->                 (* Send the block *)
          loop ()
        end
    in
    loop ()


  (* Syncing with the server *)
  let sync_with_server addr (file : filename) partition hash_function new_location =
    partition file hash_function >>= fun info_client ->
    let set, full_info_client = SC.construct_full_info info_client in
    let l = List.length set in
    Lwt_io.with_connection addr
      (
        fun (ic,oc) ->
          Lwt_io.write_value oc new_location >>= fun () ->     (* Send location for reconstruction *)
          Lwt_io.write_value oc l >>= fun () ->                (* Send size of set *)
          Lwt_io.read_value ic >>= fun (max_m, k) ->           (* Maximal values of m and k*)
          let eval_pts = EP.evalPts max_m in
          let extra_pts = EP.extraEvalPts k in
          let chi = List.map (S.CharPoly.evalCharPoly set) eval_pts in
          let extra = List.map (S.CharPoly.evalCharPoly set) extra_pts in
          Lwt_io.write_value oc (chi, extra) >>= fun () ->
          Lwt_io.read_value ic >>= fun to_send ->
          create_message to_send full_info_client >>= fun message ->
          control_hash file hash_function >>= fun total_hash ->
          let complete_message = (message, total_hash) in
          Lwt_io.write_value oc complete_message >>= fun () ->
          send_extra_blocks ic oc info_client >>= fun () ->
          Lwt_io.read_value ic >>= fun s ->
          Lwt_io.write Lwt_io.stdout s
      )


  (* ====== SERVER ====== *)


  exception Reconstruction_not_perfect


  (* Adds the location of a new block to the database and returns the new position in the file *)
  let update_database db hash_function block begin_pos location =
    let hash_block = hash_function block in
    let size = String.length block in
    let () = Signature.add_to_database (hash_block, begin_pos, size, location) ~db in
    begin_pos + size


  (* Handling requests from the client *)
  let handle_requests fd db hash_function =
    let ic = Lwt_io.of_fd ~mode:Lwt_io.input fd in
    let oc = Lwt_io.of_fd ~mode:Lwt_io.output fd in
    let hashes_server = Signature.all_keys db in
    let set_server = SC.construct hashes_server in
    let size_server = List.length set_server in
    Lwt_io.read_value ic >>= fun location ->
    Lwt_io.read_value ic >>= fun size_client ->
    let max_m, k = EP.get_max_vals size_client size_server in
    Lwt_io.write_value oc (max_m, k)  >>= fun () ->
    Lwt_io.read_value ic >>= fun (chi, extra ) ->
    let good_m, cfsNum, cfsDenom = EP.findM_server size_client chi extra set_server in
    let to_send_by_client = S.reconcile cfsNum cfsDenom in
    Lwt_io.write_value oc to_send_by_client  >>= fun () ->
    Lwt_io.read_value ic >>= fun (msg, orig_hash) ->
    let size_sent = ref 0 in    (* Calculating the size of the information has to been sent *)
    let current_pos = ref 0 in
    let decode m =
      match m with
      | Original orig ->
        let new_pos = update_database db hash_function orig !current_pos location in
        current_pos := new_pos ;
        size_sent := !size_sent + (String.length orig) ;
        Lwt.return orig
      | Hash (hash, i) ->
        let opt = Signature.get_location hash db in
        match opt with
        | Some (begin_pos, size, file) ->
          current_pos := !current_pos + size ;
          get_block begin_pos size file
        | None ->                                 (* Should be communicated back to the client, to acquire the original block. *)
          Lwt_io.write_value oc i >>= fun () ->
          Lwt_io.read_value ic >>= fun block ->
          let new_pos = update_database db hash_function block !current_pos location in
          current_pos := new_pos ;
          size_sent := !size_sent + (String.length block) ;
          Lwt.return block
    in
    Lwt_list.map_s decode msg >>= fun content ->
    Lwt_io.write_value oc (-1) >>= fun () ->
    Lwt_unix.openfile location [Lwt_unix.O_CREAT ; Lwt_unix.O_WRONLY] 0o640 >>= fun fd ->
    let loc_oc = Lwt_io.of_fd ~mode:Lwt_io.output fd in
    Lwt_list.iter_s (Lwt_io.write loc_oc) content >>= fun () ->
    control_hash location hash_function >>= fun new_hash ->
    (
      if new_hash <> orig_hash
      then Lwt_io.printlf "Reconstruction not correct. Size %i has been sent.%!" !size_sent
      else Lwt_io.printlf "Reconstruction correct. Size %i has been sent.%!" !size_sent
    ) >>= fun () ->
    Lwt_io.write_value oc "Finished\n" >>= fun () ->
    Lwt_io.flush oc


  (* Setting up the server *)
  let server soc address db hash_function =
    Lwt_unix.setsockopt soc Unix.SO_REUSEADDR true ;
    Lwt_unix.bind soc address ;
    Lwt_unix.listen soc 1024 ;
    let rec loop () =
      begin
        Lwt.catch
          (fun () ->
            Lwt_unix.accept soc >>= fun (fd, addr) ->
            Lwt.ignore_result ( handle_requests fd db hash_function ) ;
            Lwt.return ()
          )
          (fun e -> Lwt_io.printl (Printexc.to_string e) >>= fun () -> Lwt.return ())
        >>= fun () -> loop ()
      end
    in
    loop ()



end
