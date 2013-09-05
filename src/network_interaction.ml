(* Set reconciliation with network-interaction *)

open Lwt
open FiniteField
open Evaluation_points
open Set_reconciliation
open Construct_set
open Read_file
open Handle_interaction


module Sync_with_network =
  functor (F : FINITEFIELD) ->
struct

  type filename = string 

  module S = SetReconciliation(F)
  module EP = EvaluationPts(F)
  module SC = Set_constructor(F)


  (* ====== CLIENT ====== *)

  (* Sending extra blocks *)
  let send_extra_blocks ic oc info_client =
    let rec loop () =
      Lwt_io.read_value ic >>= fun i ->
      if i = -1
      then
          Lwt.return ()
      else
        begin
          let _, _, begin_pos, size, file = List.nth info_client i in
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


  (* Handling requests from the client *)
  let handle_requests fd db hash_function =
    let ic = Lwt_io.of_fd ~mode:Lwt_io.input fd in
    let oc = Lwt_io.of_fd ~mode:Lwt_io.output fd in
    Signature.all_keys db >>= fun hashes_server ->
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
        update_database db hash_function orig !current_pos location >>= fun new_pos ->
        current_pos := new_pos ;
        size_sent := !size_sent + (String.length orig) ;
        Lwt.return orig
      | Hash (hash, hash_2, i) ->
        Signature.get_location hash hash_2 db >>= fun opt ->
        match opt with
        | Some (begin_pos, size, file) ->
          Signature.add_to_database (hash, hash_2, !current_pos, size, location) ~db >>= fun () ->
          let () = current_pos := !current_pos + size in
          get_block begin_pos size file
        | None ->                                 (* Should be communicated back to the client, to acquire the original block. *)
          Lwt_io.write_value oc i >>= fun () ->
          Lwt_io.read_value ic >>= fun block ->
          update_database db hash_function block !current_pos location >>= fun new_pos ->
          current_pos := new_pos ;
          size_sent := !size_sent + (String.length block) ;
          Lwt.return block
    in
    Lwt_list.map_s decode msg >>= fun content ->
    Lwt_io.write_value oc (-1) >>= fun () ->
    construct_file location content >>= fun () ->
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
