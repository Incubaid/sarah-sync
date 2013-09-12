(* Set reconciliation with network-interaction.
   The size of the finite field is determined by the algorithm itself. *)

open Lwt
open FiniteField
open Evaluation_points
open Set_reconciliation
open Construct_set
open Read_file
open Handle_interaction


module Sync_with_network =
struct

  type filename = string


  (* ====== CLIENT ====== *)

  (* Sending extra blocks, because they were not present after all. *)
  let send_extra_blocks ic oc info_client =
    let rec loop () =
      Llio.input_int ic >>= fun i ->
      if i = -1
      then
        Lwt.return ()
      else
        begin
          let _, _, begin_pos, size, file = List.nth info_client i in
          get_block begin_pos size file >>= fun block ->            (* Acquire the block *)
          Llio.output_string oc block >>= fun () ->                 (* Send the block *)
          loop ()
        end
    in
    loop ()


  (* Syncing with the server *)
  let sync_with_server addr (file : filename) partition hash_function new_location =
    partition file hash_function >>= fun info_client ->
    Lwt_io.with_connection addr
      (
        fun (ic,oc) ->
          Llio.output_int oc (List.length info_client) >>= fun () ->
          Llio.input_int ic >>= fun f_w ->
          let module F = FiniteField.Make(struct
            let w = f_w
          end)
          in
          let module S = SetReconciliation(F) in
          let module EP = EvaluationPts(F) in
          let module SC = Set_constructor(F) in
          let set, full_info_client = SC.construct_full_info info_client in
          let l = List.length set in
          Llio.output_string oc new_location >>= fun () ->     (* Send location for reconstruction *)
          Llio.output_int oc l >>= fun () ->                   (* Send size of set *)
          Llio.input_int ic >>= fun max_m ->                   (* Maximum number of sample points *)
          Llio.input_int ic >>= fun k ->                       (* Number of extra evaluation points *)
          let eval_pts = EP.eval_pts max_m in
          let extra_pts = EP.extra_eval_pts k in
          let chi = List.map (S.CharPoly.eval_char_poly set) eval_pts in
          let extra = List.map (S.CharPoly.eval_char_poly set) extra_pts in
          Llio.output_list
            (fun oc el -> Llio.output_int oc (F.unwrap el))
            oc chi >>= fun () ->
          Llio.output_list
            (fun oc el -> Llio.output_int oc (F.unwrap el))
            oc extra  >>= fun () ->
          Llio.input_list
            (fun ic -> Llio.input_int ic >>= fun i ->
              Lwt.return (F.wrap i))
            ic >>= fun to_send_rev ->
          let to_send = List.rev to_send_rev in
          create_message to_send full_info_client >>= fun message ->
          Llio.output_list
            Llio_extra.output_message_el
            oc message >>= fun () ->
          control_hash file hash_function >>= fun total_hash ->
          Llio.output_string oc total_hash >>= fun () ->
          send_extra_blocks ic oc info_client >>= fun () ->
          Llio.input_string ic >>= fun s ->
          Llio.output_string Lwt_io.stdout s
      )


  (* ====== SERVER ====== *)


  exception Reconstruction_not_perfect


  (* Handling requests from the client *)
  let handle_requests fd db hash_function =
    let ic = Lwt_io.of_fd ~mode:Lwt_io.input fd in
    let oc = Lwt_io.of_fd ~mode:Lwt_io.output fd in
    Signature.all_keys db >>= fun hashes_server ->
    Llio.input_int ic >>= fun s_1 ->
    let k' = 1 in    (* CHANGE THIS *)
    let f_w = Field_size.get_size s_1 (List.length hashes_server) k' in
    Llio.output_int oc f_w >>= fun () ->
    Lwt_io.printlf "Decided to use w = %i.%!" f_w >>= fun () ->
    let module F = FiniteField.Make(struct
      let w = f_w
    end)
    in
    let module S = SetReconciliation(F) in
    let module EP = EvaluationPts(F) in
    let module SC = Set_constructor(F) in
    let set_server = SC.construct hashes_server in
    let size_server = List.length set_server in
    Llio.input_string ic >>= fun location ->
    Llio.input_int ic >>= fun size_client ->
    let max_m, k = EP.get_max_vals size_client size_server in
    Llio.output_int oc max_m  >>= fun () ->
    Llio.output_int oc k  >>= fun () ->
    Llio.input_list
      (fun ic -> Llio.input_int ic >>= fun i ->
        Lwt.return (F.wrap i))
      ic >>= fun chi_rev ->
    let chi = List.rev chi_rev in
    Llio.input_list
      (fun ic -> Llio.input_int ic >>= fun i ->
        Lwt.return (F.wrap i))
      ic >>= fun extra_rev ->
    let extra = List.rev extra_rev in
    let good_m, cfsNum, cfsDenom = EP.findM_server size_client chi extra set_server in
    let to_send_by_client = S.reconcile cfsNum cfsDenom in
    Llio.output_list
      (fun oc el -> Llio.output_int oc (F.unwrap el))
      oc to_send_by_client  >>= fun () ->
    Llio.input_list Llio_extra.input_message_el ic >>= fun msg_rev ->
    let msg = List.rev msg_rev in
    Llio.input_string ic >>= fun orig_hash ->
    let size_sent = ref 0 in    (* Calculating the size of the information that has been sent *)
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
          Llio.output_int oc i >>= fun () ->
          Llio.input_string ic >>= fun block ->
          update_database db hash_function block !current_pos location >>= fun new_pos ->
          current_pos := new_pos ;
          size_sent := !size_sent + (String.length block) ;
          Lwt.return block
    in
    Lwt_list.map_s decode msg >>= fun content ->
    Llio.output_int oc (-1) >>= fun () ->
    construct_file location content >>= fun () ->
    control_hash location hash_function >>= fun new_hash ->
    (
      if new_hash <> orig_hash
      then Lwt_io.printlf "Reconstruction not correct. Size %i has been sent.%!" !size_sent
      else Lwt_io.printlf "Reconstruction correct. Size %i has been sent.%!" !size_sent
    ) >>= fun () ->
    Llio.output_string oc "Finished\n" >>= fun () ->
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
