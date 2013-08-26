(* Set reconciliation with network-interaction *)

open Lwt
open FiniteField
open Evaluation_points_network
open Set_reconciliation
open Construct_set

module Sync_with_network = 
  functor (F : FINITEFIELD) ->
struct

  type filename = string

  module S = SetReconciliation(F)
  module EP = EvaluationPts(F)
  module SC = Set_constructor(F)


  (* Control hash of an entire file *)
  let control_hash (file : filename) hash_function =
    let chan = open_in file in
    let as_string = Std.input_all chan in
    let hash = hash_function as_string in
    close_in chan ;
    hash

  (* Get the block from the specified location *)
  let get_block begin_pos size file =
    let start = Int64.of_int begin_pos in
    let get_it ic = 
      Lwt_io.set_position ic start >>= fun () ->
      (
        Lwt_io.printf "Begin_pos: %i,  Pos: %i, Size: %i \n%!" begin_pos (Int64.to_int (Lwt_io.position ic)) size ;
        Lwt_io.read ~count:size ic >>= fun s ->
        (
          Lwt_io.printf "Block: %s\n%!" s ;
          Lwt.return s
        )
      )
    in
    Lwt_io.with_file ~mode:Lwt_io.input file get_it


  (* ====== CLIENT ====== *)

  (* Type of messages being sent *)
  type message = Hash of string * int    (* Hash and the block it identifies *)
                 | Original of (string Lwt.t)


  (* Creating the message to send to the server *)
  let create_message to_send full_info_client = 
    let element_or_original i (el, hash, begin_pos, size, file) =
      if List.mem el to_send
      then 
        begin
          Printf.printf "Constructing message.\n%!" ;
          Printf.printf "Original: begin_pos %i, size %i.\n%!" begin_pos size ;
          Original (get_block begin_pos size file)
        end
      else Hash (hash, i)
    in
    List.mapi element_or_original full_info_client


  (* Sending extra blocks *)
  let send_extra_blocks ic oc info_client = 
    let rec loop () = 
      Lwt_io.read_value ic >>= fun i ->
      (
        if i = -1
        then Lwt.return ()
        else 
          begin
            Printf.printf "Sending extra block: nr. %i \n%!" i;
            let _, begin_pos, size, file = List.nth info_client i in
            get_block begin_pos size file >>= fun block ->            (* Acquire the block *)
            Lwt_io.write_value oc block >>= fun () ->                 (* Send the block *)
            (
              Lwt.ignore_result (Lwt_io.printf "Block sent.\n%!") ; 
              loop ()
            )
          end 
      )
    in
    loop ()


  (* Syncing with the server *)
  let sync_with_server addr (file : filename) partition hash_function new_location =
    let total_hash = control_hash file hash_function in
    let info_client = partition file hash_function in
    let set, full_info_client = SC.construct_full_info info_client in
    let l = List.length set in
    Lwt_io.with_connection addr
	  (
        fun (ic,oc) ->
	    ( 
          Lwt_io.write_value oc l >>= fun () ->                (* Send size of set *)
          Lwt_io.read_value ic >>= fun (max_m, k) ->           (* Maximal values of m and k*)
          let eval_pts = EP.evalPts max_m in
          let extra_pts = EP.extraEvalPts k in
          let chi = List.map (S.CharPoly.evalCharPoly set) eval_pts in
          let extra = List.map (S.CharPoly.evalCharPoly set) extra_pts in
          Lwt_io.write_value oc (chi, extra)
        ) >>= fun () ->
          Lwt_io.read_value ic >>= fun to_send ->
        (
          let message = create_message to_send full_info_client in
          let complete_message = (message, total_hash) in
          Lwt_io.write_value oc complete_message
        ) >>= fun () ->
          send_extra_blocks ic oc info_client 
        >>= (* fun () ->
          Lwt_io.write oc new_location                          (* Provide the new location *) *)
            fun () -> Lwt_io.read ic >>= fun s -> Lwt_io.write Lwt_io.stdout s
            
      )


  (* ====== SERVER ====== *)

  exception Reconstruction_not_perfect

  (* Handling requests from the client *)
  let handle_requests fd db hash_function = 
    let ic = Lwt_io.of_fd ~mode:Lwt_io.input fd in
    let oc = Lwt_io.of_fd ~mode:Lwt_io.output fd in
    let hashes_server = Signature.all_keys db in
    let set_server = SC.construct hashes_server in
    let size_server = List.length set_server in
    Lwt_io.read_value ic >>= fun size_client ->
    (
      let max_m, k = EP.get_max_vals size_client size_server in
      Lwt_io.write_value oc (max_m, k)  >>= fun () ->
      Lwt_io.read_value ic >>= fun (chi, extra ) ->
      let good_m = EP.findM size_client chi extra set_server in
      let pts = EP.evalPts good_m in
      let chi_client = EP.take good_m chi in
      let to_send_by_client = S.reconcile size_client chi_client set_server pts in
      Lwt_io.write_value oc to_send_by_client 
    ) >>= fun () ->
    Lwt_io.read_value ic >>= fun (msg, orig_hash) ->
    (
      let decode m =
        match m with
        | Original orig -> orig
        | Hash (hash, i) ->
          Lwt.catch 
            ( fun () ->
              let begin_pos, size, file = Signature.get_location hash db in
              Lwt.ignore_result (Lwt_io.printf "Reconstructing.\n%!") ;
              get_block begin_pos size file
            )
            ( function 
            | Not_found ->                          (* Should be communicated back to the client, to acquire the original block. *)
              Lwt_io.write_value oc i >>= fun () ->
              Lwt_io.read_value ic
            | e -> Lwt_io.printl (Printexc.to_string e) >>= fun () -> 
              Lwt.return ""                         (* Some other exception *)
            )
      in 
      let content = List.map decode msg in
     (* Lwt_io.read ic >>= fun location ->
      (
        let stream_of_content = Lwt_stream.of_list content in
        Lwt_io.lines_to_file location stream_of_content >>= fun () ->
        (
          let new_hash = control_hash location hash_function in
          if new_hash <> orig_hash
          then raise Reconstruction_not_perfect
          else Lwt.return ()
        )
      ) *)
      Printf.printf "Decoding done.\n%!" ;
      Lwt_io.write_value oc (-1) >>= fun () ->
      Lwt_io.write oc "Finished" 
      
    ) 


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
