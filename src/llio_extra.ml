(* Extra functions for Llio, to send/receive the message elements *)

open Lwt
open Llio
open Handle_interaction


(* Sending *)
let output_message_el oc (el : message) =
  match el with
  | Hash (h_1, h_2, i) ->
    Llio.output_string oc "Hash" >>= fun () ->
    Llio.output_string oc h_1 >>= fun () ->
    Llio.output_string oc h_2 >>= fun () ->
    Llio.output_int oc i
  | Original s ->
    Llio.output_string oc "Orig" >>= fun () ->
    Llio.output_string oc s


(* Receiving *)
let input_message_el ic =
  Llio.input_string ic >>= fun id ->
  match id with
  | "Hash" ->
    Llio.input_string ic >>= fun h_1 ->
    Llio.input_string ic >>= fun h_2 ->
    Llio.input_int ic >>= fun i ->
    Lwt.return (Hash (h_1, h_2, i))
  | "Orig" ->
    Llio.input_string ic >>= fun s ->
    Lwt.return (Original s)
  | _ -> Llio.lwt_failfmt "Message invalid."
