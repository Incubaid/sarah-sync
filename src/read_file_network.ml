(* Reading of file with Lwt *)

open Lwt


(* Get the block from the specified location *)
let get_block begin_pos size file =
  let start = Int64.of_int begin_pos in
  let get_it ic =
    Lwt_io.set_position ic start >>= fun () ->
    Lwt_io.read ~count:size ic
  in
  Lwt_io.with_file ~mode:Lwt_io.input file get_it


(* Read a file, until a certain delimiter is encountered.
   A list of possible delimiters is supplied.
   Raises End_of_file on end of input.*)
let read_delim ic ~delims =
  let rec continue res =
    Lwt_io.read_char ic >>= fun c ->
    let res' = res ^ String.make 1 c in
    if List.mem c delims
    then Lwt.return res'
    else continue res'
  in
  continue ""


(* Read a file for a specified count of characters and then some more until a certain delimiter is encountered.
   A list of possible delimiters is supplied. *)
let read_size_delim ic ~size ~delims =
  Lwt_io.read ~count:size ic >>= fun s ->
  let rec continue res =
    Lwt.catch
      (fun () ->
        Lwt_io.read_char ic >>= fun c ->
        let res' = res ^ String.make 1 c in
        if List.mem c delims
        then Lwt.return res'
        else continue res'
      )
      (function
      | e -> Lwt.return res)
  in
  continue s


(* Returns a list of blocks of the file.
   The blocks all have the same size, except maybe the last one. *)
let blocks file hash_function ~size =
  let get_blocks ic =
    let rec loop bs =
      let start = Int64.to_int (Lwt_io.position ic) in
      Lwt_io.read ic ~count:size >>= fun str ->
      let s = String.length str in
      let b = hash_function str in
      let info = (b, start, s, (file : string)) in
      let bs' = info :: bs in
      Lwt_io.length ic >>= fun i ->
      let pos = Lwt_io.position ic in
      (*if s != size *)
      if pos = i
      then
        Lwt.return (List.rev bs')
      else
        loop bs'
    in
    loop []
  in
  Lwt_io.with_file ~mode:Lwt_io.input file get_blocks


(* Returns a file split on whitespace, with blocks of approximately a given size *)
let blocks_using_whitespace file hash_function ~size =
  let get_blocks ic =
    let delims = [' ' ;'\n' ; '\t' ] in
    let rec loop bs =
      let start = Int64.to_int (Lwt_io.position ic) in
      read_size_delim ic ~size ~delims >>= fun str ->
      let s = String.length str in
      let b = hash_function str in
      let info = (b, start, s, (file : string)) in
      let bs' = info :: bs in
      if s < size  (* Last block *)
      then
        Lwt.return (List.rev bs')
      else
        loop bs'
    in
    loop []
  in
  Lwt_io.with_file ~mode:Lwt_io.input file get_blocks


(* Returns a file split on the lines (newlines appended as they were present in the original) *)
let lines file hash_function =
  let get_lines ic =
    let delims = ['\n'] in
    let rec loop bs =
      let start = Int64.to_int (Lwt_io.position ic) in
      Lwt.catch
        (fun () ->
          read_delim ic ~delims >>= fun str ->
          let s = String.length str in
          let b = hash_function str in
          let info = (b, start, s, (file : string)) in
          let bs' = info :: bs in
          loop bs'
        )
        (function
        | e ->    (* Last line *)
          Lwt_io.set_position ic (Int64.of_int start) >>= fun () ->
          Lwt_io.read ic >>= fun str ->
          let s = String.length str in
          let b = hash_function str in
          let info = (b, start, s, (file : string)) in
          let bs' = info :: bs in
          Lwt.return (List.rev bs')
        )
    in
    loop []
  in
  Lwt_io.with_file ~mode:Lwt_io.input file get_lines


(* Returns a list of words, i.e. the file is split on whitespace *)
let words file hash_function =
  let get_words ic =
    let delims = [' ' ; '\t'] in
    let rec loop bs =
      let start = Int64.to_int (Lwt_io.position ic) in
      Lwt.catch
        (fun () ->
          read_delim ic ~delims >>= fun str ->
          let s = String.length str in
          let b = hash_function str in
          let info = (b, start, s, (file : string)) in
          let bs' = info :: bs in
          loop bs'
        )
        (function
        | e ->    (* Last word *)
          Lwt_io.set_position ic (Int64.of_int start) >>= fun () ->
          Lwt_io.read ic >>= fun str ->
          let s = String.length str in
          let b = hash_function str in
          let info = (b, start, s, (file : string)) in
          let bs' = info :: bs in
          Lwt.return (List.rev bs')
        )
    in
    loop []
  in
  Lwt_io.with_file ~mode:Lwt_io.input file get_words
