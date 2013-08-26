(* Reading of file *)

(* Extra function *)
let extract_string (res : Str.split_result) =
  match res with
  | Str.Text s -> s
  | Str.Delim s -> s


(* Returns a list of the lines in a file. *)
let lines file hash_function =
  let chan = open_in file in
  let as_string = Std.input_all chan in 
  let rec loop current_pos blocks =
    try
      let pos = Str.search_forward (Str.regexp "[\n]+") as_string current_pos in
      let size = pos - current_pos + 1 in
      let hash = hash_function (String.sub as_string current_pos size) in
      let blocks' = (hash, current_pos, size, file) :: blocks in
      loop (pos + 1) blocks'
    with Not_found ->
      if current_pos <> String.length as_string
      then 
        begin
          let size = String.length as_string - current_pos in
          let hash = hash_function (String.sub as_string current_pos size) in
          let blocks' = (hash, current_pos, size, file) :: blocks in
          blocks'
        end
      else
        blocks           
  in
  let all_lines = List.rev (loop 0 []) in
  close_in chan ;
  all_lines



(* Returns a list of blocks of the file. 
   The blocks have all have the same size, except maybe the first one. *)
let blocks file hash_function ~size =
  let chan = open_in file in
  let as_string = Std.input_all chan in
  let rec collect start b_list =    (* Traverse list backwards *)
    if start = -1
    then b_list
    else
      begin
        let s = min size (start + 1) in
        let block = hash_function (String.sub as_string (start - s + 1) s) in
        let start' = start - s in
        let b_list' = (block, start - s + 1, s, file) :: b_list in
        collect start' b_list'
      end
  in
  let list = collect (String.length as_string - 1) [] in
  close_in chan ;
  list

(* Return a list of words, i.e. the file is split on whitespace *)
let words file hash_function =
  let chan = open_in file in
  let as_string = Std.input_all chan in
  let ws' = Str.full_split (Str.regexp "[ \t]+") as_string in
  let current_pos = ref 0 in
  let extract result =
    let s = extract_string result in
    let size = String.length s in
    let hash = hash_function s in
    let begin_pos = !current_pos in
    current_pos := !current_pos + size ;
    (hash, begin_pos, size, file)
  in
  let ws = List.map extract ws' in
  close_in chan ;
  ws

(* Return file split on whitespace, with blocks of approximately a given size *)
let blocks_using_whitespace file hash_function ~size = 
  let chan = open_in file in
  let as_string = Std.input_all chan in
  let rec collect start b_list =    (* Traverse list backwards *)
    if start = -1
    then b_list
    else
      begin
        let s = 
          if start + 1 < size
          then start + 1
          else 
            begin
              let pos = 
                try
                  Str.search_backward (Str.regexp "[ \t\n]+") as_string (start - size + 1)
                with Not_found ->
                  start - size
              in
              start - pos
            end
        in
        let block = hash_function (String.sub as_string (start - s + 1)  s) in (* Hash of the block *)
        let start' = start - s in
        let b_list' = (block, start - s + 1, s, file) :: b_list in
        collect start' b_list'
      end
  in
  let list = collect (String.length as_string - 1) [] in
  close_in chan ;
  list 

(* Get the block from the specified location *)
let get_block begin_pos size file =
  let chan = open_in file in
  let as_string = Std.input_all chan in
  let block = String.sub as_string begin_pos size in
  close_in chan ;
  Printf.printf "Getting block.\n%!" ;
  block
;; 


(* Tests *)
(*(*let file1 = "/home/spare/Documents/FilesOmTeSyncen/old/fischer.txt" in *)
(*let file2 = "/home/spare/Documents/FilesOmTeSyncen/new/fischer.txt" in *)
(*let file1 = "/home/spare/Documents/FilesOmTeSyncen/old/small.txt" in
  let file2 = "/home/spare/Documents/FilesOmTeSyncen/new/small.txt" in *)
let file1 = "/home/spare/Documents/FilesOmTeSyncen/old/big.bmp" in
let file2 = "/home/spare/Documents/FilesOmTeSyncen/new/big.bmp" in
let size = 4000 in
let bs1 = blocks file1 ~size in
let bs2 = blocks file2 ~size in
(*let bs2 = words file2 in 
  print_int (List.length bs1) ; print_string " "; print_int (List.length bs2) ; print_newline (); *)
print_string "BLOCKS OF OLD\n" ;
(*List.iter (fun el -> (print_string el ; print_newline () )) bs1  ;*)
Printf.printf "%i\n" (List.length bs1);
print_string "\nBLOCKS OF NEW\n" ;
Printf.printf "%i\n" (List.length bs2)
(*   List.iter (fun el -> (print_string el)) bs2 ;
   print_newline () *) *)
