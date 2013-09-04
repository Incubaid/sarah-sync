(* Reading of file *)


(* Returns a list of the lines in a file, with a newline appended, except for the last one. *)
let lines file =
  let chan = open_in file in
  let list = Std.input_list chan in
  List.mapi (fun i line -> if i = List.length list - 1 then line else line ^ "\n") list


(* Returns a list of blocks of the file. 
   The blocks have all have the same size, except maybe the first one. *)
let blocks file ~size =
  let chan = open_in file in
  let as_string = Std.input_all chan in
  let rec collect start b_list =    (* Traverse list backwards *)
    if start = -1
    then b_list
    else
      begin
        let s = min size (start + 1) in
        let block = String.sub as_string (start - s + 1) s in
        let start' = start - s in
        let b_list' = block :: b_list in
        collect start' b_list'
      end
  in
  let list = collect (String.length as_string - 1) [] in
  close_in chan ;
  list 


(* Return a list of words, i.e. the file is split on whitespace *)
let words file =
  let chan = open_in file in
  let as_string = Std.input_all chan in
  let ws' = Str.full_split (Str.regexp "[ \t]+") as_string in
  let extract result =
    match result with
    | Str.Text s -> s
    | Str.Delim s -> s
  in
  let ws = List.map extract ws' in
  close_in chan ;
  ws


(* Return file split on whitespace, with blocks of approximately a given size *)
let blocks_using_whitespace file ~size = 
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
        let block = String.sub as_string (start - s + 1)  s in
        let start' = start - s in
        let b_list' = block :: b_list in
        collect start' b_list'
      end
  in
  let list = collect (String.length as_string - 1) [] in
  close_in chan ;
  list
