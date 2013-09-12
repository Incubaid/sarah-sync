(* Timing of the execution of a program *)

let time f lastarg =
  let start = Unix.gettimeofday () in
  let res = f lastarg in
  let stop = Unix.gettimeofday () in
  Printf.printf "Execution time: %fs.\n%!" (stop -. start) ;
  res
