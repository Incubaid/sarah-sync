(* Matrix module *)

open FiniteField
open Polynom

module Matrix =
  functor  (F : FINITEFIELD) ->
struct
  open F

  type element = t
  type matrix = element array array
  type vector = element array

  module P = Polynom(F)

  exception Empty_matrix
  exception Invalid_matrix
  exception System_no_solution

  (* Is matrix empty? *)
  let isEmpty mat =
    Array.length mat == 0 || Array.length mat.(0) == 0


  (* Verify whether all rows have the same number of columns *)
  let isValid mat =
    if isEmpty mat
    then raise Empty_matrix
    else
      let m = Array.length mat in
      let n = Array.length mat.(0) in
      let rec loop row =
        if row == m   (* All rows have been checked *)
        then true 
        else
          let dim = Array.length mat.(row) in
          if dim == n
          then loop (row + 1)
          else false
      in
      loop 1


  (* Dimensions *)
  let rows mat = Array.length mat

  let columns mat = Array.length mat.(0)

  let dimensions mat = (rows mat , columns mat )


  (* Index of maximal element in part of a column of a matrix *)
  let index_max_element col start stop (mat : matrix) =
    let rec find_max i curr_max_index =
      if i > stop
      then curr_max_index
      else
        begin
          let curr_max_index' =
            if mat.(col).(i) > mat.(col).(curr_max_index)
            then i
            else curr_max_index
          in
          find_max (i + 1) curr_max_index'
        end
    in
    find_max (start + 1) start


  (* Gauss elimination. Overwrites the matrix.
     When matrix is singular, the procedure terminates. *)
  let gauss_elim (mat : matrix) =
    let m, n = dimensions mat in
    let rec loop k =
      if k = m
      then () 
      else
        begin
          let i_max = index_max_element k k (m-1) mat in
          if mat.(i_max).(k) =: zero
          then ()
          else
            begin
              let tmp = mat.(k) in
              let () = mat.(k) <- mat.(i_max) in
              let () = mat.(i_max) <- tmp in
              for i = k + 1 to m - 1 do
                let factor = mat.(i).(k) /: mat.(k).(k) in
                for j = k + 1 to n - 1 do
                  mat.(i).(j) <- mat.(i).(j) -: ( mat.(k).(j) *: factor )
                done ;
              done ;
              loop (k + 1)
            end
        end
    in
    loop 0


  (* Determine whether a row contains only zeros *)
  let is_zero_row (row : vector) =
    let b,_ = P.is_zero (row, Array.length row - 1) in
    b


  (* Determine whether a row is invalid, i.e. (0,0,...,c) *)
  let is_invalid_row (row : vector) =
    let m = Array.length row in
    if row.(m-1) =: zero
    then false
    else
      begin
        let b, _ = P.is_zero ~start:(m-2) (row, m-1) in
        b
      end


  (* Get the solution for the variable corresponding to the column *)
  let get_solution (mat : matrix) last_kol kol b sols =
    let init_res = b in
    let rec get res i =
      if i = kol
      then res /: mat.(kol).(kol)
      else
        begin
          let res' = res -: (mat.(kol).(i) *: sols.(i)) in
          get res' (i - 1)
        end
    in
    get init_res last_kol
  

  (* Backward substitution. Takes zero-rows and invalid systems into account. 
     Once one valid row has been found, no invalid or zero-rows can occur anymore. *)
  let back_subst (mat : matrix) =
    let m,n = dimensions mat in
    let sols = Array.make m zero in
    let rec handle_zero_or_invalid r =
      if is_invalid_row mat.(r)
      then raise System_no_solution
      else
        begin
          if is_zero_row mat.(r)
          then
            begin
              let () = sols.(r) <- one in      (* Arbitrary solution *)
              handle_zero_or_invalid (r-1)
            end
          else r                               (* Returns the first non-zero row *)
        end
    in
    let r_init = handle_zero_or_invalid (m - 1) in
    let rec loop r =
      if r >= 0
      then
        begin
          let b = mat.(r).(n-1) in
          let res = get_solution mat (m - 1) r b sols in
          let () = sols.(r) <- res in
          loop (r-1)
        end
      else ()
    in
    loop r_init ;
    sols


  (* Solve a given system. Gauss elimination, followed by backward substitution. *)
  let solveSystem (system : matrix) =
    if isEmpty system
    then raise Empty_matrix
    else
      begin
        if isValid system
        then
          begin
            let () = gauss_elim system in
            back_subst system
          end
        else raise Invalid_matrix
      end

end
