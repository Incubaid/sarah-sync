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
      let n = Array.length mat.(0) in
      let checkDim row current_result =
        let dim = Array.length row in
        current_result && (dim == n)
      in
      Array.fold_right checkDim mat true

  (* Dimensions *)
  let rows mat = Array.length mat

  let columns mat = Array.length mat.(0)

  let dimensions mat = (rows mat , columns mat )


  (* Extra function *)
  let zipWith array1 array2 f =
    let result = Array.copy array1 in
    let combine i a =
      result.(i) <- f a array2.(i)
    in
    Array.iteri combine result ;
    result 


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
  let gaussElim (mat : matrix) =
    let m, n = dimensions mat in
    let k = ref 0 in
    let continue = ref true in
    while ( !continue && !k < m ) do
      let i_max = index_max_element !k !k (m-1) mat in
      if mat.(i_max).(!k) =: zero
      then continue := false
      else
        let tmp = mat.(!k) in
        let () = mat.(!k) <- mat.(i_max) in
        let () = mat.(i_max) <- tmp in
        for i = !k + 1 to m - 1 do
          let factor = mat.(i).(!k) /: mat.(!k).(!k) in
          for j = !k + 1 to n - 1 do
            mat.(i).(j) <- mat.(i).(j) -: ( mat.(!k).(j) *: factor )
          done ;
          mat.(i).(!k) <- zero
        done ;
        k := succ (!k)
    done

  (* Determine whether a row contains only zeros *)
  let isZeroRow (row : vector) =
    let b,_ = P.is_zero row in
    b


  (* Determine whether a row is invalid, i.e. (0,0,...,c) *)
  let isInvalidRow (row : vector) =
    let m = Array.length row in
    if row.(m-1) =: zero
    then false
    else 
      begin
        let b, _ = P.is_zero ~start:(m-2) row in
        b
      end


  (* Backward substitution. Takes zero-rows and invalid systems into account. *)
  let backSubst (mat : matrix) =
    let m,n = dimensions mat in
    let solutions = Array.make m zero in
    let rec loop r =
      if r >= 0
      then
        begin
          if isInvalidRow mat.(r)
          then raise System_no_solution
          else
            begin
              if isZeroRow mat.(r)
              then
                begin
                  let () = solutions.(r) <- one in (* Arbitrary solution *)
                  loop (r-1)
                end
              else
                begin
                  let coeffs = Array.sub mat.(r) 0 m in
                  let b = mat.(r).(n-1) in
                  let others = zipWith coeffs solutions ( *: ) in
                  let b' = Array.fold_left (-:) b others in
                  let result = b' /: coeffs.(r) in
                  let () = solutions.(r) <- result in
                  loop (r-1)
                end
            end
        end
    in
    loop (m-1) ;
    solutions

  (* Solve a given system. Gauss elimination, followed by backward substitution. *)
  let solveSystem (system : matrix) =
    if isEmpty system
    then raise Empty_matrix
    else
      begin
        if isValid system
        then
          begin      
            let () = gaussElim system in
            backSubst system 
          end
        else raise Invalid_matrix
      end

end
