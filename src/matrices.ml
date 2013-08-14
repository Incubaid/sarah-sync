(* Matrix module *)

open FiniteField

module Matrix = 
  functor  (F : FINITEFIELD) ->
struct
  type element = F.t 
  type matrix = element array array
  type vector = element array
    
  exception Empty_matrix
  exception Invalid_matrix
  exception Incompatible_matrix_dimensions
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
  let rows mat = 
    if isValid mat
    then Array.length mat
    else raise Invalid_matrix

  let columns mat = 
    if isValid mat
    then Array.length mat.(0)
    else raise Invalid_matrix

  let dimensions mat = (rows mat , columns mat )

  (* Verify whether the matrix is square *)
  let isSquare mat = 
    let m, n = dimensions mat in
    m == n
      
  (* Construct an identity matrix of a given size *)
  let identity_matrix n = 
    Array.init n (fun r -> Array.init n (fun c -> if r==c then F.one else F.zero))

  (* Select column from a matrix *)
  let getColumn i mat = 
    let m = rows mat in
    let column = Array.make m F.zero in
    let accumulate j row = column.(j) <- row.(i) in
    Array.iteri accumulate mat ;
    column
      
  (* Extra function *)
  let zipWith array1 array2 f = 
    let result = Array.copy array1 in
    let combine i a = 
      result.(i) <- f a array2.(i)
    in
    Array.iteri combine result ;
    result 
      
  (* Matrix multiplication *)
  let matrix_mult (matrix_A : matrix) (matrix_B : matrix) = 
    if ( isEmpty matrix_A || isEmpty matrix_B )
    then raise Empty_matrix
    else 
      begin 
        let mA, nA = dimensions matrix_A in
        let mB, nB = dimensions matrix_B in
        if nA != mB
        then raise Incompatible_matrix_dimensions
        else 
	  let result = Array.make_matrix mA nB F.zero in
	  for i = 0 to mA - 1 do
	    for j = 0 to nB - 1 do
              let row = matrix_A.(i) in
              let column = getColumn j matrix_B in
              let products = zipWith row column F.mult in
              let cell = Array.fold_right F.plus products F.zero in
              result.(i).(j) <- cell
	    done
	  done ;
	  result 
      end 

  (* Copy of a matrix *)
  let copy_matrix (mat : matrix) = Array.map Array.copy mat 

  (* Index of the maximal element of an array *)
  let indexMaxElement array = 
    let index = ref 0 in
    let find_max i el = 
      if el > array.(!index)
      then index := i
      else ()
    in 
    Array.iteri find_max array ;
    !index

  (* Gauss elimination. Overwrites the matrix.
     When matrix is singular, the procedure terminates. *)
  let gaussElim (mat : matrix) = 
    if isEmpty mat
    then raise Empty_matrix
    else 
      begin
        let m, n = dimensions mat in
	let k = ref 0 in
	let continue = ref true in
	while ( !continue && !k < m ) do
	  let colK = Array.sub (getColumn !k mat) !k (m - !k) in 
	  let i_max = (indexMaxElement colK) + !k in
	  if mat.(i_max).(!k) = F.zero
	  then continue := false
	  else 
	    let tmp = Array.copy mat.(!k) in
	    let () = mat.(!k) <- Array.copy mat.(i_max) in
	    let () = mat.(i_max) <- tmp in
	    for i = !k + 1 to m - 1 do
              for j = !k + 1 to n - 1 do
                mat.(i).(j) <- F.min mat.(i).(j) ( F.mult mat.(!k).(j) (F.div mat.(i).(!k) mat.(!k).(!k)) )
	      done ;
              mat.(i).(!k) <- F.zero
	    done ;
	    k := succ (!k)
	done
      end 

  (* Determine whether a row contains only zeroes *)
  let isZeroRow (row : vector) = 
    let row_list = Array.to_list row in
    List.for_all ((=) F.zero) row_list
      
  (* Determine whether a matrix contains zero-rows *)
  let hasZeroRows (mat : matrix) = 
    let list = Array.to_list mat in 
    List.exists isZeroRow list

  (* Determine whether a row is invalid, i.e. (0,0,...,c) *)
  let isInvalidRow (row : vector) = 
    let m = Array.length row in
    let coeffs = Array.sub row 0 (m - 1) in
    isZeroRow coeffs && row.(m-1) <> F.zero

  (* Backward substitution. Takes zero-rows and invalid systems into account. *)
  let backSubst (mat : matrix) =
    let m,n = dimensions mat in
    let solutions = Array.make m F.zero in
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
                  let () = solutions.(r) <- F.one in (* Arbitrary solution *)
                  loop (r-1)
	        end 
              else 
		begin
                  let coeffs = Array.sub mat.(r) 0 m in
                  let b = mat.(r).(n-1) in
                  let others = zipWith coeffs solutions F.mult in
                  let b' = Array.fold_left F.min b others in
                  let result = F.div b' coeffs.(r) in
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
    let () = gaussElim system in
    backSubst system

end
