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
       exception Matrix_not_Square
       exception Singular_matrix
       exception System_no_solution

       (* Is matrix leeg? *)
       let isEmpty mat = 
          Array.length mat == 0 || Array.length mat.(0) == 0

       (* Ga na of de alle rijen evenveel kolommen hebben *)
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

       (* Dimensies *)
       let rows mat = 
           if isValid mat
           then Array.length mat
           else raise Invalid_matrix

       let columns mat = 
           if isValid mat
           then Array.length mat.(0)
           else raise Invalid_matrix

       let dimensions mat = (rows mat , columns mat )

       (* Ga na of de matrix vierkant is *)
       let isSquare mat = 
           let m, n = dimensions mat in
           m == n
 
       (* Eenheidsmatrix *)
       let identity_matrix n = 
           Array.init n (fun r -> Array.init n (fun c -> if r==c then F.one else F.zero)) ;;

       (* Selecteer kolom uit matrix *)
       let getColumn i mat = 
           let m = rows mat in
           let column = Array.make m F.zero in
           let accumulate j row = column.(j) <- row.(i) in
           Array.iteri accumulate mat ;
           column
  
       (* Hulpfunctie *)
       let zipWith array1 array2 f = 
           let result = Array.copy array1 in
           let combine i a = 
               result.(i) <- f a array2.(i)
           in
           Array.iteri combine result ;
           result 
     
       (* Matrix vermenigvuldiging *)
       let matrix_mult (matrix_A : matrix) (matrix_B : matrix) = 
           if ( isEmpty matrix_A || isEmpty matrix_B )
           then raise Empty_matrix
           else begin 
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

       (* Kopie van matrix *)
       let copy_matrix (mat : matrix) = Array.map Array.copy mat 

       (* Index van maximaal element in een array *)
       let indexMaxElement array = 
         let index = ref 0 in
         let find_max i el = 
           if el > array.(!index)
           then index := i
           else ()
         in 
         Array.iteri find_max array ;
         !index

         (* Gauss eliminatie. Overschrijft de opgegeven matrix.
            Als matrix singulier, wordt de procedure stopgezet. *)
         let gaussElim (mat : matrix) = 
           if isEmpty mat
           then raise Empty_matrix
           else begin
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

       (* Bepaal of rij een nulrij is *)
       let isZeroRow (row : vector) = 
         let row_list = Array.to_list row in
           List.for_all ((=) F.zero) row_list
        

       (* Bepaal of matrix nulrijen heeft *)
       let hasZeroRows (mat : matrix) = 
         let list = Array.to_list mat in 
            List.exists isZeroRow list

       (* Bepaal of rij ongeldig is, i.e. (0,0,...,c) *)
       let isInvalidRow (row : vector) = 
         let m = Array.length row in
         let coeffs = Array.sub row 0 (m - 1) in
         isZeroRow coeffs && row.(m-1) <> F.zero

       (* Achterwaartse substitutie. Houdt rekening  met nulrijen en strijdige stelsels. *)
       let backSubst (mat : matrix) =
         let m,n = dimensions mat in
         let solutions = Array.make m F.zero in
         let rec loop r = 
             if r >= 0
             then  begin
               if isInvalidRow mat.(r)
               then raise System_no_solution
               else begin
                  if isZeroRow mat.(r)
                  then begin
                  let () = solutions.(r) <- F.one in (* Willekeurige oplossing *)
                  loop (r-1)
	          end 
                  else begin
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

       (* Stelsel oplossen *)
       let solveSystem (system : matrix) = 
	 let () = gaussElim system in
         backSubst system


     end
