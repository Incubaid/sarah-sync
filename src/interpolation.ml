(* Interpolation *) 

open FiniteField
open Matrices

module Interpolation =
  functor  (F : FINITEFIELD) ->
     struct
       type element = F.t 

       exception Delta_and_m_different_parity
   
       module M = Matrix(F)    (* Voor de Gauss-eliminatie *)

       (* Stelsel opstellen om te interpoleren.
          m = aantal evalutiepunten.
          delta = |S_A| - |S_B| *)
       let constructSystem (points : element list) (values : element list) m delta = 
	 let d1 = (m + delta) / 2 in  (* Operaties zorgen zelf voor de floor *)
	 let d2 = (m - delta) / 2 in
         if d1 + d2 <> m
         then raise Delta_and_m_different_parity
	 else begin
           let makeRow i = 
	     Array.init (m + 1) 
	       (fun j -> 
		 if j < d1
                 then F.exp (List.nth points i) j
                 else begin
		   if j < m
                   then F.mult (List.nth values i) (F.exp (List.nth points i) (j - d1))
                   else F.min ( F.mult (List.nth values i) (F.exp ( List.nth points i) d2) )
		              ( F.exp (List.nth points i) d1 )
		 end
	       )                
	   in 
	   ( Array.init m makeRow , d1 , d2)
	 end 

       (* Stelsel oplossen *)
       let interpolate (points : element list) (values : element list) m delta = 
	 let system, d1, d2 = constructSystem points values m delta in
         let solution =  M.solveSystem system in
	 let cfsNum = Array.init (d1 + 1) 
                                 ( fun i -> 
				     if i < d1
				     then solution.(i)
				     else F.one ) in    (* Coefficienten teller *)
	 let cfsDenom = Array.init (d2 + 1) 
                                 ( fun i -> 
				     if i < d2
				     then solution.(d1 + i)
				     else F.one ) in    (* Coefficienten noemer *)
	 cfsNum , cfsDenom


     end
