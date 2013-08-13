(* Module to perform Chien search *)

open FiniteField

module Chien =
  functor  (F : FINITEFIELD) ->
     struct
       type element = F.t 
       type polynom = element array (* Coefficienten *)
       type roots = element list    (* Wortels *)

       (* Chien-search algoritme. Gammas overschrijven steeds de coeffs-array.
          Coefficienten geordend naar stijgende machten van x. *)
       let chienSearch (coeffs : polynom) = 
         let roots = ref ([] : roots) in
          let foundRoot root =
           roots := root :: !roots ;
	 in
         if coeffs.(0) = F.zero then foundRoot F.zero ;
         let q = int_of_float (2. ** (float_of_int F.w)) in
 	 let updateGamma j _ =  
	     coeffs.(j) <- F.mult coeffs.(j) (F.exp F.primEl j)
	 in 
	 let element = ref (F.one) in    
         for i = 0 to q - 2 do
	   let sum = Array.fold_left F.plus F.zero coeffs in
           if sum = F.zero then foundRoot !element ;
	   element := F.mult !element F.primEl ;
           Array.iteri updateGamma coeffs
	 done ;
         !roots

       (* Chien-search update, met tellen wortels zodat het eventueel niet alle elementen moet bekijken. *)
       let chienSearch2 (coeffs : polynom) = 
         let t = Array.length coeffs - 1 in (* Graad veelterm *)
         let roots = ref ([] : roots) in
          let foundRoot root =
           roots := root :: !roots ;
	 in
         if coeffs.(0) = F.zero then foundRoot F.zero ;
         let q = int_of_float (2. ** (float_of_int F.w)) in
 	 let updateGamma j _ =  
	     coeffs.(j) <- F.mult coeffs.(j) (F.exp F.primEl j)
	 in 
	 let element = ref (F.one) in   
         let counter = ref 0 in
         while ( List.length !roots <> t && !counter < q - 1) do
	   let sum = Array.fold_left F.plus F.zero coeffs in
           if sum = F.zero then foundRoot !element ;
	   element := F.mult !element F.primEl ;
           Array.iteri updateGamma coeffs ;
           counter := succ (!counter)
	 done ;
         !roots
       
     end
