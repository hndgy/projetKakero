open Puzzle;;
open TypePuzzle;;

module Data = struct


    
	let init pzz =
		 let rec aux p acc =
	   		match p with
				[] -> acc
	      		| (c,imp) :: t -> aux t (acc@[(c,(imp,0),(Puzzle.ensVoisin pzz c))])
	  	in ((aux pzz []),[])


(*	let contains ldata coord = 
		let rec aux ldata acc =
			match ldata with
				[] -> acc
				| (c,(i,n)) :: t -> if c = coord then aux t true
						else aux t false
		in 
		aux ldata false*)

        





	
       


	let control_collision database (x1,y1) (x2,y2) nb =
	(*retire une ile de la liste des voisins si il y a un pont entre*)


          let cond (xa,ya) (xb,yb) =

	    if x1 = x2 && ya = yb then 
	      (((xa < x1) && (x1 < xb)) || ((xa > x1) &&( x1 > xb) )) &&
		(((y1 < ya) &&( ya< y2) ) || ((y1 < ya) && (ya< y2) ))
	    else

	      (((x1 < xa) && (xa < x2)) || ((x1 > xa) && (xa > x2))) &&
		(((ya < y1) && (y1 < yb)) || ((ya > y1) && (y1 > yb)))
		
		
		
		
	  in

	  
	  let supprCollision c lv =
	    let rec aux lv acc =
	      match lv with
		  [] -> acc
		| h :: t -> if cond c h then aux t acc else
		    aux t (acc@[h]) in
	    
	    aux lv []
	  in

	  let f (c,d,lv) = (c,d,(supprCollision c lv)) in

	  let ldata = fst database and hist = snd database in

	  let del ldata = List.map f ldata in 
	  
	  ((del ldata),hist)
		
	      

	let update datab =
	  let ldata = fst datab and data = snd datab in
	  
	  let update_aux ldata =
	    
	    
	    let rec aux ldata acc supp =
	    (*supprime les elements traite (imp = nbPont) + stock les element supprime dans supp*)
	      match ldata with
		  [] -> acc,supp
		| ((c,(imp,n),_) as h) :: t -> if imp = n then aux t acc (supp@[c])
		  else aux t (acc@[h]) supp
		    

	    in
	    
	    let res = aux ldata [] []
	    in
	    let ld = fst res and lsupp = snd res
	    in
	    let delete e (c,d,l) =
	    (* supprime dans une liste de voisin  (l dans (c,d,l) )l'element e*)
	      let rec aux l acc =
		match l with
		    [] -> acc
		  | h :: t -> if h = e then aux t acc else aux t acc@[h]
	      in
	     
	      (c,d,aux l [])
	    in

	    

	 (*supprime les elements supprimer des liste de voisin de chaque sommets restants*)
	    let rec aux1 supp acc =
	      match supp with
		  [] -> acc
		| h :: t -> aux1 t (List.map (delete h) acc)
	    in
	    aux1 lsupp ld
	  in

	(update_aux ldata,data)

	let connect database nb coord1 coord2  =

	  let database' = control_collision database coord1 coord2 nb in
	  let ldata = (fst database') and hist = (snd database') in

	  
	  
	  let rec modifNb ldata acc =
	    match ldata with
		[] -> acc
	      | ((c,(i,n),v) as h) :: t  ->
		if c = coord1 || c = coord2 then modifNb t acc@[(c,(i,n+nb),v)]
		else modifNb t acc@[h]
	  in
	  let newHist =
	    if (List.mem (coord1,coord2,1) hist) || (List.mem (coord2,coord1,1) hist) then

	      let rec aux hist acc =
		match hist with
		    [] -> acc
		  | ((c1,c2,n) as h) :: t ->
		    if (c1 = coord1 && c2 = coord2) ||  (c1 = coord2 && c2 = coord1)
		    then aux t acc@[(c1,c2,n+1)]
		    else aux t acc@[h]
	      in
	      
	      aux hist []
		
	    else hist@[(coord1,coord2,nb)]
	  
	  in
		  
	  let newDb = ((modifNb ldata []), newHist) in
	  
	  update newDb





	
	let getNbVoisin datab coord =
	  let ldata = fst datab in
	  let rec aux l acc =
	    match l with
		[] -> acc
	      | (c,d,l) :: t -> if c = coord then aux t (List.length l) else aux t acc
	  in if ldata = [] then raise Empty_list
	    else 
	      aux ldata 0


	let map_connect c ldata lv n = (*applique une incr. de "n" a toute la liste de voisin "lv" de "c"*)
	  let rec aux lv acc=
	    match lv with
		[] -> acc
	      | coord :: t -> aux t (connect acc n c coord)
	  in
	  aux lv ldata


	
	
	
end;;
