open Puzzle;;
open TypePuzzle;;

module Data = struct


    
	let init pzz = 
	(*initialise un type database avec un puzzle*)
		 let rec aux p acc =
	   		match p with
				[] -> acc
	      		| (c,imp) :: t -> aux t (acc@[(c,(imp,0),(Puzzle.ensVoisin pzz c))])
	      		(*pour chaque sommet, on créée un trilpet avec les coordonnés, l'importance,
	      		le nombre de pont connectés, et la liste des voisins disponibles *)
	  	in ((aux pzz []),[])




	let control_collision database (x1,y1) (x2,y2) nb =
	(*controle si le pont qu'on ajoute entre (x1,y1) (x2,y2) coupe des liaisons 
entre certain sommets*)


          let cond (xa,ya) (xb,yb) = (*condition pour qu'une liaison entre deux sommets
          soit rompue par un pont créé*)

	    if x1 = x2 && ya = yb then 
	      (((xa < x1) && (x1 < xb)) || ((xa > x1) &&( x1 > xb) )) &&
		(((y1 < ya) &&( ya< y2) ) || ((y1 < ya) && (ya< y2) ))
	    else if y1 = y2 && xa = xb then 
	    (((x1 < xa) && (xa < x2)) || ((x1 > xa) && (xa > x2))) &&
		(((ya < y1) && (y1 < yb)) || ((ya > y1) && (y1 > yb)))
		else false 
		
	  in

	  
	  let supprCollision c lv = (*supprime de la listes des voisins lv les sommets qui ne sont plus 
	  voisin à cause de la création d'un pont entre*)
	    let rec aux lv acc =
	      match lv with
		  [] -> acc
		| h :: t -> if cond c h then aux t acc else
		    aux t (acc@[h]) in
	    
	    aux lv []
	  in

	  let f (c,d,lv) = (c,d,(supprCollision c lv)) in (*applique le la suppression à une composante 
	  de la liste*)

	  let ldata = fst database and hist = snd database in

	  let del ldata = List.map f ldata in (*applique la suppression à tous les elements de la liste*)
	  
	  ((del ldata),hist)
		
	      

	let update datab =
	(*met à jour la database: supprime les sommet traité, ie ou le nombre de pont vaut l'importance*)
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

	 (*supprime les elements supprimés des liste de voisin de chaque sommets restants*)
	    let rec aux1 supp acc =
	      match supp with
		  [] -> acc
		| h :: t -> aux1 t (List.map (delete h) acc)
	    in
	    aux1 lsupp ld
	  in

	(update_aux ldata,data)


	exception NbPontIncorrect

	let connect database nb coord1 coord2  =
	(*connect deux sommets de coordonnés coord1 coord2 avec un nombere nb de ponts*)

	  let database' = control_collision database coord1 coord2 nb in (*on fait *)
	  let ldata = (fst database') and hist = (snd database') in

	  
	  
	  let rec modifNb ldata acc =
	  (*modifie le nombre de pont connecté pour les deux coorconné*)
	    match ldata with
		[] -> acc
	      | ((c,(i,n),v) as h) :: t  ->
		if (c = coord1 || c = coord2) && (nb + n) <= i then modifNb t acc@[(c,(i,n+nb),v)]
		else if (c = coord1 || c = coord2) && (nb + n) > i then raise NbPontIncorrect
		else modifNb t acc@[h]
	  in
	  let newHist = (*ajoute l'opération dans l'historique :
	  si il y a deja un pont de connecté alors on ajoute un autre pont 
	  sinon on ajoute un element (coord1,coord2,nb)*)
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
		  
	  try update ((modifNb ldata []), newHist) with NbPontIncorrect -> database
	  (*retourne la nouvelle database si le nb de pont est possible à ajouter sinon on retourne 
	  celle donnée en argument *)
	  
	   



	let map_connect c ldata lv n = 
	(*ajoute n pont entre c et ses voisins*)
	  let rec aux lv acc=
	    match lv with
		[] -> acc
	      | coord :: t -> aux t (connect acc n c coord)
	  in
	  aux lv ldata

	
	  

       
	
	
	
end;;
