






exception Pas_de_voisin;;

exception Empty_list;;




        

let getVoisinGauche p coord =
  let i = fst coord and j =  snd coord 
  in
  let maxX l =
    let rec aux l acc mx=
      match l with
	  [] -> acc
	| (x,y)::t -> if x > mx then aux t (x,y) x
	  else
	    aux t acc mx
    in
    aux (List.tl l) (List.hd l) (fst (List.hd l))
  in

  let rec aux p acc =
    match p with
	[] -> acc
      | ((x,y),_)::t -> if x < i && y = j then aux t (acc@[(x,y)]) else aux t acc
  in
  if (aux p []) = [] then raise Pas_de_voisin else
    maxX (aux p []);;



let getVoisinDroit p coord =
  let i = fst coord and j =  snd coord 
  in
  let minX l =
    let rec aux l acc mx=
      match l with
	  [] -> acc
	| (x,y)::t -> if x < mx then aux t (x,y) x
	  else
	    aux t acc mx
    in
    aux (List.tl l) (List.hd l) (fst (List.hd l))
  in

  let rec aux p acc =
    match p with
	[] -> acc
      | ((x,y),_)::t -> if x > i && y = j then aux t (acc@[(x,y)]) else aux t acc
  in
  if (aux p []) = [] then raise Pas_de_voisin else
    minX (aux p []);;






let getVoisinHaut p coord =
  let i = fst coord and j =  snd coord 
  in

  let maxY l =
    let rec aux l acc my=
      match l with
	  [] -> acc
	| (x,y)::t -> if y > my then aux t (x,y) y
	  else
	    aux t acc my
    in
    aux (List.tl l) (List.hd l) (snd (List.hd l))
  in
  
  let rec aux p acc =
    match p with
	[] -> acc
      | ((x,y),_)::t -> if x = i && y < j then aux t (acc@[(x,y)]) else aux t acc
  in

  if (aux p []) = [] then raise Pas_de_voisin else
    maxY (aux p []);;



let getVoisinBas p coord =
  let i = fst coord and j =  snd coord 
  in
	    (* let minY l =
	       let rec aux l acc my=
	       match l with
	       [] -> acc
	       | (x,y)::t -> if y < my then aux t (x,y) y
	       else
	       aux t acc my
	       in
	       let h = List.hd l
	       in
	       let y = snd h
	       in
	       
	       aux (List.tl l) h y
	       in*)
  
  let rec aux p acc =
    match p with
	[] -> acc
      | ((x,y),_)::t -> if x = i && y > j then aux t (acc@[(x,y)]) else aux t acc
  in

  if (aux p []) = [] then raise Pas_de_voisin else List.hd (aux p [])
    
	  (*if (aux p []) = [] then raise Pas_de_voisin else
	    minY (aux p [])*)
;;


  





let ensVoisin p coord =
  let h = try [getVoisinHaut p coord] with Pas_de_voisin -> []
  and d = try [getVoisinDroit p coord] with Pas_de_voisin -> []
  and b = try [getVoisinBas p coord] with Pas_de_voisin -> []
  and g = try [getVoisinGauche p coord] with Pas_de_voisin -> []

  in
  h@d@b@g ;;

ensVoisin p (4,4);;




		  
		

      
      
     

		       
			   
module Data = struct


    
	let init pzz =
		 let rec aux p acc =
	   		match p with
				[] -> acc
	      		| (c,imp) :: t -> aux t (acc@[(c,(imp,0),(ensVoisin pzz c))])
	  	in ((aux pzz []),[])


(*	let contains ldata coord = 
		let rec aux ldata acc =
			match ldata with
				[] -> acc
				| (c,(i,n)) :: t -> if c = coord then aux t true
						else aux t false
		in 
		aux ldata false*)

        





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


	let imp_of_coord ldata c =
	  let rec aux ldata acc =
	    match ldata with
		[] -> acc
	      | (coord,(_,imp),lv) :: t -> if c = coord then aux t imp else aux t acc
	  in
	  aux ldata 0


	
	
	
end;;






(*

type nbPont = int

type data_base = ((coordinate * (importance * nbPont )) * (coordinate list) ) list 


type data = (coordinate * coordinate * nbPont) list 

args :

datab = data base = (data_base,data)

ldata = data_base

data = data  

(c,d,l) = ( coord ,(imp, nbPont),liste_voisins)

*)




let p = [((2,0),2);((0,2),3);((2,2),8);((4,2),4);((0,4),3);((2,4),5);((4,4),3)];;

let d = Data.init p;;


let p1 = [((0,0),4) ; ((3,0),4) ; ((6,0),3);

	 ((1,2),1) ; ((3,2),4) ; ((5,2),2);
	 ((0,3),4);((6,3),5);
	 ((0,5),2) ; ((5,5),1);
	 ((2,6),1) ; ((4,6),3) ; ((6,6),4)];;

Solution.init p1 ;;

let di = Data.init p1 ;;








strategie1 (Data.init p1);;

  
solve p1;;


let pT = [((0,2),2);((2,0),1);((2,4),1);((4,2),2)];;  

let db  = Data.init pT;;


Data.connect db 1 (2,0) (2,4);;

;;



let f8  database = (*traite tous les sommets d'imp 8*)
  let ldata = fst database in 
    let rec aux ldata acc =
      
      match ldata with
	  [] -> acc
	| (c,(imp,nbPont),lv) :: t ->
	  if imp = 8 then aux t  (Data.map_connect c acc lv 2)
	  else aux t acc
    in
    aux ldata database
;;


let f6  database = (*traite tous les sommets d'imp 6*)
  (* si c'est un sommet d'importance 6 :
     si il a 3 voisins dispos et qu'il est connecté a 0 pont alors on le connecte avec des doubles ponts a chacun de ses 3 voisins
     meme chose pour 2 voisins et 2 ponts deja connectés
     et pour 1 voisin et 4 ponts deja connectés *)
  let ldata = fst database in 

  let rec aux ldata acc =
    
    match ldata with
	[] -> acc
      | (c,(imp,nbPont),lv) :: t ->

	if imp = 6 then
	  let nbV = List.length lv in
	  
	  if ( (nbV = 3) && nbPont = 0) 
	  then aux t  (Data.map_connect c acc lv 2)

	  else if ((nbV = 2) && nbPont = 2)
	  then aux t (Data.map_connect c acc lv 2)

	  else if ((nbV = 3) && nbPont = 1)
	  then aux t (Data.map_connect c acc lv 1)
	    
	  else if ( ((nbV = 1) && nbPont = 4)
	  then aux t (Data.connect acc 2 c (List.hd lv))
	      

	  else aux t acc
	    
	  else aux t acc
  in
  aux ldata database
;;

  

let f4  database = (*traite tous les sommets d'imp 4*)
  let ldata = fst database in 
  let rec aux ldata acc =
    
    match ldata with
	[] -> acc
      | (c,(imp,nbPont),lv) :: t ->
	if imp = 4 then 
	  let nbV = List.length lv in

	  
	  
	  if ((nbV = 2) && nbPont = 0)
	  then aux t (Data.map_connect c acc lv 2)

	  else if nbV = 2 && nbPont = 1
	  then aux t (Data.map_connect c acc lv 1)
	    
	  else if ( (nbV = 1) && nbPont = 2)
	  then aux t (Data.connect acc 2 c (List.hd lv))
	    
	  else if ( (nbV = 1) && nbPont = 3)
	  then aux t (Data.connect acc 1 c (List.hd lv))

	      
	  else aux t acc
	    
	  else aux t acc
  in
  aux ldata database
;;


	     
   let f2 database = (*traite tous les sommets d'imp 4*)
     let ldata = fst database in 
     let rec aux ldata acc =
       
       match ldata with
	   [] -> acc
	 | (c,(imp,nbPont),lv) :: t ->
	   if imp = 2 then
	     let nbV = List.length lv in 
	     
	     if (nbV = 1) && nbPont = 0
	     then aux t  (Data.connect acc 2 c (List.hd lv))    
	       
	     else if  (nbV  = 1) && nbPont = 1
	     then aux t  (Data.connect acc 1 c (List.hd lv))
	       
	     else aux t acc

	     else aux t acc
     in
     aux ldata database
   ;;


   


   let f1  database = (*traite tous les sommets d'imp 4*)
     let ldata = fst database in 
      let rec aux ldata acc =
        
	match ldata with
	    [] -> acc
	  | (c,(imp,nbPont),lv) :: t ->
	    if ((imp = 1) && ((List.length lv) = 1))
	    then aux t  (Data.connect acc 1 c (List.hd lv))
	    else aux t acc
      in
      aux ldata database
;;


   
   let f3 database = (*traite tous les sommets d'imp 4*)
     let ldata = fst database in 
     let rec aux ldata acc =
       
       match ldata with
	   [] -> acc
	 | (c,(imp,nbPont),lv) :: t ->
	   if imp = 3 then
	     let nbV = List.length lv in 
	     if ( nbV = 1 && nbPont = 1 )
	     then aux t  (Data.connect acc 2 c (List.hd lv))
	       
	     else if ( nbV = 1 && nbPont = 2 )
	     then aux t  (Data.connect acc 1 c (List.hd lv))
	       
	     else if nbV = 2 && nbPont = 0
	     then aux t (Data.map_connect c acc lv 1)


	     else aux t acc
	       
	     else aux t acc
     in
     aux ldata database

;;


   let f5  database = (*traite tous les sommets d'imp 4*)
     let ldata = fst database in 
     let rec aux ldata acc =
       
       match ldata with
	   [] -> acc
	 | (c,(imp,nbPont),lv) :: t ->
	   if imp = 5 then
	     let nbV = List.length lv in
	     if ( (nbV = 1) && nbPont = 3) 
	     then aux t  (Data.connect acc 2 c (List.hd lv))

	     else if nbV = 1 && nbPont = 4
	     then aux t  (Data.connect acc 1 c (List.hd lv))  

	     else if ( (nbV = 2) && (nbPont = 1))
	     then aux t (Data.map_connect c acc lv 2)
	       
	     else if ((nbV = 2 ) && (nbPont = 2 ))
	     then aux t  (Data.map_connect c acc lv 1)

	     else if nbV = 3 && nbPont = 0
	     then aux t  (Data.map_connect c acc lv 1)

	     else if nbV = 2 && nbPont = 3
	     then aux t  (Data.map_connect c acc lv 1)


	     
	     else
	       aux t acc 

	   else aux t acc
     in
     aux ldata database
;;


   let f7  database = (*traite tous les sommets d'imp 4*)
     let ldata = fst database in 
     let rec aux ldata acc =
       
       match ldata with
	   [] -> acc
	 | (c,(imp,nbPont),lv) :: t ->
	   if imp = 7 then
	     let nbV = List.length lv in
	     
	     if ( (nbV = 1) && nbPont = 5) 
	     then aux t  (Data.connect acc 2 c (List.hd lv))

	     else if ( (nbV = 1) && nbPont = 6)
	     then aux t  (Data.connect acc 1 c (List.hd lv))
	       
	       
	     else if nbV = 3 && nbPont = 1
	     then  aux t  (Data.map_connect c acc lv 2)

	     else if nbV = 4 && nbPont = 0
	     then  aux t  (Data.map_connect c acc lv 1)
	       
	     else if nbV = 3 && nbPont = 2
	     then  aux t  (Data.map_connect c acc lv 1)

	     else if nbV = 2 && nbPont = 3
	     then  aux t  (Data.map_connect c acc lv 2)

	     else if nbV = 2 && nbPont = 4 
	     then  aux t  (Data.map_connect c acc lv 1)
	     else aux t acc
	       
	       
	     else aux t acc
     in
     aux ldata database
   ;;




 

  
    
  
	    
   let strategie1  data_base =
    (* on traite les importances a tour de role mais
				d'abord les paires puis les impaires*)
    let f db =  f7(f6 (f2 (f1 (f5 (f3 (f4 (f8 db)))))))

    in

    let rec boucle db = (*boucle qui s'arrete quand il n'y plus de modification possible*)
      let res = f db in
      if db = res then res
      else
	match fst res with
	  [] -> res
	| _ -> boucle res
    in
    
		       
    boucle data_base 
    ;;


    								 


       
	    
	    
strategie1 d ;;



type coordinate = int * int

type importance = int 

type vertex = coordinate * importance

type puzzle = vertex list 

type bridge = { isVertical : bool; isDoubled : bool } 

type cell =
		 Nothing 
		| Island of importance
		| Bridge of bridge 
	

type solution = cell list list

type nbPont = int

type liste_data = ((coordinate * (importance * nbPont )) * (coordinate list) ) list 

  
type historique = (coordinate * coordinate * nbPont) list

type data_base = liste_data * historique

module Solution =
struct


 

    
 let empty n =
  let aux_ligne n =
    let rec aux n acc =
      match n with
	  0 -> acc
	| i -> aux (i-1) (Nothing::acc)
    in
    aux n []
  in
  let l = aux_ligne n
  in
  
  let rec aux_sol_vide n acc =
    match n with
	0-> acc
      | i -> aux_sol_vide (i-1) (l::acc)
  in
  aux_sol_vide n []



 let add (x,y) c s = (* ajoiute la cell c en (x,y) dans s*)
   let addLine l x =
     let rec aux l x acc =
       match l,x with
	   [],_ -> acc
	 | h :: t,i ->
	   if i = 0 then aux t (i-1) (acc@[c])
	   else
	     aux t (i-1) (acc@[h])
     in
     aux l x []
   in
   let rec aux s y acc =
     match s,y with
	 [],_ -> acc
       | h :: t, i ->
	 if i = 0 then aux t (i-1) (acc@[addLine h x])
	 else aux t (i-1) (acc@[h])
   in
   aux s y []

 let addIsland c imp s = add c (Island imp) s

 let addBridge c isV isD s = add c (Bridge {isVertical = isV ; isDoubled = isD}) s

 let addBridgeH s taille (x,y) isD =
   let rec aux n acc =
     match n with
	 i when i = taille -> acc
       | i -> aux (i+1) (addBridge (x+i,y) false isD acc)
   in
   aux 0 s

 let addBridgeV s taille (x,y) isD =
   let rec aux n acc =
     match n with
	 i when i = taille -> acc
       | i -> aux (i+1) (addBridge (x,y+i) true isD acc)
   in
   aux 0 s
     

 let relier s (((x1,y1) as c1),((x2,y2) as c2),n) =
   let isD = n = 2
   and isV = x1 = x2
   and t = Puzzle.distance p c1 c2 in

   if isV then
     if y1 < y2 then addBridgeV s t (x1,y1+1) isD
     else addBridgeV s t (x2,y2+1) isD
   else
     if x1 < x2 then addBridgeH s t (x1+1,y1)  isD
     else addBridgeH s t (x2+1,y2) isD
     

   
   






 let init p =
  let rec aux p acc =
     match p with
	 [] -> acc
       | (c,imp) :: t -> aux t (addIsland c imp acc)  
  in
  aux p (empty (Puzzle.size p))

 


end;;

let s1 = Solution.init p;;

Solution.relier s1 ((2, 2), (2, 0), 2);;


  


exception Sommet_incompatible;;

module Puzzle =
struct

  let size p =
    let max x1 x2 = if x1 > x2 then x1 else x2 in 
    let rec maxAbs p m =
      match p with
	  [] -> m
	| ((x,_),_) :: t ->  maxAbs t (max x m)
    in
     let rec maxOrd p m =
      match p with
	  [] -> m
	| ((_,y),_) :: t ->  maxOrd t (max y m)
     in

     ( max (maxAbs p 0) (maxOrd p 0)) + 1



  let distance p (x1,y1) (x2,y2) =

    let abs a = if a < 0 then -a else a in
    if x1 = x2 then abs (y1 - y2) - 1
    else if y1 = y2 then abs (x1 - x2) - 1
    else raise Sommet_incompatible

 
    

end ;;




let solve p =
   let resStrategie1 = strategie1 (Data.init p) in
   let hist = snd resStrategie1 in
    let rec aux l acc =
    match l with
	[] -> acc
      | h :: t -> aux t (Solution.relier acc h)
    in
    aux hist (Solution.init p);;
   


solve p;;


let p2 = [((2,0),1);((4,0),3);((6,0),1);((0,1),2);((5,1),1);((2,2),4);((4,2),5);((0,3),4);((4,5),1);((0,6),3);((2,6),3);((5,6),2)];;




strategie1 (Data.init p2);;


let p4 = [((0,2),1);((0,4),1);((0,6),3);((1,0),2);((3,0),6);((3,2),6);((3,6),5);((4,3),1);((4,5),1);((5,2),2);((5,6),2);((6,0),3);((6,3),3);((6,5),2)];;
strategie1 (Data.init p4);;
