






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

        



	let incr_nbPont datab nb coord1 sommet  =
	   let ldata = (fst datab) and data = (snd datab) in
	  
	  
	  let coord2 = (fun (x,y,z) -> x) sommet in
	  
		let rec aux ldata acc=
			match ldata with
			[] -> acc
			  | ((c,(i,n),v) as h) :: t  ->
			    if c = coord1 || c = coord2 then aux t acc@[(c,(i,n+nb),v)]
				else aux t acc@[h]
		in
	        ((aux ldata []), data@[(coord1,coord2,nb)])



	let incr_nbPont2 datab nb coord1 coord2  =

	  let ldata = (fst datab) and data = (snd datab) in
	  
	  
	  let rec aux ldata acc =
	    match ldata with
		[] -> acc
	      | ((c,(i,n),v) as h) :: t  ->
		if c = coord1 || c = coord2 then aux t acc@[(c,(i,n+nb),v)]
		else aux t acc@[h]
	  in
	  ((aux ldata []), data@[(coord1,coord2,nb)])

       



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




let d1 = Data.incr_nbPont2 d 2 (2,2) (2,0) ;;
Data.update d1 ;;

let d2 = Data.incr_nbPont2 d1 2 (2,2) (0,2) ;;

let d3 = Data.incr_nbPont2 d2 2 (2,2) (4,2) ;;

let d4 = Data.incr_nbPont2 d3 2 (2,2) (2,4) ;;


let d5 = Data.update d4;;

Data.getNbVoisin d1 (2,4);;





let strategie data_base =
  let ldata = fst data_base and data = snd data_base in
    
  let mapIncr c ldata lv n = (*applique une incr. de "n" a toute la liste de voisin "lv" de "c"*)
    let rec aux lv acc=
      match lv with
	  [] -> acc
	| coord :: t -> aux t (Data.incr_nbPont2 acc n c coord)
    in
    aux lv ldata
  in


  let f8 ldata database = (*traite tous les sommets d'imp 8*)
    let rec aux ldata acc =
      let acc' = Data.update acc in
      match ldata with
	  [] -> acc'
	| (c,(imp,nbPont),lv) :: t ->
	  if imp = 8 then aux t (mapIncr c acc' lv 2)
	  else aux t acc'
    in
    aux ldata database
  in

   let f4 ldata database = (*traite tous les sommets d'imp 4*)
    let rec aux ldata acc =
      let acc' = Data.update acc in
      match ldata with
	  [] -> acc'
	| (c,(imp,nbPont),lv) :: t ->
	  if ((imp = 4) && ((Data.getNbVoisin acc' c) <= 2))
	  then aux t (mapIncr c acc' lv 2)
	  else aux t acc'
    in
    aux ldata database
   in

    let f2 ldata database = (*traite tous les sommets d'imp 4*)
    let rec aux ldata acc =
      let acc' = Data.update acc in
      match ldata with
	  [] -> acc'
	| (c,(imp,nbPont),lv) :: t ->
	  if ((imp = 2) && ((Data.getNbVoisin acc' c) = 1))
	  then aux t (Data.incr_nbPont2 acc' 2 c (List.hd lv))
	  else aux t acc'
    in
    aux ldata database
   in

   


   let f3 ldata database = (*traite tous les sommets d'imp 4*)
     let rec aux ldata acc =
       let acc' = Data.update acc in
       match ldata with
	   [] -> acc'
	 | (c,(imp,nbPont),lv) :: t ->
	   if ((imp = 3) && ((Data.getNbVoisin acc' c) = 1) && nbPont = 2)
	   then aux t (Data.incr_nbPont2 acc' (imp - nbPont) c (List.hd lv))
	   else aux t acc'
     in
     aux ldata database
   in

   let f5 ldata database = (*traite tous les sommets d'imp 4*)
     let rec aux ldata acc =
       let acc' = Data.update acc in
       match ldata with
	   [] -> acc'
	 | (c,(imp,nbPont),lv) :: t ->
	   if ((imp = 5) && ((Data.getNbVoisin acc' c) = 1) && nbPont >= 3) 
	   then aux t (Data.incr_nbPont2 acc' (imp - nbPont) c (List.hd lv))
	   else aux t acc'
     in
     aux ldata database
   in


   
     
  
  
 

  
    
  
	    
  

  let aux ldata database = 
    let d8 = f8 ldata database in
    let ld8 = fst d8 in

    let d4 = f4 ld8 d8 in
    let ld4 = fst d4 in

    let d2 = f2 ld4 d4 in
    let ld2 = fst d2 in

    let d3 = f3 ld2 d2 in
    let ld3 = fst d3 in

    f5 ld3 d3

							       
								 
				 
    
  in
  aux ldata data_base

;;
	
       
	    
	    
strategie d ;;


[((2, 2), (2, 0), 2);

 
 ((2, 2), (4, 2), 2);

 
 ((2, 2), (2, 4), 2);

 
 ((2, 2), (0, 2), 2);

 
 ((4, 2), (4, 4), 2);

 
 ((4, 4), (2, 4), 1);
 
 ((0, 2), (0, 4), 1);

 
 ((2, 4), (0, 4), 2) ]
