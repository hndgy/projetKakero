open Solution;;
open Database;;
open TypePuzzle;;

module Solveur = 
struct

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
	    
	  else if ((nbV = 1) && nbPont = 4)
	  then aux t (Data.connect acc 2 c (List.hd lv))

	  else if ((nbV = 1) && nbPont = 5)
	  then aux t (Data.connect acc 1 c (List.hd lv))

	      
	      

	  else aux t acc
	    
	  else aux t acc
  in
  aux ldata database


  

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



	     
   let f2 database = (*traite tous les sommets d'imp 2*)
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


   


   let f1  database = (*traite tous les sommets d'imp 1*)
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



   
   let f3 database = (*traite tous les sommets d'imp 3*)
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




   let f5  database = (*traite tous les sommets d'imp 5*)
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



   let f7  database = (*traite tous les sommets d'imp 7*)
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
   



let strategie1  data_base =
  
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





let strategie2 data_base =

  let hyp_all_voisin db (c,d,lv) =
    let rec aux lv acc =
      match lv with
	  [] -> acc
	| h :: t -> let hypDb = strategie1 (Data.connect db 1 c h) in
		   aux t acc@[hypDb]
    in
    aux lv []
  in	 
  let list_hyp db =
    let rec aux ldata acc =
      match ldata with
	  [] -> acc
	| s :: t -> aux t acc@(hyp_all_voisin db s)
    in
    aux (fst db) []
  in
	  
  let next_hyp l = List.flatten (List.map list_hyp l)
  in

  let test db = fst db = []
  in

  let rec boucle l_hyp =
    try List.find test l_hyp 
    with Not_found -> boucle (next_hyp l_hyp)
  in

  boucle (list_hyp data_base)
    



let solve p =
  let resS1 = strategie1 (Data.init p) in

  let res = if fst resS1 = [] then resS1 else strategie2 resS1 in
  
  let hist = snd res in


  let relier_all hist=
    let rec aux l acc =
      match l with
	  [] -> acc
	| h :: t -> aux t (Solution.relier acc p h)
    in
    aux hist (Solution.init p)
  in

  relier_all hist

  


end;;