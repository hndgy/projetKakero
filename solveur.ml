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
	  if imp = 8 then
	    let nbV = List.length lv in
	    if (nbV = 4 && nbPont = 0) ||  (nbV = 3 && nbPont = 2) ||  (nbV = 2 && nbPont = 4) ||  (nbV = 1 && nbPont = 6) then
	      aux t  (DataBase.map_connect c acc lv 2)
	    else if ( nbV = 4 && nbPont = 1 ) || ( nbV = 3 && nbPont = 3 ) || ( nbV = 2 && nbPont = 5 ) || (nbV = 1 && nbPont = 7) then
	      aux t  (DataBase.map_connect c acc lv 1)
	    else aux t acc
	  else aux t acc
    in
    aux ldata database



let f6  database = (*traite tous les sommets d'imp 6*)
  let ldata = fst database in 

  let rec aux ldata acc =
    
    match ldata with
	[] -> acc
      | (c,(imp,nbPont),lv) :: t ->

	if imp = 6 then
	  let nbV = List.length lv in
	  
	  if ( (nbV = 3) && nbPont = 0) 
	  then aux t  (DataBase.map_connect c acc lv 2)

	  else if ((nbV = 2) && nbPont = 2)
	  then aux t (DataBase.map_connect c acc lv 2)

	  else if ((nbV = 3) && nbPont = 1)
	  then aux t (DataBase.map_connect c acc lv 1)
	    
	  else if ((nbV = 1) && nbPont = 4)
	  then aux t (DataBase.connect acc 2 c (List.hd lv))

	  else if ((nbV = 1) && nbPont = 5)
	  then aux t (DataBase.connect acc 1 c (List.hd lv))

	      
	      

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
	  then aux t (DataBase.map_connect c acc lv 2)

	  else if nbV = 2 && nbPont = 1
	  then aux t (DataBase.map_connect c acc lv 1)
	    
	  else if ( (nbV = 1) && nbPont = 2)
	  then aux t (DataBase.connect acc 2 c (List.hd lv))
	    
	  else if ( (nbV = 1) && nbPont = 3)
	  then aux t (DataBase.connect acc 1 c (List.hd lv))




	      
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
	     
	     if nbV = 1 && nbPont = 0
	     then aux t  (DataBase.connect acc 2 c (List.hd lv))    
	       
	     else if nbV  = 1 && nbPont = 1
	     then aux t  (DataBase.connect acc 1 c (List.hd lv))

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
	    then aux t  (DataBase.connect acc 1 c (List.hd lv))
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
	     then aux t  (DataBase.connect acc 2 c (List.hd lv))
	       
	     else if ( nbV = 1 && nbPont = 2 )
	     then aux t  (DataBase.connect acc 1 c (List.hd lv))
	       
	     else if nbV = 2 && nbPont = 0
	     then aux t (DataBase.map_connect c acc lv 1)


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
	     then aux t  (DataBase.connect acc 2 c (List.hd lv))

	     else if nbV = 1 && nbPont = 4
	     then aux t  (DataBase.connect acc 1 c (List.hd lv))  

	     else if ( (nbV = 2) && (nbPont = 1))
	     then aux t (DataBase.map_connect c acc lv 2)
	       
	     else if ((nbV = 2 ) && (nbPont = 2 ))
	     then aux t  (DataBase.map_connect c acc lv 1)

	     else if nbV = 3 && nbPont = 0
	     then aux t  (DataBase.map_connect c acc lv 1)
    
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
	     then aux t  (DataBase.connect acc 2 c (List.hd lv))

	     else if ( (nbV = 1) && nbPont = 6)
	     then aux t  (DataBase.connect acc 1 c (List.hd lv))
	       
	       
	     else if nbV = 3 && nbPont = 1
	     then  aux t  (DataBase.map_connect c acc lv 2)

	     else if nbV = 4 && nbPont = 0
	     then  aux t  (DataBase.map_connect c acc lv 1)
	       
	     else if nbV = 3 && nbPont = 2
	     then  aux t  (DataBase.map_connect c acc lv 1)

	     else if nbV = 2 && nbPont = 3
	     then  aux t  (DataBase.map_connect c acc lv 2)

	     else if nbV = 2 && nbPont = 4 
	     then  aux t  (DataBase.map_connect c acc lv 1)
	     else aux t acc
	       
	       
	     else aux t acc
     in
     aux ldata database
   



let strategie1  data_base =
(*applique la strategie 1*)
  
  let f db =  f7 (f3 (f5 (f1 (f2 (f4 (f6 (f8 db)))))))
  (*la fonction f retourne une database avec tous les  sommets "traitables" traités *)

  in

  let rec boucle db = 
  (*tant qu'on peut traiter des sommets on le fait sinon on le retourne*)
    let res = f db in 
    if db = res then res
    else
      match fst res with
	  [] -> res
	| _ -> boucle res
  in
  
  boucle data_base 



let isConnexe database p =
	(*retourne true si la solution de la database est connexe, false sinon*)
  let hist = snd database in
  let connexe = List.map (fun (c,i) -> (c,false)) p in
  (*associe à chaque sommet du puzzle un false dans une liste,(au départ aucun sommet n'est connexe)*)

  let setConnexe l c = let rec aux l acc =
  (*associe true a un sommet de coordonné c dans l*)
			match l with
			    [] -> acc
			  | ((coord,_) as h)  :: t -> if c = coord then aux t acc@[(c,true)] else aux t acc@[h]
		       in
		       aux l []
  in
  	       
 
  let compConnex hist lconnex= 
  (*retourne une composante connexe avec l'historique (à un moment donné)des opérations effectuées*)
    let rec aux hist acc =
      match hist with
	  [] -> acc
	| (c1,c2,_) :: t -> let isConn1 = List.assoc c1 acc  and  isConn2 = List.assoc c2 acc in
	  if isConn1 && not isConn2 (*si c1 est connex et qu'il est connecté à c2 alors c2 est connexe*)
	  then aux t (setConnexe acc c2)
	  else  if isConn2 && not isConn1 (*si c2 est connex et qu'il est connecté à c1 alors c1 est connexe*)
	    then aux t (setConnexe acc c1)
	  else if isConn1 && isConn2 then aux t acc (*si ils sont deja connexe ont fait rien*)
	  else aux t acc (*si les deux sommets ne sont pas connexe alors ont fait rien *)
    in aux hist lconnex 
  in
  let rec aux hist lconnex = (*recupere une composante connexe dans l'historique des operations*)
  (*tant qu'il y a des sommet à traiter on les traite*)
    let res = compConnex hist lconnex in
    if lconnex = res then lconnex
    else aux hist res
  in
  let s1 = (fun (c1,c2,n) -> c1) (List.hd hist) 
  (*récupère le premier de l'historique (choix arbitraire)*)
  in
  let resCC =  aux hist (setConnexe connexe s1) in (*tous les sommets associés à un boolean, 
  disant si il est connexe ou pas*)
  let isAllconnexe= List.for_all (fun (c,isCo) -> isCo) resCC (*retourne si tous les sommets sont connexes*)
  in
  isAllconnexe

  let tousTraite database = (*verifie que tous les sommets sont traités (nbPont = imp)*)
   let rec aux ldata acc =
      match ldata with
	  [] -> acc
	| (_,(imp,nb),_) :: t ->
	  if imp = nb then aux t true
	  else aux [] false
   in
   aux (fst database) false




let strategie2 data_base p =(*applique la strategie 2 en créant un arbre de possiblité *)

  let hyp_all_voisin db (c,d,lv) = (*créée une liste de possiblités en ajoutant 
  un pont entre le sommet de coordonnés c à tous ses voisins dans lv, 
et en appliquant la stratégie 1 *)
    let rec aux lv acc =
      match lv with
	  [] -> acc
	| h :: t -> let hypDb = strategie1 (DataBase.connect db 1 c h) in
		   aux t acc@[hypDb]
    in
    aux lv []
  in	 
  let list_hyp db = (*ajoute toutes les possiblités pour chaque sommets et ses voisins*)
    let rec aux ldata acc =
      match ldata with
	  [] -> acc
	| s :: t -> aux t acc@(hyp_all_voisin db s)
    in
    aux (fst db) []
  in
	  
  let next_hyp l = List.flatten (List.map list_hyp l) (*réapplique *)
  in

  let test db = (tousTraite db) && (isConnexe db p)(*teste si tous les sommets sont traités 
  et la connexité de la solution*)
  in

  let rec boucle l_hyp = (*tant qu'il n'y a pas de solution connexe et complète 
  alors on créée une nouvelle génération dans l'arbre
ou on s'arrete si il n'y plus de chagement *)
    try List.find test l_hyp 
    with Not_found -> boucle (next_hyp l_hyp) 
  in

  boucle (list_hyp data_base)



let solve p =
	(*resout un puzzle*)
  let resS1 = strategie1 (DataBase.init p) in (*applique d'abors la strategie 1*)

  let res = if tousTraite resS1 then resS1 else strategie2 resS1 p in 
  (*si la solution n'est pas somplète alors un applique la stratégie 2*)
  
  let hist = snd res in

  let relier_all hist=
  (*relie les sommets de la solution selon l'historique des opérations*)
    let rec aux l acc =
      match l with
	  [] -> acc
	| h :: t -> aux t (Solution.relier acc p h)
    in
    aux hist (Solution.init p)
  in

  relier_all hist

  


end;;