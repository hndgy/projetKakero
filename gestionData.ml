

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

type data = (coordinate * (importance * nbPont ) * coordinate list) list 




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
	      | ((x,y),imp)::t -> if x < i && y = j then aux t (acc@[((x,y),imp)]) else aux t acc
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
		| ((x,y),imp)::t -> if x > i && y = j then aux t (acc@[((x,y),imp)]) else aux t acc
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
		| ((x,y),imp)::t -> if x = i && y < j then aux t (acc@[(x,y)]) else aux t acc
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
		| ((x,y),imp)::t -> if x = i && y > j then aux t (acc@[((x,y),imp)]) else aux t acc
	    in

	   if (aux p []) = [] then raise Pas_de_voisin else List.hd (aux p [])
	    
	    (*if (aux p []) = [] then raise Pas_de_voisin else
	      minY (aux p [])*)
	  ;;

	  getVoisinBas p (2,0)
	





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
	  	in aux pzz []


(*	let contains ldata coord = 
		let rec aux ldata acc =
			match ldata with
				[] -> acc
				| (c,(i,n)) :: t -> if c = coord then aux t true
						else aux t false
		in 
		aux ldata false*)




	let incr_nbPont ldata nb coord1 sommet  =
	  let coord2 = (fun (x,y,z) -> x) sommet in
	  
		let rec aux ldata acc=
			match ldata with
			[] -> acc
			| ((c,(i,n),v) as h) :: t  -> if c = coord1 || c = coord2 then aux t acc@[(c,(i,n+nb),v)]
				else aux t acc@[h]
		in
		aux ldata []



	let incr_nbPont2 ldata nb coord1 coord2  =
	  
	  
	  let rec aux ldata acc=
	    match ldata with
		[] -> acc
	      | ((c,(i,n),v) as h) :: t  -> if c = coord1 || c = coord2 then aux t acc@[(c,(i,n+nb),v)]
		else aux t acc@[h]
	  in
	  aux ldata []

       




	let update data =
	  
	  let rec aux ldata acc supp =
	    match ldata with
		[] -> acc,supp
	      | ((c,(imp,n),lv) as h) :: t -> if imp = n then aux t acc (supp@[(c,imp)])
		else aux t (acc@[h]) supp
       

	  in
     
	  let res = aux data [] []
	  in
	  let ld = fst res and lsupp = snd res
	  in
	  let delete e (c,d,l) =
	    let rec aux l acc =
	      match l with
		  [] -> acc
		| h :: t -> if h = e then aux t acc else aux t acc@[h]
	    in
	    
	   (c,d,aux l [])
	  in

	 	

	 let rec aux1 supp acc =
	    match supp with
		[] -> acc
	      | h :: t -> aux1 t (List.map (delete h) acc)
	  in
	  aux1 lsupp ld



	let getNbVoisin ldata coord =
	  let rec aux l acc =
	    match l with
		[] -> acc
	      | (c,d,l) :: t -> if c = coord then aux t (List.length l) else aux t acc
	  in if ldata = [] then raise Empty_list
	    else 
	      aux ldata 0

	
		
	   
	
	     
	      
	    
		
	    
		
 end;;




let p = [((2,0),2);((0,2),3);((2,2),8);((4,2),4);((0,4),3);((2,4),5);((4,4),3)];;

let d = Data.init p;;


let d1 = Data.incr_nbPont d 2 (2,2) (2,0) ;;

let d2 = Data.incr_nbPont d1 2 (2,2) (0,2) ;;

let d3 = Data.incr_nbPont d2 2 (2,2) (4,2) ;;

let d4 = Data.incr_nbPont d3 2 (2,2) (2,4) ;;


let d5 = Data.update d4;;

Data.getNbVoisin d5 (2,4);;





let strategie ldata =
  let mapIncr c ldata lv n =
    let rec aux lv acc=
      match lv with
	  [] -> acc
	| (coord,_) :: t -> aux t (Data.incr_nbPont2 acc n c coord)
    in
    aux lv ldata
  in
  

  let rec aux l acc =
    let ldataUPD = Data.update l in 
    match ldataUPD with
	[] -> acc
      | (c,(i,n),lv)  :: t -> let nb = Data.getNbVoisin acc c in
	if i = 8 then aux t (mapIncr c acc lv 2)
	else
	  if nb = 1 then let (cv,iv) = List.hd lv in
			 aux t (Data.incr_nbPont2 acc (i - n) c cv)
	  else
	    aux t acc
  in
  aux ldata ldata;;

strategie d;;


