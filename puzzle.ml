open TypePuzzle;;

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
    maxX (aux p [])



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
    minX (aux p [])





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
    maxY (aux p [])



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




let ensVoisin p coord =
  let h = try [getVoisinHaut p coord] with Pas_de_voisin -> []
  and d = try [getVoisinDroit p coord] with Pas_de_voisin -> []
  and b = try [getVoisinBas p coord] with Pas_de_voisin -> []
  and g = try [getVoisinGauche p coord] with Pas_de_voisin -> []

  in
  h@d@b@g 
      
    

 
    

end ;;