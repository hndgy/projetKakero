open Puzzle;;
open TypePuzzle;;

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
     

 let relier s p (((x1,y1) as c1),((x2,y2) as c2),n) =
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


 
 let print s =

   let s' = List.rev (List.map (List.rev) s) in
    
   let string_of_cell cell =
     match cell with
       | Island x -> "("^string_of_int x^")"
       | Nothing -> "   "
       | (Bridge {isVertical =isV; isDoubled = isD}) -> 
	 if isD && isV then "| |"
	 else if not isD && isV then " | "
	 else if isD && not isV then "==="
	 else "---"
   in
   

   let toString_ligne l =
     let rec aux l acc =
       match l with
	   [] -> acc
	 | h :: t -> aux t (string_of_cell h)^acc
     in
     aux l ""
   in
   let rec aux s acc =
     match s with
	 [] -> acc
       | h :: t -> aux t "\n"^(toString_ligne h)^acc
   in
   print_string (aux s' "\n")


end;;
	
