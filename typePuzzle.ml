type coordinate = int * int

type importance = int 



type puzzle = (coordinate* importance) list 

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


exception Sommet_incompatible


exception Pas_de_voisin

exception Empty_list
