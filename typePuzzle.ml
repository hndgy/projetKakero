
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

type data = coordinate * (importance * nbPont ) list 


exception Pas_de_voisin 