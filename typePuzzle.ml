
type coordinate = int * int

type importance = int 

type puzzle = vertex list 

type bridge = { isVertical : bool; isDoubled : bool } 

type cell =
		 Nothing 
		| Island of importance
		| Bridge of bridge 
	

type solution = cell list list

type nbPont = int

type data_base = ((coordinate * (importance * nbPont )) * (coordinate list) ) list 


type data = (coordinate * coordinate * nbPont) list 



