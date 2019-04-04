
type coordinate = int * int

type importance = int 

type puzzle = (coordinate * importance) list 

type bridge = { isVertical : bool; isDoubled : bool } 

type cell =
		 Nothing 
		| Island of importance
		| Bridge of bridge 
	

type solution = cell list list

type nbPont = int

type data = coordinate * (importance * nbPont ) list 

type liaison = 
	Null 
	| Voisin of Voisin * liaison
<<<<<<< HEAD
=======

>>>>>>> 0710a27784b818b647bbeb8fa4a6e9dab7686591
