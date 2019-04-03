module Type_jeu = struct
	

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

end