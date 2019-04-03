

module Gestion_data = 
struct
	type d = TypePuzzle.data 
	type c = TypePuzzle.coordinate

	val empty : unit -> d
	val add : c -> d
	val contains : c -> bool
	
end