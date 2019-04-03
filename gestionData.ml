
module Gestion_data = 
struct

	let init p =
		 let rec aux p acc =
    		match p with
				[] -> acc
      			| (c,imp) :: t -> aux t (acc@[(c,(imp,0))])
  		in aux p []


	let contains ldata coord = 
		let rec aux ldata acc =
			match ldata with
				[] -> acc
				| (c,(i,n)) :: t -> if c = coord then aux t true
			else aux t false
		in 
		aux ldata false



end;;


