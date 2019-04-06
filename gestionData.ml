module Data = struct


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




	let set_nbPont ldata coord nbPont =
		let rec aux ldata acc=
			match ldata with
			[] -> acc
			| ((c,(i,n)) as h) :: t  -> if c = coord then aux t acc@[(c,(i,nbPont))]
				else aux t acc@[h]
		in
		aux ldata []
		
end 







