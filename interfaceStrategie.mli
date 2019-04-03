module type Strategie_type = 
sig 
	type d

	val resoudre : d -> d 
end

