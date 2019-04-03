module type Solveur_type = 
sig

	type coord
	

	val relier : coord -> coord -> cell -> solution
end