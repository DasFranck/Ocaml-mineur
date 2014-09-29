let occurence_casse chr str = 
	let rec occ chr str c a =
	match c with
	|0 -> a
	|c -> 
	begin 
		if chr = str.[c]
			then occ chr str (c-1) (a+1)
		else occ chr str (c-1) (a)
	end in
	occ chr str (String.length str -1) 0;;