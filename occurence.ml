let occurence chr str = 
	let rec occ chr str c a =
	match c with
	|c when c<0 -> a
	|c -> 
	begin 
		if (chr = str.[c]) || (int_of_char chr = (int_of_char str.[c]) + 32) 
			then occ chr str (c-1) (a+1)
		else occ chr str (c-1) (a)
	end in
	occ chr str (String.length str -1) 0;;
















String.length b
32