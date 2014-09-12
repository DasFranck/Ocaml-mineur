let ex3 x = let c = x mod 10 in
			let a = x / 100 in
		    let b = ((x mod 100) - c)/10 in			
			x <= 999 && x > 0 && (a + b + c) < 10 && (a * b) < c && (b = (c - a));;
