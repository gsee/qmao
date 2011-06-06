convert.illions <-
function(x) {
		bmt <- substr(x,nchar(x),nchar(x))
		if(bmt=="B" || bmt=="M" || bmt=="T") {
			out <- NA				
			num <- as.numeric(substr(x,1,nchar(x)-1)) 		
			switch(bmt, B={out <- num * 1000000000 }, 
				M={out <- num * 1000000}, 
				T={out <- num * 1000000000000})
		} else out <- x		
		out
} #convert.illions("5.43M"); convert.illions("1.2T")

