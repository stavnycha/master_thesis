diff_factors_set <- function(factors1, factors2){
	 return(mapply(diff_factors, factors1, factors2))
}

diff_factors <- function(fact1, fact2){
	bord1 = borders(fact1)
	bord2 = borders(fact2)
	if (max(bord1) < min(bord2)) { return(min(bord2) - max(bord1))}
	if (fact1 == fact2) { return(0) }
	return(min(bord1) - max(bord2))
}

borders <- function(fact){
	res = switch(as.character(fact),
		"0.5" = c(0.5, 0.5),
         	"1" = c(1,1),
        	"2-3" = c(2,3),
        	"4-6" = c(4,6),
		"7-11" = c(7,11),
		"12-20" = c(12,20),
		"21-40" = c(21,40),
		">40" = c(40,150)
	)
	return(res)
}

diff_set <- function(factors, numbers){
	 return(mapply(diff, factors, numbers))
}

diff <- function(fact, number){
	number <- as.numeric(number)
	res = switch(as.character(fact),
		"0.5" = diff2(0.5, 0.5, number),
        	"1" = diff2(1, 1, number),
        	"2-3" = diff2(2, 3, number),
        	"4-6" = diff2(4, 6, number),
		"7-11" = diff2(7, 11, number),
		"12-20" = diff2(12, 20, number),
		"21-40" = diff2(21, 40, number),
		">40" = diff2(40, max(40,number), number)
	)
	return(res)
}

diff2 <- function(a, b, num){
	if (num >=a && num <=b) { return(0)}
	if (num < a) { return(a-num)}
	return (num - b)
}

factor_median <- function(factors){
	return(sort(factors)[ceiling(length(factors)/2)])
}