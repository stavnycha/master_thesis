smooth_data <- function(data, by_number){
	res <- c()
	for (i in (by_number:length(data))){
		res[length(res) + 1] <- mean(data[(i-by_number+1):i])
	}
	return(c(rep(NA, by_number-1),res))
}