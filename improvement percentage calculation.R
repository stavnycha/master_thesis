# calcualting improvement percentage


actual <- data.frame(c(0.668), c(0.727), c(0.501), c(0.578))
baseline_mi <- data.frame(c(0.561), c(0.665), c(0.439), c(0.512))
baseline_t <- data.frame(c(0.435), c(0.538), c(0.345), c(0.431))

colnames(actual) <- c("0.5h", "1h", "0.1", "0.25")
colnames(baseline_mi) <- c("0.5h", "1h", "0.1", "0.25")
colnames(baseline_t) <- c("0.5h", "1h", "0.1", "0.25")

PRED <- PRED[, c(3,4,1,2)]

percentages <- function(v){
	if (is.data.frame(v)){
		v <- v[, c("0.5h", "1h", "0.1", "0.25")]
		v = v[1, ]
	}
	print("")
	print("")
	print("")
	print("")

	print("BP:")
	print(round(v * 100 / baseline_mi[1,], digits=1) - 100)
	print("AP:")
	print(round(v * 100 / actual[1,], digits=1) - 100)	
	return("")
}

