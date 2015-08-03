# helper for drawing cumulative MRE ranges
drawCumulativeGain <- function(filepath, v.errors, chart.name, error.name = "Error", error.range = NULL, pop.range = NULL) {
	if(length(error.range)==0) {
		error.range <- c(0, max(c(v.errors,1.0)))
	}
	if(length(pop.range)==0) {
		pop.range <- c(0, 1)
	}
	
	v.counts <- rle(sort(v.errors))
	v.count <- length(v.errors)
	
	v.c <- c(0)
	for(i in 1:length(v.counts$lengths)) {
		v.c <- c(v.c, v.c[i] + v.counts$lengths[i])
	}
	
	v.df <- data.frame(Error=c(0,v.counts$values), Population=as.numeric(v.c)/as.numeric(v.count))
	write.csv2(v.df, file=paste(filepath, "debug", chart.name, ".csv",sep=""))
	
	return(ggplot(v.df, aes(x=Error, y=Population)) + geom_step() + ggtitle(chart.name) + ylab("% population") + xlab(error.name) + theme_bw() + scale_x_continuous(limits=error.range, expand=c(0,0)) + scale_y_continuous(limits=pop.range, expand=c(0,0)))
}