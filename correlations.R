draw_all_correlations <- function(data, path) {
	lines <- seq(1, 30 , 3)
	temp_data <- data[lines, ]
	draw_kendall(temp_data, path)
	draw_spearman(temp_data, path)
	draw_pearson(temp_data, path)
}

draw_kendall <- function(temp_data, path){
	path <- paste(path, "kendall.png")
	temp_data$kendall <- as.numeric(temp_data$kendall)
	my.p <- ggplot(temp_data, aes(x=time.passed, y=kendall), 
		main="Kendall correlation") + geom_point() + geom_line() + 
		scale_x_continuous(limits=c(0,1), breaks=(0:10)*0.1, expand=c(0,0)) + 
		scale_y_continuous(limits=c(-1,1), breaks=(-10:10)*0.1, expand=c(0,0)) + 
		xlab("ratio of data used") + 
		ylab("ratio of data within given error range")
	ggsave(filename = path, plot = my.p, width = 9,
    		height = 5, units = "in")

}

draw_spearman <- function(temp_data, path){
	path <- paste(path, "spearman.png")
	temp_data$spearman <- as.numeric(temp_data$spearman)
	my.p <- ggplot(temp_data, aes(x=time.passed, y=spearman), 
		main="Spearman correlation") + geom_point() + geom_line() + 
		scale_x_continuous(limits=c(0,1), breaks=(0:10)*0.1, expand=c(0,0)) + 
		scale_y_continuous(limits=c(-1,1), breaks=(-10:10)*0.1, expand=c(0,0)) + 
		xlab("ratio of data used") + 
		ylab("ratio of data within given error range")
	ggsave(filename = path, plot = my.p, width = 9,
    		height = 5, units = "in")

}

draw_pearson <- function(temp_data, path){
	path <- paste(path, "pearson.png")
	temp_data$pearson <- as.numeric(temp_data$pearson)
	my.p <- ggplot(temp_data, aes(x=time.passed, y=pearson), 
		main="Pearson correlation") + geom_point() + geom_line() + 
		scale_x_continuous(limits=c(0,1), breaks=(0:10)*0.1, expand=c(0,0)) + 
		scale_y_continuous(limits=c(-1,1), breaks=(-10:10)*0.1, expand=c(0,0)) + 
		xlab("ratio of data used") + 
		ylab("ratio of data within given error range")
	ggsave(filename = path, plot = my.p, width = 9,
    		height = 5, units = "in")

}