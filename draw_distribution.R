draw_distributions <- function(dist_data) {
	plots = list()
	clusters <- length(unique(dist_data$cluster))
	for (cl in 1:clusters) {
		plots[[cl]] <- ggplot(dist_data[dist_data$cluster == cl, ] )
		plots[[cl]] = plots[[cl]] + geom_histogram(mapping=aes(group=cluster, x=time), 
			fill="#9999CC") + ggtitle(paste("Cluster ", cl, " (", nrow(dist_data[dist_data$cluster == cl, ]), " elements)", sep="")) 
	}
	ids <- rownames(dist_data)
	path <- paste(base_path, "/distribution ", ids[length(ids)], ".png", sep="")
	print(path)
	png(filename = path, bg = "transparent")
	multiplot(plotlist = plots, cols = 2)
	dev.off()
	#ggsave(filename = path, plot = p, width = 5, height = 10, units = "in")
}