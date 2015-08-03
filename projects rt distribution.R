project_names <- unique(as.character(data$project_name))

plots <- list()
for (pn in project_names) {
	print(pn)
	temp <- data[as.character(data$project_name) == pn, ]
	title <- paste("Resolution Time distribution\nfor project ",
		pn, " (", nrow(temp), " elements)", sep="")
	plots[[length(plots) + 1]] <- ggplot(temp) + 
		geom_histogram(mapping=aes(x=timespent), fill="#9999CC") + 
		ggtitle(title) +  theme(plot.title = element_text(size=12)) + labs(list(x="timespent (hours)"))
}
path <- "projects RT distributions.png"
png(filename = path, bg = "transparent", units="px", width=600, height=1100)
multiplot(plotlist = plots, cols = 2)
dev.off()
