library(ggplot2)
library(tm)
library("clue")
library(gsubfn)
library("XLConnect")
library(e1071)
library(stringr)
library(skmeans)
library(parallel)
library(clv)
folder.root <- "."

setwd("D:/tartuUniversity/for_ms/scripts")
source(file="mre.r")
source(file="mode.r")
source(file="kmeans_automatic_k_detection_cosine_lsa_silhuette_dynamic_k.r")
source(file="smooth.r")
source(file="factor_diff.r")
source(file="multiplot.r")
source(file="draw_distribution.r")
source(file="correlations.r")
source(file="cluster_validity.r")
setwd("D:/tartuUniversity/for_ms")


data <- read.csv(file="fortumo_data_with_time_eng_closed_processed.csv",head=TRUE,sep=",")
data$eventualID <- 1:nrow(data)
data <- data[data$is_estonian == FALSE, ]

data <- data[order(data$created), ]
rownames(data) <- as.character(1:nrow(data))

errranges_relative <- c(0.1, 0.25, 0.33)
errranges_absolute <- c(1, 3, 5)
clustersNumber <- c(7)

clusterValueMethods = c("median") 

data$description <- as.character(data$description)
data$title <- as.character(data$title)
data[data$description == "",]$description <- data[data$description == "",]$title

corpus = Corpus(VectorSource(paste(data$title)))

data$timespent_hours <- ceiling(data$timespent / 3600.0)
data$timespent_seconds <- data$timespent
data[data$timespent_seconds <= 1800, ]$timespent_hours <- 0.5

data[data$timespent_hours == 0.5,]$timespent <-  "0.5"
data[data$timespent_hours == 1,]$timespent <- "1"
data[data$timespent_hours >= 2 & data$timespent_hours < 4,]$timespent = "2-3"
data[data$timespent_hours >= 4 & data$timespent_hours < 7,]$timespent = "4-6"
data[data$timespent_hours >= 7 & data$timespent_hours < 12,]$timespent = "7-11"
data[data$timespent_hours >= 12 & data$timespent_hours < 21,]$timespent = "12-20"
data[data$timespent_hours >= 21 & data$timespent_hours < 41,]$timespent = "21-40"
data[data$timespent_hours > 40,]$timespent = ">40"
levels = c("0.5", "1", "2-3", "4-6", "7-11", "12-20", "21-40", ">40")
data$timespent = factor(data$timespent, levels = levels)
data$created <- as.numeric(as.Date(as.character(data$created)))

folder.root <- "./cosine_lsa"

calculate <- function(i){
	print(i)
	firstrow <- if (dataVolume == "allData") { 1 } else { max(1, i-bunch+1) } 
	rows <- if (dataVolume == "allData") { i } else { min(i, bunch) }
	lastrow <- firstrow + rows - 1
	rM <- as.data.frame(matrix(nrow=0,ncol=20, dimnames=list(NULL,c("time.passed",
		"model", "inrange", "terms", "train", "test", "clusters", "validity", "minsparse", 
		"maxsparse", "mintfidf", "maxtfidf", "kendall", "spearman", "pearson", "abs", "actual_class", 
		"actual", "predicted", "rel"))))

	#CHECK weighting!!
	bug.tdm <- DocumentTermMatrix(corpus[firstrow:lastrow], control = list(minWordLength = 2, maxWordLength=999))
	#bug.tdm <- removeSparseTerms(bug.tdm0, sparse=0.995)
	bug.terms <- Terms(bug.tdm)

	bug.terms.minsparse <- min(apply(as.matrix(bug.tdm), 2, function(x){sum(as.numeric(x > 0))}))/nrow(bug.tdm)
	bug.terms.maxsparse <- max(apply(as.matrix(bug.tdm), 2, function(x){sum(as.numeric(x > 0))}))/nrow(bug.tdm)
	bug.terms.mintfidf <- min(apply(as.matrix(bug.tdm), 2, sum))
	bug.terms.maxtfidf <- min(apply(as.matrix(bug.tdm), 2, sum))

	rt_data <- data[c("id", "timespent", "timespent_hours")][firstrow:lastrow, ]
	rt_data_actual <- data[c("id", "timespent_hours")][firstrow:lastrow, ]

	colnames(rt_data) <- c("ID", "ResolutionTime", "ResolutionTimeHours")
	predicted_size <- 1
	break_point <- (nrow(bug.tdm) - predicted_size)
	
	clusters <- 0
	cl_validity <- NA
	bug.predict.classonly <- tryCatch({
		bug.predict <- kmeans_f_automatic_cosine_lsa_silhuette(i, bug.tdm, rt_data, sheet_path)
		clusters <- bug.predict$clusters
		cl_validity <- bug.predict$validity
		print('algorythm done')
		bug.predict$prediction
	}, error = function(e){
		print(e)
		NA
	})
	
	
	write.csv2(as.matrix(bug.predict.classonly),paste(error_ranges_graph, "bug.predict",i,".csv",sep=""))
	bug.predict.classonly.aerr <- if (is.na(bug.predict.classonly)) {NA} else {
		diff(bug.predict.classonly, rt_data_actual[(1 + break_point):nrow(rt_data),2])
	}

	temp <- data.frame(bug.predict.classonly.aerr, rt_data_actual[(1 + break_point):nrow(rt_data),2])
	colnames(temp) <- c("error", "actual")
	#temp <- temp[complete.cases(temp),]
	bug.predict.classonly.rerr <- temp$error / temp$actual	

	for(errrange in errranges_relative) {
		inrange <- sum(as.numeric(bug.predict.classonly.rerr <= errrange))/length(bug.predict.classonly.rerr)

		rM[nrow(rM)+1, c("time.passed", "model", "inrange", 
		"terms", "train", "test", "clusters", "validity", "minsparse", 
		"maxsparse", "mintfidf", "maxtfidf", "kendall", "spearman", "pearson", "abs", "actual_class", "actual", 
		"predicted", "rel")] <- 
			c(i, paste("RE <", errrange), inrange, length(bug.terms), nrow(bug.tdm) - predicted_size, predicted_size, clusters, 
			cl_validity, bug.terms.minsparse, 
			bug.terms.maxsparse, bug.terms.mintfidf, bug.terms.maxtfidf, NA, NA, NA, bug.predict.classonly.aerr,
			as.character(rt_data[nrow(rt_data),2]), rt_data_actual[nrow(rt_data),2], as.character(bug.predict.classonly), 
			bug.predict.classonly.rerr)
	}
	
	for(errrange in errranges_absolute) {
		inrange <- sum(as.numeric(bug.predict.classonly.aerr <= errrange))/length(bug.predict.classonly.aerr)

		rM[nrow(rM)+1, c("time.passed", "model", "inrange",
		"terms", "train", "test", "clusters", "validity", "minsparse", 
		"maxsparse", "mintfidf", "maxtfidf", "kendall", "spearman", "pearson", "abs", "actual_class", "actual", 
		"predicted", "rel")] <- 
			c(i, paste("AE <", errrange), inrange, length(bug.terms), nrow(bug.tdm) - predicted_size, predicted_size, clusters, 
			cl_validity, bug.terms.minsparse, 
			bug.terms.maxsparse, bug.terms.mintfidf, bug.terms.maxtfidf, NA, NA, NA, bug.predict.classonly.aerr,
			as.character(rt_data[nrow(rt_data),2]), rt_data_actual[nrow(rt_data),2], as.character(bug.predict.classonly), 
			bug.predict.classonly.rerr)
	}

	path = paste(error_ranges_graph, "classification_description",i,".csv",sep="")
	print(path)
	write.csv2(rM, path)

	gc()
	
	print(rM)
	return(rM)
}


trim <- function (x) gsub("^\\s+|\\s+$", "", x)

bunches = c(50, 200)

fun = 'kmeans silhuette'
dataVolume  <- "10percent"
clusterValueMethod <- "median"
clusters <- "automatic"
bunch = 200
part = "description"

	folder_name = paste(fun, clusters, part)
	error_ranges_graph <- paste(folder.root, dataVolume, 'really dynamic k', folder_name, bunch, "error ranges ", sep="/")
	error_ranges_graph <- paste(error_ranges_graph, clusters, clusterValueMethod)
	base_path <- paste(folder.root, dataVolume, 'really dynamic k', folder_name, bunch, sep="/")
	
	validities_path <- paste(error_ranges_graph, " all_validities.csv", sep="")
	unlink(validities_path)

	all_data_path <- paste(error_ranges_graph, "all_data.csv")
	if (file.exists(all_data_path) == TRUE){
		print("reading existing full set of data...")
		rM <- read.csv(all_data_path, header = TRUE, sep = ",", row.names = 1)
	} else {
		clusters_history <- c()
		rL <- mclapply(bunch:nrow(data), calculate)
		rM <- rL[[1]]
		for(i in 2:length(rL)) {
			if(is.data.frame(rL[[i]]))
				rM <- rbind(rM, rL[[i]])
			else
				print(rL[[i]])
		}
		write.csv(file=all_data_path, x=rM)
	}

	for(i in c(1,3:12, 16, 18, 20 )) {
		rM[,i] <- as.numeric(rM[,i])
	}

	
	smoothings = c(10, 50)
	for (smoothing in smoothings){
				
		for (err in c("RE < 0.1", "RE < 0.25", "RE < 0.33", "AE < 1", "AE < 3", "AE < 5")) {
			table <- rM[rM$model == err,]
			smoothing_inrange <- smooth_data(table$inrange, smoothing)	
			smoothing_inrange_factors <- smooth_data(table$inrange_factors, smoothing)
			table$smoothing_inrange <- smoothing_inrange
			table$smoothing_factors <- smoothing_inrange_factors
			my.p <- ggplot(table)
			my.p <- my.p + geom_point(mapping=aes(x=time.passed, y=inrange), size=1.1) + 
				scale_x_continuous(limits=c(1,nrow(data)), breaks=(1:23)*100) + ggtitle(paste("Percentage of issues with", err, ", smoothing by", smoothing))
			my.p <- my.p + geom_line(mapping=aes(x=time.passed, y=smoothing_inrange), color="#9999CC", size=1.1)
			path <- paste(base_path, "/factor-number/", "error ranges predictive quality smoothing by "
				,smoothing, ", ", gsub("<","l",err), ".png", sep="")
			ggsave(filename = path, plot = my.p, width = 50, height = 5, units = "in", limitsize=FALSE)

			my.p <- ggplot(table)
			my.p <- my.p + geom_point(mapping=aes(x=time.passed, y=inrange), size=1.1) + 
				scale_x_continuous(limits=c(1,nrow(data)), breaks=(1:23)*100) + ggtitle(paste("Percentage of issues with", err, ", smoothing by", smoothing))
			my.p <- my.p + geom_line(mapping=aes(x=time.passed, y=smoothing_inrange_factors), color="#9999CC", size=1.1)
			path <- paste(base_path, "/factors/", "error ranges predictive quality smoothing by "
				,smoothing, ", ", gsub("<","l",err), ".png", sep="")
			ggsave(filename = path, plot = my.p, width = 50, height = 5, units = "in", limitsize=FALSE)
		}
		shortened_table <- rM[rM$model == "RE < 0.1",] # whatever model, just 1 line per result is needed
		shortened_table$smoothing_abs <- smooth_data(shortened_table$abs, smoothing)	
		shortened_table$smoothing_abs_factors <- smooth_data(shortened_table$abs_factors, smoothing)
		shortened_table$smoothing_rel <- smooth_data(shortened_table$rel, smoothing)	
		shortened_table$smoothing_rel_factors <- smooth_data(shortened_table$rel_factors, smoothing)

		my.p <- ggplot(shortened_table)
		my.p <- my.p + geom_line(mapping=aes(x=time.passed, y=abs), color="#AAAAAA") + 
			scale_x_continuous(limits=c(1,nrow(data)), breaks=(1:23)*100) + ggtitle(paste("Absolute error, smoothing by", smoothing))
		my.p <- my.p + geom_line(mapping=aes(x=time.passed, y=smoothing_abs), color="#6600CC", size=1.1)
		path <- paste(base_path, "/factor-number/", "error ranges AE smoothing by "
			,smoothing, ".png", sep="")
		ggsave(filename = path, plot = my.p, width = 50, height = 5, units = "in", limitsize=FALSE)

		my.p <- ggplot(shortened_table)
		my.p <- my.p + geom_line(mapping=aes(x=time.passed, y=abs_factors), color="#AAAAAA") + 
			scale_x_continuous(limits=c(1,nrow(data)), breaks=(1:23)*100) + ggtitle(paste("Absolute error, smoothing by", smoothing))
		my.p <- my.p + geom_line(mapping=aes(x=time.passed, y=smoothing_abs_factors), color="#6600CC", size=1.1)
		path <- paste(base_path, "/factors/", "error ranges AE smoothing by "
			,smoothing, ".png", sep="")
		ggsave(filename = path, plot = my.p, width = 50, height = 5, units = "in", limitsize=FALSE)

		my.p <- ggplot(shortened_table)
		my.p <- my.p + geom_line(mapping=aes(x=time.passed, y=rel), color="#AAAAAA") + 
			scale_x_continuous(limits=c(1,nrow(data)), breaks=(1:23)*100) + ggtitle(paste("Relative error, smoothing by", smoothing))
		my.p <- my.p + geom_line(mapping=aes(x=time.passed, y=smoothing_rel), color="#6600CC", size=1.1)
		path <- paste(base_path, "/factor-number/", "error ranges RE smoothing by "
			,smoothing, ".png", sep="")
		ggsave(filename = path, plot = my.p, width = 50, height = 5, units = "in", limitsize=FALSE)

		my.p <- ggplot(shortened_table)
		my.p <- my.p + geom_line(mapping=aes(x=time.passed, y=rel_factors), color="#AAAAAA") + 
			scale_x_continuous(limits=c(1,nrow(data)), breaks=(1:23)*100) + ggtitle(paste("Relative error, smoothing by", smoothing))
		my.p <- my.p + geom_line(mapping=aes(x=time.passed, y=smoothing_rel_factors), color="#6600CC", size=1.1)
		path <- paste(base_path, "/factors/", "error ranges RE smoothing by "
			,smoothing, ".png", sep="")
		ggsave(filename = path, plot = my.p, width = 50, height = 5, units = "in", limitsize=FALSE)


		shortened_table$smoothing_clusters <- smooth_data(shortened_table$clusters, smoothing)
		shortened_table$smoothing_validity <- smooth_data(shortened_table$validity, smoothing)

		my.p <- ggplot(shortened_table)
		my.p <- my.p + geom_point(mapping=aes(x=time.passed, y=clusters), color="#AAAAAA") + 
			scale_x_continuous(limits=c(1,nrow(data)), breaks=(1:23)*100) + ggtitle(paste("Clusters number, smoothing by", smoothing))
		my.p <- my.p + geom_line(mapping=aes(x=time.passed, y=smoothing_clusters), color="#6600CC", size=1.1)
		path <- paste(base_path, "/factor-number/", "clusters numbers, smoothing by "
			,smoothing, ".png", sep="")
		ggsave(filename = path, plot = my.p, width = 50, height = 5, units = "in", limitsize=FALSE)

		
		my.p <- ggplot(shortened_table)
		my.p <- my.p + geom_line(mapping=aes(x=time.passed, y=validity), color="#AAAAAA") + 
			scale_x_continuous(limits=c(1,nrow(data)), breaks=(1:23)*100) + ggtitle(paste("Clusters validity, smoothing by", smoothing))
		my.p <- my.p + geom_line(mapping=aes(x=time.passed, y=smoothing_validity), color="#6600CC", size=1.1)
		path <- paste(base_path, "/factor-number/", "clusters validity, smoothing by "
			,smoothing, ".png", sep="")
		ggsave(filename = path, plot = my.p, width = 50, height = 5, units = "in", limitsize=FALSE)

		
	}

	rM$true_positives <- (rM$abs == 0)
	shortened_table <- rM[rM$model == "RE < 0.1",] # whatever model, just 1 line per result is needed
	true_positives_percentage <- nrow(shortened_table[shortened_table$true_positives == TRUE,])/nrow(shortened_table)
	tpp_path <- paste(error_ranges_graph, "true_positives_percentage.csv")
	write.csv(file=tpp_path, x=data.frame("True Positive Percentage", true_positives_percentage))

	rM_no_na <- rM[!is.na(rM$predicted),]
	
	rM_no_na$predicted <- change_time_to_new_scale(rM_no_na$predicted)
	rM_no_na$abs  <- distances_range_hour(rM_no_na$predicted, rM_no_na$actual)
	rM_no_na$rel <- rM_no_na$abs/rM_no_na$actual


	PRED <- data.frame(nrow(rM_no_na[rM_no_na$rel <= 0.1, ]), nrow(rM_no_na[rM_no_na$rel <= 0.25, ]), 
		nrow(rM_no_na[rM_no_na$abs <= 0.5, ]), nrow(rM_no_na[rM_no_na$abs <= 1, ]))
	colnames(PRED) <- c("0.1", "0.25", "0.5h", "1h")
	PRED <- PRED/nrow(rM_no_na)
	
	print(PRED)
	print("Prediction rate:")
	print(nrow(rM_no_na)/nrow(rM))

}
}


for (i in rerun){
	calculate(i)
}