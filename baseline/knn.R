
library(ggplot2)
library(tm)
library(SnowballC)
library("clue")
library(gsubfn)
library("XLConnect")
library(kohonen)
library(e1071)
library(kknn)
library(clv)
library(stringr)
library(parallel)
library(lsa)
folder.root <- "."

setwd("D:/tartuUniversity/for_ms/baseline/scripts")
source('./knn_cosine.r')
setwd("D:/tartuUniversity/for_ms")

data <- read.csv(file="fortumo_data_with_time_eng_closed_processed.csv",head=TRUE,sep=",")
data$eventualID <- 1:nrow(data)
data <- data[data$is_estonian == FALSE, ]

data <- data[order(data$created), ]
rownames(data) <- as.character(1:nrow(data))

corpus_descriptions = Corpus(VectorSource(as.character(data$description)))
corpus_titles = Corpus(VectorSource(as.character(data$title)))

data$timespent_hours <- data$timespent / 3600.0
data$timespent_seconds <- data$timespent
#data[data$timespent_seconds <= 1800, ]$timespent_hours <- 0.5

#data[data$timespent_hours == 0.5,]$timespent <-  "0.5"
#data[data$timespent_hours == 1,]$timespent <- "1"
#data[data$timespent_hours >= 2 & data$timespent_hours < 4,]$timespent = "2-3"
#data[data$timespent_hours >= 4 & data$timespent_hours < 7,]$timespent = "4-6"
#data[data$timespent_hours >= 7 & data$timespent_hours < 12,]$timespent = "7-11"
#data[data$timespent_hours >= 12 & data$timespent_hours < 21,]$timespent = "12-20"
#data[data$timespent_hours >= 21 & data$timespent_hours < 41,]$timespent = "21-40"
#data[data$timespent_hours > 40,]$timespent = ">40"
#levels = c("0.5", "1", "2-3", "4-6", "7-11", "12-20", "21-40", ">40")
#data$timespent = factor(data$timespent, levels = levels)
data$created <- as.numeric(as.Date(as.character(data$created)))
setwd("D:/tartuUniversity/for_ms/baseline")

errranges_relative <- c(0.1, 0.25, 0.3, 0.33, 0.5)
errranges_absolute <- c(0.5, 1,3,5)

data$timespent <- normalize_time(data$timespent_seconds)
calculate <- function(i){
	print(i)
	res_path = paste(error_ranges_graph, "classification_description",i,".csv",sep="")

	#if (file.exists(res_path) == TRUE){
	if (FALSE) {
		print("reading existing full set of data...")
		rM <- read.csv(res_path, header = TRUE, sep = ";", row.names = 1)
	} else {
		
	firstrow <- 1
	rows <- i
	lastrow <- firstrow + rows - 1
	rM <- as.data.frame(matrix(nrow=0,ncol=18, dimnames=list(NULL,c("time.passed",
		"model", "inrange", "terms", "train", "test", "k", "minsparse", 
		"maxsparse", "mintfidf", "maxtfidf", "kendall", "spearman", "pearson", "abs", 
		"actual", "predicted", "rel" ))))

	involved_data <- data[firstrow:lastrow, ]
	#CHECK weighting!!
	bug.tdm_desc0 <- DocumentTermMatrix(corpus_descriptions[firstrow:lastrow], control = list(minWordLength = 2, maxWordLength=100, weighting=weightTfIdf))
	bug.tdm_desc <- removeSparseTerms(bug.tdm_desc0, sparse=0.995)
	bug.terms_desc <- Terms(bug.tdm_desc)

	bug.tdm_title <- DocumentTermMatrix(corpus_titles[firstrow:lastrow], control = list(minWordLength = 2, maxWordLength=100, weighting=weightTfIdf))
	#bug.tdm_title <- removeSparseTerms(bug.tdm_title0, sparse=0.995)
	bug.terms_title <- Terms(bug.tdm_title)


	bug.terms.minsparse <- min(apply(as.matrix(bug.tdm_desc), 2, function(x){sum(as.numeric(x > 0))}))/nrow(bug.tdm_desc)
	bug.terms.maxsparse <- max(apply(as.matrix(bug.tdm_desc), 2, function(x){sum(as.numeric(x > 0))}))/nrow(bug.tdm_desc)
	bug.terms.mintfidf <- min(apply(as.matrix(bug.tdm_desc), 2, sum))
	bug.terms.maxtfidf <- min(apply(as.matrix(bug.tdm_desc), 2, sum))

	rt_data <- data[c("id", "timespent", "timespent_hours")][firstrow:lastrow, ]

	colnames(rt_data) <- c("ID", "ResolutionTime", "ResolutionTimeHours")
	predicted_size <- 1
	break_point <- (nrow(bug.tdm_desc) - predicted_size)
	
	bug.predict.desc  <- knn_cosines(bug.tdm_desc, k_vector, rt_data)
	bug.predict.title <- knn_cosines(bug.tdm_title, k_vector, rt_data)	
	bug.predict.classonly_full <- bug.predict.title
	if (!any(is.na(bug.predict.desc$predicted))){
		bug.predict.classonly_full$predicted <- (bug.predict.desc$predicted + bug.predict.title$predicted)/2
	}
	
	print('algorythm done')
	write.csv2(as.matrix(bug.predict.classonly_full),paste(error_ranges_graph, "bug.predict",i,".csv",sep=""))
	for (k in k_vector){
		bug.predict.classonly <- bug.predict.classonly_full[bug.predict.classonly_full$k == k, ]$predicted
		bug.predict.classonly.aerr <- abs(bug.predict.classonly - rt_data[(1 + break_point):nrow(rt_data),3])

		temp <- data.frame(bug.predict.classonly.aerr, rt_data[(1 + break_point):nrow(rt_data),3])
		colnames(temp) <- c("error", "actual")
		temp <- temp[complete.cases(temp),]
		bug.predict.classonly.rerr <- temp$error / temp$actual		

		for(errrange in errranges_relative) {
			inrange <- sum(as.numeric(bug.predict.classonly.rerr <= errrange))/length(bug.predict.classonly.rerr)

			rM[nrow(rM)+1, c("time.passed", "model", "inrange",  
			"terms", "train", "test", "k", "minsparse", 
			"maxsparse", "mintfidf", "maxtfidf", "kendall", "spearman", "pearson", "abs", "actual", 
			"predicted", "rel")] <- 
				c(i, paste("RE <", errrange), inrange, length(bug.terms_desc), nrow(bug.tdm_desc) - predicted_size, predicted_size, k, 
				bug.terms.minsparse, 
				bug.terms.maxsparse, bug.terms.mintfidf, bug.terms.maxtfidf, NA, NA, NA, bug.predict.classonly.aerr,
				rt_data[nrow(rt_data),3], as.character(bug.predict.classonly), 
				bug.predict.classonly.rerr)
		}
	
		for(errrange in errranges_absolute) {
			inrange <- sum(as.numeric(bug.predict.classonly.aerr <= errrange))/length(bug.predict.classonly.aerr)

			rM[nrow(rM)+1, c("time.passed", "model", "inrange",
			"terms", "train", "test", "k", "minsparse", 
			"maxsparse", "mintfidf", "maxtfidf", "kendall", "spearman", "pearson", "abs", "actual", 
			"predicted", "rel")] <- 
				c(i, paste("AE <", errrange), inrange, length(bug.terms_desc), nrow(bug.tdm_desc) - predicted_size, predicted_size, k, 
				bug.terms.minsparse, 
				bug.terms.maxsparse, bug.terms.mintfidf, bug.terms.maxtfidf, NA, NA, NA, bug.predict.classonly.aerr,
				rt_data[nrow(rt_data),3], as.character(bug.predict.classonly), 
				bug.predict.classonly.rerr)
		}
	}

	path = paste(error_ranges_graph, "classification_description",i,".csv",sep="")
	print(path)
	write.csv2(rM, path)

	gc()
	}
	print(rM)
	return(rM)
}

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

k_vector <- c(1,3,5,7,9)
fun = 'knn'

	folder_name = paste(fun)
	error_ranges_graph <- paste(folder.root, folder_name, "error ranges ", sep="/")
	error_ranges_graph <- paste(error_ranges_graph)
	base_path <- paste(folder.root, folder_name, sep="/")

	print(error_ranges_graph)
	print(base_path)

	validities_path <- paste(error_ranges_graph, " all_validities.csv", sep="")
	unlink(validities_path)
	print(validities_path)
	
	all_data_path <- paste(error_ranges_graph, "all_data.csv")
	print(all_data_path)
	if (file.exists(all_data_path) == TRUE){
		print("reading existing full set of data...")
		rM_full <- read.csv(all_data_path, header = TRUE, sep = ",", row.names = 1)
	} else {
		unlink(validities_path)
		rL <- mclapply(50:nrow(data), calculate)
		rM_full <- rL[[1]]
		for(i in 2:length(rL)) {
			if(is.data.frame(rL[[i]]))
				rM_full <- rbind(rM_full, rL[[i]])
			else
				print(rL[[i]])
		}
		write.csv(file=all_data_path, x=rM_full)
	}

	for(i in c(1,3:11, 15:18)) {
		rM_full[,i] <- as.numeric(rM_full[,i])
	}
	
	res <- as.data.frame(matrix(nrow=0,ncol=3, dimnames=list(NULL,c("model", "average value", "k"))))
	for (k in k_vector) {
		rM <- rM_full[rM_full$k == k, ]
		rM$predicted_class <- hours_to_class(rM$predicted)
		rM$abs_distance <- distances_range_hour(rM$predicted_class, rM$actual)
		rM$rel_distance <- rM$abs_distance/rM$actual
		

		for (errrange in errranges_relative){
			rM_temp <- rM[rM$model == paste("RE <", errrange), ]
			apqre <- nrow(rM_temp[rM_temp$rel_distance <= errrange, ])/nrow(rM_temp)
			res[nrow(res) + 1, c("model", "average value", "k")] <- c(paste("RE <", errrange), apqre, k)
		}

		for (errrange in errranges_absolute){
			rM_temp <- rM[rM$model == paste("AE <", errrange), ]
			apqre <- nrow(rM_temp[rM_temp$abs_distance <= errrange, ])/nrow(rM_temp)
			res[nrow(res) + 1, c("model", "average value", "k")] <- c(paste("AE <", errrange), apqre, k)
		}
	}

}
}