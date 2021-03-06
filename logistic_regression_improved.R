library(ggplot2)
library(tm)
library(gsubfn)
library("XLConnect")
library(e1071)
library(clv)
library(stringr)
library(caret)
library(doParallel)
library(RWeka)
require(MASS)
folder.root <- "."

setwd("D:/tartuUniversity/for_ms/scripts")
source(file="smooth.r")
source(file="create_labels.r")
source(file="multiplot.r")
source(file="draw_distribution.r")
source(file="normalize_time.r")

source(file="../baseline/scripts/fixed-polr.r")

setwd("D:/tartuUniversity/for_ms")

data <- read.csv(file="fortumo_data_with_time_eng_closed_processed.csv",head=TRUE,sep=",")
data$eventualID <- 1:nrow(data)
data <- data[data$is_estonian == FALSE, ]

data <- data[order(data$created), ]
rownames(data) <- as.character(1:nrow(data))

errranges_relative <- c(0.1, 0.25, 0.33)
errranges_absolute <- c(0.5, 1, 3)

clusterValueMethods = c("median") 
dataVolumes = c("10percent", "allData")
corpus = Corpus(VectorSource(paste(data$description, data$title)))

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

factor_median <- function(factors){
	i = ceiling(length(factors)/2)
	return(sort(factors)[i])
}

for (elem in c("reporter", "project_name", "type")) {
	avg <- c()
	for (i in 2:nrow(data)){
		current_elem = data[i, elem]
		prev_data = data[1:(i - 1), ]
		prev_data <- prev_data[prev_data[[elem]] == current_elem, ]
		avg[i] <- if (nrow(prev_data) == 0) {NA} else { factor_median(prev_data$timespent) }
	}
	attribute = paste("average", "rt", elem, sep="_")
	data[, attribute] <- avg
}

setwd("D:/tartuUniversity/for_ms")


logistic_regression <- function(important_data){
	important_data$reporter <- as.character(important_data$reporter)
	important_data$type <- as.character(important_data$type)
	important_data$project_name <- as.character(important_data$project_name)
		
	times <- table(important_data$timespent)
	absent <- names(times[times == 0])
	new_levels <- setdiff(levels, absent)
	important_data$timespent <- factor(important_data$timespent, levels=new_levels)

	important_data_train <- important_data[1:(nrow(important_data)-1),]
	important_data_new <- important_data[nrow(important_data):nrow(important_data),]
	
	prediction <- tryCatch({
		reg <- polr(timespent ~ ., data=important_data_train, method="logistic")

		predict(reg, important_data_new)
	}, error = function(e){
		print(e)
		NA
	})
	
	return(prediction)
}


diff_range_number <- function(fact, number){
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


calculate <- function(i){
	print(i)
	firstrow <- if (dataVolume == "allData") { 1 } else { max(1, i-bunch+1) } 
	rows <- if (dataVolume == "allData") { i } else { min(i, bunch) }
	lastrow <- firstrow + rows - 1
	rM <- as.data.frame(matrix(nrow=0,ncol=11, dimnames=list(NULL,c("time.passed",
		"model", "inrange", "train", "test", "validity", "abs", 
		"actual_class", "actual", "predicted", "rel"))))

	involved_data <- data[firstrow:lastrow, ]

	rt_data <- data[c("id", "timespent", "timespent_hours")][firstrow:lastrow, ]
	rt_data_actual <- data[c("id", "timespent_hours")][firstrow:lastrow, ]

	colnames(rt_data) <- c("ID", "ResolutionTime", "ResolutionTimeHours")
	predicted_size <- 1
	break_point <- (nrow(involved_data) - predicted_size)
	
	attributes.important <- c("type", "project_name", "reporter", "labels",
			"average_rt_project_name", "average_rt_reporter", "average_rt_type")

	involved_data <- involved_data[, c(attributes.important, "timespent")]
	involved_data_all_labels <- create_labels(involved_data)

	lbs <- setdiff(colnames(involved_data_all_labels), colnames(involved_data))
	important_labels <- c("label_no_labels", "label_manualwork")
	for (lb in lbs){
		print(lb)
		if (!(lb %in% important_labels)) {
			involved_data_all_labels[[lb]] <- NULL
		}
	}
	involved_data <- involved_data_all_labels

	bug.predict.classonly <- tryCatch({
		logistic_regression(involved_data)
	}, error = function(e){
		print(e)
		NA
	})
	
	print('algorythm done')
	write.csv2(as.matrix(bug.predict.classonly),paste(error_ranges_graph, "bug.predict",i,".csv",sep=""))
	bug.predict.classonly.aerr <- if (is.na(bug.predict.classonly)) {NA} else {
		diff_range_number(bug.predict.classonly, rt_data_actual[(1 + break_point):nrow(rt_data),2])
	}

	temp <- data.frame(bug.predict.classonly.aerr, rt_data_actual[(1 + break_point):nrow(rt_data),2])
	colnames(temp) <- c("error", "actual")
	#temp <- temp[complete.cases(temp),]
	bug.predict.classonly.rerr <- temp$error / temp$actual	

	for(errrange in errranges_relative) {
		inrange <- sum(as.numeric(bug.predict.classonly.rerr <= errrange))/length(bug.predict.classonly.rerr)
		rM[nrow(rM)+1, c("time.passed", "model", "inrange", 
		"train", "test", "validity", "abs", "actual_class", "actual", 
		"predicted", "rel")] <- 
			c(i, paste("RE <", errrange), inrange, nrow(involved_data) - predicted_size, predicted_size, 
			0, bug.predict.classonly.aerr,
			as.character(rt_data[nrow(rt_data),2]), rt_data_actual[nrow(rt_data),2], as.character(bug.predict.classonly), 
			 bug.predict.classonly.rerr )
	}
	
	for(errrange in errranges_absolute) {
		inrange <- sum(as.numeric(bug.predict.classonly.aerr <= errrange))/length(bug.predict.classonly.aerr)
		
		rM[nrow(rM)+1, c("time.passed", "model", "inrange", 		
		"train", "test", "validity", "abs", "actual_class", "actual", 
		"predicted", "rel")] <- 
			c(i, paste("AE <", errrange), inrange, nrow(involved_data) - predicted_size, predicted_size, 
			0, bug.predict.classonly.aerr,
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

dataVolume  <- "allData"
fun = 'logistic regression'
bunch = 50

folder_name = paste(fun)
error_ranges_graph <- paste(folder.root, dataVolume, folder_name, "error ranges ", sep="/")
error_ranges_graph <- paste(error_ranges_graph)
base_path <- paste(folder.root, folder_name, sep="/")

print(error_ranges_graph)
print(base_path)
	
all_data_path <- paste(error_ranges_graph, "all_data.csv")
print(all_data_path)
	
if (file.exists(all_data_path) == TRUE){
		print("reading existing full set of data...")
		rM <- read.csv(all_data_path, header = TRUE, sep = ",", row.names = 1)
	} else {
		rL <- mclapply(100:nrow(data), calculate)
		rM <- rL[[1]]
		for(i in 2:length(rL)) {
			if(is.data.frame(rL[[i]]))
				rM <- rbind(rM, rL[[i]])
			else
				print(rL[[i]])
		}
		write.csv(file=all_data_path, x=rM)
	}

	for(i in c(1,3:7, 9, 11)) {
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
		my.p <- my.p + geom_line(mapping=aes(x=time.passed, y=rel), color="#AAAAAA") + 
			scale_x_continuous(limits=c(1,nrow(data)), breaks=(1:23)*100) + ggtitle(paste("Relative error, smoothing by", smoothing))
		my.p <- my.p + geom_line(mapping=aes(x=time.passed, y=smoothing_rel), color="#6600CC", size=1.1)
		path <- paste(base_path, "/factor-number/", "error ranges RE smoothing by "
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


}
}