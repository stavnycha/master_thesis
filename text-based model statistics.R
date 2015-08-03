# statistics without filtering
# description 50

# 0 predictions with (40, +Inf)
# 3 prediction (20, 40), 2 are correct
# for all actual >40 only 1 expert estimate, it is (3,6) , same as model prediction

# description 200

# 0 predictions with (40, +Inf)
# 1 prediction (20, 40), is correct
# for all actual >40 only 1 expert estimate, it is (3,6) , same as model prediction


# title 50

# 1  >40 prediction, is correct
# 7 predictions 20-40, 4 are correct, expert estimate for all of them is extrmely wrong

# title 200
# 0 predictions >40
# 3 predictions 20-40, 2 are correct, experts estimates are too bad
# for all issues >40 predictions are bad, experts one also

display_statistics <- function(part, bunch, filtering=FALSE) {
	table <- get_data(part, bunch, filtering)
	display_columns <- c("time.passed", "actual_class", "predicted", "timeestimate")
	print("Cases where we predict >40")
	print(table[table$predicted == "(40; +Inf)", display_columns])
	print("")
	print("Cases where we predict 20-40")
	print(table[table$predicted == "(20; 40]", display_columns])
	print("")
	print("Where there is actually >40")
	print(table[table$actual_class == "(40; +Inf)", display_columns])
}

get_data <- function(part, bunch, filtering=FALSE){
	fun = if (filtering) { fun = 'kmeans silhuette with filtering'} else {'kmeans silhuette' }
	dataVolume  <- "10percent"
	clusterValueMethod <- "median"
	clusters <- "automatic"
	folder_name = paste(fun, clusters, part)
	print(paste(part, bunch))

	error_ranges_graph <- paste(folder.root, dataVolume, 'really dynamic k', folder_name, bunch, "error ranges ", sep="/")
	error_ranges_graph <- paste(error_ranges_graph, clusters, clusterValueMethod)

	all_data_path <- paste(error_ranges_graph, "all_data.csv")

	rM <- read.csv(all_data_path, header = TRUE, sep = ",", row.names = 1)

	for(i in c(1,3:12, 16, 18, 20 )) {
		rM[,i] <- as.numeric(rM[,i])
	}
	shortened_table <- rM[rM$model == "RE < 0.1",]
	shortened_table <- shortened_table[!is.na(shortened_table$predicted),]

	shortened_table$predicted <- change_time_to_new_scale(shortened_table$predicted)
	shortened_table$actual_class <- change_time_to_new_scale(shortened_table$actual_class)


	# adding actual prediction
	tryCatch({
		data[data$timeestimate == 0 & !is.na(data$timeestimate), ]$timeestimate <- NA
	}, error=function(err){})
	
	normalized_estimate <- normalize_time(data$timeestimate)
	shortened_table$timeestimate <- normalized_estimate[shortened_table$time.passed]
	return(shortened_table)
}

statistic_percentages <- function(part, bunch, filtering=FALSE){
	table <- get_data(part, bunch, filtering)
	levels = c("[0; 0.5]", "(0.5; 1]", "(1; 3]", "(3; 6]",
		"(6; 11]", "(11; 20]", "(20; 40]", "(40; +Inf)")

	res <- as.data.frame(matrix(nrow=0,ncol=2, dimnames=list(NULL,c("level","correct_percentage"))))

	for (level in levels){
		shortened_table <- table[table$actual_class == level,]
		correct_predictions <- shortened_table[shortened_table$predicted == level,]
		percentage <- round((nrow(correct_predictions) / nrow(shortened_table)) * 100, 2)
		res[nrow(res)+1, c("level","correct_percentage")] <- 
			c(level, percentage)
	}
	return(res)
}

statistic_percentages_full <- function(part, bunch){
	before <- statistic_percentages(part, bunch, FALSE)
	after  <- statistic_percentages(part, bunch, TRUE)
	change = (as.numeric(after$correct_percentage) * 100 / as.numeric(before$correct_percentage)) - 100

	before[, "change with filtering"] <- round(change, 2)
	return(before)
}

title50 <- statistic_percentages_full("title", 50)
title200 <- statistic_percentages_full("title", 200)
description50 <- statistic_percentages_full("description", 50)
description200 <- statistic_percentages_full("description", 200)

models <- list(title50, title200, description50, description200)
model_names <- c("title-based (by 50 issue reports)", 
"title-based (by 200 issue reports)", 
"description-based (by 50 issue reports)", 
"description-based (by 200 issue reports)")

plot_data <- as.data.frame(matrix(nrow=0,ncol=3, dimnames=list(NULL,c("model", "level", "percentage"))))

for (level in levels){
	for (i in 1:length(models)){
		plot_data[nrow(plot_data)+1, c("model", "level", "percentage")] <- 
			c(model_names[i], level, models[[i]][models[[i]]$level == level, "change with filtering"])
	}
}

plot_data$level <- factor(plot_data$level, levels=levels)
plot_data$percentage <- round(as.numeric(plot_data$percentage), 1)
plot_data[is.infinite(plot_data$percentage) | is.nan(plot_data$percentage), ]$percentage <- NA

p <- ggplot(plot_data) + geom_line(mapping=aes(color=model, group=model, x=level, y=percentage)) + 
	 scale_y_continuous(limits=c(-100, 100)) + 
	ggtitle("The change in percentage after removing outliers for every class of RT") +
	xlab("Class of RT") + ylab("Percentage") + scale_color_discrete(name = "Model Type")