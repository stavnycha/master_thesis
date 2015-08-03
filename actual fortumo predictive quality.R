source('normalize_time.R')

full_data <- data[!is.na(data$timeestimate),]
full_data <- full_data[full_data$timeestimate > 0,]

# only 894 rows

errranges_relative <- c(0.1, 0.25)
errranges_absolute <- c(0.5, 1)

full_data$timeestimate_secods <- full_data$timeestimate
full_data$timeestimate <- normalize_time(full_data$timeestimate_secods)

absolute_diff <- distances_range_hour(full_data$timeestimate, full_data$timespent_hours)
relative_diff <- absolute_diff/full_data$timespent_hours

inranges_relative <- c()
for(errrange in errranges_relative) {
	inranges_relative[[as.character(errrange)]] <- sum(as.numeric(relative_diff <= errrange))/length(relative_diff)
}


inranges_absolute <- c()
for(errrange in errranges_absolute) {
	inranges_absolute[[as.character(errrange)]] <- sum(as.numeric(absolute_diff <= errrange))/length(absolute_diff)
}
