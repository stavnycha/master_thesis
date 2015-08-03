levels = c("[0; 0.5]", "(0.5; 1]", "(1; 3]", "(3; 6]",
		"(6; 11]", "(11; 20]", "(20; 40]", "(40; +Inf)")


normalize_time <- function(set){
	hours <- set / 3600.0
	return(hours_to_class(hours))
}

hour_to_class <- function(hour){
	if (is.na(hour)) {return(NA)}
	classes = list(c(0, 0.5), c(0.5, 1), c(1,3), c(3,6),
		c(6,11), c(11,20), c(20,40), c(40, 10000))
	for (i in 1:length(classes)) {
		class = classes[[i]]
		from = class[1]
		to = class[2]
		if (hour <= to & hour >= from) {
			return(levels[i])
		} 
	}
}

levels = c("[0; 0.5]", "(0.5; 1]", "(1; 3]", "(3; 6]",
		"(6; 11]", "(11; 20]", "(20; 40]", "(40; +Inf)")

distances_range_hour <- function(ranges, hours) {
	return(mapply(distance_range_hour, ranges, hours))
}

distance_range_hour <- function(range, hour){
	if (is.na(range) | is.na(hour)) { return(NA) }
	interval = class_to_range(range)
	from = interval[1]
	to = interval[2]
	if (hour >= from & hour <= to) {
		return(0)
	} else {
		return(min(abs(hour-from), abs(hour-to)))
	}
}

class_to_range <- function(class){
	classes <- c()
	classes[["[0; 0.5]"]] = c(0, 0.5)
	classes[["(0.5; 1]"]] = c(0.5, 1)
	classes[["(1; 3]"]] = c(1, 3)
	classes[["(3; 6]"]] = c(3, 6)
	classes[["(6; 11]"]] = c(6,11)
	classes[["(11; 20]"]] = c(11, 20)
	classes[["(20; 40]"]] = c(20,40)
	classes[["(40; +Inf)"]] = c(40, 10000)
	return(classes[[class]])
}

hours_to_class <- function(hours){	
	return(factor(sapply(hours, hour_to_class), levels=levels))
}


change_time_to_new_scale <- function(set){
	old_scale = c("0.5", "1", "2-3", "4-6", "7-11", "12-20", "21-40", ">40")
	new_scale = c("[0; 0.5]", "(0.5; 1]", "(1; 3]", "(3; 6]",
		"(6; 11]", "(11; 20]", "(20; 40]", "(40; +Inf)")
	res = as.character(set)	
	df = data.frame(set, res)
	df$res <- as.character(df$res)
	df$set <- as.character(df$set)
	
	for (i in 1:length(old_scale)) {
		old <- old_scale[i]
		new <- new_scale[i]	
		if (nrow(df[df$set == old, ]) > 0) {
			df[df$set == old, ]$res <- new 
		}
	}
	return(factor(df$res, levels=new_scale))
}