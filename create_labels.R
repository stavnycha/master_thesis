library(stringr)
library(stringi)

label_for <- function(true_label){
	res <- paste("label_", str_replace_all(as.character(true_label), "-|/", ""), sep="")	
	return(res)
}

create_labels <- function(data){
	labels <- as.character(data$labels)
	if (all(labels == "")) {
		data$label_no_labels <- TRUE
		data$labels <- NULL
		return(data)
	}
	splitted_labels <- unique(unlist(strsplit(labels, ",")))
	uniq <- label_for(splitted_labels)
	for (attr in splitted_labels){		
		data[[label_for(attr)]] <- stri_detect_fixed(data$labels, attr)
	}
	data$label_no_labels <- apply(as.matrix(data[, uniq]), 1, function(s) {all(s == FALSE)})
	data$labels <- NULL
	return(data)
}
