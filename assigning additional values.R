# year of the issue
data$year = data$created
data$year <- format(as.Date(as.character(data$year )), "%Y")
data$year <- as.factor(data$year )


# number of issues of reporter

# average rt of reporter

# averate rt of project

# number of issues in project

# average rt of type

# average rt of priority

# number of issues in type

# number of issues in priority


# number of issues
# of reporter

no_issues_reporter <- c()
for (i in 2:nrow(data)){
	reporter = data[i,]$reporter	
	prev_data = data[1:(i - 1), ]
	no_issues_reporter[i] <- nrow(prev_data[prev_data$reporter == reporter, ])
} 
data$no_issues_reporter <- no_issues_reporter

# of project
no_issues_project <- c()
for (i in 2:nrow(data)){
	project = data[i,]$project_name	
	prev_data = data[1:(i - 1), ]
	no_issues_project[i] <- nrow(prev_data[prev_data$project_name == project, ])
} 
data$no_issues_project <- no_issues_project


# of type
no_issues_type <- c()
for (i in 2:nrow(data)){
	type = data[i,]$type	
	prev_data = data[1:(i - 1), ]
	no_issues_type[i] <- nrow(prev_data[prev_data$type == type, ])
} 
data$no_issues_type <- no_issues_type



# of priority
no_issues_priority <- c()
for (i in 2:nrow(data)){
	priority = data[i,]$priority	
	prev_data = data[1:(i - 1), ]
	no_issues_priority[i] <- nrow(prev_data[prev_data$priority == priority, ])
} 
data$no_issues_priority <- no_issues_priority

levels <- c('0.5', '1', '2-3', '4-6', '7-11', '12-20', '21-40', '>40')

for (elem in c("reporter", "project_name", "type", "priority")) {
	avg <- c()
	for (i in 2:nrow(data)){
		current_elem = data[i, elem]
		prev_data = data[1:(i - 1), ]
		prev_data <- prev_data[prev_data[[elem]] == current_elem, ]
		avg[i] <- if (nrow(prev_data) == 0) {NA} else { as.character(factor_median(prev_data$timespent)) }
	}
	attribute = paste("average", "rt", elem, sep="_")
	data[, attribute] <- factor(avg, levels=levels)	
}

factor_median <- function(factors){
	i = ceiling(length(factors)/2)
	return(sort(factors)[i])
}

library(Hmisc)

# spearman correlation
correlations <- c()
for ( attr in c("no_issues_priority", "no_issues_type", "no_issues_project", "no_issues_reporter")){
	correlations[[attr]] <- rcorr(data[[attr]], data$timespent, type ="spearman") 
}

# kruskall wallis test

kwt <- c()
for (elem in c("reporter", "project_name", "type", "priority")) {
	attribute = paste("average", "rt", elem, sep="_")
	fml <- as.formula(paste("timespent", attribute, sep=" ~ "))
	print(fml)
	test <- kruskal.test(fml, data)
	kwt[[attribute ]] <- test
}


kwt <- c()
for (attribute in c("reporter", "project_name", "type", "priority")) {
	fml <- as.formula(paste("timespent", attribute, sep=" ~ "))
	print(fml)
	test <- kruskal.test(fml, data)
	kwt[[attribute ]] <- test
}


labeled_data <- create_labels(data)
labels <- setdiff(colnames(labeled_data), colnames(data))

frm <- as.formula(paste("timespent", paste(labels, collapse=" + "), sep=" ~ "))
kruskal.test(frm , data = labeled_data)

label_tests <- c()
for (label in labels){
	print(label)
	cont_table <- table(labeled_data[[label]], labeled_data$timespent)
	label_tests[[label]] <- chisq.test(cont_table)
}


# number of issues
for (label in labels){
	print(label)
	print(nrow(labeled_data[labeled_data[[label]] == TRUE, ]))
}

