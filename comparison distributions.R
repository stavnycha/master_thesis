# actual 

data$timespent <- change_time_to_new_scale(data$timespent)

actual <- ggplot(data) + geom_histogram(aes(x=timespent)) +
		ggtitle("Distribution of Actual Resolution Time (RT)") + 
		xlab("Classes of RT") + ylab("Number of Issues") +
		theme(text=element_text(size=9)) + scale_y_continuous(limits=c(0, 1750))



# text-based

all_data_path <- "cosine_lsa/10percent/really dynamic k/kmeans silhuette automatic title/50/error ranges  automatic median all_data.csv"
rM <- read.csv(all_data_path, header = TRUE, sep = ",", row.names = 1)
rM_no_na <- rM[!is.na(rM$predicted),]	
rM_no_na$predicted <- change_time_to_new_scale(rM_no_na$predicted)
shortened_table <- rM_no_na[rM_no_na$model == "RE < 0.1",]
shortened_table$predicted <- factor(shortened_table$predicted, levels=levels)

textbased <- ggplot(shortened_table) + geom_histogram(aes(x=predicted)) +
		ggtitle("Distribution of Predicted Resolution Time (RT), text-based model") + 
		xlab("Classes of RT") + ylab("Number of Issues") +
		theme(text=element_text(size=9)) + scale_y_continuous(limits=c(0, 1750))



# meta-information based


all_data_path <- "allData/logistic regression preprocessed/error ranges  all_data.csv"
rM <- read.csv(all_data_path, header = TRUE, sep = ",", row.names = 1)
rM_no_na <- rM[!is.na(rM$predicted),]	
rM_no_na$predicted <- change_time_to_new_scale(rM_no_na$predicted)
shortened_table <- rM_no_na[rM_no_na$model == "RE < 0.1",]
shortened_table$predicted <- factor(shortened_table$predicted, levels=levels)

table_data <- table(shortened_table$predicted)
print(table_data)
olr <- ggplot() + geom_histogram(aes(x=factor(names(table_data), levels=levels), y=as.numeric(table_data)), stat="identity") +
		ggtitle("Distribution of Predicted Resolution Time (RT), meta-information based model, OLR") + 
		xlab("Classes of RT") + ylab("Number of Issues") +
		theme(text=element_text(size=9)) + scale_y_continuous(limits=c(0, 1750))





all_data_path <- "allData/random-forest/error ranges  all_data.csv"
rM <- read.csv(all_data_path, header = TRUE, sep = ",", row.names = 1)
rM_no_na <- rM[!is.na(rM$predicted),]	
rM_no_na$predicted <- change_time_to_new_scale(rM_no_na$predicted)
shortened_table <- rM_no_na[rM_no_na$model == "RE < 0.1",]
shortened_table$predicted <- factor(shortened_table$predicted, levels=levels)

table_data <- table(shortened_table$predicted)
print(table_data)
rf <- ggplot() + geom_histogram(aes(x=factor(names(table_data), levels=levels), y=as.numeric(table_data)), stat="identity") +
		ggtitle("Distribution of Predicted Resolution Time (RT), meta-information based model, RF") + 
		xlab("Classes of RT") + ylab("Number of Issues") + 
		theme(text=element_text(size=9)) + scale_y_continuous(limits=c(0, 1750))

multiplot(olr, rf, textbased, actual)
