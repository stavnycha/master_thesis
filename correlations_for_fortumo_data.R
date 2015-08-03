library(ggplot2)
library(tm)
library(gsubfn)
library(e1071)
library(clv)
library(polycor)
folder.root <- "."

setwd("D:/tartuUniversity/for_ms")

data <- read.csv(file="fortumo_data_with_time_eng_closed_processed.csv",head=TRUE,sep=",")
#pattern = "%m/%d/%Y %H:%M"
#data$ResolutionTime <- data$ResolutionTimeDays
#data <- data[data$ResolutionTime < 200, ]
#data <- data[!is.na(data$ResolutionTime), ]
#data <- data[data$Type == 'Defect', ]
data$eventualID <- 1:nrow(data)
data <- data[data$is_estonian == FALSE, ]

data <- data[order(data$created), ]
rownames(data) <- as.character(1:nrow(data))

errranges_relative <- c(0.1, 0.25, 0.33)
errranges_absolute <- c(1, 3, 5)
clustersNumber <- c(7)
neighs <- c(5, 10, 50, 100, 150)
ns <- c(1, 4) 
ms <- c(4)
clusterValueMethods = c("mean", "median", 'mode') 
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
data$created <- as.numeric(as.Date(as.character(data$created)))
data$priority <- factor(data$priority, levels=priorities)

contingency_priority <- table(data$priority, data$timespent)
chisq_prior <- chisq.test(contingency_priority) #independant


contingency_type <- table(data$type, data$timespent)
chisq_type <- chisq.test(contingency_type) #dependant a lot!


contingency_project <- table(data$project_name, data$timespent)
chisq_project <- chisq.test(contingency_project) #dependant a lot!


contingency_reporter <- table(data$reporter, data$timespent)
chisq_reporter <- chisq.test(contingency_reporter) #dependant a lot!


contingency_created <- table(data$created, data$timespent)
chisq_created <- chisq.test(contingency_created) #dependant a lot!


kruskal.test(timespent ~ label_Penny, data = data_labeled) 
