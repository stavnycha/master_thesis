# data is DocumentTermMatrix of current part of dataset
# rt_data is resolution time data of current part of dataset
# in function body 10% of data is taken as testing set by default
# returns lest of predicted value for every income issue

validitySilhuette <- function(clustering_res, data.lsa){
	silh = silhouette(clustering_res, data=data.lsa)
	kl_table <- table(clustering_res$cluster)
	single_value_clusters <- as.numeric(names(kl_table[kl_table == 1]))
	width <- summary(silh)$clus.avg.widths
	width[single_value_clusters] <- 0 # should be 0 instead of INF
	return(mean(width))
}

clusters_history <- c()

add_cluster_to_history <- function(cluster){
	clusters_history[length(clusters_history) + 1] <<- cluster
}

default_clusters_number <- function(){
	projects <- length(unique(data$project_name))
	return(projects)
}

clusters_for_clustering <- function(){
	optimal = if (length(clusters_history) < 2){
		default_clusters_number()
	} else {
		clusters_history[length(clusters_history)]
	}
	optimal = min(optimal, bunch-2)
	optimal = max(4, optimal)
	return((optimal-2):(optimal+2))
}

require(lsa)
toLsa <- function(bug.tdm){
	td.mat <- t(as.matrix(bug.tdm))
	td.mat.lsa <- lw_logtf(td.mat) * gw_idf(td.mat)
	lsaSpace <- lsa(td.mat.lsa)  # create LSA space
	lsaSpaceMat <- lsaSpace$tk %*% diag(lsaSpace$sk) %*% t(lsaSpace$dk)
	return(t(lsaSpaceMat))
}

# remove zero rows
normalize_lsa_matrix <- function(data.lsa){
	rowsums <- rowSums(data.lsa)
	zero_rows <- names(rowsums[rowsums == 0])
	normalized <- data.lsa[!(rownames(data.lsa) %in% zero_rows), ]
	return(normalized)
}



automatic_clustering_cosine_lsa_silhuette <- function(data.lsa, i){
	clusters_range <- clusters_for_clustering()

	validities <- c()
	cluster_results <- list()
	max_validity <- -1 
	max_validity_cluster_number <- -1 

	for (cl in clusters_range){
		kmeansval <- skmeans(data.lsa, cl)
		validities[cl] <- validitySilhuette(kmeansval, data.lsa)
		cluster_results[[cl]] <- kmeansval$cluster
		if (max_validity < validities[cl]) {
			max_validity <- validities[cl]
			max_validity_cluster_number = cl
		}
	}
	add_cluster_to_history(max_validity_cluster_number)
	print(paste("Validities:", validities))

	res_data <- data.frame(i, max_validity, max_validity_cluster_number)
	colnames(res_data) <- c("i", "validity", "cluster number")
	validities_path <- paste(error_ranges_graph, " all_validities.csv", sep="")

	print(res_data)
	if (file.exists(validities_path) == TRUE){
		write.table(res_data, file = validities_path, sep = ",", 
            	col.names = FALSE, row.names = FALSE,  append=TRUE)
	} else {
		write.table(res_data, file = validities_path, sep = ",", 
            	row.names = FALSE)
	}
	res <- c()
	res$clusters <- cluster_results[[max_validity_cluster_number]]
	res$validity <- max_validity
	return(res) 
}

kmeans_f_automatic_cosine_lsa_silhuette <- function(i, bug.tdm, rt_data, sheet_name){
	data.lsa <- toLsa(bug.tdm)
	data.lsa <- normalize_lsa_matrix(data.lsa)
	rt_data <- rt_data[rownames(rt_data) %in% rownames(data.lsa),]
	predicted_size <- 1
	break_point <- (nrow(data.lsa) - predicted_size)
	cluster_results_path <- paste(error_ranges_graph, " all_clusters", i, ".csv", sep="")

	kmeansval_all <- automatic_clustering_cosine_lsa_silhuette(data.lsa, i)
	kmeansval <- kmeansval_all$clusters
	df_res <- data.frame(rt_data$ID, rt_data$ResolutionTime, paste("cluster_", kmeansval, sep=''))
	colnames(df_res) <- c("defectID", "Resolution Time", "cluster")

	predicted <- kmeansval[(break_point+ 1):nrow(data.lsa)]
	actual <- rt_data[(break_point + 1):nrow(data.lsa),]$ResolutionTime
	existing <- kmeansval[1:break_point]
	temp <- data.frame(kmeansval, rt_data[1:nrow(data.lsa),]$ResolutionTime, rt_data[1:nrow(data.lsa),]$ResolutionTimeHours)
	colnames(temp) <- c('cluster', 'time', 'time_hours')
	write.csv2(temp, file = cluster_results_path)

	draw_distributions(temp)
	print("Distributions done")
	#print(head(temp))
	bug.class.rt <- aggregate(time ~ cluster, temp, factor_median)	
	bug.class.number <- aggregate(time ~ cluster, temp,  length )
	bug.class.rt$size <- bug.class.number$time
	bug.class.rt$sd <- aggregate(time_hours ~ cluster, temp,  sd )$time_hours

	write.csv2(bug.class.rt, file = paste(error_ranges_graph, "predictors", i, ".csv", sep=""))
	
	print(bug.class.rt)
	bug.predict <- unlist(lapply(predicted, FUN=function(x){bug.class.rt[paste(x),2]}))
	clusters <- length(unique(as.numeric(kmeansval)))
	res <- data.frame(bug.predict, clusters, kmeansval_all$validity)
	colnames(res) <- c("prediction", "clusters", "validity")
	print(res)
	return(res)
}