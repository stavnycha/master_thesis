knn_cosines <- function(tdm, k_vector, rt_data){
	rM <- as.data.frame(matrix(nrow=0,ncol=2, dimnames=list(NULL,c("k", "predicted"))))
	distances <- cosine_distances(tdm)
	for (k in k_vector){
		res <- knn_cosine(distances , k, rt_data)
		rM[nrow(rM)+1, c("k", "predicted")] <- c(k, res)
	}	
	return(rM)
}


knn_cosine <- function(distances, k, rt_data){
	issues <- k_max(distances, k)
	return(mean(rt_data[issues, ]$ResolutionTimeHours))
}

cosine_distances <- function(tdm){
	tdm <- as.matrix(tdm)
	new <- tdm[nrow(tdm), ]
	old <- tdm[1:(nrow(tdm)-1), ]
	res <- c()
	for (i in 1:nrow(old)){
		res[i] <- cosine(new, old[i, ])
	}
	names(res) <- rownames(old)
	return(res)
}

k_max <- function(v, k){
	res <- c()
	for (i in 1:k){
		max <- which.max(v)
		index <- names(max)
		v <- v[!names(v) %in% index]
		res[i] <- index
	}
	return(res)
}