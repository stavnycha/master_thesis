aknn_cosines <- function(tdm, a_vector, rt_data){
	rM <- as.data.frame(matrix(nrow=0,ncol=2, dimnames=list(NULL,c("a", "predicted"))))
	distances <- cosine_distances(tdm)

	for (a in a_vector){
		res <- aknn_cosine(distances , a, rt_data)
		rM[nrow(rM)+1, c("a", "predicted")] <- c(a, res)
	}	
	return(rM)
}

aknn_cosine <- function(distances, a, rt_data){
	distances <- distances[!is.na(distances)]
	issues <- names(distances[distances >= a])
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
