sqr <- function(x){
	return(x*x)
}

intra_diameter <- function(cls.scatt) {
	clusters <- length(cls.scatt$intracls.centroid)
	intra <- sum(mapply(sqr, cls.scatt$intracls.centroid))/clusters
	return(intra)
}

inter_distance <- function(cls.scatt){
	inters <- min(apply(cls.scatt$intercls.centroid, 1, function(v) { min(mapply(sqr, v[! v %in% c(0)])) }))
	return(inters)
}

validity <- function(data, pred) {
	cls.scatt <- cls.scatt.data(data, pred)
	intra <- intra_diameter(cls.scatt)
	inter <- inter_distance(cls.scatt)
	validity <- intra/inter
	return(validity)
}
