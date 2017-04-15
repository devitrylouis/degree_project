removing_neighborhoods <- function(data)
{
	l<-c('StoneBr','NridgHt','NoRidge')
	index<-which(data[["Neighborhood"]]%in%l)
	data<-data[-index,]
	return(data)
}