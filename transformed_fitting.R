transformed <- function(model,output)
{
  
	# Copy of dataframe

  		data<-df

  	# Removing outliers
    #  vec<-sort((as.integer(output[[5]])),decreasing = TRUE)
     # index <- which(as.numeric(rownames(df)) %in% vec)
      data<-data[!output[[5]],]
      
	# Boxcox transformation

		lambda<-round(unlist(output[[1]]))

		if(lambda==0)
		{
			data[["SalePrice"]]<-log(as.numeric(data[["SalePrice"]]))
			fit<-lm(log(SalePrice)~.,qr=T,data=data)
		}
  		if(lambda!=0)
  		{
  			data[["SalePrice"]]<-(data[["SalePrice"]]^lambda -1)/lambda
  			fit <- lm(SalePrice~.,qr=T,data=data)
  		}
  	assign("data",data,envir=globalenv())
  	# Fitting

	  	fit <- lm(SalePrice~.,qr=T,data=data)

  	# Output transformed Linear Model
  	return(fit)
}