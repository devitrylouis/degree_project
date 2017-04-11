transformed <- function(model,output)
{
  
	# Copy of dataframe

  		data<-df

  	# Removing outliers

  		df<-df[ouput[[5]],]

	# Boxcox transformation

		lambda<-ceiling(output[[1]])

		if(lambda==0)
		{
			data[["SalePrice"]]<-log(data[["SalePrice"]])
		}
  		if(lamda!=0)
  		{
  			data[["SalePrice"]]<-(data[["SalePrice"]]^lambda -1)/lambda
  		}
  	
  	# Fitting

	  	X <- as.matrix(df[,-35])
	  	fit <- lm(SalePrice~X,qr=T,data=df)

  	# Output transformed Linear Model
  	return(fit)
}