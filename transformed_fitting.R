transformed <- function(model,output,df,id)
{
  
	# Copy of dataframe
    if(id==1){name<-"df1"}
    if(id==2){name<-"df2"}
  		data<-df

  	# Removing outliers
    #  vec<-sort((as.integer(output[[5]])),decreasing = TRUE)
     # index <- which(as.numeric(rownames(df)) %in% vec)
      data<-data[!output[[5]],]
      
	# Boxcox transformation
    if(typeof(output[[1]])!="character")
    {  
		lambda<-round(unlist(output[[1]]))

		if(lambda==0)
		{
			data[["SalePrice"]]<-log(as.numeric(data[["SalePrice"]]))
		}
  		if(lambda!=0)
  		{
  			data[["SalePrice"]]<-(data[["SalePrice"]]^lambda -1)/lambda
  		}
    }
      assign(name,data,envir = globalenv())
      
  	# Fitting

	  	fit <- lm(SalePrice~.,qr=T,data=data)

  	# Output transformed Linear Model
  	return(fit)
}