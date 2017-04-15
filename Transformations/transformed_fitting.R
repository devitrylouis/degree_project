transformed <- function(model,output,df,id)
{
  
	# Copy of dataframe
    if(id==1){name<-"df1"}
    if(id==2){name<-"df2"}
    if(id==3){name<-"df3"}
    if(id==4){name<-"df4"}
  
  
    		data<-df

  	# Removing outliers

    	if(!is.character(output[[5]]))
    	{
        data<-data[!output[[5]],]
    	}
      
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