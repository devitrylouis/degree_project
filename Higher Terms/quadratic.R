quadratic <- function(df)
{
	predictors = c("OverallQual","GarageCars","GarageArea"
		, "1stFlrSF","YearBuilt","YearRemodAdd","MasVnrArea","FullBath"
		, "TotRmsAbvGrd","Fireplaces","GarageYrBlt","LotFrontage"
		, "BsmtFinSF1","2ndFlrSF")

	p_value<-numeric(length(predictors))
	significance <- logical(length(predictors))

	for (i in 1:length(predictors)) 
	{
		fit1 <- lm(df$SalePrice ~.,data=df)
		fit2 <- lm(df$SalePrice ~. +poly(df[[ predictors[i] ]],2,raw=TRUE) ,data=df)
		anov<-anova(fit1,fit2)
		p_value[i]<-anov$`Pr(>F)`[2]
		if(p_value[i]<=0.01)
		{
		  significance[i]<-TRUE
		}
	}

	mat<-cbind(predictors,p_value,significance)

	output<-list()
	output[[1]]<-predictors
	output[[2]]<-p_value
	output[[3]]<-significance
  return(output)
}