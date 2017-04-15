cubic_predictors <- function(df)
{
  names <- character()

  predictors = c("MSSubClass","LotFrontage","LotArea","OverallQual","OverallCond"
                   ,"YearBuilt","YearRemodAdd","MasVnrArea","BsmtFinSF1","BsmtFinSF2","BsmtUnfSF","1stFlrSF","2ndFlrSF"
                   ,"LowQualFinSF","BsmtFullBath","BsmtHalfBath","FullBath","HalfBath","BedroomAbvGr"
                   ,"KitchenAbvGr","TotRmsAbvGrd","GarageYrBlt","GarageCars","GarageArea","WoodDeckSF","OpenPorchSF","EnclosedPorch"
                   ,"3SsnPorch","ScreenPorch","PoolArea","MoSold","YrSold")
j<-0
while(length(predictors)>0)
  {
	j<-j+1
    p_value<-numeric(length(predictors))
    significance <- logical(length(predictors))
    for (i in 1:length(predictors)) 
    {
      fit1 <- lm(df$SalePrice ~.,data=df)
      fit2 <- lm(df$SalePrice ~. +I(df[[ predictors[i] ]]^3) ,data=df)
      anov<-anova(fit1,fit2)
      p_value[i]<-anov$`Pr(>F)`[2]
      if(p_value[i]<=0.01)
      {
        significance[i]<-TRUE
      }
    }
    
    # Predictor higher term with the lowest p-value
    
    names[j] <- predictors[which.min(p_value)]
    
    # Predictors of interest are kept
    
    indices <- c(which.min(p_value), which(significance==FALSE))
    
    predictors <- predictors[-indices]
    
    # Add the most significant to the data frame
    
    df[[paste(names[j],"3")]] <- df[[names[j]]]^3 ### Concatenate name
  }
  
  return(df)
}
