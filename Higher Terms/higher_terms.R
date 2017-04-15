higher_terms <- function(df,k_max)
{
  names <- character()

  predictors = c("MSSubClass","LotFrontage","LotArea","OverallQual","OverallCond"
                   ,"YearBuilt","YearRemodAdd","BsmtFinSF1","BsmtFinSF2","BsmtUnfSF","1stFlrSF","2ndFlrSF"
                   ,"LowQualFinSF","BsmtFullBath","BsmtHalfBath","FullBath","HalfBath","BedroomAbvGr"
                   ,"KitchenAbvGr","TotRmsAbvGrd","GarageYrBlt","GarageArea","WoodDeckSF","OpenPorchSF","EnclosedPorch"
                   ,"3SsnPorch","ScreenPorch","MoSold","YrSold")
j<-0
while(length(predictors)>0)
  {
j<-j+1
    p_value <- matrix(NA,nrow=k_max-1,ncol=length(predictors))
    significance <- matrix(FALSE,nrow=k_max-1,ncol=length(predictors))
    for (i in 1:length(predictors)) 
    {
      for(k in 2:k_max)
      {
        fit1 <- lm(df$SalePrice ~.,data=df)
        fit2 <- lm(df$SalePrice ~. +I(df[[ predictors[i] ]]^k) ,data=df)
        anov<-anova(fit1,fit2)
        p_value[k-1,i]<-anov$`Pr(>F)`[2]
        if(p_value[k-1,i]<=0.01)
        {
          significance[k-1,i]<-TRUE
        }
      }
    }
    
    # Predictor higher term with the lowest p-value
    inds<-which(p_value == min(p_value), arr.ind=TRUE)
    names[j] <- predictors[inds[2]]
    
    # Predictors of interest are kept
    test_significance <- logical(k_max-1)
    
    indices<-inds
    
    for(l in 1:length(predictors))
    {
      if(all(significance[,l]==test_significance))
      {
        indices<-c(indices,l)
      }
    }
    
    predictors <- predictors[-indices]
    
    # Add the most significant to the data frame
    degree<-as.character(inds[1]+1)
    df[[paste(names[j],degree)]] <- df[[names[j]]]^(inds[1]+1) ### Concatenate name
  }
  

  return(df)
}
