new_names <<- c('paved','regshape','alley_pave','flat','gentle_slope','cul_de_sac_fr3'
                ,'nbhd_price_level','prox1','prox2','dwelling','house_style_price_level','roof_style'
                ,'roof_matl','ext1_price_level','ext2_price_level','masonry_type'
                ,'exterior_qual_level','exterior_cond_level','foundation_level'
                ,'basement_qual_level','basement_cond_level','basement_exposure_level'
                ,'basementfin_type1_level','basementfin_type2_level','heating_level'
                ,'heating_qc_level','central_air_yes','electrical_level','kitchen_qual_level'
                ,'functionality_level','fireplace_qual_level','garage_type_level'
                ,'garage_finish_level','garage_qual_level','garage_cond_level'
                ,'paved_driveway_level','pool_qc_level','fence_qual_level','sale_type_level'
                ,'sale_condition_level','zoning_level')

# Original names of the categorical predictors

old_names <<- c('Street','LotShape','Alley','LandContour','LandSlope','LotConfig'
                ,'Neighborhood','Condition1','Condition2','BldgType','HouseStyle'
                ,'RoofStyle','RoofMatl','Exterior1st','Exterior2nd','MasVnrType'
                ,'ExterQual','ExterCond','Foundation','BsmtQual','BsmtCond'
                ,'BsmtExposure','BsmtFinType1','BsmtFinType2','Heating','HeatingQC'
                ,'CentralAir','Electrical','KitchenQual','Functional','FireplaceQu'
                ,'GarageType','GarageFinish','GarageQual','GarageCond','PavedDrive'
                ,'PoolQC','Fence','SaleType','SaleCondition','MSZoning')


test$GarageYrBlt[is.na(test$GarageYrBlt)] <- 0
test$MasVnrArea[is.na(test$MasVnrArea)] <- 0
test$LotFrontage[is.na(test$LotFrontage)] <- 0

# Factoring categorical predictors
price_summary<-list()
for (i in 1:length(old_names))
{
  # Construction of the list
  
  price_summary[[i]]<-summarize(group_by(train, train[[old_names[i]]]),mean(SalePrice, na.rm=T))
  matrix<-as.matrix(price_summary[[i]])
  
  # Handling NA levels
  
  # Temporary variable to control NA values
  
  temp<-0
  
  # Factor NA level if any
  
  if(is.na(matrix[dim(matrix)[1],1]))
  {
    temp<-1
    test[[new_names[i]]][is.na(test[[old_names[i]]])]<-matrix[dim(matrix)[1],2]
  }
  
  # Factor non NA levels
  
  for (j in 1:(dim(matrix)[1]-temp))
  {
    test[[new_names[i]]][test[[old_names[i]]]==matrix[j,1]]<-matrix[j,2]
  }
  
  # Convert factors into numeric ones
  
  test[[new_names[i]]]<-as.numeric(test[[new_names[i]]])
}
# Deleting factored/unwanted predictors

test$Id <- NULL
test$GrLivArea <- NULL
test$TotalBsmtSF <- NULL
test$Utilities<-NULL
test$MiscFeature <- NULL

assign("seqq",which(colnames(test) %in% old_names))

# Linear model preparation



# Removing duplicate/unwanted columns

df_test <- test
df_test <- subset(df_test, select = -seqq )
mat <- scale(df_test)
df_test <- data.table(mat)
df_test[["1stFlrSF2"]]<-df_test[["1stFlrSF"]]^2
df_test[["TotRmsAbvGrd2"]]<-df_test[["TotRmsAbvGrd"]]^2



lasso.pred<-exp(exp(predict(lasso[[3]],s=lasso[[2]],as.matrix(df_test))))

lasso.pred<-as.data.frame(lasso.pred)

lasso.pred[is.na(lasso.pred)]<-200000