factoring <- function(train)
{
	#Creating new names for categorical predictors

		new_names = c('paved','regshape','alley_pave','flat','gentle_slope','cul_de_sac_fr3'
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

	# Future list of the mean price per level for each predictor

		price_summary <- list()
	
	# Factoring the data accoding to the aforementioned list

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
						train[[new_names[i]]][is.na(train[[old_names[i]]])]<-matrix[dim(matrix)[1],2]
					}

			# Factor non NA levels

				for (j in 1:(dim(matrix)[1]-temp))
				{
					train[[new_names[i]]][train[[old_names[i]]]==matrix[j,1]]<-matrix[j,2]
				}

			# Convert factors into numeric ones

				train[[new_names[i]]]<-as.numeric(train[[new_names[i]]])
		}
		return(train)
}