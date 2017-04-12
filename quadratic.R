quadratic <- function()
{
	trans_predictors = c("OverallQual","GarageCars","GarageArea"
		, "1stFlrSF","YearBuilt","YearRemodAdd","MasVnrArea","FullBath"
		, "TotRmsAbvGrd","Fireplaces","GarageYrBlt","LotFrontage"
		, "BsmtFinSF1","2ndFlrSF")

	new_pred=c("paved","alley_pave" ,"roof_style","roof_matl","ext1_price_level"
			,"ext2_price_level","masonry_type","exterior_qual_level","exterior_cond_level"
			,"basement_qual_level","basement_cond_level","basementfin_type1_level"
			,"basementfin_type2_level"
			,"BsmtFinSF1"	# Per feet
			,"BsmtFinSF2"	# Per feet
			,"heating_level","heating_qc_level","central_air_yes","electrical_level"
			,"BsmtFullBath","BsmtHalfBath","FullBath","HalfBath","kitchen_qual_level"
			,"functionality_level","fireplace_qual_level","Fireplaces","garage_finish_level"
			,"garage_qual_level","garage_cond_level","fence_qual_level","sale_condition_level"
			,"sale_type_level")

	trans_predictors<-cbind(trans_predictors,new_pred)

	p_value<-numeric(length(trans_predictors))
	significance <- logical(length(trans_predictors))

	for (i in 1:length(trans_predictors)) 
	{
		fit1 <- lm(df$SalePrice ~.,data=df)
		fit2 <- lm(df$SalePrice ~. +poly(df[[ trans_predictors[i] ]],2) ,data=df)
		anov<-anova(fit1,fit2)
		p_value[i]<-anov$`Pr(>F)`[2]
		if(p_value[i]<=0.01)
		{
		  significance[i]<-TRUE
		}
	}

	mat<-cbind(trans_predictors,p_value,significance)

}