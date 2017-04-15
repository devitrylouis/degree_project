fitting <- function(train)
{
	# Fix some NAs

   	train$GarageYrBlt[is.na(train$GarageYrBlt)] <- mean(train$GarageYrBlt[!is.na(train$GarageYrBlt)])
		train$MasVnrArea[is.na(train$MasVnrArea)] <- mean(train$MasVnrArea[!is.na(train$MasVnrArea)])
		train$LotFrontage[is.na(train$LotFrontage)] <- mean(train$LotFrontage[!is.na(train$LotFrontage)])

	# Factoring categorical predictors
		
		train<-factoring(train)		
	
	# Deleting factored/unwanted predictors

		train$Id <- NULL
		train$GrLivArea <- NULL
		train$TotalBsmtSF <- NULL
		train$Utilities<-NULL
		train$MiscFeature <- NULL
		train$MiscVal <- NULL

		assign("seq",which(colnames(train) %in% old_names))

	# Linear model preparation

		
		
		# Removing duplicate/unwanted columns
			
			df <- train
			df <- subset(df, select = -seq )

		# Removing outliers
			
			assign("df",df,envir=globalenv())

	# Fitting the model
			
   	#	X<<-as.matrix(df[,-35])
   		
		  fit <- lm(SalePrice~.,qr=T,data=df)
			
	# Returning the fit

		return(fit)
}