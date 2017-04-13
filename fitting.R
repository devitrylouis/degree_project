fitting <- function(train)
{
	# Fix some NAs

   	train$GarageYrBlt[is.na(train$GarageYrBlt)] <- 0
		train$MasVnrArea[is.na(train$MasVnrArea)] <- 0
		train$LotFrontage[is.na(train$LotFrontage)] <- 0

	# Factoring categorical predictors
		
		train<-factoring(train)		
	
	# Deleting factored/unwanted predictors

		train$Id <- NULL
		train$GrLivArea <- NULL
		train$TotalBsmtSF <- NULL
		train$Utilities<-NULL
		train$MiscFeature <- NULL

		assign("seq",which(colnames(train) %in% old_names))

	# Linear model preparation

		
		
		# Removing duplicate/unwanted columns
			
			df <- train
			df <- subset(df, select = -seq )

		# Removing outliers
			
			# assign("train",train[-c(31,496,524,633,945,1299),],envir=globalenv())
			
			assign("df",df,envir=globalenv())

	# Fitting the model
			
   	#	X<<-as.matrix(df[,-35])
   		
		  fit <- lm(SalePrice~.,qr=T,data=df)
			
	# Returning the fit

		return(fit)
}