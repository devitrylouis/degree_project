# Necessaries libraries to explore the data

	load.libraries <- c('data.table', 'testthat', 'gridExtra', 'corrplot', 'GGally', 'ggplot2', 'e1071', 'dplyr','leaps','car','MASS','gtools')
	install.lib <- load.libraries[!load.libraries %in% installed.packages()]
	for(libs in install.lib) install.packages(libs, dependences = TRUE)
	sapply(load.libraries, require, character = TRUE)

# Function sources

	source('~/Desktop/factoring.R')
	source('~/Desktop/analysis.R')
	source('~/Documents/Education/KTH/Bachelor thesis/R scripts/Regression/Multiple Regression/stepwise_selection.R')
	source('~/Documents/Education/KTH/Bachelor thesis/R scripts/Regression/Multiple Regression/predict.regsubsets.R')
	source('~/Desktop/expected_gain_tab.R')
	source('~/Desktop/expected_gain.R')
	source('~/Desktop/fitting.R')
	source('~/Documents/Education/KTH/Bachelor thesis/R scripts/Regression/Multiple Regression/Suggestive Analysis.R')
	source('~/Desktop/Suggestive Analysis.R')
	source('~/Desktop/predict_feet.R')
	
# Importation of training and test set

	train <- fread("/Users/louisdevitry/Documents/Education/KTH/Bachelor thesis/Data/train.csv" ,stringsAsFactors=FALSE)
	test <- fread("/Users/louisdevitry/Documents/Education/KTH/Bachelor thesis/Data/test.csv" ,stringsAsFactors=FALSE)

	# Linear Model

	model<-fitting(train)
	summary(model)
	analysis(df,model, residuals=TRUE, boxcox=TRUE, multicollinearity=TRUE,outliers=TRUE)
	tabs<-compute_tabs(9,model,train)

# Others models
	
	#stepwise(df,forward=TRUE)
	#backward(train)
	#ridge(train)
	#lasso(train)


# Sugestive analysis
	tabs<-compute_tabs(9,model,train)

