# Necessaries libraries to explore the data

	load.libraries <- c('data.table', 'testthat', 'gridExtra', 'corrplot', 'GGally', 'ggplot2', 'e1071', 'dplyr','leaps','car','MASS','gtools')
	install.lib <- load.libraries[!load.libraries %in% installed.packages()]
	for(libs in install.lib) install.packages(libs, dependences = TRUE)
	sapply(load.libraries, require, character = TRUE)

# Function sources

	source('~/Desktop/factoring.R')
	source('~/Desktop/analysis.R')
	source('~/Documents/Education/KTH/Bachelor thesis/Bachelor Thesis/analysis.R', echo=TRUE)
	source('~/Documents/Education/KTH/Bachelor thesis/Bachelor Thesis/fitting.R')
	
# Importation of training and test set

	train <- fread("/Users/louisdevitry/Documents/Education/KTH/Bachelor thesis/Data/train.csv" ,stringsAsFactors=FALSE)
	test <- fread("/Users/louisdevitry/Documents/Education/KTH/Bachelor thesis/Data/test.csv" ,stringsAsFactors=FALSE)

	# Linear Model

	model<-fitting(train)
	analysis(df,model,residuals=TRUE,boxcox=TRUE,multicollinearity=TRUE,outliers=TRUE)
