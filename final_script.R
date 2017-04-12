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
	source('~/Documents/Education/KTH/Bachelor thesis/Bachelor Thesis/transformed_fitting.R')
	source('~/Documents/Education/KTH/Bachelor thesis/Bachelor Thesis/quadratic.R')
	source('~/Desktop/quadratic_transform.R')
# Importation of training and test set

	train <- fread("/Users/louisdevitry/Documents/Education/KTH/Bachelor thesis/Data/train.csv" ,stringsAsFactors=FALSE)
	test <- fread("/Users/louisdevitry/Documents/Education/KTH/Bachelor thesis/Data/test.csv" ,stringsAsFactors=FALSE)

	# Linear Model

	model<-fitting(train)
	output<-analysis(df,model,residuals=TRUE,boxcox=TRUE,multicollinearity=TRUE,outliers=TRUE,high_terms=FALSE)

  
	model_transformed <- transformed(model,output,df,1)
  output_transformed<-analysis(df1,model_transformed,residuals=TRUE,boxcox=FALSE,multicollinearity=TRUE,outliers=FALSE,high_terms=TRUE)
  
  model_squared <- quadratic_transform(output_transformed,df1)
  output_squared <- analysis(data_squared,model_squared,residuals=TRUE,boxcox=FALSE,multicollinearity=TRUE,outliers=TRUE,high_terms=FALSE)
  
  model_squared_transformed <- transformed(model_squared,output_squared,data_squared,2)
  output_squared_transformed <- analysis(df2,model_squared_transformed,residuals=TRUE,boxcox=FALSE,multicollinearity=TRUE,outliers=FALSE,high_terms=FALSE)

  
  