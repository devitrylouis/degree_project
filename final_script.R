# Necessaries libraries to explore the data

	load.libraries <- c('data.table', 'testthat', 'gridExtra', 'corrplot', 'GGally', 'ggplot2', 'e1071', 'dplyr','leaps','car','MASS','gtools','DAAG','glmnet')
	install.lib <- load.libraries[!load.libraries %in% installed.packages()]
	for(libs in install.lib) install.packages(libs, dependences = TRUE)
	sapply(load.libraries, require, character = TRUE)
	# include DAAG
# Function sources

	source('~/Desktop/factoring.R')
	source('~/Desktop/analysis.R')
	source('~/Documents/Education/KTH/Bachelor thesis/Bachelor Thesis/analysis.R', echo=TRUE)
	source('~/Documents/Education/KTH/Bachelor thesis/Bachelor Thesis/fitting.R')
	source('~/Documents/Education/KTH/Bachelor thesis/Bachelor Thesis/transformed_fitting.R')
	source('~/Documents/Education/KTH/Bachelor thesis/Bachelor Thesis/quadratic.R')
	source('~/Desktop/quadratic_transform.R')
	source('~/Documents/Education/KTH/Bachelor thesis/Bachelor Thesis/remove_neighborhood.R')
	source('~/Documents/Education/KTH/Bachelor thesis/Bachelor Thesis/plot_comparison.R')
	source('~/Documents/Education/KTH/Bachelor thesis/Bachelor Thesis/ridge_regression.R')
	source('~/Documents/Education/KTH/Bachelor thesis/Bachelor Thesis/lasso.R')
	source('~/Documents/Education/KTH/Bachelor thesis/Bachelor Thesis/quadratic_predictors.R')
	source('~/Documents/Education/KTH/Bachelor thesis/Bachelor Thesis/cubic.R')
# Importation of training and test set

	train <- fread("/Users/louisdevitry/Documents/Education/KTH/Bachelor thesis/Data/train.csv" ,stringsAsFactors=FALSE)
	test <- fread("/Users/louisdevitry/Documents/Education/KTH/Bachelor thesis/Data/test.csv" ,stringsAsFactors=FALSE)
  
# Choose neighborhoods of interest	

		neigh<-FALSE
  	if(neigh){train<-removing_neighborhoods(train)}
	
# Linear Model

	# Multiple Linear Model
		
  	model<-fitting(train)
  	output<-analysis(df,model,residuals=TRUE,boxcox=TRUE,multicollinearity=TRUE,outliers=TRUE)
    
  # Transformed Multiple Linear Model
  
  	model_transformed <- transformed(model,output,df,1)
    output_transformed<-analysis(df1,model_transformed,residuals=TRUE,boxcox=FALSE,multicollinearity=TRUE,outliers=TRUE)
    
  # Multiple Linear Model with higher terms of interest
    df2<-quadratic_predictors(df1)
    model_squared <- transformed(model_transformed,output_transformed,df2,2)
    output_squared <- analysis(df2,model_squared,residuals=TRUE,boxcox=FALSE,multicollinearity=TRUE,outliers=TRUE)

  # Transformed Multiple Linear Model with higher terms
    
    model_squared_transformed <- transformed(model_squared,output_squared,df2,3)
    output<-analysis(df3,model_squared_transformed,residuals=TRUE,boxcox=FALSE,multicollinearity=TRUE,outliers=FALSE)
    df3<-cubic_predictors(df3)
    
  # Lasso
    
    lasso <- lasso_model(df3)
    
  # Ridge Regression
    
    ridge <- ridge_regression_model(df3)
    
  # Plot of the models assessment
    
    plot_comparison()