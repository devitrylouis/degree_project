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
	
# Importation of training and test set

	train <- fread("/Users/louisdevitry/Documents/Education/KTH/Bachelor thesis/Data/train.csv" ,stringsAsFactors=FALSE)
	test <- fread("/Users/louisdevitry/Documents/Education/KTH/Bachelor thesis/Data/test.csv" ,stringsAsFactors=FALSE)
  
# Choose neighborhoods of interest	

		neigh<-FALSE
  	if(neigh){train<-removing_neighborhoods(train)}
	
# Linear Model

	# Multiple Linear Model
		
  	model<-fitting(train)
  	output<-analysis(df,model,residuals=TRUE,boxcox=TRUE,multicollinearity=TRUE,outliers=TRUE,high_terms=FALSE)
    
  # Transformed Multiple Linear Model
  
  	model_transformed <- transformed(model,output,df,1)
    output_transformed<-analysis(df1,model_transformed,residuals=TRUE,boxcox=FALSE,multicollinearity=TRUE,outliers=FALSE,high_terms=TRUE)
    
  # Multiple Linear Model with higher terms of interest

    model_squared <- quadratic_transform(output_transformed,df1)
    output_squared <- analysis(data_squared,model_squared,residuals=TRUE,boxcox=FALSE,multicollinearity=TRUE,outliers=TRUE,high_terms=FALSE)

  # Transformed Multiple Linear Model with higher terms
    
    model_squared_transformed <- transformed(model_squared,output_squared,data_squared,2)
    output<-analysis(df2,model_squared_transformed,residuals=TRUE,boxcox=FALSE,multicollinearity=TRUE,outliers=FALSE,high_terms=FALSE)
    
  # Plot of the models assessment
    
    plot_comparison()
    
  # Lasso
    
    lasso<-lasso_model(df2)
