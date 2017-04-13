ridge_regression_model <-function(train)
{
  # Output
    output <- list()
  # Predictors  
    x=model.matrix(SalePrice~.,train)[,-1]
  # Response
    y=train$SalePrice
  # Grid for lambda
    grid=10^seq(10,-4,length=100)
  # Basic Ridge-Regression
    ridge.mod=glmnet(x,y,alpha=0,lambda=grid)
  
  # Validation approach
  
    set.seed (1)
    train_set=sample(1:nrow(x), nrow(x)/2)
    test_set=(-train_set)
    y.test=y[test_set]
    
    ridge.mod=glmnet(x[train_set,],y[train_set],alpha=0,lambda=grid, thresh =1e-12)
    output[[1]]<-ridge.mod
  
  # Cross-Validation for lambda
  
    set.seed (1)
    cv.out=cv.glmnet(x[train_set,],y[train_set],alpha=0)
    plot(cv.out,main="Ridge Regression cross-validation")
    bestlam=cv.out$lambda.min
    output[[2]]<-bestlam
    ridge.pred=predict(ridge.mod,s=bestlam ,newx=x[test_set,])
    output[[3]]<-mean((ridge.pred-y.test)^2)
    
  # Output return
    return(output)

}