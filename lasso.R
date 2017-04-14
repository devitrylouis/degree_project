lasso_model <- function(df)
{
  output<-list()
  # Predictors
    x=model.matrix(log(SalePrice)~.,df)[,-1]
  # Response
    y=log(df$SalePrice)
  # Gris of lambda
    grid=10^seq(10,-4,length=100)
  # Train and test set
    set.seed (1)
    df_set=sample(1:nrow(x), nrow(x)/2)
    test_set=(-df_set)
    y.test=y[test_set]
  # Lasso
    lasso.mod=glmnet(x[df_set ,],y[df_set],alpha=1,lambda=grid) 

    set.seed(1)

  # Cross-Valisation for lambda
    cv.out=cv.glmnet(x[df_set ,],y[df_set],alpha=1)
    plot.cv.glmnet(cv.out,main="Lasso cross-validation",sub = NULL)
    bestlam <- cv.out$lambda.min



  # Lasso Results
    
    lasso.pred = predict(lasso.mod,s=bestlam ,newx=x[test_set,])
    ms <- mean((lasso.pred-y.test)^2)
    lasso.mod=glmnet(x[df_set ,],y[df_set],alpha=1,lambda=bestlam)
    output[[1]]<-ms
    output[[2]]<-bestlam
    output[[3]]<-lasso.mod
    
    return(output)
  }
