lasso_model <- function()
 {
  
# Lasso
x=model.matrix(SalePrice~.,train)[,-1]
y=train$SalePrice
grid=10^seq(10,-4,length=100)

lasso.mod=glmnet(x[train_set ,],y[train_set],alpha=1,lambda=grid) 
plot(lasso.mod)

set.seed(1)

cv.out=cv.glmnet(x[train_set ,],y[train_set],alpha=1)

plot(cv.out,main="Lasso cross-validation")

bestlam=cv.out$lambda.min

lasso.pred=predict(lasso.mod,s=bestlam ,newx=x[test_set,])

mean((lasso.pred-y.test)^2)
  }
