x=model.matrix(SalePrice~.,train)[,-1]
y=train$SalePrice

# Ridge Regression

library(glmnet)
grid=10^seq(10,-4,length=100)

ridge.mod=glmnet(x,y,alpha=0,lambda=grid)


set.seed (1)
train_set=sample(1:nrow(x), nrow(x)/2)
test_set=(-train_set)
y.test=y[test_set]

ridge.mod=glmnet(x[train_set,],y[train_set],alpha=0,lambda=grid, thresh =1e-12)


set.seed (1)
cv.out=cv.glmnet(x[train_set,],y[train_set],alpha=0)
plot(cv.out,main="Ridge Regression cross-validation")
bestlam=cv.out$lambda.min
ridge.pred=predict(ridge.mod,s=bestlam ,newx=x[test_set,])
mean((ridge.pred-y.test)^2)

