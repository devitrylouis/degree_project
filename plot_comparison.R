plot_comparison <- function()
{
  a<-list()
  ms<-numeric(4)
  a[[1]]<-cv.lm(df,model,m=4,plotit=FALSE)
  a[[2]]<-cv.lm(df1,model_transformed,m=4,plotit=FALSE)
  a[[3]]<-cv.lm(data_squared,model_squared,m=4,plotit=FALSE)
  a[[4]]<-cv.lm(df2,model_squared_transformed,m=4,plotit=FALSE)
  
  ms[1]<-attr(a[[2]],"ms")
  ms[2]<-attr(a[[3]],"ms")
  ms[3]<-attr(a[[4]],"ms")
  
  
  b<-list()
  adj_r<-numeric(4)
  b[[1]]<-summary(model)
  b[[2]]<-summary(model_transformed)
  b[[3]]<-summary(model_squared)
  b[[4]]<-summary(model_squared_transformed)
  adj_r[1]<-b[[1]]$adj.r.squared
  adj_r[2]<-b[[2]]$adj.r.squared
  adj_r[3]<-b[[3]]$adj.r.squared
  adj_r[4]<-b[[4]]$adj.r.squared
  
  mult_r<-numeric(4)

  mult_r[1]<-b[[1]]$r.squared
  mult_r[2]<-b[[2]]$r.squared
  mult_r[3]<-b[[3]]$r.squared
  mult_r[4]<-b[[4]]$r.squared
  
  ms[4]<-lasso[[1]]
  ms[5]<-ridge[[3]]
  
  plot(ms,xlab = "Index",ylab="Mean Square Error",main="Comparing MSE among models")
  plot(adj_r,xlab = "Index",ylab="Adjusted R-squared",main="Comparing Adjusted R-squared among models")
  plot(mult_r,xlab = "Index",ylab="Multiple R-squared",main="Comparing Multiple R-squared among models")
  
}