analysis <- function(df,fit,residuals=TRUE,boxcox=TRUE,multicollinearity=TRUE,outliers=TRUE)
{	
	# Ressidual Analysis

  output <- list()
  
		if(residuals)
		{
			influence<-influence(fit)
			
			# Computing the (ordered) studentized residuals
			
				ex.stud.res <- studres(fit)*(dim(df)[1]-dim(df)[2]-1)*sqrt(sum(fit$residuals^(2)))/(sqrt(dim(df)[1]-2)*sqrt((dim(df)[1]-dim(df)[2])*(sum(fit$residuals^(2))/sqrt(dim(df)[1]-2))-fit$residuals^(2)/(1-(influence$hat))))
				ordered.ex.stud.res <- mixedsort(ex.stud.res, decreasing = FALSE)

			# Plot of the ordered Externally Studentized Oresiduals and the Fitted Values
				
				plot(fit$fitted.values, ordered.ex.stud.res, ylab = "Ordered Externally studentized residuals", xlab="Fitted values",main="Residual plot of fitted values")
				abline(h=0,col="red")

			# Normal probability plot

				# Construction of the cumulated probability vector

					cumul.proba <- vector(mode="double", length=dim(df)[1])
					for ( i in 1:dim(df)[1]) {cumul.proba[i]<-(i-1/2)/dim(df)[1]}

				# Normal probability plot

					plot(ordered.ex.stud.res,cumul.proba,xlab="Externally Studentized Residuals", ylab="Probability",main="Normal probability plot") 
		}

	# Box-Cox transformation

		if(boxcox)	
		{
			bc<-boxcox(fit,data=df)
			lambda <- bc$x[which(bc$y==max(bc$y))]
			output[[1]]<-lambda
		}
  if(!boxcox)
  {
    output[[1]]<-"No Box-Cox transformation"
  }

	# Multicollinearity Analysis

		if(multicollinearity)
		{
			infl = lm.influence(fit,do.coef = FALSE)

			vec<-seq(1, dim(df)[1])
			
			# Leverage points
			
				lev.pnts<- which(infl$hat>2*(dim(df)[2])/dim(df)[1])
				output[[2]] <-as.numeric(names(lev.pnts))

				# Plotting leverage points

					plot(vec,infl$hat,main="Leverage points",xlab="Index",ylab="Hat values")
					abline(h=2*(dim(df)[2])/dim(df)[1],col="red")

			# Cook's distance

				# Cook's distance computations
				
					cooksd <- cooks.distance(fit, infl,res = weighted.residuals(fit),sd = sqrt(deviance(fit)/df.residual(fit)),hat = infl$hat)
					cooksd <- na.omit(cooksd)
				
				# Plot of the Cook's distance with Influential threshold
					
					v <- seq(1, length(cooksd))

					plot(v,cooksd,main="Cook's distance",xlab="Index",ylab="Cook's distance")
					abline(h=4/(dim(df)[1]-dim(df)[2]-1),col="red")
					

				# Influential points with regard to Cook's distance

					sum.influencial.points <- sum(cooksd > 4/(dim(df)[1]-dim(df)[2]-1)) 
					cookd.inf <- which(cooksd >4/(dim(df)[1]-dim(df)[2]-1))
					output[[3]]<-as.numeric(names(cookd.inf))
				
			# CovRatio

				# Computing CovRatio values
			
					covrat <- covratio(fit, infl,res = weighted.residuals(fit))
					covrat <- na.omit(covrat)

				# Plot of the CovRatio values with the influential threshold

					plot(vec,covrat,main="CovRatio",xlab="Index",ylab="CovRatio values")
					abline(h=1+(3*(dim(df)[2]-1)/dim(df)[1]),col="red")
					abline(h=1-(3*(dim(df)[2]-1)/dim(df)[1]),col="red")
				
				# Computing which are the influential points

					sum.influencial.points.cov <- sum(covrat>1+(3*(dim(df)[2]-1)/dim(df)[1]))+sum(covrat<1-(3*(dim(df)[2]-1)/dim(df)[1]))
					output[[4]]<-as.numeric(c(names(which(covrat>1+(3*(dim(df)[2]-1)/dim(df)[1]))),names(which(covrat<1-(3*(dim(df)[2]-1)/dim(df)[1])))))
		}
    if(!multicollinearity)
    {
      ouput[[2]]<-"No informations regarding Leverage points"
      output[[3]]<-"No informations regarding Influential points (Cook's distance)"
      output[[4]]<-"No informations regarding Influential points (CovRatio)"
    }

	
	# Regressor transformations

	# Outlier Analysis

		if(outliers)
		{
			out<-outlierTest(fit, cutoff=0.05, n.max=100)
			index<-as.numeric(names(abs(out$rstudent>5)))
			vector<-logical(length = dim(df)[1])
			vector[index]<-TRUE
			output[[5]]<-vector
		}
  if(!outliers)
  {
    output[[5]]<-"No informations regarding outliers"
  }
  
  
	return(output)
}


	
