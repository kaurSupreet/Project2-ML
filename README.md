# Project2-ML
install.packages("Rmosek", type="source", INSTALL_opts="--no-multiarch",repos="http://download.mosek.com/R/8",configure.vars="PKG_MOSEKHOME=C:/Program Files/Mosek/8/tools/platform/win64x86 PKG_MOSEKLIB=mosek64")
library(caret)
require(Rmosek)


ModelComparison <- function()
{
	blog.set <- read.csv('D:/UB/Spring/ML/Project1/BlogFeedback/blogData_train.csv',header=FALSE,    sep=",")

	set.seed(456)
	#To return the same values every time the code is run

	train <- blog.set[sample(nrow(blog.set), 5000), ]
	trainDataset <- sample(5000, 4000)
	
	Training <- train[trainDataset, ]
	trainTest <- train[-trainDataset, ]
	
		
	optimizer <- list()
	optimizer$sense <- "min"
		
	require(Rmosek)

#Matrix Creation
	xMatrix <- data.matrix(Training[,51:60])
	yMatrix <- data.matrix(Training[,"V281"])
		
	cMatrix <- crossprod(xMatrix,yMatrix)
	optimizer$c <- as.vector(cMatrix)
	
	Final_Matrix <- crossprod(xMatrix, xMatrix)
	Final_Matrix[upper.tri(Final_Matrix)]<-0
	index_values <- which(Final_Matrix != 0, arr.ind=TRUE)
	optimizer$qobj <- list(i = index_values[,1], j = index_values[,2], v = Final_Matrix[index_values])
	
	optimizer$A <- Matrix( rep(0, 10), nrow = 1, byrow=TRUE, sparse=TRUE )

	
	optimizer$bc <- rbind(blc=-Inf, buc=Inf)
	
	
	
	optimizer$bx<-rbind(blx=rep(-Inf,10), bux=rep(Inf,10))
	r<-mosek(optimizer)
	
	print("Train Data - Mosek Error")
	Mosek_MSE(Training[, 51:60], Training[, "V281"])
	print("Test Data - Mosek Error")
	Mosek_MSE(trainTest[, 51:60], trainTest[, "V281"])
	
	
	Linear_MSE(Training, trainTest)

}


#===============================Convex Optimization - Mosek=======================================================================================================
Mosek_MSE <- function(val, targetData){

	yValue = val[,1]*-0.20473016 + val[,2]*-0.28727579 + val[,3]*0.02768148 + val[,4]*0.21594995 + val[,5]*0.00000000 + val[,6]*2.30850597 + val[,7]*-3.28659353 + val[,8]*-1.41793199 + val[,9]*-1.78338969
		
	errors <- (targetData - yValue)*(targetData - yValue)
		
	Mean_Square_Error <- mean(errors)

	print(Mean_Square_Error)
}

#===============================Linear Regression==================================================================================================================
Linear_MSE <- function(val, testData) {

	values <- val[, c(51:60, 281)]
	print(colnames(values))
	
	Linear_Model <- lm(V281~., data=values)
	print("Train Data - LM Error")
	print(mean(Linear_Model$residuals^2))
	
	
	predicted <- predict(Linear_Model, testData[, c(51:60, 281)], se.fit=TRUE)
		
	MEAN_SQUARE_ERROR = mean((predicted$fit - testData[,281])^2)
	print("Test Data - LM Error")
	print(MEAN_SQUARE_ERROR)
}
