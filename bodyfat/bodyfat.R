library(caret)
#Lendo o arquivo de dados
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
body_fat_data = read.table("bodyfat",skip=117,nrows = 252)
x <- data.matrix(body_fat_data[,c(1,3,4,5,6,7,8,9,10,11,12,13,14,15)])
y<-data.matrix(body_fat_data[,c(2)])
#Criando os folds
folds <- createFolds(y,k=10)
betas <-matrix(0,ncol(x),10)
rms_error = 0
# 10 fold cross validation
for(i in 1:10){
  xtrain <- x[-folds[[i]],]
  ytrain <- y[-folds[[i]],]
  xtraint <-t(xtrain)
  beta <- solve(xtraint%*%xtrain)%*%xtraint%*%ytrain
  res <- y[folds[[i]],] - x[folds[[i]],]%*%beta
  rms_error = rms_error + t(res)%*%res
  betas[,i] <- beta
}
# Calculo dos valores finais
rms_error <-sqrt(rms_error)
mean_beta <- (betas %*% matrix(1,ncol(betas),1))/ncol(betas)