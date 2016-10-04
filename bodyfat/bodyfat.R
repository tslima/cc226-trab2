rm(list=ls())
library(caret)
#Lendo o arquivo de dados
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
body_fat_data = read.table("bodyfat",skip=117,nrows = 252)
#pairs(body_fat_data[,c(2,1,3:5)],labels = c("% Bodyfat","Density","Age","Weight","Height"))
#pairs(body_fat_data[,c(2,6:10)],labels = c("% Bodyfat","Neck","Chest","Abdomen","Hip","Thigh"))
#testando <-pairs(body_fat_data[,c(2,11:15)],labels = c("% Bodyfat","Knee","Ankle","Biceps","Forearm","Wrist"))

labels <- c("Density","% Bodyfat","Age","Weight","Height","Neck","Chest","Abdomen","Hip","Thigh","Knee","Ankle","Biceps","Forearm","Wrist")
par(mfcol=c(3,3))
plot(body_fat_data[,1],body_fat_data[,2],main="Density",ylab="Body Fat",xlab = "Density")
for (i in 3:15) {
  plot(body_fat_data[,i],body_fat_data[,2],main=labels[i],ylab="Body Fat",xlab = labels[i])
}
par(mfcol=c(1,1))


x <- data.matrix(body_fat_data[,c(1)])
y <-data.matrix(body_fat_data[,c(2)])
x <- cbind(matrix(1,nrow(x),1),x)
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
  rms_error = rms_error + sqrt((t(res)%*%res)/nrow(res))
  betas[,i] <- beta
}
# Calculo dos valores finais
rms_error<-rms_error/10
mean_beta <- (betas %*% matrix(1,ncol(betas),1))/ncol(betas)
#Plotando o grafico
y_final <- x%*%mean_beta
index <-1:252
plot(index,y,col="blue")
points(index,y_final,col="red",pch="+")
print(rms_error)
plot(body_fat_data[,1],body_fat_data[,2],xlab = "Density",ylab = "Body Fat(%)", main="Density x Body Fat")
lines(body_fat_data[,1],y_final,col='red')