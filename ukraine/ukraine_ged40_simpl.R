library(plot3D)
library(caret)
library(mvtnorm)
rm(list=ls())
#Importando os dados
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
ukraine = read.csv("ukraine_ged40_simpl.csv",skip=1)
dados = data.matrix(ukraine)
colnames(dados) <- c("lat", "lon", "fatal")
outliner = c()
for (i in 1 : nrow(dados)){
  #if (dados[i,2] < 36 || dados[i,3] <=0){
  if (dados[i,2] < 36 ){
    outliner <- rbind(outliner,i)
  }
}
dados <- dados[-outliner,]
for (i in 1:nrow(dados)) {
  j<-i+1
  while(j < nrow(dados)){
    if(dados[i,1]==dados[j,1] && dados[i,2]==dados[j,2]){
      dados[i,3]<- dados[i,3] + dados[j,3]
      dados <- dados[-j,]
    } else {
      j <-j+1}
    
  }
}
bi_gaussian <- function(x,y,xc,yc) {
  varx = 0.5^2
  vary = 0.5^2
  result <- exp(-(((x-xc)^2/(2*varx^2))+((y-yc)^2/(2*vary^2))))#/(2*pi*varx*vary)
  #sigma_matrix <- matrix(c(1,0.5,0.5,1), ncol=2)
  #result <- dmvnorm(x=matrix(c(x,y),ncol=2), mean=c(xc,yc), sigma = sigma_matrix)
  return(result)
}
dados <- dados[order(dados[,3]),]

z <- dados[,3]
x <- dados[,2]
y <- dados[,1]

rms_error = 0
folds <- createFolds(y,k=10)

z_phi_func <- function(x,y){
  result<-bi_gaussian(x,y,dados[nrow(dados),2],dados[nrow(dados),1])
  for(i in 1:4){
    result<-cbind(result,bi_gaussian(x,y,dados[nrow(dados)-i,2],dados[nrow(dados)-i,1]))
  }
  return(result)
}
betas <-matrix(0,5,10)
for (i in 1:10) {
  xtrain <- x[-folds[[i]]]
  ytrain <- y[-folds[[i]]]
  ztrain <- z[-folds[[i]]]
  
  #z_phi <- cbind(phi_1(xtrain,ytrain),phi_2(xtrain,ytrain),phi_3(xtrain,ytrain))
  z_phi<-z_phi_func(xtrain,ytrain)
  beta<-solve(t(z_phi)%*%z_phi)%*%t(z_phi)%*%ztrain
  res<- z[folds[[i]]] - z_phi_func(x[folds[[i]]],y[folds[[i]]])%*%beta
  rms_error = rms_error + sqrt((t(res)%*%res)/nrow(res))  
  betas[,i] <- beta
}
mean_beta <- (betas %*% matrix(1,ncol(betas),1))/ncol(betas)
rms_error<-rms_error/10
k<-200
lat<- seq(min(dados[,1]),max(dados[,1]),length.out = k)
lon<- seq(min(dados[,2]),max(dados[,2]),length.out = k)
z <- z_phi_func(lon,lat[1])%*%mean_beta
for (i in 2:k){
z <-cbind(z,z_phi_func(lon,lat[i])%*%mean_beta)  
}

filled.contour(lon,lat,z,plot.title = {points(dados[,2],dados[,1])})
#scatter2D(dados[,2],dados[,1],colvar = dados[,3])
