library(plot3D)
library(caret)
library(mvtnorm)
rm(list=ls())
#Importando os dados
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
ukraine = read.csv("ukraine_ged40_simpl.csv",skip=1)
original_data = data.matrix(ukraine)
centers = data.matrix(ukraine)
colnames(centers) <- c("lat", "lon", "fatal")
outliner = c()
for (i in 1 : nrow(centers)){
  if (centers[i,2] < 36 ){
    outliner <- rbind(outliner,i)
  }
}
centers <- centers[-outliner,]
original_data <- original_data[-outliner,]
centers <- centers[order(centers[,3],decreasing = TRUE),]
DESV<-0.25

for (i in 1:NROW(centers)) {
  j<-i+1
  while(j <= NROW(centers)){
    if( sqrt((centers[i,1]-centers[j,1])^2+(centers[i,2]-centers[j,2])^2)<DESV){
      centers[i,3]<- centers[i,3] + centers[j,3]
      centers <- centers[-j,]
    } else {
      j <-j+1
    }
  }
}

for (i in 1:NROW(original_data)) {
  j<-i+1
  while(j <= NROW(original_data)){
    if(sqrt((original_data[i,1]-original_data[j,1])^2+(original_data[i,2]-original_data[j,2])^2)<0.001){
      original_data[i,3]<- original_data[i,3] + original_data[j,3]
      original_data <- original_data[-j,]
    } else {
      j <-j+1
    }
  }
}


bi_gaussian <- function(x,y,xc,yc) {
  varx <- (2*DESV^2)
  vary <- varx
  result <- exp(-(((x-xc)^2/(2*varx^2))+((y-yc)^2/(2*vary^2))))
  return(result)
}

myheatmap <- function(lon,lat,z,title,centers){
  mi <- min(z)
  ma <- max(z)
  nlevels <- 30
  lvls <- seq(mi, ma, length.out = nlevels)
  filled.contour(lon,lat,z,plot.title =  {title(xlab="Longitude", ylab="Latitude", main=title,plot.axes={axis(1,round(seq(min(lon),max(lon),length.out = 5),digits = 2)) 
    axis(2,round(seq(min(lat),max(lat),length.out = 5),digits = 2))})},levels = lvls, col=cols,plot.axes = {points(centers[,2],centers[,1])})
  
}

z_phi_func <- function(x,y){
  
  #result <- matrix(1,NROW(x),1)
  result<-bi_gaussian(x,y,centers[1,2],centers[1,1])
  for(i in 2:NROW(centers)){
    result<-cbind(result,bi_gaussian(x,y,centers[i,2],centers[i,1]))
  }
  return(result)
}


teste <- z_phi_func(original_data[,2],original_data[,1])
beta <- solve(t(teste)%*%teste)%*%t(teste)%*%original_data[,3]
error_mat <- original_data[,3] - z_phi_func(original_data[,2],original_data[,1])%*%beta
#print(error)
#error<- t(error)%*%error
error <- sum(error_mat^2)
error<-sqrt(error/(nrow(original_data)))

k<-200
lat<- seq(min(original_data[,1]),max(original_data[,1]),length.out = k)
lon<- seq(min(original_data[,2]),max(original_data[,2]),length.out = k)
#teste<-z_phi_func(lon,lat[1])

z <- z_phi_func(lon,lat[1])%*%beta
for (i in 2:k){
  z <-cbind(z,z_phi_func(lon,lat[i])%*%beta)
}

mi <- min(z)
ma <- max(z)
nlevels <- 30
lvls <- seq(mi, ma, length.out = nlevels)
cols <- colorRampPalette(c("darkgreen","white","red")) (nlevels - 1)
myheatmap(lon,lat,z,"Heat Map",original_data)

#scatter3D(original_data[,2],original_data[,1],original_data[,3], surf = list(x =lon,y = lat,z = z))
#filled.contour(lon,lat,z,levels = lvls, col=cols,plot.title = {points(original_data[,2],original_data[,1])})
#persp(lon,lat,z,plot ={points3D(centers[,2],centers[,1],centers[,3])})
scatter2D(original_data[,2],abs(error_mat),colvar = original_data[,1],main="Erro Absoluto", ylab="Erro (Fatalidades)",xlab="Longitude",clab = "Latitude")

z <- z_phi_func(lon,lat[1])%*%matrix(1,nrow(centers),1)
for (i in 2:k){
  z <-cbind(z,z_phi_func(lon,lat[i])%*%matrix(1,nrow(centers),1))
}
#scatter3D(original_data[,2],original_data[,1],original_data[,3])
#persp(lon,lat,z)

myheatmap(lon,lat,z,"Kernels",centers)


