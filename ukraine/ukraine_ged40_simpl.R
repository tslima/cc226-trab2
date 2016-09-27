library(plot3D)
rm(list=ls())
#Importando os dados
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
ukraine = read.csv("ukraine_ged40_simpl.csv",skip=1)
dados = data.matrix(ukraine)
colnames(dados) <- c("lat", "lon", "fatal")
dados <- dados[order(dados[,3]),]
outliner = c()
for (i in 1 : nrow(dados)){
  if (dados[i,2] < 36 || dados[i,3] <=0){
    outliner <- rbind(outliner,i)
  }
}

dados <- dados[-outliner,]
scatter2D(dados[,2],dados[,1],colvar = dados[,3])

bi_gaussian <- function(x,y,xc,yc) {
  varx = 1
  vary = 1
  result <- exp(-((x-xc)^2/(2*varx^2)+(y-yc)^2/(2*vary^2)))/(2*pi*varx*vary)
  return(result)
}

phi_1 = bi_gaussian(dados[,2],dados[,1],dados[nrow(dados),2],dados[nrow(dados),1])