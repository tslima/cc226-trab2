rm(list=ls())
#Importando os dados
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
ukraine = read.csv("ukraine_ged40_simpl.csv",skip=1)
latitude = data.matrix(ukraine[,1])
longitude = data.matrix(ukraine[,2])
best_est = data.matrix(ukraine[,3])
plot(longitude,latitude)