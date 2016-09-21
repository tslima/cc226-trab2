rm(list=ls())
#Lendo o arquivo de dados
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
treinamento = read.table("pendigits.tra",sep=",")
teste=read.table("pendigits.tra",sep=",")
treinamento <- data.matrix(treinamento)
teste <- data.matrix(teste)