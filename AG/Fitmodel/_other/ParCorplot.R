rm(list=ls())

# load libraries
library(ppcor)
library(corrplot)
library(data.table)

# load fit results AG
load("/Users/sarajahfari/Github/Control_Conflict/AG/Fitmodel/Fit_output/ConncetivityStrength_withBIC7models_MnoF.rdat")


# set datadir Base
dir_base="/Users/sarajahfari/Github/Control_Conflict/AG/Fitmodel"

#dir AG_codes, example_codes,
dir_agcodes=paste(dir_base,'/AG_codes',sep='')
dir_excodes=paste(dir_base,'/Fit_codes',sep='')
dir_data=paste(dir_base,'/Fit_data',sep='')
dir_output=paste(dir_base,'/Fit_output',sep='')

# Read yDat list with single subject beta's for each trial/roi, and make index list for the conditions of interest
source(paste(dir_excodes,'/index_yDatRLtest.R',sep=''), chdir = F)
out=c('MotorCBA4aL','type')
#lapply(yDat, cor)
all=rbindlist(yDat)


lapply(yDat,function(x) pcor(x[,-which(colnames(x)%in%out)])$estimate) -> pcorestimate

lapply(pcorestimate, function(x) mean(x['GPe30exc','maxGPi30exc']))-> gpegpi
lapply(pcorestimate, function(x) mean(x['DLPFCposterior','preSMAsmall']))-> dl_pre
lapply(pcorestimate, function(x) mean(x['PvmPFCNoventri','preSMAsmall']))-> vm_pre 


# compare partial correlation strength gpe/gpi to more distant regions
mean(unlist(gpegpi));sd(unlist(gpegpi))
mean(unlist(dl_pre));sd(unlist(dl_pre))
mean(unlist(vm_pre));sd(unlist(vm_pre))

t.test(unlist(gpegpi),unlist(dl_pre),paired=T)
t.test(unlist(gpegpi),unlist(vm_pre),paired=T)







# pcor(all[,-c(4,11)])$estimate ->pcorgroup
# cor(all[,-c(4,11)]) ->corgroup
# 
# apply(simplify2array(pcorestimate), 1:2, mean) -> mpcor
# apply(simplify2array(pcorestimate), 1:2, sd) -> sdpcor
# 
# colnames(pcorgroup)=c('DLPFC','GPe','M1','Thalamus','GPi','STN','preSMA','Striatum','vmPFC')
# rownames(pcorgroup)=c('DLPFC','GPe','M1','Thalamus','GPi','STN','preSMA','Striatum','vmPFC')
# 
# install.packages('RColorBrewer')
# 
# dev.new()
# library(RColorBrewer)
# corrplot(pcorgroup, method = "color", outline = T, addgrid.col = "darkgray", 
#          order="hclust", addrect = 4, rect.col = "darkgray", rect.lwd = 1,
#          cl.pos = "b", tl.col = "indianred4", tl.cex = 1, cl.cex = 1, 
#          addCoef.col = "black", number.digits = 1, number.cex = 0.6, 
#          col = colorRampPalette(c("darkred","white","midnightblue"))(100))

# set.seed(223)
# x1 <- rnorm(100)
# x2 <- x1 + rnorm(100,sd=0.5)
# x3 <- x1 + x2 + rnorm(100,sd=0.5)
# cor2pcor(cor(cbind(x1,x2,x3)))