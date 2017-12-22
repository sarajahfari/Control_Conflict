rm(list=ls())
# set datadir Base
dir_base="/Users/sarajahfari/Github/Control_Conflict/AG/Fitmodel"

#dir AG_codes, example_codes,
dir_agcodes=paste(dir_base,'/AG_codes',sep='')
dir_excodes=paste(dir_base,'/Fit_codes',sep='')
dir_data=paste(dir_base,'/Fit_data',sep='')
dir_output=paste(dir_base,'/Fit_output',sep='')

# source checked packages
source("http://bioconductor.org/biocLite.R")
biocLite("graph")
biocLite("RBGL")
biocLite("igraph")
biocLite("MASS")


# just to check
library(MASS)
library(RBGL)
library(graph)
install.packages(paste(dir_agcodes,'/Rpackage_checked/ggm-1.995-3.tar.gz',sep=''), repos = NULL, type="source")

sessionInfo()

# source AG_codes
file.sources = list.files(paste(dir_agcodes,'/rFunctions2',sep=''),pattern="*.R",full.names = TRUE,recursive=T)
sapply(file.sources,source,.GlobalEnv)

# Read yDat list with single subject beta's for each trial/roi, and make index list for the conditions of interest
source(paste(dir_excodes,'/index_yDatRLtest.R',sep=''), chdir = F)

#########################################################################

# these are the conditions that are evalauted seperatly in Fitmodels_draft.R	
# cond1 = list(ewwL,ewwR,ellL,ellR,eBwlL,eBwlR,eMwlL,eMwlR,eSwlL,eSwlR) # correct and incorrect trials, wl = only new pairs (not used in learning)
# names(cond1)=c('ewwL','ewwR','ellL','ellR','BwlL','BwlR','MwlL','MwlR','SwlL','SwlR')

# these are the conditions that are evalauted seperatly in Fitmodels_draft.R	
cond2 = list(ewwL,ewwR,eww,ellL,ellR,ell) # correct and incorrect trials, wl = only new pairs (not used in learning)
names(cond2)=c('ewwL','ewwR','eww','ellL','ellR','ell')

# select regions that go into graph
# MGO=c("PvmPFCNoventri" ,"preSMAsmall", 
#       "maxSTN25exc","GPe30exc","maxGPi30exc", "DLPFCposterior" ,
#       "Thalamus40exc","PstriatumNoVentri","MotorBA4")

# select the graph nodes for connectivity network
M0roi=c("CaudateR40exc" ,"PreSMARsmall","IFGR",
        "maxSTNR25exc","maxGPeR30exc","maxGPiR30exc",
        "ThalamusR40exc")

C.Lab=M0roi


#C.Lab = MGO
cond=cond2

########################################################################

# Source the Decision making models to use, these are the defined PFC-BG models to evaluate for connectivity/fits 
#source(paste(dir_excodes,'/gomodel_draft.R',sep=''), chdir = F) 

# Now do the connectivity evaluation for each subject, each model/condition, to compute aic/bic and n-fits
# fit evaluate STOP
source(paste(dir_excodes,'/StopModel.R',sep=''), chdir = F) 
source(paste(dir_excodes,'/Fitmodels_draft.R',sep=''), chdir = F)


BMT


# in both AIC and BIC model7 is the best!
save(BMT,file='/Users/sarajahfari/Github/Control_Conflict/AG/Fitmodel/_other/BICFIT_RLtestStopNetwork.Rdat')

#------------------------------------------------------------------------------#
#           Compute (indvidual) connectivity strengths winning model           #
#------------------------------------------------------------------------------#

# direct + indirect + hyperdirect	(winning model ww,ll, wl errors +correct)		
Model=makeMG(dg=DAG(
  PstriatumNoVentri~ preSMAsmall +PvmPFCNoventri+ DLPFCposterior,
  maxSTN25exc~preSMAsmall +PvmPFCNoventri+ DLPFCposterior,
  GPe30exc~PstriatumNoVentri,
  maxGPi30exc~PstriatumNoVentri+GPe30exc+maxSTN25exc,
  Thalamus40exc~maxGPi30exc,
  MotorBA4~ Thalamus40exc			
),ug=UG(~preSMAsmall*PvmPFCNoventri*DLPFCposterior))


EwwL=list() 
EwwR=list()
EllL=list()
EllR=list()
EBwlL=list()
EBwlR=list()
EMwlL=list()
EMwlR=list()
ESwlL=list()
ESwlR=list()

for (i in 1:length(yDat))
{
  EwwL[[i]]=yDat[[i]][cond[[1]][[i]],C.Lab]
  EwwR[[i]]=yDat[[i]][cond[[2]][[i]],C.Lab]
  EllL[[i]]=yDat[[i]][cond[[3]][[i]],C.Lab]
  EllR[[i]]=yDat[[i]][cond[[4]][[i]],C.Lab]
  EBwlL[[i]]=yDat[[i]][cond[[5]][[i]],C.Lab]
  EBwlR[[i]]=yDat[[i]][cond[[6]][[i]],C.Lab]
  EMwlL[[i]]=yDat[[i]][cond[[7]][[i]],C.Lab]
  EMwlR[[i]]=yDat[[i]][cond[[8]][[i]],C.Lab]
  ESwlL[[i]]=yDat[[i]][cond[[9]][[i]],C.Lab]
  ESwlR[[i]]=yDat[[i]][cond[[10]][[i]],C.Lab]
  
}

# compute covariance matrix
covList.EwwL =lapply(EwwL,cov)
covList.EwwR =lapply(EwwR,cov)
covList.EllL =lapply(EllL,cov)
covList.EllR =lapply(EllR,cov)
covList.EBwlL =lapply(EBwlL,cov)
covList.EBwlR =lapply(EBwlR,cov)
covList.EMwlL =lapply(EMwlL,cov)
covList.EMwlR =lapply(EMwlR,cov)
covList.ESwlL =lapply(ESwlL,cov)
covList.ESwlR =lapply(ESwlR,cov)

fitEwwL=list() 
fitEwwR=list()
fitEllL=list()
fitEllR=list()
fitEBwlL=list()
fitEBwlR=list()
fitEMwlL=list()
fitEMwlR=list()
fitESwlL=list()
fitESwlR=list()

for (i in 1:length(yDat))
{
  fitEwwL[[i]]=fitAncestralGraph(Model, covList.EwwL[[i]],dim(EwwL[[i]])[1])
  fitEwwR[[i]]=fitAncestralGraph(Model, covList.EwwR[[i]],dim(EwwR[[i]])[1])
  fitEllL[[i]]=fitAncestralGraph(Model, covList.EllL[[i]],dim(EllL[[i]])[1])
  fitEllR[[i]]=fitAncestralGraph(Model, covList.EllR[[i]],dim(EllR[[i]])[1])
  fitEBwlL[[i]]=fitAncestralGraph(Model, covList.EBwlL[[i]],dim(EBwlL[[i]])[1])
  fitEBwlR[[i]]=fitAncestralGraph(Model, covList.EBwlR[[i]],dim(EBwlR[[i]])[1])
  fitEMwlL[[i]]=fitAncestralGraph(Model,covList.EMwlL[[i]],dim(EMwlL[[i]])[1])
  fitEMwlR[[i]]=fitAncestralGraph(Model,covList.EMwlR[[i]],dim(EMwlR[[i]])[1])
  fitESwlL[[i]]=fitAncestralGraph(Model,covList.ESwlL[[i]],dim(ESwlL[[i]])[1])
  fitESwlR[[i]]=fitAncestralGraph(Model,covList.ESwlR[[i]],dim(ESwlR[[i]])[1])
  
}


# beta
beta.EwwL = matrix(,length(yDat),length(ag.theta(fitEwwL[[1]])))
beta.EwwR = matrix(,length(yDat),length(ag.theta(fitEwwR[[1]])))
beta.EllL = matrix(,length(yDat),length(ag.theta(fitEllL[[1]])))
beta.EllR = matrix(,length(yDat),length(ag.theta(fitEllL[[1]])))

beta.EBwlL = matrix(,length(yDat),length(ag.theta(fitEBwlL[[1]])))
beta.EBwlR = matrix(,length(yDat),length(ag.theta(fitEBwlR[[1]])))
beta.EMwlL = matrix(,length(yDat),length(ag.theta(fitEMwlL[[1]])))
beta.EMwlR = matrix(,length(yDat),length(ag.theta(fitEMwlR[[1]])))
beta.ESwlL = matrix(,length(yDat),length(ag.theta(fitESwlL[[1]])))
beta.ESwlR = matrix(,length(yDat),length(ag.theta(fitESwlR[[1]])))



for (i in 1:length(yDat))
  
{	

  beta.EwwL[i,] = ag.theta(fitEwwL[[i]])
  beta.EwwR[i,] = ag.theta(fitEwwR[[i]])
  beta.EllL[i,] = ag.theta(fitEllL[[i]])
  beta.EllR[i,] = ag.theta(fitEllR[[i]])
  beta.EBwlL[i,] = ag.theta(fitEBwlL[[i]])
  beta.EBwlR[i,] = ag.theta(fitEBwlR[[i]])
  beta.EMwlL[i,] = ag.theta(fitEMwlL[[i]])
  beta.EMwlR[i,] = ag.theta(fitEMwlR[[i]])
  beta.ESwlL[i,] = ag.theta(fitESwlL[[i]])
  beta.ESwlR[i,] = ag.theta(fitESwlR[[i]])	
  
}


#############################################################################################
########################## now remove columns that are of no interest #######################
#############################################################################################



beta.EwwL = beta.EwwL[,-c(13,16,18:24)]
beta.EwwR = beta.EwwR[,-c(13,16,18:24)]
beta.EllL = beta.EllL[,-c(13,16,18:24)]
beta.EllR = beta.EllR[,-c(13,16,18:24)]

beta.EBwlL = beta.EBwlL[,-c(13,16,18:24)]
beta.EBwlR = beta.EBwlR[,-c(13,16,18:24)]
beta.EMwlL = beta.EMwlL[,-c(13,16,18:24)]
beta.EMwlR = beta.EMwlR[,-c(13,16,18:24)]
beta.ESwlL = beta.ESwlL[,-c(13,16,18:24)]
beta.ESwlR = beta.ESwlR[,-c(13,16,18:24)]

beta.testRLag4.MnoF=list(beta.EwwL,beta.EwwR,beta.EllL,beta.EllR,
                          beta.EBwlL,beta.EBwlR,beta.EMwlL,beta.EMwlR,beta.ESwlL,beta.ESwlR)

names(beta.testRLag4.MnoF)=c('EwwL', 'EwwR', 'EllL', 'EllR',
                              'EBwlL', 'EBwlR','EMwlL', 'EMwlR','ESwlL', 'ESwlR')

# give beta names:

for (n in 1:length(beta.testRLag4.MnoF))
{
  colnames(beta.testRLag4.MnoF[[n]])=c('vmPFC-stn','vmPFC-str','preSMA-stn','preSMA-str','stn-gpi',
                                     'gpe-gpi','gpi-tha','dlpfc-stn','dlpfc-str','tha-motor','str-gpe','str-gpi'
                                     ,'vmPFC-preSMA','vmPFC-dlpfc','preSMA-dlpfc')
  rownames(beta.testRLag4.MnoF[[n]])=names(yDat)
}

#-------------------------------------------------------------------------------------------#
#                               Collapse over hands                                         #
#-------------------------------------------------------------------------------------------#
beta_noF=beta.testRLag4.MnoF
# collapse over left and right
b.ww=Reduce('+',beta_noF[1:2])/2; b.ll=Reduce('+',beta_noF[3:4])/2; b.wl=Reduce('+',beta_noF[5:10])/6
b.Bwl=Reduce('+',beta_noF[5:6])/2
b.Mwl=Reduce('+',beta_noF[7:8])/2
b.Swl=Reduce('+',beta_noF[9:10])/2
b.wwllwl=list(b.ww, b.ll, b.wl,b.Bwl,b.Mwl,b.Swl)
names(b.wwllwl)=c('b.ww', 'b.ll', 'b.wl','b.Bwl','b.Mwl','b.Swl')

#############################################################################################
########################## now test whether wl small, medium and big differ            ######
#############################################################################################


# 1) do win-win divistions differ across conditions?

frame.wlB=as.data.frame(as.table(b.wwllwl$b.Bwl))
frame.wlM=as.data.frame(as.table(b.wwllwl$b.Mwl))
frame.wlS=as.data.frame(as.table(b.wwllwl$b.Swl))


frame.wlB=data.frame(frame.wlB,diff='BIG');colnames(frame.wlB)=c('ppn','connection','beta','diff')
frame.wlM=data.frame(frame.wlM,diff='MED');colnames(frame.wlM)=c('ppn','connection','beta','diff')
frame.wlS=data.frame(frame.wlS,diff='SM');colnames(frame.wlS)=c('ppn','connection','beta','diff')

WL.av=as.data.frame(rbind(frame.wlB,frame.wlM,frame.wlS))

WL.size=lm(beta~diff,data= WL.av,random=~1|ppn/diff)
anova(WL.size) # no effect

# 2) if no, then it is justified to use win-low (reduced over diff)
# this is already in b.wwllwl

# Evaluate patterns ww, ll, wl for:
# c('b.ww', 'b.ll', 'b.wl','b.Bwl','b.Mwl','b.Swl')
# c('vmPFC-stn','vmPFC-str','preSMA-stn','preSMA-str','stn-gpi',
#   'gpe-gpi','gpi-tha','dlpfc-stn','dlpfc-str','tha-motor','str-gpe','str-gpi'
#   ,'vmPFC-preSMA','vmPFC-dlpfc','preSMA-dlpfc')

# topdown to striatum
vmPFC.str=cbind(ww=b.wwllwl$b.ww[,'vmPFC-str'],ll=b.wwllwl$b.ll[,'vmPFC-str'],wl=b.wwllwl$b.wl[,'vmPFC-str'],
                wlB=b.wwllwl$b.Bwl[,'vmPFC-str'],wlM=b.wwllwl$b.Bwl[,'vmPFC-str'],wlS=b.wwllwl$b.Bwl[,'vmPFC-str'])

preSMA.str=cbind(ww=b.wwllwl$b.ww[,'preSMA-str'],ll=b.wwllwl$b.ll[,'preSMA-str'],wl=b.wwllwl$b.wl[,'preSMA-str'],
                 wlB=b.wwllwl$b.Bwl[,'preSMA-str'],wlM=b.wwllwl$b.Bwl[,'preSMA-str'],wlS=b.wwllwl$b.Bwl[,'preSMA-str'])

dlpfc.str=cbind(ww=b.wwllwl$b.ww[,'dlpfc-str'],ll=b.wwllwl$b.ll[,'dlpfc-str'],wl=b.wwllwl$b.wl[,'dlpfc-str'],
                wlB=b.wwllwl$b.Bwl[,'dlpfc-str'],wlM=b.wwllwl$b.Bwl[,'dlpfc-str'],wlS=b.wwllwl$b.Bwl[,'dlpfc-str'])

# topdown to stn
vmPFC.stn=cbind(ww=b.wwllwl$b.ww[,'vmPFC-stn'],ll=b.wwllwl$b.ll[,'vmPFC-stn'],wl=b.wwllwl$b.wl[,'vmPFC-stn'],
                wlB=b.wwllwl$b.Bwl[,'vmPFC-stn'],wlM=b.wwllwl$b.Bwl[,'vmPFC-stn'],wlS=b.wwllwl$b.Bwl[,'vmPFC-stn'])

preSMA.stn=cbind(ww=b.wwllwl$b.ww[,'preSMA-stn'],ll=b.wwllwl$b.ll[,'preSMA-stn'],wl=b.wwllwl$b.wl[,'preSMA-stn'],
                 wlB=b.wwllwl$b.Bwl[,'preSMA-stn'],wlM=b.wwllwl$b.Bwl[,'preSMA-stn'],wlS=b.wwllwl$b.Bwl[,'preSMA-stn'])

dlpfc.stn=cbind(ww=b.wwllwl$b.ww[,'dlpfc-stn'],ll=b.wwllwl$b.ll[,'dlpfc-stn'],wl=b.wwllwl$b.wl[,'dlpfc-stn'],
                wlB=b.wwllwl$b.Bwl[,'dlpfc-stn'],wlM=b.wwllwl$b.Bwl[,'dlpfc-stn'],wlS=b.wwllwl$b.Bwl[,'dlpfc-stn'])


# put into one list, and 
b.topstr=list(vmPFCtostr =vmPFC.str, preSMAtostr=preSMA.str, dlpfctostr= dlpfc.str)
b.Topstr=Reduce('+', b.topstr)/3

b.topstn=list(vmPFCtostn =vmPFC.stn, preSMAtostn=preSMA.stn, dlpfctostn=dlpfc.stn)
b.Topstn=Reduce('+', b.topstn)/3	# deepdown

Connectivity=list(Betaoverhand=b.wwllwl,b.Topstn=b.Topstn,b.Topstr=b.Topstr,ind_PFCSTN=b.topstn,ind_PFCSTR=b.topstr)

save(list=c('beta.testRLag4.MnoF','BMT','Connectivity'),file=paste(dir_base,'/Fit_output/ConncetivityStrength_withBIC7models_MnoF.rdat',sep=''))













