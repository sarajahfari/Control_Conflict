rm(list=ls())
#---------------      load preprocessed data files  ----------------------------#

load('./Processed_data/Behavior_Qpar.Rdat') # load summarized behavior and Qlearning estimates
load('./AG/Fitmodel/Fit_output/ConncetivityStrength_withBIC7models_MnoF.rdat')


#---------------        load libraries              ----------------------------#

library(MASS)
library(plotrix)
library(reshape2)
library(nlme)
require(multcomp)
library(ez)

#-------------------------------------------------------------------------------#
#--------------         Results Draft               ----------------------------#
#-------------------------------------------------------------------------------#


#--------------       1. Uncertainty and Conflict-induced slowing    --------------#

# prepare
# learning phase accuracy
RL_perf=RL$learn[,7:9]

# win,win (ww), lose,lose(ll), and win,lose (w,l)

RLwl.med=cbind(rowMeans(RL[[3]][,c(4,5,12)],na.rm=T),
               rowMeans(RL[[3]][,c(9,11,15)],na.rm=T),rowMeans(RL[[3]][,c(6:7,8,13,10,14)],na.rm=T),rowMeans(RL[[3]][,c(1:3,6:7,8,13,10,14)],na.rm=T))

RLwl.mean=cbind(rowMeans(RL[[2]][,c(4,5,12)],na.rm=T),
                rowMeans(RL[[2]][,c(9,11,15)],na.rm=T),rowMeans(RL[[2]][,c(6:7,8,13,10,14)],na.rm=T),rowMeans(RL[[2]][,c(1:3,6:7,8,13,10,14)],na.rm=T))

RLwl.ac=cbind(rowMeans(RL[[4]][,c(4,6,12)],na.rm=T),
              rowMeans(RL[[4]][,c(9,11,15)],na.rm=T),rowMeans(RL[[4]][,c(6:7,8,13,10,14)],na.rm=T),rowMeans(RL[[4]][,c(1:3,6:7,8,13,10,14)],na.rm=T))


# wl.u = win loose pairs not previously seen during learning
colnames(RLwl.med)=c('ww','ll','wl.u','wl')
colnames(RLwl.mean)=c('ww','ll','wl.u','wl')
colnames(RLwl.ac)=c('ww','ll','wl.u','wl')

# slowing 
# RT
diff.ll=RLwl.med[,2]-RLwl.med[,3]
diff.ww=RLwl.med[,1]-RLwl.med[,3]

# accuracy
diff.ll.ac=RLwl.ac[,3]-RLwl.ac[,2]
diff.ww.ac=RLwl.ac[,3]-RLwl.ac[,1]

slowing=cbind(diff.ll,diff.ww, diff.ll.ac, diff.ww.ac)


# test
# is there a main effect of condition on 
#accuracy: yes -> wl differs from ww and ll 
RL.av.ac=as.data.frame(as.table(RLwl.ac[,1:3]))
colnames(RL.av.ac)=c('ppn','cond','AC')
lme_ac=lme(AC~cond,data=RL.av.ac,random=~1|ppn)
anova(lme_ac) 
# dit is precies hetzelfde als repeated measures anova spps
summary(glht(lme_ac, linfct=mcp(cond = "Tukey")), 
        test = adjusted(type = "bonferroni"))	


#RT: yes -> ll differs from wl and ww
RL.av.med=as.data.frame(as.table(RLwl.med[,1:3]))
colnames(RL.av.med)=c('ppn','cond','RT')
lme_medRT=lme(RT~cond,data=RL.av.med,random=~1|ppn)
anova(lme_medRT) 
# dit is precies hetzelfde als repeated measures anova spps df(2,88)
summary(glht(lme_medRT, linfct=mcp(cond = "Tukey")), 
        test = adjusted(type = "bonferroni"))

#----------- startegy and learning phase accuracy --------------#
# beta and learningphase accuracy
#apply(RL_perf,2,mean);apply(RL_perf,2,sd)
cor.test(RL.model[[1]][,1], RL_perf[,1]); #AB learn
cor.test(RL.model[[1]][,1], RL_perf[,2]);#cd learn
cor.test(RL.model[[1]][,1], RL_perf[,3])#ef learn

#----------- RM anova with Strategy and Condition --------------#


# prepare data
ACRT=merge(RL.av.ac, RL.av.med)
av.beta=as.data.frame(as.table(RL.model[[1]][,1]))
colnames(av.beta)=c('ppn','beta')
av.beta.beh= merge(ACRT,av.beta)

# accuracy, yes! main effect beta,cond, and interaction
AC_beta.av=ezANOVA(data=av.beta.beh,dv=AC,within=cond,between=beta,wid=ppn)
print(AC_beta.av)

# RT, yes! main effect beta,cond, and interaction
RT_beta.av=ezANOVA(data=av.beta.beh,dv=RT,within=cond,
                   between=beta,wid=ppn, return_aov = T)
print(RT_beta.av)

# lme results are same as ezANOVA (just checking :)
RT_ph=lme(RT~cond*beta,data= av.beta.beh,random=~1|ppn/cond)
anova(RT_ph)

#----- group split comparisons to illustrate interaction conflict*strategy ------------#

# Expoit/Explore

# group split accuracy
Explore.ac= RLwl.ac[RL.model[[1]][,1]<median(RL.model[[1]][,1]),1:3]
Exploit.ac= RLwl.ac[RL.model[[1]][,1]>median(RL.model[[1]][,1]),1:3]
bar_error_group=rbind(colMeans(Exploit.ac),colMeans(Explore.ac))
rownames(bar_error_group)=c('Exploit','Explore')

# group split RT
Explore= RLwl.med[RL.model[[1]][,1]<median(RL.model[[1]][,1]),1:3]
Exploit= RLwl.med[RL.model[[1]][,1]>median(RL.model[[1]][,1]),1:3]
bar_error_group_RT=rbind(colMeans(Exploit),colMeans(Explore))
rownames(bar_error_group_RT)=c('Exploit','Explore')

sbar=rbind(apply(Explore,2,sd)/sqrt(length(Explore[,1])),apply(Exploit,2,sd)/sqrt(length(Exploit[,1])))
sbarRT=rbind(apply(Explore,2,sd)/sqrt(length(Explore[,1])),apply(Exploit,2,sd)/sqrt(length(Exploit[,1])))

# for plot
#RT
t.test(Exploit[,2],Explore[,2],var.equal=T) # p=0.001 
t.test(Exploit[,3],Explore[,3],var.equal=T) # p=0.04
t.test(Exploit[,1],Explore[,1],var.equal=T) # p=0.09

#Accuracy
t.test(Exploit.ac[,1],Explore.ac[,1],var.equal=T) # p=0.002
t.test(Exploit.ac[,3],Explore.ac[,3],var.equal=T) # p=0.000
t.test(Exploit.ac[,2],Explore.ac[,2],var.equal=T) # p=0.08 



#--------------       2. Control and Conflict-induced slowing    --------------#

# make data.frame where extra subjects in RL task are excluded
# what is the relationship between control and learning
Reg.data=data.frame(
  ll_RT=RLwl.med[-which(rownames(RLwl.med)%in%Ex.stop),'ll'],
  ww_RT=RLwl.med[-which(rownames(RLwl.med)%in%Ex.stop),'ww'],
  wl_RT=RLwl.med[-which(rownames(RLwl.med)%in%Ex.stop),'wl.u'],
  ll_ac=RLwl.ac[-which(rownames(RLwl.med)%in%Ex.stop),'ll'],
  ww_ac=RLwl.ac[-which(rownames(RLwl.med)%in%Ex.stop),'ww'],
  wl_ac=RLwl.ac[-which(rownames(RLwl.med)%in%Ex.stop),'wl.u'],
  ll_slrt=slowing[-which(rownames(slowing)%in%Ex.stop),'diff.ll'],
  SSRT=STOP[,'SSRT'],
  Beta=RL.model[[1]][-which(rownames(RL.model[[1]])%in%Ex.stop),'Beta'])


# correlations reported results section
cor.test(Reg.data[,'ll_RT'],Reg.data[,'SSRT'])
cor.test(Reg.data[,'ll_slrt'],Reg.data[,'SSRT'])
cor.test(Reg.data[,'ww_RT'],Reg.data[,'SSRT'])
cor.test(Reg.data[,'Beta'],Reg.data[,'SSRT'])

Multi.reg1=lm(ll_RT~Beta+SSRT,data=Reg.data)
summary(Multi.reg1)


# ssrt and lose-lose accuracy
cor.test(Reg.data[,'ll_ac'],Reg.data[,'SSRT'])

#--------------       3. The efficacy of control in the STN during full stops and slowing    --------------#

# please see FIR analysis preformed by Tomas knapen


#--------------       4. Conflict-induced slowing in cortico-basal ganglia pathways    --------------#


# is there an effect of condition or conflict for top-down connections?

# Prepare test file
bSTN.av=as.data.frame(as.table(Connectivity$b.Topstn[,c(1,2,3)]))
colnames(bSTN.av)=c('ppn','cond','beta')
bStr.av=as.data.frame(as.table(Connectivity$b.Topstr[,1:3]))
colnames(bStr.av)=c('ppn','cond','beta')
STN=data.frame(bSTN.av,connection='STN')
STR=data.frame(bStr.av,connection='STR')
TOP=as.data.frame(rbind(STN,STR))
# do anova 
TOP.av=ezANOVA(data=TOP,dv=.(beta),within=.(cond,connection),wid=.(ppn))
print(TOP.av) # only  main effect for connection, connectivity towards STR is stronger

# how is top-down related to behavior?
cor.test(Connectivity$b.Topstn[,'ll'], RLwl.med[,'ll']) # yes, to LL RT
cor.test(Connectivity$b.Topstn[,'ll'], slowing[,'diff.ll'])# yes, to LL slowing

# not observed for ww
cor.test(Connectivity$b.Topstn[,'ww'], RLwl.med[,'ww']) # no
cor.test(Connectivity$b.Topstn[,'ww'], slowing[,'diff.ww']) # no
# or for top-striatum
cor.test(Connectivity$b.Topstr[,'ll'], RLwl.med[,'ll']) # no
cor.test(Connectivity$b.Topstr[,'ll'], slowing[,'diff.ll']) #no

cor.test(Connectivity$b.Topstn[,2], RLwl.med[,2]) 
cor.test(Connectivity$b.Topstn[,2], slowing[,1])

# does top-STN connectviity on LL relate to explore/exploit?
cor.test(Connectivity$b.Topstn[,'ll'], RL.model$learn[,'Beta']) # Yes!!

# now do a regression to evaluate explore/exploit, at the same time as RT.
Reg.data1=data.frame(ll_RT=RLwl.med[,'ll'], 
                     llPFC_STN=Connectivity$b.Topstn[,'ll'],
                     llPFC_STR=Connectivity$b.Topstr[,'ll'],
                     Beta=RL.model$learn[,'Beta'])

Multi.reg2=lm(llPFC_STN ~Beta+ ll_RT,data=Reg.data1)
summary(Multi.reg2)

# SSRT and hyperdirect
cor.test(Connectivity$b.Topstn[-which(rownames(Connectivity$b.Topstn)%in%Ex.stop),'ll'], STOP[,'SSRT']) 

# en of results section!!!!







