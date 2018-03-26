	
	
	rm(list=ls())
	base='/Users/sarajahfari/Github/Control_Conflict'
	load(paste(base,'/Processed_data/Behavior_Qpar.Rdat',sep=''))# group split accuracy
	# Explore.ac= RLwl.ac[RL.model[[1]][,1]<median(RL.model[[1]][,1]),1:3]
	# Exploit.ac= RLwl.ac[RL.model[[1]][,1]>median(RL.model[[1]][,1]),1:3]
	# bar_error_group=rbind(colMeans(Exploit.ac),colMeans(Explore.ac))
	# rownames(bar_error_group)=c('Exploit','Explore')
	# 
	# load('~/Dropbox/projects/Pearl/Beh_files/RL_learn_2blocklist.Rdat')
	# 
	# SQlist=data.frame()
	# 
	# for (sub in names(RL_learn))
	# {
	# 
	#   tmp=do.call("rbind",RL_learn[[sub]])
	#   Tmp=data.frame(tmp,SUB=sub)
	#   SQlist=rbind(SQlist,Tmp)
	# 
	# }
	
	names(SQlist)
	SQlist=data.frame(SQlist,strategy=ifelse(SQlist$Beta_Q<median(RL.model[[1]][,1]),'explore','exploit'))
	
	win=c('A','C','E')
	loss=c('B','D','F')
	
	Chosen=ifelse(SQlist$Choice%in%win,'Optimal','sub-Optimal')
	Chosen[which(SQlist$Choice=='n/a')]='n/a'
	SQlist=data.frame(SQlist,Chosen)
	
	Count=data.frame()
	C=0
	
	for (suB in names(RL_learn)) 
	{
	  use=SQlist[SQlist$SUB==suB,]
	  
	  # aantal optimal
	  
	  
	  # 
	  sd(use[use$Choice%in%win,'Q_chosen'])->SDwin
	  sd(use[use$Choice%in%loss,'Q_chosen'])->SDloss
	  sd(use[use$Choice='A','Q_chosen'])->SDA
	  sd(use[use$Choice='A','Q_chosen'])->SDB
	  sd(use[use$Choice='A','Q_chosen'])->SDC
	  sd(use[use$Choice='A','Q_chosen'])->SDD
	  sd(use[use$Choice='A','Q_chosen'])->SDE
	  sd(use[use$Choice='A','Q_chosen'])->SDF
	}
	names(SQlist$SUB)
	
	

	#rm(Chosen)
	

	# SQmin=SQlist[SQlist$Chosen!='n/a',c("Q_chosen","strategy","Chosen","Choice")]
	# SQexplore=SQmin[SQmin$strategy=='explore',c("Q_chosen","Chosen")]
	# SQexploit=SQmin[SQmin$strategy=='exploit',c("Q_chosen","Chosen")]
	# SQexploitchoice=SQmin[SQmin$strategy=='exploit',c("Q_chosen","Choice")]
	# SQexplorechoice=SQmin[SQmin$strategy=='explore',c("Q_chosen","Choice")]
	# 
	# SQloss=SQlist[SQlist$Choice=='F',c("Q_chosen","strategy")]
	# 
	# SQexploitAC=SQexploit[SQexploit$strategy=='explore',c("Q_chosen","Chosen")]
	# 
	# # ok try out ggplot
	# # install.packages('ggplot2')
	# # library(ggplot2)
	# 
	# l=qplot(Q_chosen, data=SQexploit, geom="density", fill=Choice, alpha=I(.6),
	#         main="", xlab="Q-Value",
	#         ylab="density posterior",xlim=c(0,1))
	# l <- l+ scale_fill_manual( values = c("green","red"))
	# 
	# l<- l+ theme_bw()
	# l<- l+ theme(legend.title=element_blank())
	# l
	# 
	# 
	# l=qplot(Q_chosen, data=SQexploitchoice, geom="density", fill=Choice, alpha=I(.6),
	#         main="", xlab="Q-Value",
	#         ylab="density posterior",xlim=c(0,1))
	# l <- l+ scale_fill_manual( values = 1:6)
	# 
	# l<- l+ theme_bw()
	# l<- l+ theme(legend.title=element_blank())
	# l
	# 
	# 
	# g=qplot(Q_chosen, data=SQexplorechoice, geom="density", fill=Choice, alpha=I(.6),
	#         main="", xlab="Q-Value",
	#         ylab="density posterior",xlim=c(0,1))
	# g <- g+ scale_fill_manual( values = 1:6)
	# 
	# g<- g+ theme_bw()
	# g<- g+ theme(legend.title=element_blank())
	# g
	# 
	# 
	# 
	# 
	# 
