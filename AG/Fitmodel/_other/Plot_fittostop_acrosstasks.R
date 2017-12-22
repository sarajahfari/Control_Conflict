load("/Users/sarajahfari/Github/Control_Conflict/AG/Fitmodel/_other/BICFIT_RLtestStopNetwork.Rdat")
RL_stopfit=BMT;rm(BMT)
load("/Users/sarajahfari/Github/Control_Conflict/AG/Fitmodel/_other/BICFIT_Stoptask.Rdat")
ST_stopfit=BMT; rm(BMT)

RL.st.p=(RL_stopfit[,'chi1']/45)*100
ST.st.p=(ST_stopfit[,'chi1']/43)*100

all=data.frame(ST=ST.st.p[1],SR=ST.st.p[2],Go=c(ST.st.p[4:5]),WW=c(RL.st.p[1:2]),LL=c(RL.st.p[4:5]))

dev.off()
dev.new()

par(cex= 1, cex.main=2,mar = c(4, 6, 3, 2), mgp = c(3.9, 1, 0),
    cex.lab = 1.3, cex.axis = 1.3, bty = "n", las=2, 
    font.axis=2,font.lab=2,font=2,lwd=3, family='Arial',bg='white')

barplot(as.matrix(all[,c('WW','LL','Go','ST','SR')]),beside=T
        ,ylim=c(0,110), las=1,col=c('blue','blue',
                                    'blue','blue',
                                    'darkgreen','darkgreen',
                                    'red','red','orange','orange'),
        density=c(rep(c(20,100),3),rep(100,4)),ylab='% N Fits',las=1)
legend('topleft',c('Left hand','Right hand'),density=c(20,100),bty='n')
