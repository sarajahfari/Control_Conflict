	AIC_direct = round(matrix(,length(yDat),length(models)),digits=2)
	colnames(AIC_direct) =  c(paste('aic',1:length(models),sep=''))
	rownames(AIC_direct)= names(yDat)
	
	Fit_direct = round(matrix(,length(yDat),length(models)),digits=2)
	colnames(Fit_direct) = c(paste('chi',1:length(models),sep=''))
	rownames(Fit_direct)= names(yDat)
	
	P_direct = round(matrix(,length(yDat),length(models)),digits=2)
	colnames(P_direct) = c(paste('chi-p',1:length(models),sep=''))
	rownames(P_direct)= names(yDat)

	logL_direct = round(matrix(,length(yDat),length(models)),digits=2) # maak een nieuwe matrix voor de log-likelihood per models en per ppn
	colnames(logL_direct) = c(paste('logL',1:length(models),sep=''))
	rownames(logL_direct)= names(yDat)
	
	nObs = round(matrix(,length(yDat),length(models)),digits=2) # maak een nieuwe matrix voor de log-likelihood per model en per ppn
	colnames(nObs) = c(paste('nobs',1:length(models),sep=''))
	rownames(nObs)= names(yDat)
	
	AMT=round(matrix(,length(cond),length(models)*2),digits=2)
	colnames(AMT)=c(colnames(AIC_direct),colnames(Fit_direct))
	rownames(AMT)=names(cond)
	
	BMT=round(matrix(,length(cond),length(models)*2),digits=2)
	colnames(BMT)=c(colnames(AIC_direct),colnames(Fit_direct))
	rownames(BMT)=names(cond)
	
	for (k in 1:length(cond))
	{
	
	for (i in 1:length(yDat))
	
	{
		Aic =c()
		Fit =c()
		P =c()
        nPars = c() # vecotr met nr par voor ieder models
		nobs=c()
		
		# models contains the models
		# yDat contains the data for all subjects in a list
		# C.Lab indicates the Region of input for the models
		# stig indicates what rows in yDat[i] contain the data for condition stinhibit congruent
		
		for (j in 1:length(models))
		
			{
				Aic[j]=fitAG(yDat[i], models[[j]],C.Lab, cond[[k]][[i]],detail='both')$aic
                nPars[j]=fitAG(yDat[i], models[[j]],C.Lab, cond[[k]][[i]],detail='both')$npars # bewaar per models nr par
				Fit[j]=fitAG(yDat[i], models[[j]],C.Lab, cond[[k]][[i]],detail='both')$fit$chi2
				P[j]=fitAG(yDat[i], models[[j]],C.Lab, cond[[k]][[i]],detail='both')$fit$p
				nobs[j] = length(cond[[k]][[i]])
				}
		
		AIC_direct[i,] = round(Aic,digits=2)
		Fit_direct[i,] = round(Fit,digits=2)
		P_direct[i,] = round(P,digits=2)
		logL_direct[i,] = Aic - 2*nPars # de log-likelihood is dan de aic - de factor van de aic
        nObs[i,]=round(nobs[j],digits=2)
                
		}	


		A=cbind(AIC_direct
		, P_direct)
		
		AIC.direct <- 
		apply(logL_direct,2,sum)+2*nPars 		

		#colMeans(A)
		AIC.direct
		apply(A,2,sd)
		aic.rfx = 
		apply(ifelse(A[,(length(models)+1):(2*length(models))]>0.049,1,0),2,sum)
		#aic.rfx
		AMT[k,]=c(AIC.direct,aic.rfx)
		
		BIC.totaal <- 
		apply(logL_direct,2,sum)+(nPars*log(apply(nObs,2,sum)))

		BMT[k,]=c(BIC.totaal,aic.rfx)
		
		
		
		}

