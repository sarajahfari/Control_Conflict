# define conditions for AG model fitting testphase RT task Control and Conflict

# define conditions for AG model fitting davinci

# load library
load(paste(dir_data,"/Singletrial_connectivityinputAG_RLtest.rdat",sep='')) # no-ftest

#################################################

	# correct trials
	AbL= list();AbR= list()
	CdL= list();CdR= list()
	EfL= list();EfR= list()
	AcL= list();AcR= list()
	AdL= list();AdR= list()
	AeL= list();AeR= list()
	AfL= list();AfR= list()
	bCL= list();bCR= list()
	bDL= list();bDR= list()
	bEL= list();bER= list()
	bFL= list();bFR= list()
	CeL= list();CeR= list()
	CfL= list();CfR= list()
	dEL= list();dER= list()
	dFL= list();dFR= list()

	error= list();miss= list() # zet hier alle errors samen
		
	# collapsed over hand
	
	Ab= list()
	Cd= list()
	Ef= list()
	Ac= list()
	Ad= list()
	Ae= list()
	Af= list()
	bC= list()
	bD= list()
	bE= list()
	bF= list()
	Ce= list()
	Cf= list()
	dE= list()
	dF= list()
	
	# collapsed over approach, avoid (pure: with all trials, strong: with bd,ac removed)
	
	# approach (pure, p; strong, s) 
	pApAL=list();pApAR=list();pApA=list()
	sApAL=list();sApAR=list();sApA=list()
	# avoid (pure, p; strong, s) 
	pAvBL=list();pAvBR=list();pAvB=list()
	sAvBL=list();sAvBR=list();sAvB=list()
	
	# approach (pure, p; strong, s) - error + incorrect
	epApAL=list();epApAR=list();epApA=list()
	esApAL=list();esApAR=list();esApA=list()
	# avoid (pure, p; strong, s) 
	epAvBL=list();epAvBR=list();epAvB=list()
	esAvBL=list();esAvBR=list();esAvB=list()
	
	# collapsed over ww, ll, wl
	
	wwL=list();wwR=list();ww=list()
	llL=list();llR=list();ll=list()
	wlL=list();wlR=list();wl=list()
	
	# pure trials with no learning phase example
	pwlL=list();pwlR=list();pwl=list()
	
	# divide wl in big, medium, small
	BwlL=list();BwlR=list();Bwl=list()
	MwlL=list();MwlR=list();Mwl=list()
	SwlL=list();SwlR=list();Swl=list()

	# correct + incorrect trials
	eAbL= list();eAbR= list()
	eCdL= list();eCdR= list()
	eEfL= list();eEfR= list()
	eAcL= list();eAcR= list()
	eAdL= list();eAdR= list()
	eAeL= list();eAeR= list()
	eAfL= list();eAfR= list()
	ebCL= list();ebCR= list()
	ebDL= list();ebDR= list()
	ebEL= list();ebER= list()
	ebFL= list();ebFR= list()
	eCeL= list();eCeR= list()
	eCfL= list();eCfR= list()
	edEL= list();edER= list()
	edFL= list();edFR= list()

		
	# collapsed over hand
	
	eAb= list()
	eCd= list()
	eEf= list()
	eAc= list()
	eAd= list()
	eAe= list()
	eAf= list()
	ebC= list()
	ebD= list()
	ebE= list()
	ebF= list()
	eCe= list()
	eCf= list()
	edE= list()
	edF= list()
	
	# collapsed over ww, ll, wl
	
	ewwL=list();ewwR=list();eww=list()
	ellL=list();ellR=list();ell=list()
	ewlL=list();ewlR=list();ewl=list()
	
	# pure trials with no learning phase example
	epwlL=list();epwlR=list();epwl=list()

	# divide wl in big, medium, small - correct + incorrect
	eBwlL=list();eBwlR=list();eBwl=list()
	eMwlL=list();eMwlR=list();eMwl=list()
	eSwlL=list();eSwlR=list();eSwl=list()

	######################################################

	for (c in 1:length(yDat))
	
		{	
			# seperate errors form rest
			test=yDat[[c]][-grep('error', yDat[[c]]$type),]
			error[[c]]=yDat[[c]][grep('error', yDat[[c]]$type),]
			miss[[c]]=yDat[[c]][grep('Miss', yDat[[c]]$type),]
			
			AbL[[c]] =grep('AbL', test$type);AbR[[c]] =grep('AbR', test$type);Ab[[c]]=c(AbL[[c]],AbR[[c]])
			CdL[[c]] =grep('CdL', test$type);CdR[[c]] =grep('CdR', test$type);Cd[[c]]=c(CdL[[c]],CdR[[c]])
			EfL[[c]] =grep('EfL', test$type);EfR[[c]] =grep('EfR', test$type);Ef[[c]]=c(EfL[[c]],EfR[[c]])
			AcL[[c]] =grep('AcL', test$type);AcR[[c]] =grep('AcR', test$type);Ac[[c]]=c(AcL[[c]],AcR[[c]])
			AdL[[c]] =grep('AdL', test$type);AdR[[c]] =grep('AdR', test$type);Ad[[c]]=c(AdL[[c]],AdR[[c]])
			AeL[[c]] =grep('AeL', test$type);AeR[[c]] =grep('AeR', test$type);Ae[[c]]=c(AeL[[c]],AeR[[c]])
			AfL[[c]] =grep('AfL', test$type);AfR[[c]] =grep('AfR', test$type);Af[[c]]=c(AfL[[c]],AfR[[c]])
			bCL[[c]] =grep('bCL', test$type);bCR[[c]] =grep('bCR', test$type);bC[[c]]=c(bCL[[c]],bCR[[c]])
			bDL[[c]] =grep('bDL', test$type);bDR[[c]] =grep('bDR', test$type);bD[[c]]=c(bDL[[c]],bDR[[c]])
			bEL[[c]] =grep('bEL', test$type);bER[[c]] =grep('bER', test$type);bE[[c]]=c(bEL[[c]],bER[[c]])
			bFL[[c]] =grep('bFL', test$type);bFR[[c]] =grep('bFR', test$type);bF[[c]]=c(bFL[[c]],bFR[[c]])
			CeL[[c]] =grep('CeL', test$type);CeR[[c]] =grep('CeR', test$type);Ce[[c]]=c(CeL[[c]],CeR[[c]])
			CfL[[c]] =grep('CfL', test$type);CfR[[c]] =grep('CfR', test$type);Cf[[c]]=c(CfL[[c]],CfR[[c]])
			dEL[[c]] =grep('dEL', test$type);dER[[c]] =grep('dER', test$type);dE[[c]]=c(dEL[[c]],dER[[c]])
			dFL[[c]] =grep('dFL', test$type);dFR[[c]] =grep('dFR', test$type);dF[[c]]=c(dFL[[c]],dFR[[c]])
			
			
			# approach (pure, p; strong, s) 
				pApAL[[c]]=c(AcL[[c]],AdL[[c]],AeL[[c]],AfL[[c]]);
				pApAR[[c]]=c(AcR[[c]],AdR[[c]],AeR[[c]],AfR[[c]]);
				pApA[[c]]=c(pApAL[[c]],pApAR[[c]])
					
					sApAL[[c]]=c(AdL[[c]],AeL[[c]],AfL[[c]]);
					sApAR[[c]]=c(AdR[[c]],AeR[[c]],AfR[[c]]);
					sApA[[c]]=c(sApAL[[c]],sApAR[[c]])		
			
			# avoid (pure, p; strong, s) 
				pAvBL[[c]]=c(bCL[[c]],bDL[[c]],bEL[[c]],bFL[[c]]);
				pAvBR[[c]]=c(bCR[[c]],bDR[[c]],bER[[c]],bFR[[c]]);
				pAvB[[c]]=c(pAvBL[[c]],pAvBR[[c]])
					
					sAvBL[[c]]=c(bCL[[c]],bEL[[c]],bFL[[c]]);
					sAvBR[[c]]=c(bCR[[c]],bER[[c]],bFR[[c]]);
					sAvB[[c]]=c(sAvBL[[c]],sAvBR[[c]])
			
			
			# collapsed over ww, ll, wl
			wwL[[c]]=c(AcL[[c]],AeL[[c]],CeL[[c]]);
			wwR[[c]]=c(AcR[[c]],AeR[[c]],CeR[[c]]);
			ww[[c]]=c(wwL[[c]],wwR[[c]]);
			
			llL[[c]]=c(bDL[[c]],bFL[[c]],dFL[[c]]);
			llR[[c]]=c(bDR[[c]],bFR[[c]],dFR[[c]]);
			ll[[c]]=c(llL[[c]],llR[[c]]);
			
			wlL[[c]]=c(AbL[[c]],CdL[[c]],EfL[[c]],AdL[[c]],AfL[[c]],bCL[[c]],bEL[[c]],CfL[[c]],dEL[[c]]);
			wlR[[c]]=c(AbR[[c]],CdR[[c]],EfR[[c]],AdR[[c]],AfR[[c]],bCR[[c]],bER[[c]],CfR[[c]],dER[[c]]);
			wl[[c]]=c(wlL[[c]],wlR[[c]])
			
			# pure
			pwlL[[c]]=c(AdL[[c]],AfL[[c]],bCL[[c]],bEL[[c]],CfL[[c]],dEL[[c]]);
			pwlR[[c]]=c(AdR[[c]],AfR[[c]],bCR[[c]],bER[[c]],CfR[[c]],dER[[c]]);
			pwl[[c]]=c(pwlL[[c]],pwlR[[c]])
			
			# wl Big
			BwlL[[c]]=c(AdL[[c]],bCL[[c]]);
			BwlR[[c]]=c(AdR[[c]],bCR[[c]]);
			Bwl[[c]]=c(BwlL[[c]],BwlR[[c]])
			
			# wl medium
			MwlL[[c]]=c(AfL[[c]],bEL[[c]]);
			MwlR[[c]]=c(AfR[[c]],bER[[c]]);
			Mwl[[c]]=c(MwlL[[c]],MwlR[[c]])
			
			# wl small
			SwlL[[c]]=c(CfL[[c]],dEL[[c]]);
			SwlR[[c]]=c(CfR[[c]],dER[[c]]);
			Swl[[c]]=c(SwlL[[c]],SwlR[[c]])
			
			
			# now for error and non error trials combined
			
			eAbL[[c]] =grep('AbL', yDat[[c]]$type);eAbR[[c]] =grep('AbR', yDat[[c]]$type);eAb[[c]]=c(eAbL[[c]],eAbR[[c]])
			eCdL[[c]] =grep('CdL', yDat[[c]]$type);eCdR[[c]] =grep('CdR', yDat[[c]]$type);eCd[[c]]=c(eCdL[[c]],eCdR[[c]])
			eEfL[[c]] =grep('EfL', yDat[[c]]$type);eEfR[[c]] =grep('EfR', yDat[[c]]$type);eEf[[c]]=c(eEfL[[c]],eEfR[[c]])
			eAcL[[c]] =grep('AcL', yDat[[c]]$type);eAcR[[c]] =grep('AcR', yDat[[c]]$type);eAc[[c]]=c(eAcL[[c]],eAcR[[c]])
			eAdL[[c]] =grep('AdL', yDat[[c]]$type);eAdR[[c]] =grep('AdR', yDat[[c]]$type);eAd[[c]]=c(eAdL[[c]],eAdR[[c]])
			eAeL[[c]] =grep('AeL', yDat[[c]]$type);eAeR[[c]] =grep('AeR', yDat[[c]]$type);eAe[[c]]=c(eAeL[[c]],eAeR[[c]])
			eAfL[[c]] =grep('AfL', yDat[[c]]$type);eAfR[[c]] =grep('AfR', yDat[[c]]$type);eAf[[c]]=c(eAfL[[c]],eAfR[[c]])
			ebCL[[c]] =grep('bCL', yDat[[c]]$type);ebCR[[c]] =grep('bCR', yDat[[c]]$type);ebC[[c]]=c(ebCL[[c]],ebCR[[c]])
			ebDL[[c]] =grep('bDL', yDat[[c]]$type);ebDR[[c]] =grep('bDR', yDat[[c]]$type);ebD[[c]]=c(ebDL[[c]],ebDR[[c]])
			ebEL[[c]] =grep('bEL', yDat[[c]]$type);ebER[[c]] =grep('bER', yDat[[c]]$type);ebE[[c]]=c(ebEL[[c]],ebER[[c]])
			ebFL[[c]] =grep('bFL', yDat[[c]]$type);ebFR[[c]] =grep('bFR', yDat[[c]]$type);ebF[[c]]=c(ebFL[[c]],ebFR[[c]])
			eCeL[[c]] =grep('CeL', yDat[[c]]$type);eCeR[[c]] =grep('CeR', yDat[[c]]$type);eCe[[c]]=c(eCeL[[c]],eCeR[[c]])
			eCfL[[c]] =grep('CfL', yDat[[c]]$type);eCfR[[c]] =grep('CfR', yDat[[c]]$type);eCf[[c]]=c(eCfL[[c]],eCfR[[c]])
			edEL[[c]] =grep('dEL', yDat[[c]]$type);edER[[c]] =grep('dER', yDat[[c]]$type);edE[[c]]=c(edEL[[c]],edER[[c]])
			edFL[[c]] =grep('dFL', yDat[[c]]$type);edFR[[c]] =grep('dFR', yDat[[c]]$type);edF[[c]]=c(edFL[[c]],edFR[[c]])
			
			# approach (pure, p; strong, s) 
				epApAL[[c]]=c(eAcL[[c]],eAdL[[c]],eAeL[[c]],eAfL[[c]]);
				epApAR[[c]]=c(eAcR[[c]],eAdR[[c]],eAeR[[c]],eAfR[[c]]);
				epApA[[c]]=c(epApAL[[c]],epApAR[[c]])
					
					esApAL[[c]]=c(eAdL[[c]],eAeL[[c]],eAfL[[c]]);
					esApAR[[c]]=c(eAdR[[c]],eAeR[[c]],eAfR[[c]]);
					esApA[[c]]=c(esApAL[[c]],esApAR[[c]])		
			
			# avoid (pure, p; strong, s) 
				epAvBL[[c]]=c(ebCL[[c]],ebDL[[c]],ebEL[[c]],ebFL[[c]]);
				epAvBR[[c]]=c(ebCR[[c]],ebDR[[c]],ebER[[c]],ebFR[[c]]);
				epAvB[[c]]=c(epAvBL[[c]],epAvBR[[c]])
					
					esAvBL[[c]]=c(ebCL[[c]],ebEL[[c]],ebFL[[c]]);
					esAvBR[[c]]=c(ebCR[[c]],ebER[[c]],ebFR[[c]]);
					esAvB[[c]]=c(esAvBL[[c]],esAvBR[[c]])
				
			# collapsed over ww, ll, wl
			ewwL[[c]]=c(eAcL[[c]],eAeL[[c]],eCeL[[c]]);
			ewwR[[c]]=c(eAcR[[c]],eAeR[[c]],eCeR[[c]]);
			eww[[c]]=c(ewwL[[c]],ewwR[[c]]);
			
			ellL[[c]]=c(ebDL[[c]],ebFL[[c]],edFL[[c]]);
			ellR[[c]]=c(ebDR[[c]],ebFR[[c]],edFR[[c]]);
			ell[[c]]=c(ellL[[c]],ellR[[c]]);
			
			ewlL[[c]]=c(eAbL[[c]],eCdL[[c]],eEfL[[c]],eAdL[[c]],eAfL[[c]],ebCL[[c]],ebEL[[c]],eCfL[[c]],edEL[[c]]);
			ewlR[[c]]=c(eAbR[[c]],eCdR[[c]],eEfR[[c]],eAdR[[c]],eAfR[[c]],ebCR[[c]],ebER[[c]],eCfR[[c]],edER[[c]]);
			ewl[[c]]=c(ewlL[[c]],ewlR[[c]])
			
							
			epwlL[[c]]=c(eAdL[[c]],eAfL[[c]],ebCL[[c]],ebEL[[c]],eCfL[[c]],edEL[[c]]);
			epwlR[[c]]=c(eAdR[[c]],eAfR[[c]],ebCR[[c]],ebER[[c]],eCfR[[c]],edER[[c]]);
			epwl[[c]]=c(epwlL[[c]],epwlR[[c]])
			
			
			# wl Big (50)
			eBwlL[[c]]=c(eAdL[[c]],ebCL[[c]]);
			eBwlR[[c]]=c(eAdR[[c]],ebCR[[c]]);
			eBwl[[c]]=c(eBwlL[[c]],eBwlR[[c]])
			
			# wl medium (40)
			eMwlL[[c]]=c(eAfL[[c]],ebEL[[c]]);
			eMwlR[[c]]=c(eAfR[[c]],ebER[[c]]);
			eMwl[[c]]=c(eMwlL[[c]],eMwlR[[c]])
			
			# wl small(30)
			eSwlL[[c]]=c(eCfL[[c]],edEL[[c]]);
			eSwlR[[c]]=c(eCfR[[c]],edER[[c]]);
			eSwl[[c]]=c(eSwlL[[c]],eSwlR[[c]])

		}
		
	
	# give names of subjects to each list	
	names(pApAL)= names(yDat)
	names(sApAL)= names(yDat)
	names(pAvBL)= names(yDat)
	names(sAvBL)= names(yDat)
	names(pApAR)= names(yDat)
	names(sApAR)= names(yDat)
	names(pAvBR)= names(yDat)
	names(sAvBR)= names(yDat)
	names(pApA)= names(yDat)
	names(sApA)= names(yDat)
	names(pAvB)= names(yDat)
	names(sAvB)= names(yDat)
	
	
	names(BwlL)= names(yDat)
	names(MwlL)= names(yDat)
	names(SwlL)= names(yDat)
	names(BwlR)= names(yDat)
	names(MwlR)= names(yDat)
	names(SwlR)= names(yDat)
	names(Bwl)= names(yDat)
	names(Mwl)= names(yDat)
	names(Swl)= names(yDat)

	
	names(wwL)= names(yDat)
	names(llL)= names(yDat)
	names(wlL)= names(yDat)
	names(pwlL)= names(yDat) # no learning pairs
	names(wwR)= names(yDat)
	names(llR)= names(yDat)#
	names(wlR)= names(yDat)# 
	names(pwlR)= names(yDat)# no learning pairs
	names(ww)= names(yDat)# 
	names(ll)= names(yDat)# 
	names(wl)= names(yDat)
	names(pwl)= names(yDat)# no learning pairs
	
	
	names(ewwL)= names(yDat)
	names(ellL)= names(yDat)
	names(ewlL)= names(yDat)
	names(epwlL)= names(yDat)# no learning pairs
	names(ewwR)= names(yDat)
	names(ellR)= names(yDat)
	names(ewlR)= names(yDat)
	names(epwlR)= names(yDat)# no learning pairs
	names(eww)= names(yDat)
	names(ell)= names(yDat)
	names(ewl)= names(yDat)
	names(epwl)= names(yDat)# no learning pairs
	
	names(error)= names(yDat)
	names(miss)= names(yDat)
	
	# no learning pairs
	names(eBwlL)= names(yDat)
	names(eMwlL)= names(yDat)
	names(eSwlL)= names(yDat)
	names(eBwlR)= names(yDat)
	names(eMwlR)= names(yDat)
	names(eSwlR)= names(yDat)
	names(eBwl)= names(yDat)
	names(eMwl)= names(yDat)
	names(eSwl)= names(yDat)
	
	names(epApAL)= names(yDat)
	names(esApAL)= names(yDat)
	names(epAvBL)= names(yDat)
	names(esAvBL)= names(yDat)
	names(epApAR)= names(yDat)
	names(esApAR)= names(yDat)
	names(epAvBR)= names(yDat)
	names(esAvBR)= names(yDat)
	names(epApA)= names(yDat)
	names(esApA)= names(yDat)
	names(epAvB)= names(yDat)
	names(esAvB)= names(yDat)
	
	
	
		