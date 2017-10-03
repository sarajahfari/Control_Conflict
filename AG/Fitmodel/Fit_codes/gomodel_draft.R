
	# classic hyper-indirect model

	# direct				
	ag1=makeMG(dg=DAG(PstriatumNoVentri ~ preSMAsmall + PvmPFCNoventri + DLPFCposterior,
					maxGPi30exc~PstriatumNoVentri,
					Thalamus40exc~maxGPi30exc,
					MotorBA4~Thalamus40exc			
					),ug=UG(~preSMAsmall*PvmPFCNoventri*DLPFCposterior+maxSTN25exc+GPe30exc))
					
	# indirect				
	ag2=makeMG(dg=DAG(PstriatumNoVentri ~ preSMAsmall + PvmPFCNoventri + DLPFCposterior,
					GPe30exc~PstriatumNoVentri,
					maxGPi30exc~GPe30exc,
					Thalamus40exc~maxGPi30exc,
					MotorBA4~Thalamus40exc			
					),ug=UG(~preSMAsmall*PvmPFCNoventri*DLPFCposterior+maxSTN25exc))
					
	# hyperdirect			
	ag3=makeMG(dg=DAG(#PstriatumNoVentri ~ preSMAsmall + PvmPFCNoventri + DLPFCposterior,
					maxSTN25exc~preSMAsmall +PvmPFCNoventri+ DLPFCposterior,
					#GPe30exc~PstriatumNoVentri,
					maxGPi30exc~maxSTN25exc,
					Thalamus40exc~maxGPi30exc,
					MotorBA4~Thalamus40exc			
					),ug=UG(~preSMAsmall*PvmPFCNoventri*DLPFCposterior+GPe30exc+ PstriatumNoVentri+ GPe30exc))
					
	# direct + hyperdirect				
	ag4=makeMG(dg=DAG(
					PstriatumNoVentri~ preSMAsmall +PvmPFCNoventri + DLPFCposterior,
					maxSTN25exc~preSMAsmall +PvmPFCNoventri+ DLPFCposterior,
					#GPe30exc~PstriatumNoVentri,
					maxGPi30exc~PstriatumNoVentri+maxSTN25exc,
					Thalamus40exc~maxGPi30exc,
					MotorBA4~Thalamus40exc			
					),ug=UG(~preSMAsmall*PvmPFCNoventri*DLPFCposterior+GPe30exc))
					

	# direct + indirect				
	ag5=makeMG(dg=DAG(
					PstriatumNoVentri~ preSMAsmall +PvmPFCNoventri+ DLPFCposterior,
					#maxSTN25exc~GPe30exc,
					GPe30exc~PstriatumNoVentri,
					maxGPi30exc~PstriatumNoVentri+GPe30exc,
					Thalamus40exc~maxGPi30exc,
					MotorBA4~ Thalamus40exc			
					),ug=UG(~preSMAsmall*PvmPFCNoventri*DLPFCposterior+maxSTN25exc))
					
	# hyperdirect + indirect			
	ag6=makeMG(dg=DAG(
					PstriatumNoVentri~ preSMAsmall +PvmPFCNoventri+ DLPFCposterior,
					maxSTN25exc~preSMAsmall +PvmPFCNoventri+ DLPFCposterior,
					GPe30exc~PstriatumNoVentri,
					maxGPi30exc~GPe30exc+maxSTN25exc,
					Thalamus40exc~maxGPi30exc,
					MotorBA4~ Thalamus40exc			
					),ug=UG(~preSMAsmall*PvmPFCNoventri*DLPFCposterior))
							
	# direct + indirect + hyperdirect			
	ag7=makeMG(dg=DAG(
					PstriatumNoVentri~ preSMAsmall +PvmPFCNoventri+ DLPFCposterior,
					maxSTN25exc~preSMAsmall +PvmPFCNoventri+ DLPFCposterior,
					GPe30exc~PstriatumNoVentri,
					maxGPi30exc~PstriatumNoVentri+GPe30exc+maxSTN25exc,
					Thalamus40exc~maxGPi30exc,
					MotorBA4~ Thalamus40exc			
					),ug=UG(~preSMAsmall*PvmPFCNoventri*DLPFCposterior))
					

					
					
	
	

	models=list(ag1,ag2,ag3,ag4,ag5,ag6,ag7)
   
    names(models)=c('dir','indir','hypdir','dir-hyp','dir-indir','indi-hyp','dir-indi-hyp')