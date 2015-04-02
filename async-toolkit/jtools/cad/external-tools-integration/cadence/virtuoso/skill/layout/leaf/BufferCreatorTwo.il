(defun MakeBuffer (CellView ViewName)
	(let (text parameter1 parameter2 parameter3 parameter4 cordnx1 cordnx2 cordr1nx1 cordr1nx2 cordr1px1 cordr1px2 cordR0x1 cordR0x2 cordR0y1 cordR0y2 cordR1x1 cordR1x2 cordR1y2 cordResx2 cordresx1 cordrvx1 cordrvx2 cordrvy2 cordLex1 cordLex2 cordLey2 R0NWidth R0PWidth R1NWidth R1PWidth LeNWidth Nr0Width Pr0Width LePWidth ResPWidth ResNWidth rvNWidth rvPWidth ActualcordR1x2 ActualcordR1x1 ActualcordR0x2 ActualcordR0x1 i Actualcordrvx1 ActualcordLex1 actualleftPoly ActualleftPoly ActualrightPoly Actualcordrvx2 prbound pry1 pry2 pry3 pry4 INST SHAPE VIA)
	i=0
	fl = infile("Parameter.txt")
	while( gets(k fl) != nil
  	text = parseString( k " \n")
	parameter1 = nth(0 text) 
	parameter2 = nth(1 text) 
	parameter3 = nth(2 text) 
	parameter4 = nth(3 text) 
 ;setting instance parameters as per the spec file
	foreach(INST CellView->instances
		(let (N P Nparam Pparam paraN1 paraN2 paraP1 paraP2 paraP3 paraP4 paraP5 paraP6 paraP7 paraP8 paraP9 paraP10)
		when( (INST->name==parameter1) && (INST->cellName=="gate.INV.0-L1_1-R")
		N=evalstring(parameter3)
		Nparam=N/1000000
		P=evalstring(parameter4)
		Pparam=P/1000000
		when((INST->cellName=="gate.INV.0-L1_1-R") && (INST->name=="R.0")
			R0PWidth=P
			R0NWidth=N)
		when((INST->cellName=="gate.INV.0-L1_1-R") && (INST->name=="R.1")
			R1NWidth=N
			R1PWidth=P)
		when((INST->cellName=="gate.INV.0-L1_1-R") && (INST->name=="L.e")
			LeNWidth=N
			LePWidth=P)
		when((INST->cellName=="gate.INV.0-L1_1-R") && (INST->name=="Reset")
			ResPWidth=P)
		when((INST->cellName=="gate.INV.0-L1_1-R") && (INST->name=="_Reset")
			ResNWidth=N)
		dbReplaceProp(INST "NW1" "float" Nparam)
		dbReplaceProp(INST "PW1" "float" Pparam)
		)
		when( (INST->name==parameter1) && (INST->cellName=="gate.NAND3.0-L1_1-R")
		N=evalstring(parameter3)
		Nparam=N/1000000
		P=evalstring(parameter4)
		Pparam=P/1000000
			rvPWidth=P
			rvNWidth=N
		dbReplaceProp(INST "NW3" "float" Nparam)
		dbReplaceProp(INST "PW1" "float" Pparam)
		)
		when( ( (INST->cellName=="stack.NMOS_CHAIN_2.0-L1-R") || (INST->cellName=="stack.PMOS_CHAIN_2.0-L1-R"))
		paraN1 = strcat( parameter1 "." "N1")
		paraN2 = strcat( parameter1 "." "N2")
		paraP1 = strcat( parameter1 "." "P1")
		paraP2 = strcat( parameter1 "." "P2")
		when(((INST->name==paraN1) || (INST->name==paraP1) || (INST->name==paraN2) || (INST->name==paraP2) )
		N=evalstring(parameter3)
		Nparam=N/2000000
		P=evalstring(parameter4)
		Pparam=P/2000000
		when(((INST->cellName=="stack.NMOS_CHAIN_2.0-L1-R") && (INST->name=="_r.0.N1"))
		Nr0Width=N)
		when(((INST->cellName=="stack.NMOS_CHAIN_2.0-L1-R") && (INST->name=="_r.0.P1"))
		Pr0Width=P)
		dbReplaceProp(INST "NW2" "float" Nparam)
		dbReplaceProp(INST "PW2" "float" Pparam)
		))
		)))
; Calculates the XY origin and bBox to move instances w.r.t the new spec
	foreach(INST CellView->instances
		(let (cordNx cordNx1 cordNx2 cordr1Nx cordr1Nx1 cordr1Nx2 cordPx cordPx1 cordPx2 cordr1Px cordr1Px1 cordr1Px2 xy1 xy2 XY bx1 bx2 by1 by2 diff)
		xy1=car(INST~>xy)
		xy2=cadr(INST~>xy)	
		bx1=caar(INST~>bBox)
		by1=cadar(INST~>bBox)
		bx2=caadr(INST~>bBox)
		by2=cadadr(INST~>bBox)
		when(((INST->cellName=="stack.NMOS_CHAIN_2.0-L1-R") && (INST->name=="_r.0.N1"))
			cordNx=caar(INST~>bBox)
			cordNx1=cordNx+0.02
			cordnx1=cordNx1
			cordNx2=cordNx1-0.06
			cordnx2=cordNx2)
		when(((INST->cellName=="stack.NMOS_CHAIN_2.0-L1-R") && (INST->name=="_r.1.N1"))
			cordr1Nx=caar(INST~>bBox)
			cordr1Nx1=cordr1Nx+0.02
			cordr1nx1=cordr1Nx1
			cordr1Nx2=cordr1Nx1-0.06
			cordr1nx2=cordr1Nx2)
		when(((INST->cellName=="stack.PMOS_CHAIN_2.0-L1-R") && (INST->name=="_r.0.P1"))
			cordPx=caadr(INST~>bBox)
			Lpinx=caar(INST~>bBox)
			cordPx1=cordPx-0.02
			cordpx1=cordPx1
			cordPx2=cordPx1+0.06
			cordpx2=cordPx2)
		when(((INST->cellName=="stack.PMOS_CHAIN_2.0-L1-R") && (INST->name=="_r.1.P1"))
			cordr1Px=caadr(INST~>bBox)
			cordr1Px1=cordr1Px-0.02
			cordr1px1=cordr1Px1
			cordr1Px2=cordr1Px1+0.06
			cordr1px2=cordr1Px2)
		when(((INST->cellName=="gate.INV.0-L1_1-R") && (INST->name=="R.0"))
			cordR0x1=caar(INST~>bBox)
			cordR0x2=caadr(INST~>bBox)
			cordR0y1=cadar(INST~>bBox)
			cordR0y2=cadadr(INST~>bBox))
		when(((INST->cellName=="gate.INV.0-L1_1-R") && (INST->name=="R.1"))
			cordR1x1=caar(INST~>bBox)
			cordR1x2=caadr(INST~>bBox)
			cordR1y2=cadadr(INST~>bBox))
		when(((INST->cellName=="gate.INV.0-L1_1-R") && (INST->name=="Reset"))
		when(ResPWidth>=0.21
			dbReplaceProp(INST "p_contact_offset" "float" 0.06))
		when(ResPWidth<0.21
			dbReplaceProp(INST "p_contact_offset" "float" (0.475-0.2-ResPWidth)))
			cordResx2=caadr(INST~>bBox))
		when(((INST->cellName=="gate.INV.0-L1_1-R") && (INST->name=="_Reset"))
		when(ResNWidth>=0.21
			dbReplaceProp(INST "n_contact_offset" "float" 0.06))
		when(ResNWidth<0.21
			dbReplaceProp(INST "n_contact_offset" "float" (0.47-0.2-ResNWidth)))
			cordresx1=caar(INST~>bBox))
		when(((INST->cellName=="gate.NAND3.0-L1_1-R") && (INST->name=="rv"))
			cordrvx1=caar(INST~>bBox)
			cordrvx2=caadr(INST~>bBox)	
			cordrvy2=cadadr(INST~>bBox))
		when(((INST->cellName=="gate.INV.0-L1_1-R") && (INST->name=="L.e"))
			cordLex1=caar(INST~>bBox)
			cordLex2=caadr(INST~>bBox)	
			cordLey2=cadadr(INST~>bBox))
		))

	foreach(INST CellView->instances
		(let (xy1 xy2 XY bx1 bx2 by1 by2 diff)
		xy1=car(INST~>xy)
		xy2=cadr(INST~>xy)	
		bx1=caar(INST~>bBox)
		by1=cadar(INST~>bBox)
		bx2=caadr(INST~>bBox)
		by2=cadadr(INST~>bBox)
		when(((INST->cellName=="gate.INV.0-L1_1-R") && (INST->name=="L.e"))
		when(cordLey2==2.95
		when(cordLex1+0.07>0.495
		dbReplaceProp(INST "n_contact_offset" "float" (0.525-0.2-(LeNWidth/6)))		
		)
		when(cordLex1+0.07<=0.495
		dbReplaceProp(INST "n_contact_offset" "float" 0.06)		
		))
		when(cordLey2==2.82
		when(cordLex1+0.07>0.495
		dbReplaceProp(INST "n_contact_offset" "float" (0.525-0.2-(LeNWidth/5)))		
		)
		when(cordLex1+0.07<=0.495
		dbReplaceProp(INST "n_contact_offset" "float" 0.06)		
		))
		when(cordLey2==2.69
		when(cordLex1+0.07>0.495
		dbReplaceProp(INST "n_contact_offset" "float" (0.525-0.2-(LeNWidth/4)))		
		)
		when(cordLex1+0.07<=0.495
		dbReplaceProp(INST "n_contact_offset" "float" 0.06)		
		))
		when(cordLey2==2.56
		when(cordLex1+0.07>0.495
		dbReplaceProp(INST "n_contact_offset" "float" (0.525-0.2-(LeNWidth/3)))		
		)
		when(cordLex1+0.07<=0.495
		dbReplaceProp(INST "n_contact_offset" "float" 0.06)		
		))
		when(cordLey2==2.43
		when(cordLex1+0.07>0.495
		dbReplaceProp(INST "n_contact_offset" "float" (0.525-0.2-(LeNWidth/2)))		
		)
		when(cordLex1+0.07<=0.495
		dbReplaceProp(INST "n_contact_offset" "float" 0.06)		
		))
		when(cordLey2==2.3
		when(cordLex1+0.07>0.495
		dbReplaceProp(INST "n_contact_offset" "float" (0.525-0.2-LeNWidth))		
		)
		when(cordLex1+0.07<=0.495
		dbReplaceProp(INST "n_contact_offset" "float" 0.06)	
		))
		ActualcordLex1=caar(INST~>bBox))

		when((INST->cellName=="gate.INV.0-L1_1-R") && (INST->name=="R.1")
		when(cordR1y2==6.07
		when((R1PWidth/6)>=0.21
		dbReplaceProp(INST "p_contact_offset" "float" 0.06))		
		when((R1PWidth/6)<0.21
		dbReplaceProp(INST "p_contact_offset" "float" (0.475-0.2-(R1PWidth/6)))))
		when(cordR1y2==5.94
		when((R1PWidth/5)>=0.21
		dbReplaceProp(INST "p_contact_offset" "float" 0.06))		
		when((R1PWidth/5)<0.21
		dbReplaceProp(INST "p_contact_offset" "float" (0.475-0.2-(R1PWidth/5)))))
		when(cordR1y2==5.81
		when((R1PWidth/4)>=0.21
		dbReplaceProp(INST "p_contact_offset" "float" 0.06))		
		when((R1PWidth/4)<0.21
		dbReplaceProp(INST "p_contact_offset" "float" (0.475-0.2-(R1PWidth/4)))))
		when(cordR1y2==5.68
		when((R1PWidth/3)>=0.21
		dbReplaceProp(INST "p_contact_offset" "float" 0.06))		
		when((R1PWidth/3)<0.21
		dbReplaceProp(INST "p_contact_offset" "float" (0.475-0.2-(R1PWidth/3)))))
		when(cordR1y2==5.55
		when((R1PWidth/2)>=0.21
		dbReplaceProp(INST "p_contact_offset" "float" 0.06))		
		when((R1PWidth/2)<0.21
		dbReplaceProp(INST "p_contact_offset" "float" (0.475-0.2-(R1PWidth/2)))))
		when(cordR1y2==5.42
		when(R1PWidth>=0.21
		dbReplaceProp(INST "p_contact_offset" "float" 0.06))		
		when(R1PWidth<0.21
		dbReplaceProp(INST "p_contact_offset" "float" (0.475-0.2-R1PWidth))))
		ActualcordR1x2=caadr(INST~>bBox)
		ActualcordR1x1=caar(INST~>bBox)
		)
		when((INST->cellName=="gate.INV.0-L1_1-R") && (INST->name=="R.0")
		when(cordR0y2==1.13
		when((R0PWidth/6)>=0.21
		dbReplaceProp(INST "p_contact_offset" "float" 0.06))		
		when((R0PWidth/6)<0.21
		dbReplaceProp(INST "p_contact_offset" "float" (0.475-0.2-(R0PWidth/6)))))
		when(cordR0y2==1.0
		when((R0PWidth/5)>=0.21
		dbReplaceProp(INST "p_contact_offset" "float" 0.06))		
		when((R0PWidth/5)<0.21
		dbReplaceProp(INST "p_contact_offset" "float" (0.475-0.2-(R0PWidth/5)))))
		when(cordR0y2==0.87
		when((R0PWidth/4)>=0.21
		dbReplaceProp(INST "p_contact_offset" "float" 0.06))		
		when((R0PWidth/4)<0.21
		dbReplaceProp(INST "p_contact_offset" "float" (0.475-0.2-(R0PWidth/4)))))
		when(cordR0y2==0.74
		when((R0PWidth/3)>=0.21
		dbReplaceProp(INST "p_contact_offset" "float" 0.06))		
		when((R0PWidth/3)<0.21
		dbReplaceProp(INST "p_contact_offset" "float" (0.475-0.2-(R0PWidth/3)))))
		when(cordR0y2==0.61
		when((R0PWidth/2)>=0.21
		dbReplaceProp(INST "p_contact_offset" "float" 0.06))		
		when((R0PWidth/2)<0.21
		dbReplaceProp(INST "p_contact_offset" "float" (0.475-0.2-(R0PWidth/2)))))
		when(cordR0y2==0.48
		when(R0PWidth>=0.21
		dbReplaceProp(INST "p_contact_offset" "float" 0.06))		
		when(R0PWidth<0.21
		dbReplaceProp(INST "p_contact_offset" "float" (0.475-0.2-R0PWidth))))
		ActualcordR0x2=caadr(INST~>bBox)
		ActualcordR0x1=caar(INST~>bBox)
		)
	))

	foreach(INST CellView->instances
		(let (xy1 xy2 XY bx1 bx2 by1 by2 diff)
		xy1=car(INST~>xy)
		xy2=cadr(INST~>xy)	
		bx1=caar(INST~>bBox)
		by1=cadar(INST~>bBox)
		bx2=caadr(INST~>bBox)
		by2=cadadr(INST~>bBox)
		when(((INST->cellName=="gate.NAND3.0-L1_1-R") && (INST->name=="rv"))
		when(cordrvy2==3.86
		when((rvPWidth/2<=0.21)
		dbReplaceProp(INST "p_contact_offset" "float" (0.475-0.2-(rvPWidth/2)))
		)		
		when((rvPWidth/2>0.21)
		dbReplaceProp(INST "p_contact_offset" "float" 0.06)
		)	
		when((cordrvx1+0.07>0.305)&&(ActualcordLex1>cordrvx1+0.13)
		dbReplaceProp(INST "n_contact_offset" "float" (0.7-0.2-(rvNWidth/2)))		
		)
		when((cordrvx1+0.07>0.305)&&(ActualcordLex1<=cordrvx1+0.13)
		dbReplaceProp(INST "n_contact_offset" "float" (abs(ActualcordLex1-0.17-0.95)-0.2-(rvNWidth/2)))		
		)
		when((cordrvx1+0.07<=0.305)&&(ActualcordLex1>cordrvx1+0.13)
		dbReplaceProp(INST "n_contact_offset" "float" 0.06)		
		)
		when((cordrvx1+0.07<=0.305)&&(ActualcordLex1<=cordrvx1+0.13)
		dbReplaceProp(INST "n_contact_offset" "float" (abs(ActualcordLex1-0.17-0.95)-0.2-(rvNWidth/2)))		
		))
		when(cordrvy2==3.47
		when((rvPWidth<=0.21)
		dbReplaceProp(INST "p_contact_offset" "float" (0.475-0.2-rvPWidth))
		)		
		when((rvPWidth>0.21)
		dbReplaceProp(INST "p_contact_offset" "float" 0.06)
		)	
		when((cordrvx1+0.07>0.305)&&(ActualcordLex1>cordrvx1+0.13)
		dbReplaceProp(INST "n_contact_offset" "float" (0.7-0.2-rvNWidth))		
		)
		when((cordrvx1+0.07>0.305)&&(ActualcordLex1<=cordrvx1+0.13)
		dbReplaceProp(INST "n_contact_offset" "float" (abs(ActualcordLex1-0.17-0.95)-0.2-rvNWidth))		
		)
		when((cordrvx1+0.07<=0.305)&&(ActualcordLex1>cordrvx1+0.13)
		dbReplaceProp(INST "n_contact_offset" "float" 0.06)
		)
		when((cordrvx1+0.07<=0.305)&&(ActualcordLex1<=cordrvx1+0.13)
		dbReplaceProp(INST "n_contact_offset" "float" (abs(ActualcordLex1-0.17-0.95)-0.2-rvNWidth))			
		))
		Actualcordrvx1=caar(INST~>bBox)
		Actualcordrvx2=caadr(INST~>bBox))
		when((INST~>cellName=="M1_POLYmin")&&(((xy2==1.235)&&(xy1==0.11))||((xy2==1.495)&&(xy1==0.11))||((xy2==1.755)&&(xy1==0.11))||((xy2==4.355)&&(xy1==0.11))||((xy2==4.615)&&(xy1==0.11))||((xy2==5.005)&&(xy1==0.11)))
		when(Nr0Width<=0.61
		when((ActualcordLex1+0.07)>0.435
		XY=(xy1+0.04:xy2))
		when((ActualcordLex1+0.07)<=0.435
		XY=(ActualcordLex1-0.215:xy2)))
		when(Nr0Width>0.61
		when(((ActualcordLex1+0.07)>0.435)&&(cordnx1+0.06>=xy1+0.1)
		XY=(xy1+0.04:xy2))
		when(((ActualcordLex1+0.07)>0.435)&&(cordnx1+0.06<xy1+0.1)
		XY=(cordnx1-0.02:xy2))
		when(((ActualcordLex1+0.07)<=0.435)&&(cordnx1+0.06>=ActualcordLex1-0.155)
		XY=(ActualcordLex1-0.215:xy2))
		when(((ActualcordLex1+0.07)<=0.435)&&(cordnx1+0.06<ActualcordLex1-0.155)
		XY=(cordnx1-0.02:xy2)))
		actualleftPoly=car(XY)
		dbSet(INST XY "xy")) ; moves the M1_Polymin vias on the left side of the leaf cell
	))
	foreach(INST CellView->instances
		(let (xy1 xy2 XY bx1 bx2 by1 by2 diff)
		xy1=car(INST~>xy)
		xy2=cadr(INST~>xy)	
		bx1=caar(INST~>bBox)
		by1=cadar(INST~>bBox)
		bx2=caadr(INST~>bBox)
		by2=cadadr(INST~>bBox)
		when((INST~>cellName=="M1_POLYmin")&&(((xy2==1.235)&&(xy1==actualleftPoly))||((xy2==1.495)&&(xy1==actualleftPoly))||((xy2==1.755)&&(xy1==actualleftPoly))||((xy2==4.355)&&(xy1==actualleftPoly))||((xy2==4.615)&&(xy1==actualleftPoly))||((xy2==5.005)&&(xy1==actualleftPoly)))&&(ActualcordR1x1<=actualleftPoly)&&(ActualcordR1x1<=ActualcordR0x1)
		XY=(ActualcordR1x1-0.02:xy2)
		ActualleftPoly=car(XY)
		dbSet(INST XY "xy"))
		when((INST~>cellName=="M1_POLYmin")&&(((xy2==1.235)&&(xy1==actualleftPoly))||((xy2==1.495)&&(xy1==actualleftPoly))||((xy2==1.755)&&(xy1==actualleftPoly))||((xy2==4.355)&&(xy1==actualleftPoly))||((xy2==4.615)&&(xy1==actualleftPoly))||((xy2==5.005)&&(xy1==actualleftPoly)))&&(ActualcordR0x1<=actualleftPoly)&&(ActualcordR0x1<=ActualcordR1x1)
		XY=(ActualcordR0x1-0.02:xy2)
		ActualleftPoly=car(XY)
		dbSet(INST XY "xy"))
		when((INST~>cellName=="M1_POLYmin")&&(((xy2==1.235)&&(xy1==actualleftPoly))||((xy2==1.495)&&(xy1==actualleftPoly))||((xy2==1.755)&&(xy1==actualleftPoly))||((xy2==4.355)&&(xy1==actualleftPoly))||((xy2==4.615)&&(xy1==actualleftPoly))||((xy2==5.005)&&(xy1==actualleftPoly)))&&(ActualcordR0x1>actualleftPoly)&&(ActualcordR1x1>actualleftPoly)
		ActualleftPoly=actualleftPoly)
		when((INST~>cellName=="M1_POLYmin")&&(((xy2==1.625)&&(xy1==2.18))||((xy2==1.885)&&(xy1==2.18))||((xy2==4.485)&&(xy1==2.18))||((xy2==4.745)&&(xy1==2.18)))
		when(((cordpx1-0.06)<=1.565)&&((Actualcordrvx2-0.2)<=1.565)
		XY=(cordResx2+0.37:xy2))
		when(((cordpx1-0.06)>1.565)&&((Actualcordrvx2-0.2)<=1.565)
		XY=(cordpx1+0.02:xy2))
		when(((cordpx1-0.06)<=(Actualcordrvx2-0.2))&&((Actualcordrvx2-0.2)>1.565)
		XY=(Actualcordrvx2+0.06:xy2))
		when(((cordpx1-0.06)>(Actualcordrvx2-0.2))&&((Actualcordrvx2-0.2)>1.565)
		XY=(cordpx1+0.02:xy2))
		ActualrightPoly=car(XY)
		dbSet(INST XY "xy")) ; moves the M1_Polymin vias on the right side of the leaf cell
	))

	foreach(INST CellView->instances
		(let (xy1 xy2 XY bx1 bx2 by1 by2 diff)
		xy1=car(INST~>xy)
		xy2=cadr(INST~>xy)	
		bx1=caar(INST~>bBox)
		by1=cadar(INST~>bBox)
		bx2=caadr(INST~>bBox)
		by2=cadadr(INST~>bBox)
		when((INST->cellName=="stack.PPLUG.0")&&(xy1==2.125&&xy2==2.86)
		XY=((ActualrightPoly-0.055):xy2)	
		dbSet(INST XY "xy"))
		when((INST->cellName=="stack.NPLUG.0")&&(xy1==0.15&&xy2==3.935)
		XY=((ActualleftPoly+0.045):xy2)	
		dbSet(INST XY "xy"))
		when((INST~>cellName=="M2_M1_2cut_p1_2")&&((xy1==0.755&&xy2==5.98)||(xy1==1.145&&xy2==5.98)||(xy1==0.755&&xy2==5.72)||(xy1==1.145&&xy2==5.72)||(xy1==0.755&&xy2==5.46)||(xy1==1.145&&xy2==5.46))
		when(((cordR1y2==5.94)||(cordR1y2==5.81))&&((xy1==0.755&&xy2==5.98)||(xy1==1.145&&xy2==5.98))
		dbDeleteObject(INST))
		when(((cordR1y2==5.68)||(cordR1y2==5.55))&&((xy1==0.755&&xy2==5.98)||(xy1==1.145&&xy2==5.98)||(xy1==0.755&&xy2==5.72)||(xy1==1.145&&xy2==5.72))
		dbDeleteObject(INST))
		when((cordR1y2==5.42)&&((xy1==0.755&&xy2==5.98)||(xy1==1.145&&xy2==5.98)||(xy1==0.755&&xy2==5.72)||(xy1==1.145&&xy2==5.72)||(xy1==0.755&&xy2==5.46)||(xy1==1.145&&xy2==5.46))
		dbDeleteObject(INST))
		)
		when((INST~>cellName=="M2_M1_2cut_p1_2")&&((xy1==0.755&&xy2==1.04)||(xy1==1.145&&xy2==1.04)||(xy1==0.755&&xy2==0.78)||(xy1==1.145&&xy2==0.78)||(xy1==0.755&&xy2==0.52)||(xy1==1.145&&xy2==0.52))
		when(((cordR0y2==1.0)||(cordR0y2==0.87))&&((xy1==0.755&&xy2==1.04)||(xy1==1.145&&xy2==1.04))
		dbDeleteObject(INST))
		when(((cordR0y2==0.74)||(cordR0y2==0.61))&&((xy1==0.755&&xy2==1.04)||(xy1==1.145&&xy2==1.04)||(xy1==0.755&&xy2==0.78)||(xy1==1.145&&xy2==0.78))
		dbDeleteObject(INST))
		when((cordR0y2==0.48)&&((xy1==0.755&&xy2==1.04)||(xy1==1.145&&xy2==1.04)||(xy1==0.755&&xy2==0.78)||(xy1==1.145&&xy2==0.78)||(xy1==0.755&&xy2==0.52)||(xy1==1.145&&xy2==0.52))
		dbDeleteObject(INST))
		)
		when((INST~>cellName=="M2_M1_2cut_p1_2")&&((xy1==1.145&&xy2==3.51)||(xy1==1.145&&xy2==3.77))
		when((cordrvy2==3.47)
		dbDeleteObject(INST)))
		when((INST~>cellName=="M2_M1_2cut_p1_2")&&((xy1==0.755&&xy2==2.86)||(xy1==0.755&&xy2==2.6)||(xy1==1.145&&xy2==2.6)||(xy1==0.755&&xy2==2.34)||(xy1==1.145&&xy2==2.34))
		when(((cordLey2==2.82)||(cordLey2==2.69))&&(xy1==0.755&&xy2==2.86)
		dbDeleteObject(INST))
		when(((cordLey2==2.56)||(cordLey2==2.43))&&((xy1==0.755&&xy2==2.86)||(xy1==0.755&&xy2==2.6)||(xy1==1.145&&xy2==2.6))
		dbDeleteObject(INST))
		when((cordLey2==2.3)&&((xy1==0.755&&xy2==2.86)||(xy1==0.755&&xy2==2.6)||(xy1==1.145&&xy2==2.6)||(xy1==0.755&&xy2==2.34)||(xy1==1.145&&xy2==2.34))
		dbDeleteObject(INST)))
	))

	foreach(SHAPE CellView->shapes
		(let (bx1 bx2 by1 by2 x1 x2 x3 x4 x5 x6 x7 x8 y1 y2 y3 y4 y5 y6 y7 y8 xy1 xy2 pnts bbox orgn)
		bx1=caar(SHAPE~>bBox)
		by1=cadar(SHAPE~>bBox)
		bx2=caadr(SHAPE~>bBox)
		by2=cadadr(SHAPE~>bBox)
		xy1=car(SHAPE~>xy)
		xy2=cadr(SHAPE~>xy)	
		x1=caar(SHAPE~>points)
		y1=cadar(SHAPE~>points)
		x2=caadr(SHAPE~>points)
		y2=cadadr(SHAPE~>points)
		x3=caaddr(SHAPE~>points)
		y3=car(cdaddr(SHAPE~>points))
		x4=car(cadddr(SHAPE~>points))
		y4=cadr(cadddr(SHAPE~>points))
		x5=caar(cddddr(SHAPE~>points))
		y5=cadar(cddddr(SHAPE~>points))
		x6=caadr(cddddr(SHAPE~>points))
		y6=cadadr(cddddr(SHAPE~>points))
		x7=caaddr(cddddr(SHAPE~>points))
		y7=car(cdaddr(cddddr(SHAPE~>points)))
		x8=car(cadddr(cddddr(SHAPE~>points)))
		y8=cadr(cadddr(cddddr(SHAPE~>points)))
		when((SHAPE~>layerName=="M1")&&((bx1==0.21&&by1==1.92)||(bx1==0.21&&by1==1.4))
		bbox=list((cordnx1+0.06):by1 bx2:by2)		
		dbSet(SHAPE bbox "bBox")) ; moves the M1 layer on the GND terminals of the C2 gates on the NMOS side for r.o
		when((SHAPE~>layerName=="M1")&&((bx1==0.21&&by1==4.78)||(bx1==0.21&&by1==4.26))
		bbox=list((cordr1nx1+0.06):by1 bx2:by2)		
		dbSet(SHAPE bbox "bBox")) ; moves the M1 layer on the GND terminals of the C2 gates on the NMOS side for r.1
		when((SHAPE~>layerName=="M1")&&((bx1==1.04&&by1==1.92)||(bx1==1.04&&by1==1.4))
		bbox=list(bx1:by1 cordpx1-0.06:by2)		
		dbSet(SHAPE bbox "bBox")) ; moves the M1 layer on the Vdd terminals of the C2 gates on the PMOS side for r.o
		when((SHAPE~>layerName=="M1")&&((bx1==1.04&&by1==4.78)||(bx1==1.04&&by1==4.26))
		bbox=list(bx1:by1 cordr1px1-0.06:by2)		
		dbSet(SHAPE bbox "bBox")) ; moves the M1 layer on the Vdd terminals of the C2 gates on the PMOS side for r.1
		when((SHAPE~>layerName=="CO")&&((bx1==0.79&&by1==4.66)||(bx1==0.68&&by1==4.66)||(bx1==0.57&&by1==4.66)||(bx1==0.46&&by1==4.66)||(bx1==0.35&&by1==4.66)||(bx1==0.24&&by1==4.66)||(bx1==0.79&&by1==1.54)||(bx1==0.68&&by1==1.54)||(bx1==0.57&&by1==1.54)||(bx1==0.46&&by1==1.54)||(bx1==0.35&&by1==1.54)||(bx1==0.24&&by1==1.54))
		dbDeleteObject(SHAPE)
		i = 0.83
		for(k 0 20
		dbCreateRect(CellView "CO" list(i:by1 i-0.04:by2))
		when((i-0.15)<(cordnx1+0.07)
		k=20
		i = 0.94)
		when((i-0.15)>=(cordnx1+0.07)
		k = k+1
		i = i - 0.11)))
		when((SHAPE~>layerName=="CO")&&((bx1==0.79&&by1==4.4)||(bx1==0.68&&by1==4.4)||(bx1==0.57&&by1==4.4)||(bx1==0.46&&by1==4.4)||(bx1==0.35&&by1==4.4)||(bx1==0.24&&by1==4.4)||(bx1==0.79&&by1==1.8)||(bx1==0.68&&by1==1.8)||(bx1==0.57&&by1==1.8)||(bx1==0.46&&by1==1.8)||(bx1==0.35&&by1==1.8)||(bx1==0.24&&by1==1.8))
		dbDeleteObject(SHAPE)
		i = 0.83
		for(k 0 20
		dbCreateRect(CellView "CO" list(i:by1 i-0.04:by2))
		when((i-0.15)<(cordnx1+0.07)
		k=20
		i = 0.94)
		when((i-0.15)>=(cordnx1+0.07)
		k = k+1
		i = i - 0.11)))
		when((SHAPE~>layerName=="CO")&&((bx1==1.07&&by1==4.66)||(bx1==1.18&&by1==4.66)||(bx1==1.29&&by1==4.66)||(bx1==1.4&&by1==4.66)||(bx1==1.51&&by1==4.66)||(bx1==1.62&&by1==4.66)||(bx1==1.73&&by1==4.66)||(bx1==1.84&&by1==4.66)||(bx1==1.07&&by1==1.54)||(bx1==1.18&&by1==1.54)||(bx1==1.29&&by1==1.54)||(bx1==1.4&&by1==1.54)||(bx1==1.51&&by1==1.54)||(bx1==1.62&&by1==1.54)||(bx1==1.73&&by1==1.54)||(bx1==1.84&&by1==1.54))
		dbDeleteObject(SHAPE)
		i = 1.07
		for(k 0 20
		dbCreateRect(CellView "CO" list(i:by1 i+0.04:by2))
		when((i+0.15)>(cordpx1-0.07)
		k=20
		i = 1.07)
		when((i+0.15)<=(cordpx1-0.07)
		k = k+1
		i = i + 0.11)))
		when((SHAPE~>layerName=="CO")&&((bx1==1.07&&by1==4.4)||(bx1==1.18&&by1==4.4)||(bx1==1.29&&by1==4.4)||(bx1==1.4&&by1==4.4)||(bx1==1.51&&by1==4.4)||(bx1==1.62&&by1==4.4)||(bx1==1.73&&by1==4.4)||(bx1==1.84&&by1==4.4)||(bx1==1.07&&by1==1.8)||(bx1==1.18&&by1==1.8)||(bx1==1.29&&by1==1.8)||(bx1==1.4&&by1==1.8)||(bx1==1.51&&by1==1.8)||(bx1==1.62&&by1==1.8)||(bx1==1.73&&by1==1.8)||(bx1==1.84&&by1==1.8))
		dbDeleteObject(SHAPE)
		i = 1.07
		for(k 0 20
		dbCreateRect(CellView "CO" list(i:by1 i+0.04:by2))
		when((i+0.15)>(cordpx1-0.07)
		k=20
		i = 1.07)
		when((i+0.15)<=(cordpx1-0.07)
		k = k+1
		i = i + 0.11)))
		when((SHAPE~>layerName=="OD")&&((bx1==1.06&&by1==1.795)||(bx1==1.06&&by1==1.535))
		bbox=list(bx1:by1 cordpx1-0.06:by2)	
		dbSet(SHAPE bbox "bBox")) ;for r.o
		when((SHAPE~>layerName=="OD")&&((bx1==1.06&&by1==4.655)||(bx1==1.06&&by1==4.395))
		bbox=list(bx1:by1 cordr1px1-0.06:by2)	
		dbSet(SHAPE bbox "bBox")) ; for r.1
		when((SHAPE~>layerName=="OD")&&((bx1==0.23&&by1==1.795)||(bx1==0.23&&by1==1.535))
		bbox=list(cordnx1+0.06:by1 bx2:by2)	
		dbSet(SHAPE bbox "bBox")) ;for r.0
		when((SHAPE~>layerName=="OD")&&((bx1==0.23&&by1==4.395)||(bx1==0.23&&by1==4.655))
		bbox=list(cordr1nx1+0.06:by1 bx2:by2)	
		dbSet(SHAPE bbox "bBox")) ;for r.1
		when((SHAPE~>layerName=="M1")&&bx1==0.21&&by1==1.53
		when((cordnx1+0.06<=0.38)
		bbox=list(cordnx1+0.06:by1 bx2:by2))		
		when((cordnx1+0.06>0.38)
		bbox=list(0.38:by1 bx2:by2))	
		dbSet(SHAPE bbox "bBox")) ; moves the M1 layer on the terminals connecting the internal nets of the C2 gates on the NMOS side for r.0
		when((SHAPE~>layerName=="M1")&&bx1==0.21&&by1==1.79
		when((cordnx1+0.06<=0.38)
		bbox=list(cordnx1+0.06:by1 bx2:by2))		
		when((cordnx1+0.06>0.38)
		bbox=list(0.51:by1 bx2:by2))	
		dbSet(SHAPE bbox "bBox")) ; moves the M1 layer on the terminals connecting the internal nets of the C2 gates on the NMOS side for r.0
		when((SHAPE~>layerName=="M1")&&bx1==0.21&&by1==4.65
		when((cordr1nx1+0.06<=0.38)
		bbox=list(cordr1nx1+0.06:by1 bx2:by2))		
		when((cordr1nx1+0.06>0.38)
		bbox=list(0.38:by1 bx2:by2))	
		dbSet(SHAPE bbox "bBox")) ; moves the M1 layer on the terminals connecting the internal nets of the C2 gates on the NMOS side for r.1
		when((SHAPE~>layerName=="M1")&&bx1==0.21&&by1==4.39
		when((cordr1nx1+0.06<=0.38)
		bbox=list(cordr1nx1+0.06:by1 bx2:by2))		
		when((cordr1nx1+0.06>0.38)
		bbox=list(0.51:by1 bx2:by2))	
		dbSet(SHAPE bbox "bBox")) ; moves the M1 layer on the terminals connecting the internal nets of the C2 gates on the NMOS side for r.1

		when((SHAPE~>layerName=="M1")&&((bx1==1.04&&by1==4.65)||(bx1==1.04&&by1==4.39)||(bx1==1.06&&by1==1.535)||(bx1==1.06&&by1==4.655))
		when((SHAPE~>layerName=="CO")&&((bx1==1.07&&by1==4.66)||(bx1==1.18&&by1==4.66)||(bx1==1.29&&by1==4.66)||(bx1==1.4&&by1==4.66)||(bx1==1.51&&by1==4.66)||(bx1==1.62&&by1==4.66)||(bx1==1.73&&by1==4.66)||(bx1==1.84&&by1==4.66)||(bx1==1.07&&by1==1.54)||(bx1==1.18&&by1==1.54)||(bx1==1.29&&by1==1.54)||(bx1==1.4&&by1==1.54)||(bx1==1.51&&by1==1.54)||(bx1==1.62&&by1==1.54)||(bx1==1.73&&by1==1.54)||(bx1==1.84&&by1==1.54))
		dbDeleteObject(SHAPE))
		when(bx1==1.04&&by1==4.39
		when((cordr1px1-0.06>=1.52)
		bbox=list(bx1:by1 cordr1px1-0.06:by2))		
		when((cordr1px1-0.06<1.52)
		bbox=list(bx1:by1 1.39:by2)))	
		when(bx1==1.04&&by1==4.65
		when((cordr1px1-0.06>=1.52)
		bbox=list(bx1:by1 cordr1px1-0.06:by2))		
		when((cordr1px1-0.06<1.52)
		bbox=list(bx1:by1 1.52:by2)))
		dbSet(SHAPE bbox "bBox")) ; moves the M1 layer on the terminals connecting the internal nets of the C2 gates on the PMOS side
		when((SHAPE~>layerName=="M1")&&bx1==1.04&&by1==1.79
		when((cordpx1-0.06>=1.52)
		bbox=list(bx1:by1 cordpx1-0.06:by2))		
		when((cordpx1-0.06<1.52)
		bbox=list(bx1:by1 1.39:by2))	
		dbSet(SHAPE bbox "bBox")) ; moves the M1 layer on the terminals connecting the internal nets of the C2 gates on the PMOS side
		when((SHAPE~>layerName=="M1")&&bx1==1.04&&by1==1.53
		when((cordpx1-0.06>=1.52)
		bbox=list(bx1:by1 cordpx1-0.06:by2))		
		when((cordpx1-0.06<1.52)
		bbox=list(bx1:by1 1.52:by2))	
		dbSet(SHAPE bbox "bBox")) ; moves the M1 layer on the terminals connecting the internal nets of the C2 gates on the PMOS side
		when((SHAPE~>layerName=="M1")&&bx1==0.255&&by1==0.11
		when((ActualcordLex1+0.07)>0.435
		pnts=list(ActualcordR0x2-0.07:y1 ActualcordR0x2-0.07:y2 x3:y3))
		when((ActualcordLex1+0.07)<=0.435
		pnts=list(ActualcordR0x2-0.07:y1 ActualcordR0x2-0.07:y2 ActualcordLex1-0.06:y3))
		dbSet(SHAPE pnts "points")) ; moves the M1 connecting the _r.0 input from M2 to the input of R.0 INV gate
		when((SHAPE~>layerName=="M1")&&bx1==0.555&&by1==1.66
		when((ActualcordLex1+0.07)>0.435
		bbox=list(0.305:by1 cordpx1-0.06:by2))
		when((ActualcordLex1+0.07)<=0.435
		bbox=list(ActualcordLex1-0.06:by1 cordpx1-0.06:by2))
		dbSet(SHAPE bbox "bBox")) ; moves the M1 output of _r.0 in C2 stat
		when((SHAPE~>layerName=="M2")&&bx1==0.465&&by1==2.115
		pnts=list(ActualcordLex1+0.07:y1 ActualcordLex1+0.07:y2)	
		dbSet(SHAPE pnts "points"))	; moves the rv input on M2 on left side
		when((SHAPE~>layerName=="M1")&&bx1==0.43&&by1==3.35
		pnts=list(ActualcordLex1+0.07:y1 0.95:y2)	
		dbSet(SHAPE pnts "points"))
		when((SHAPE~>layerName=="M2")&&bx1==0.275&&by1==0.135
		when((ActualcordLex1+0.07)>0.435
		pnts=list(x1:y1 x2:y2))
		when((ActualcordLex1+0.07)<=0.435
		pnts=list(ActualcordLex1-0.06:y1 ActualcordLex1-0.06:y2)) ; moves the _r.0 input on the left side on M2
		dbSet(SHAPE pnts "points"))
		when((SHAPE~>layerName=="M1")&&bx1==0.195&&by1==3.155
		when((ActualcordLex1+0.07)>0.435
		pnts=list(Actualcordrvx1+0.06:y1 x2:y2))
		when((ActualcordLex1+0.07)<=0.435
		pnts=list(Actualcordrvx1+0.06:y1 ActualcordLex1-0.06:y2))
		when(cordrvy2==3.47
		dbCreatePath(CellView "M1" list(Actualcordrvx1+0.06:y1 0.82:y2) 0.06))
		dbSet(SHAPE pnts "points"))
		when((SHAPE~>layerName=="M1")&&((bx1==0.09&&by1==0.36)||(bx1==0.09&&by1==4.995))
		pnts=list(x1:y1 ActualleftPoly:y2 ActualleftPoly:y3)
		dbSet(SHAPE pnts "points"))
		when((SHAPE~>layerName=="M1")&&((bx1==0.08&&by1==1.465)||(bx1==0.08&&by1==4.325))
		pnts=list(ActualleftPoly:y1 ActualleftPoly:y2)
		dbSet(SHAPE pnts "points"))
		when((SHAPE~>layerName=="M2")&&((bx1==0.08&&by1==0.27)||(bx1==0.08&&by1==4.325))
		pnts=list(ActualleftPoly:y1 ActualleftPoly:y2)
		dbSet(SHAPE pnts "points"))
		when((SHAPE~>layerName=="PO")&&((bx1==0.055&&by1==1.22)||(bx1==0.11&&by1==1.48)||(bx1==0.11&&by1==1.74)||(bx1==0.11&&by1==4.34)||(bx1==0.11&&by1==4.6)||(bx1==0.11&&by1==4.99))
		pnts=list(ActualleftPoly:y1 x2:y2)
		dbSet(SHAPE pnts "points"))
		when((SHAPE~>layerName=="M1")&&bx1==0.53&&by1==3.98
		pnts=list(x1:y1 cordresx1+0.07:y2 cordresx1+0.07:y3)	
		dbSet(SHAPE pnts "points")) ; moves the M1 connecting the Reset input to the _Reset INV gate
		when((SHAPE~>layerName=="M2")&&bx1==1.435&&by1==3.4
		pnts=list(cordResx2-0.055:y1 cordResx2-0.055:y2)	
		dbSet(SHAPE pnts "points")) ; moves the M2 wire connecting to the M3 _RESET pin
		when((SHAPE~>layerName=="M1")&&bx1==1.365&&by1==4.065
		pnts=list(cordResx2-0.055:y1 x2:y2)	
		dbSet(SHAPE pnts "points"))
		when((SHAPE~>layerName=="M2")&&bx1==1.565&&by1==3.03
		when((Actualcordrvx2-0.2)<=1.565
		pnts=list(cordResx2+0.075:y1 cordResx2+0.075:y2))	
		when((Actualcordrvx2-0.2)>1.565
		pnts=list(Actualcordrvx2-0.2:y1 Actualcordrvx2-0.2:y2))
		dbSet(SHAPE pnts "points")) ; moves the M2 wire connecting the _Reset input to the NAND3 rv gate
		when((SHAPE~>layerName=="M1")&&bx1==0.82&&by1==3.87
		when((Actualcordrvx2-0.2)<=1.565
		pnts=list(x1:y1 cordResx2+0.075:y2))
		when((Actualcordrvx2-0.2)>1.565
		pnts=list(x1:y1 Actualcordrvx2-0.2:y2))	
		dbSet(SHAPE pnts "points")); moves the M1 wire connecting the _Reset input 
		when((SHAPE~>layerName=="M1")&&bx1==1.33&&by1==3.025
		when(cordrvy2==3.86
		when((Actualcordrvx2-0.2)<=1.565
		pnts=list(Actualcordrvx2-0.2:y1 cordResx2+0.075:y2))
		when((Actualcordrvx2-0.2)>1.565
		pnts=list(Actualcordrvx2-0.17:y1 Actualcordrvx2-0.23:y2)))
		when(cordrvy2==3.47
		when((Actualcordrvx2-0.2)<=1.565
		pnts=list(Actualcordrvx2-0.07:y1 cordResx2+0.075:y2))
		when((Actualcordrvx2-0.2)>1.565
		pnts=list(Actualcordrvx2-0.04:y1 Actualcordrvx2-0.23:y2)))
		dbSet(SHAPE pnts "points")); moves the M1 wire connecting the _Reset input 
		when((SHAPE~>layerName=="M2")&&bx1==1.71&&by1==3.3
		when((Actualcordrvx2-0.2)<=1.565
		pnts=list(cordResx2+0.205:y1 cordResx2+0.205:y2))	
		when((Actualcordrvx2-0.2)>1.565
		pnts=list(Actualcordrvx2-0.07:y1 Actualcordrvx2-0.07:y2))
		dbSet(SHAPE pnts "points")) ; moves the M2 wire connecting the _r.1 input to the NAND3 rv gate
		when((SHAPE~>layerName=="M1")&&bx1==1.495&&by1==3.285
		when((Actualcordrvx2-0.2)<=1.565
		pnts=list(Actualcordrvx2-0.07:y1 cordResx2+0.205:y2))
		when((Actualcordrvx2-0.2)>1.565
		pnts=list(Actualcordrvx2-0.04:y1 Actualcordrvx2-0.1:y2))
		dbSet(SHAPE pnts "points"))
		when((SHAPE~>layerName=="M1")&&bx1==1.74&&by1==5.25
		when((Actualcordrvx2-0.2)<=1.565
		pnts=list(cordResx2+0.205:y1 ActualcordR1x2-0.07:y2 ActualcordR1x2-0.07:y3))
		when((Actualcordrvx2-0.2)>1.565
		pnts=list(Actualcordrvx2-0.07:y1 ActualcordR1x2-0.07:y2 ActualcordR1x2-0.07:y3))
		dbSet(SHAPE pnts "points"))
		when((SHAPE~>layerName=="M1")&&bx1==0.765&&by1==4.52
		when((Actualcordrvx2-0.2)<=1.565
		pnts=list(x1:y1 cordResx2+0.205:y2))
		when((Actualcordrvx2-0.2)>1.565
		pnts=list(x1:y1 Actualcordrvx2-0.07:y2))
		dbSet(SHAPE pnts "points"))
		when((SHAPE~>layerName=="PO")&&((bx1==0.95&&by1==1.61)||(bx1==0.95&&by1==1.87)||(bx1==0.95&&by1==4.47)||(bx1==0.95&&by1==4.73))
		pnts=list(ActualrightPoly:y1 x2:y2)
		dbSet(SHAPE pnts "points"))
		when((SHAPE~>layerName=="M2")&&bx1==2.15&&by1==1.595
		pnts=list(ActualrightPoly:y1 ActualrightPoly:y2)
		dbSet(SHAPE pnts "points"))
		when((SHAPE~>layerName=="M1")&&((bx1==2.15&&by1==1.595)||(bx1==2.15&&by1==4.455))
		pnts=list(ActualrightPoly:y1 ActualrightPoly:y2)
		dbSet(SHAPE pnts "points"))
		when((SHAPE~>layerName=="M1")&&bx1==0.12&&by1==3.74
		pnts=list(x1:y1 ActualleftPoly+0.045:y2)
		dbSet(SHAPE pnts "points"))
		when((SHAPE~>layerName=="M1")&&bx1==1.04&&by1==2.83
		pnts=list(x1:y1 ActualrightPoly-0.055:y2 ActualrightPoly-0.055:y3)
		dbSet(SHAPE pnts "points"))
		when((SHAPE~>layerName=="PP")&&bx1==0&&by1==3.41
		bbox=list(ActualleftPoly-0.105:by1 ActualleftPoly-0.025:by2)
		dbSet(SHAPE bbox "bBox"))
		when((SHAPE~>layerName=="NP")&&bx1==2.23&&by1==2.86
		bbox=list(ActualrightPoly+0.11:by1 ActualrightPoly+0.05:by2)
		dbSet(SHAPE bbox "bBox"))
		when((SHAPE~>layerName=="PO")&&bx2==0.04&&by2==6.24
		bbox=list(ActualleftPoly-0.07:by1 ActualleftPoly-0.15:by2)
		dbSet(SHAPE bbox "bBox")) ;moves the cut_poly on the left side of the leaf cell
		when((SHAPE~>layerName=="PO")&&bx2==2.33&&by2==6.24
		bbox=list(ActualrightPoly+0.07:by1 ActualrightPoly+0.15:by2)
		dbSet(SHAPE bbox "bBox")) ;moves the cut_poly on the right side of the leaf cell
		when((SHAPE~>layerName=="PO")&&((bx1==-0.005&&by1==0.05)||(bx1==-0.005&&by1==0.18)||(bx1==-0.005&&by1==0.31)||(bx1==-0.005&&by1==0.44)||(bx1==-0.005&&by1==0.57)||(bx1==-0.005&&by1==0.7)||(bx1==-0.005&&by1==0.83)||(bx1==-0.005&&by1==0.96)||(bx1==-0.005&&by1==1.09)||(bx1==-0.005&&by1==1.22)||(bx1==-0.005&&by1==1.35)||(bx1==-0.005&&by1==1.48)||(bx1==-0.005&&by1==1.61)||(bx1==-0.005&&by1==1.74)||(bx1==-0.005&&by1==1.87)||(bx1==-0.005&&by1==2)||(bx1==-0.005&&by1==2.13)||(bx1==-0.005&&by1==2.26)||(bx1==-0.005&&by1==2.39)||(bx1==-0.005&&by1==2.52)||(bx1==-0.005&&by1==2.65)||(bx1==-0.005&&by1==2.78)||(bx1==-0.005&&by1==3.17)||(bx1==-0.005&&by1==3.3)||(bx1==-0.005&&by1==3.43)||(bx1==-0.005&&by1==3.56)||(bx1==-0.005&&by1==3.95)||(bx1==-0.005&&by1==4.08)||(bx1==-0.005&&by1==4.21)||(bx1==-0.005&&by1==4.34)||(bx1==-0.005&&by1==4.47)||(bx1==-0.005&&by1==4.6)||(bx1==-0.005&&by1==4.73)||(bx1==-0.005&&by1==4.86)||(bx1==-0.005&&by1==4.99)||(bx1==-0.005&&by1==5.12)||(bx1==-0.005&&by1==5.25)||(bx1==-0.005&&by1==5.38)||(bx1==-0.005&&by1==5.51)||(bx1==-0.005&&by1==5.64)||(bx1==-0.005&&by1==5.77)||(bx1==-0.005&&by1==5.9)||(bx1==-0.005&&by1==6.03)||(bx1==-0.005&&by1==6.16))
		bbox=list(ActualleftPoly-0.115:by1 ActualrightPoly+0.115:by2)
		dbSet(SHAPE bbox "bBox")) ; moves the layout grid polys
		when((SHAPE~>layerName=="M3")&&((bx1==0&&by1==0.025)||(bx1==0&&by1==1.465)||(bx1==0&&by1==1.585)||(bx1==0&&by1==3.025)||(bx1==0&&by1==3.145)||(bx1==0&&by1==4.585)||(bx1==0&&by1==4.705)||(bx1==0&&by1==6.145))
		when((ActualleftPoly-0.07<=ActualcordR1x1)&&(ActualleftPoly-0.07<=ActualcordR0x1)&&(ActualrightPoly+0.07>=ActualcordR0x2)&&(ActualrightPoly+0.07>=ActualcordR1x2)
		bbox=list(ActualleftPoly-0.11:by1 ActualrightPoly+0.11:by2)
		LeftLeaf=ActualleftPoly-0.11
		RightLeaf=(ActualrightPoly+0.11)-(ActualleftPoly-0.11))
		when((ActualleftPoly-0.07<=ActualcordR1x1)&&(ActualleftPoly-0.07>ActualcordR0x1)&&(ActualrightPoly+0.07>=ActualcordR0x2)&&(ActualrightPoly+0.07>=ActualcordR1x2)
		bbox=list(ActualcordR0x1-0.04:by1 ActualrightPoly+0.11:by2)
		LeftLeaf=ActualcordR0x1-0.04
		RightLeaf=(ActualrightPoly+0.11)-(ActualcordR0x1-0.04))
		when((ActualleftPoly-0.07>ActualcordR1x1)&&(ActualleftPoly-0.07<=ActualcordR0x1)&&(ActualrightPoly+0.07>=ActualcordR0x2)&&(ActualrightPoly+0.07>=ActualcordR1x2)
		bbox=list(ActualcordR1x1-0.04:by1 ActualrightPoly+0.11:by2)
		LeftLeaf=ActualcordR1x1-0.04
		RightLeaf=(ActualrightPoly+0.11)-(ActualcordR1x1-0.04))
		when((ActualleftPoly-0.07>ActualcordR1x1)&&(ActualleftPoly-0.07>ActualcordR0x1)&&(ActualcordR1x1<ActualcordR0x1)&&(ActualrightPoly+0.07>=ActualcordR0x2)&&(ActualrightPoly+0.07>=ActualcordR1x2)
		bbox=list(ActualcordR1x1-0.04:by1 ActualrightPoly+0.11:by2)
		LeftLeaf=ActualcordR1x1-0.04
		RightLeaf=(ActualrightPoly+0.11)-(ActualcordR1x1-0.04))
		when((ActualleftPoly-0.07<=ActualcordR1x1)&&(ActualleftPoly-0.07>ActualcordR0x1)&&(ActualcordR1x1>=ActualcordR0x1)&&(ActualrightPoly+0.07>=ActualcordR0x2)&&(ActualrightPoly+0.07>=ActualcordR1x2)
		bbox=list(ActualcordR0x1-0.04:by1 ActualrightPoly+0.11:by2)
		LeftLeaf=ActualcordR0x1-0.04
		RightLeaf=(ActualrightPoly+0.11)-(ActualcordR0x1-0.04))
		when((ActualleftPoly-0.07<=ActualcordR1x1)&&(ActualleftPoly-0.07<=ActualcordR0x1)&&(ActualrightPoly+0.07<ActualcordR0x2)&&(ActualrightPoly+0.07>=ActualcordR1x2)
		bbox=list(ActualleftPoly-0.11:by1 ActualcordR0x2+0.04:by2)
		LeftLeaf=ActualleftPoly-0.11
		RightLeaf=(ActualcordR0x2+0.04)-(ActualleftPoly-0.11))
		when((ActualleftPoly-0.07<=ActualcordR1x1)&&(ActualleftPoly-0.07>ActualcordR0x1)&&(ActualrightPoly+0.07<ActualcordR0x2)&&(ActualrightPoly+0.07>=ActualcordR1x2)
		bbox=list(ActualcordR0x1-0.04:by1 ActualcordR0x2+0.04:by2)
		LeftLeaf=ActualcordR0x1-0.04
		RightLeaf=(ActualcordR0x2+0.04)-(ActualcordR0x1-0.04))
		when((ActualleftPoly-0.07>ActualcordR1x1)&&(ActualleftPoly-0.07<=ActualcordR0x1)&&(ActualrightPoly+0.07<ActualcordR0x2)&&(ActualrightPoly+0.07>=ActualcordR1x2)
		bbox=list(ActualcordR1x1-0.04:by1 ActualcordR0x2+0.04:by2)
		LeftLeaf=ActualcordR1x1-0.04
		RightLeaf=(ActualcordR0x2+0.04)-(ActualcordR1x1-0.04))
		when((ActualleftPoly-0.07>ActualcordR1x1)&&(ActualleftPoly-0.07>ActualcordR0x1)&&(ActualcordR1x1<ActualcordR0x1)&&(ActualrightPoly+0.07<ActualcordR0x2)&&(ActualrightPoly+0.07>=ActualcordR1x2)
		bbox=list(ActualcordR1x1-0.04:by1 ActualcordR0x2+0.04:by2)
		LeftLeaf=ActualcordR1x1-0.04
		RightLeaf=(ActualcordR0x2+0.04)-(ActualcordR1x1-0.04))
		when((ActualleftPoly-0.07>ActualcordR1x1)&&(ActualleftPoly-0.07>ActualcordR0x1)&&(ActualcordR1x1>=ActualcordR0x1)&&(ActualrightPoly+0.07<ActualcordR0x2)&&(ActualrightPoly+0.07>=ActualcordR1x2)
		bbox=list(ActualcordR0x1-0.04:by1 ActualcordR0x2+0.04:by2)
		LeftLeaf=ActualcordR0x1-0.04
		RightLeaf=(ActualcordR0x2+0.04)-(ActualcordR0x1-0.04))
		when((ActualleftPoly-0.07<=ActualcordR1x1)&&(ActualleftPoly-0.07<=ActualcordR0x1)&&(ActualrightPoly+0.07>=ActualcordR0x2)&&(ActualrightPoly+0.07<ActualcordR1x2)
		bbox=list(ActualleftPoly-0.11:by1 ActualcordR1x2+0.04:by2)
		LeftLeaf=ActualleftPoly-0.11
		RightLeaf=(ActualcordR1x2+0.04)-(ActualleftPoly-0.11))
		when((ActualleftPoly-0.07<=ActualcordR1x1)&&(ActualleftPoly-0.07>ActualcordR0x1)&&(ActualrightPoly+0.07>=ActualcordR0x2)&&(ActualrightPoly+0.07<ActualcordR1x2)
		bbox=list(ActualcordR0x1-0.04:by1 ActualcordR1x2+0.04:by2)
		LeftLeaf=ActualcordR0x1-0.04
		RightLeaf=(ActualcordR1x2+0.04)-(ActualcordR0x1-0.04))
		when((ActualleftPoly-0.07>ActualcordR1x1)&&(ActualleftPoly-0.07<=ActualcordR0x1)&&(ActualrightPoly+0.07>=ActualcordR0x2)&&(ActualrightPoly+0.07<ActualcordR1x2)
		bbox=list(ActualcordR1x1-0.04:by1 ActualcordR1x2+0.04:by2)
		LeftLeaf=ActualcordR1x1-0.04
		RightLeaf=(ActualcordR1x2+0.04)-(ActualcordR1x1-0.04))
		when((ActualleftPoly-0.07>ActualcordR1x1)&&(ActualleftPoly-0.07>ActualcordR0x1)&&(ActualcordR1x1<ActualcordR0x1)&&(ActualrightPoly+0.07>=ActualcordR0x2)&&(ActualrightPoly+0.07<ActualcordR1x2)
		bbox=list(ActualcordR1x1-0.04:by1 ActualcordR1x2+0.04:by2)
		LeftLeaf=ActualcordR1x1-0.04
		RightLeaf=(ActualcordR1x2+0.04)-(ActualcordR1x1-0.04))
		when((ActualleftPoly-0.07>ActualcordR1x1)&&(ActualleftPoly-0.07>ActualcordR0x1)&&(ActualcordR1x1>=ActualcordR0x1)&&(ActualrightPoly+0.07>=ActualcordR0x2)&&(ActualrightPoly+0.07<ActualcordR1x2)
		bbox=list(ActualcordR0x1-0.04:by1 ActualcordR1x2+0.04:by2)
		LeftLeaf=ActualcordR0x1-0.04
		RightLeaf=(ActualcordR1x2+0.04)-(ActualcordR0x1-0.04))
		when((ActualleftPoly-0.07<=ActualcordR1x1)&&(ActualleftPoly-0.07<=ActualcordR0x1)&&(ActualrightPoly+0.07<ActualcordR0x2)&&(ActualrightPoly+0.07<ActualcordR1x2)&&(ActualcordR0x2<ActualcordR1x2)
		bbox=list(ActualleftPoly-0.11:by1 ActualcordR1x2+0.04:by2)
		LeftLeaf=ActualleftPoly-0.11
		RightLeaf=(ActualcordR1x2+0.04)-(ActualleftPoly-0.11))
		when((ActualleftPoly-0.07<=ActualcordR1x1)&&(ActualleftPoly-0.07>ActualcordR0x1)&&(ActualrightPoly+0.07<ActualcordR0x2)&&(ActualrightPoly+0.07<ActualcordR1x2)&&(ActualcordR0x2<ActualcordR1x2)
		bbox=list(ActualcordR0x1-0.04:by1 ActualcordR1x2+0.04:by2)
		LeftLeaf=ActualcordR0x1-0.04
		RightLeaf=(ActualcordR1x2+0.04)-(ActualcordR0x1-0.04))
		when((ActualleftPoly-0.07>ActualcordR1x1)&&(ActualleftPoly-0.07<=ActualcordR0x1)&&(ActualrightPoly+0.07<ActualcordR0x2)&&(ActualrightPoly+0.07<ActualcordR1x2)&&(ActualcordR0x2<ActualcordR1x2)
		bbox=list(ActualcordR1x1-0.04:by1 ActualcordR1x2+0.04:by2)
		LeftLeaf=ActualcordR1x1-0.04
		RightLeaf=(ActualcordR1x2+0.04)-(ActualcordR1x1-0.04))
		when((ActualleftPoly-0.07>ActualcordR1x1)&&(ActualleftPoly-0.07>ActualcordR0x1)&&(ActualcordR1x1<ActualcordR0x1)&&(ActualrightPoly+0.07<ActualcordR0x2)&&(ActualrightPoly+0.07<ActualcordR1x2)&&(ActualcordR0x2<ActualcordR1x2)
		bbox=list(ActualcordR1x1-0.04:by1 ActualcordR1x2+0.04:by2)
		LeftLeaf=ActualcordR1x1-0.04
		RightLeaf=(ActualcordR1x2+0.04)-(ActualcordR1x1-0.04))
		when((ActualleftPoly-0.07>ActualcordR1x1)&&(ActualleftPoly-0.07>ActualcordR0x1)&&(ActualcordR1x1>=ActualcordR0x1)&&(ActualrightPoly+0.07<ActualcordR0x2)&&(ActualrightPoly+0.07<ActualcordR1x2)&&(ActualcordR0x2<ActualcordR1x2)
		bbox=list(ActualcordR0x1-0.04:by1 ActualcordR1x2+0.04:by2)
		LeftLeaf=ActualcordR0x1-0.04
		RightLeaf=(ActualcordR1x2+0.04)-(ActualcordR0x1-0.04))
		when((ActualleftPoly-0.07<=ActualcordR1x1)&&(ActualleftPoly-0.07<=ActualcordR0x1)&&(ActualrightPoly+0.07<ActualcordR0x2)&&(ActualrightPoly+0.07<ActualcordR1x2)&&(ActualcordR0x2>=ActualcordR1x2)
		bbox=list(ActualleftPoly-0.11:by1 ActualcordR1x2+0.04:by2)
		LeftLeaf=ActualleftPoly-0.11
		RightLeaf=(ActualcordR1x2+0.04)-(ActualleftPoly-0.11))
		when((ActualleftPoly-0.07<=ActualcordR1x1)&&(ActualleftPoly-0.07>ActualcordR0x1)&&(ActualrightPoly+0.07<ActualcordR0x2)&&(ActualrightPoly+0.07<ActualcordR1x2)&&(ActualcordR0x2>=ActualcordR1x2)
		bbox=list(ActualcordR0x1-0.04:by1 ActualcordR0x2+0.04:by2)
		LeftLeaf=ActualcordR0x1-0.04
		RightLeaf=(ActualcordR0x2+0.04)-(ActualcordR0x1-0.04))
		when((ActualleftPoly-0.07>ActualcordR1x1)&&(ActualleftPoly-0.07<=ActualcordR0x1)&&(ActualrightPoly+0.07<ActualcordR0x2)&&(ActualrightPoly+0.07<ActualcordR1x2)&&(ActualcordR0x2>=ActualcordR1x2)
		bbox=list(ActualcordR1x1-0.04:by1 ActualcordR0x2+0.04:by2)
		LeftLeaf=ActualcordR1x1-0.04
		RightLeaf=(ActualcordR0x2+0.04)-(ActualcordR1x1-0.04))
		when((ActualleftPoly-0.07>ActualcordR1x1)&&(ActualleftPoly-0.07>ActualcordR0x1)&&(ActualcordR1x1<ActualcordR0x1)&&(ActualrightPoly+0.07<ActualcordR0x2)&&(ActualrightPoly+0.07<ActualcordR1x2)&&(ActualcordR0x2>=ActualcordR1x2)
		bbox=list(ActualcordR1x1-0.04:by1 ActualcordR0x2+0.04:by2)
		LeftLeaf=ActualcordR1x1-0.04
		RightLeaf=(ActualcordR0x2+0.04)-(ActualcordR1x1-0.04))
		when((ActualleftPoly-0.07>ActualcordR1x1)&&(ActualleftPoly-0.07>ActualcordR0x1)&&(ActualcordR1x1>=ActualcordR0x1)&&(ActualrightPoly+0.07<ActualcordR0x2)&&(ActualrightPoly+0.07<ActualcordR1x2)&&(ActualcordR0x2>=ActualcordR1x2)
		bbox=list(ActualcordR0x1-0.04:by1 ActualcordR0x2+0.04:by2)
		LeftLeaf=ActualcordR0x1-0.04
		RightLeaf=(ActualcordR0x2+0.04)-(ActualcordR0x1-0.04))
		dbSet(SHAPE bbox "bBox")) ;moves the M3 power grid(GND/Vdd) wires
		when((SHAPE~>layerName=="M3")&&((bx1==0.04&&by1==0.295)||(bx1==0.04&&by1==2.895)||(bx1==0.04&&by1==4.975))
		bbox=list(ActualleftPoly-0.07:by1 bx2:by2)
		dbSet(SHAPE bbox "bBox")) ;moves the M3 L.o/L.1/L.e pins on the left side of the leaf cell
		when((SHAPE~>layerName=="M3")&&((bx1==0.93&&by1==0.295)||(bx1==1.225&&by1==2.895)||(bx1==0.925&&by1==4.975))
		bbox=list(bx1:by1 ActualrightPoly+0.07:by2)
		dbSet(SHAPE bbox "bBox")) ;moves the M3 R.o/R.1/R.e pins on the left side of the leaf cell
		when((SHAPE~>layerName=="M3")&&bx1==0.04&&by1==3.415
		bbox=list(ActualleftPoly-0.07:by1 ActualrightPoly+0.07:by2)
		dbSet(SHAPE bbox "bBox")) ;moves the M3 _RESET pin
		when((SHAPE~>layerName=="PO")&&((bx1==-0.005&&by1==2.91)||(bx1==-0.005&&by1==3.04))
		bbox=list(ActualleftPoly-0.115:by1 ActualrightPoly-0.135:by2)
		dbSet(SHAPE bbox "bBox")) ;moves the layout grid poly near the PPLUG
		when((SHAPE~>layerName=="PO")&&((bx1==0.23&&by1==3.69)||(bx1==0.23&&by1==3.82))
		bbox=list(ActualleftPoly+0.125:by1 ActualrightPoly+0.115:by2)
		dbSet(SHAPE bbox "bBox")) ;moves the layout grid poly near the NPLUG
		when(((SHAPE~>layerName=="NW")||(SHAPE~>layerName=="NP"))&&((bx1==0&&by1==6.07)||(bx1==0&&by1==6.11)||(bx1==0&&by1==-0.13))
		bbox=list(ActualleftPoly-0.11:by1 ActualrightPoly+0.11:by2)
		dbSet(SHAPE bbox "bBox")) ; moves the NW/NP layers on top and bottom
		when((SHAPE~>layerName=="NP")&&bx1==0&&by1==0.17
		pnts=list(x1:y1 ActualleftPoly-0.11:y2 ActualleftPoly-0.11:y3 ActualleftPoly+0.15:y4 ActualleftPoly+0.15:y5 ActualleftPoly-0.11:y6  ActualleftPoly-0.11:y7 x8:y8)
		dbSet(SHAPE pnts "points")) ; moves the NP layer on the left side of the leaf cell
		when((SHAPE~>layerName=="PP")&&bx1==0.95&&by1==0.17
		pnts=list(ActualrightPoly+0.11:y1 ActualrightPoly-0.16:y2 ActualrightPoly-0.16:y3 ActualrightPoly+0.11:y4 ActualrightPoly+0.11:y5 x6:y6 x7:y7 ActualrightPoly+0.11:y8)
		dbSet(SHAPE pnts "points")) ; moves the PP layer on the right side of the leaf cell
		when(((SHAPE~>layerName=="NW")||(SHAPE~>layerName=="PM"))&&bx1==0.95&&by1==0.13
		bbox=list(bx1:by1 ActualrightPoly+0.11:by2)
		dbSet(SHAPE bbox "bBox")) ; moves the NW/PM layer on the right side of the leaf cell

	))
		when(prbound=GetPrbound(CellView)
		pry1=cadar(prbound~>points)
		pry2=cadadr(prbound~>points)
		pry3=car(cdaddr(prbound~>points))
		pry4=cadr(cadddr(prbound~>points))
		when((ActualleftPoly-0.07<=ActualcordR1x1)&&(ActualleftPoly-0.07<=ActualcordR0x1)&&(ActualrightPoly+0.07>=ActualcordR0x2)&&(ActualrightPoly+0.07>=ActualcordR1x2)
		pntss=list(ActualleftPoly-0.11:pry1 ActualleftPoly-0.11:pry2 ActualrightPoly+0.11:pry3 ActualrightPoly+0.11:pry4))

		when((ActualleftPoly-0.07<=ActualcordR1x1)&&(ActualleftPoly-0.07>ActualcordR0x1)&&(ActualrightPoly+0.07>=ActualcordR0x2)&&(ActualrightPoly+0.07>=ActualcordR1x2)
		pntss=list(ActualcordR0x1-0.04:pry1 ActualcordR0x1-0.04:pry2 ActualrightPoly+0.11:pry3 ActualrightPoly+0.11:pry4))

		when((ActualleftPoly-0.07>ActualcordR1x1)&&(ActualleftPoly-0.07<=ActualcordR0x1)&&(ActualrightPoly+0.07>=ActualcordR0x2)&&(ActualrightPoly+0.07>=ActualcordR1x2)
		pntss=list(ActualcordR1x1-0.04:pry1 ActualcordR1x1-0.04:pry2 ActualrightPoly+0.11:pry3 ActualrightPoly+0.11:pry4))

		when((ActualleftPoly-0.07>ActualcordR1x1)&&(ActualleftPoly-0.07>ActualcordR0x1)&&(ActualcordR1x1<ActualcordR0x1)&&(ActualrightPoly+0.07>=ActualcordR0x2)&&(ActualrightPoly+0.07>=ActualcordR1x2)
		pntss=list(ActualcordR1x1-0.04:pry1 ActualcordR1x1-0.04:pry2 ActualrightPoly+0.11:pry3 ActualrightPoly+0.11:pry4))

		when((ActualleftPoly-0.07<=ActualcordR1x1)&&(ActualleftPoly-0.07>ActualcordR0x1)&&(ActualcordR1x1>=ActualcordR0x1)&&(ActualrightPoly+0.07>=ActualcordR0x2)&&(ActualrightPoly+0.07>=ActualcordR1x2)
		pntss=list(ActualcordR0x1-0.04:pry1 ActualcordR0x1-0.04:pry2 ActualrightPoly+0.11:pry3 ActualrightPoly+0.11:pry4))

		when((ActualleftPoly-0.07<=ActualcordR1x1)&&(ActualleftPoly-0.07<=ActualcordR0x1)&&(ActualrightPoly+0.07<ActualcordR0x2)&&(ActualrightPoly+0.07>=ActualcordR1x2)
		pntss=list(ActualleftPoly-0.11:pry1 ActualleftPoly-0.11:pry2 ActualcordR0x2+0.04:pry3 ActualcordR0x2+0.04:pry4))

		when((ActualleftPoly-0.07<=ActualcordR1x1)&&(ActualleftPoly-0.07>ActualcordR0x1)&&(ActualrightPoly+0.07<ActualcordR0x2)&&(ActualrightPoly+0.07>=ActualcordR1x2)
		pntss=list(ActualcordR0x1-0.04:pry1 ActualcordR0x1-0.04:pry2 ActualcordR0x2+0.04:pry3 ActualcordR0x2+0.04:pry4))

		when((ActualleftPoly-0.07>ActualcordR1x1)&&(ActualleftPoly-0.07<=ActualcordR0x1)&&(ActualrightPoly+0.07<ActualcordR0x2)&&(ActualrightPoly+0.07>=ActualcordR1x2)
		pntss=list(ActualcordR1x1-0.04:pry1 ActualcordR1x1-0.04:pry2 ActualcordR0x2+0.04:pry3 ActualcordR0x2+0.04:pry4))

		when((ActualleftPoly-0.07>ActualcordR1x1)&&(ActualleftPoly-0.07>ActualcordR0x1)&&(ActualcordR1x1<ActualcordR0x1)&&(ActualrightPoly+0.07<ActualcordR0x2)&&(ActualrightPoly+0.07>=ActualcordR1x2)
		pntss=list(ActualcordR1x1-0.04:pry1 ActualcordR1x1-0.04:pry2 ActualcordR0x2+0.04:pry3 ActualcordR0x2+0.04:pry4))

		when((ActualleftPoly-0.07>ActualcordR1x1)&&(ActualleftPoly-0.07>ActualcordR0x1)&&(ActualcordR1x1>=ActualcordR0x1)&&(ActualrightPoly+0.07<ActualcordR0x2)&&(ActualrightPoly+0.07>=ActualcordR1x2)
		pntss=list(ActualcordR0x1-0.04:pry1 ActualcordR0x1-0.04:pry2 ActualcordR0x2+0.04:pry3 ActualcordR0x2+0.04:pry4))

		when((ActualleftPoly-0.07<=ActualcordR1x1)&&(ActualleftPoly-0.07<=ActualcordR0x1)&&(ActualrightPoly+0.07>=ActualcordR0x2)&&(ActualrightPoly+0.07<ActualcordR1x2)
		pntss=list(ActualleftPoly-0.11:pry1 ActualleftPoly-0.11:pry2 ActualcordR1x2+0.04:pry3 ActualcordR1x2+0.04:pry4))

		when((ActualleftPoly-0.07<=ActualcordR1x1)&&(ActualleftPoly-0.07>ActualcordR0x1)&&(ActualrightPoly+0.07>=ActualcordR0x2)&&(ActualrightPoly+0.07<ActualcordR1x2)
		pntss=list(ActualcordR0x1-0.04:pry1 ActualcordR0x1-0.04:pry2 ActualcordR1x2+0.04:pry3 ActualcordR1x2+0.04:pry4))

		when((ActualleftPoly-0.07>ActualcordR1x1)&&(ActualleftPoly-0.07<=ActualcordR0x1)&&(ActualrightPoly+0.07>=ActualcordR0x2)&&(ActualrightPoly+0.07<ActualcordR1x2)
		pntss=list(ActualcordR1x1-0.04:pry1 ActualcordR1x1-0.04:pry2 ActualcordR1x2+0.04:pry3 ActualcordR1x2+0.04:pry4))

		when((ActualleftPoly-0.07>ActualcordR1x1)&&(ActualleftPoly-0.07>ActualcordR0x1)&&(ActualcordR1x1<ActualcordR0x1)&&(ActualrightPoly+0.07>=ActualcordR0x2)&&(ActualrightPoly+0.07<ActualcordR1x2)
		pntss=list(ActualcordR1x1-0.04:pry1 ActualcordR1x1-0.04:pry2 ActualcordR1x2+0.04:pry3 ActualcordR1x2+0.04:pry4))

		when((ActualleftPoly-0.07>ActualcordR1x1)&&(ActualleftPoly-0.07>ActualcordR0x1)&&(ActualcordR1x1>=ActualcordR0x1)&&(ActualrightPoly+0.07>=ActualcordR0x2)&&(ActualrightPoly+0.07<ActualcordR1x2)
		pntss=list(ActualcordR0x1-0.04:pry1 ActualcordR0x1-0.04:pry2 ActualcordR1x2+0.04:pry3 ActualcordR1x2+0.04:pry4))

		when((ActualleftPoly-0.07<=ActualcordR1x1)&&(ActualleftPoly-0.07<=ActualcordR0x1)&&(ActualrightPoly+0.07<ActualcordR0x2)&&(ActualrightPoly+0.07<ActualcordR1x2)&&(ActualcordR0x2<ActualcordR1x2)
		pntss=list(ActualleftPoly-0.11:pry1 ActualleftPoly-0.11:pry2 ActualcordR1x2+0.04:pry3 ActualcordR1x2+0.04:pry4))

		when((ActualleftPoly-0.07<=ActualcordR1x1)&&(ActualleftPoly-0.07>ActualcordR0x1)&&(ActualrightPoly+0.07<ActualcordR0x2)&&(ActualrightPoly+0.07<ActualcordR1x2)&&(ActualcordR0x2<ActualcordR1x2)
		pntss=list(ActualcordR0x1-0.04:pry1 ActualcordR0x1-0.04:pry2 ActualcordR1x2+0.04:pry3 ActualcordR1x2+0.04:pry4))

		when((ActualleftPoly-0.07>ActualcordR1x1)&&(ActualleftPoly-0.07<=ActualcordR0x1)&&(ActualrightPoly+0.07<ActualcordR0x2)&&(ActualrightPoly+0.07<ActualcordR1x2)&&(ActualcordR0x2<ActualcordR1x2)
		pntss=list(ActualcordR1x1-0.04:pry1 ActualcordR1x1-0.04:pry2 ActualcordR1x2+0.04:pry3 ActualcordR1x2+0.04:pry4))

		when((ActualleftPoly-0.07>ActualcordR1x1)&&(ActualleftPoly-0.07>ActualcordR0x1)&&(ActualcordR1x1<ActualcordR0x1)&&(ActualrightPoly+0.07<ActualcordR0x2)&&(ActualrightPoly+0.07<ActualcordR1x2)&&(ActualcordR0x2<ActualcordR1x2)
		pntss=list(ActualcordR1x1-0.04:pry1 ActualcordR1x1-0.04:pry2 ActualcordR1x2+0.04:pry3 ActualcordR1x2+0.04:pry4))

		when((ActualleftPoly-0.07>ActualcordR1x1)&&(ActualleftPoly-0.07>ActualcordR0x1)&&(ActualcordR1x1>=ActualcordR0x1)&&(ActualrightPoly+0.07<ActualcordR0x2)&&(ActualrightPoly+0.07<ActualcordR1x2)&&(ActualcordR0x2<ActualcordR1x2)
		pntss=list(ActualcordR0x1-0.04:pry1 ActualcordR0x1-0.04:pry2 ActualcordR1x2+0.04:pry3 ActualcordR1x2+0.04:pry4))

		when((ActualleftPoly-0.07<=ActualcordR1x1)&&(ActualleftPoly-0.07<=ActualcordR0x1)&&(ActualrightPoly+0.07<ActualcordR0x2)&&(ActualrightPoly+0.07<ActualcordR1x2)&&(ActualcordR0x2>=ActualcordR1x2)
		pntss=list(ActualleftPoly-0.11:pry1 ActualleftPoly-0.11:pry2 ActualcordR0x2+0.04:pry3 ActualcordR0x2+0.04:pry4))

		when((ActualleftPoly-0.07<=ActualcordR1x1)&&(ActualleftPoly-0.07>ActualcordR0x1)&&(ActualrightPoly+0.07<ActualcordR0x2)&&(ActualrightPoly+0.07<ActualcordR1x2)&&(ActualcordR0x2>=ActualcordR1x2)
		pntss=list(ActualcordR0x1-0.04:pry1 ActualcordR0x1-0.04:pry2 ActualcordR0x2+0.04:pry3 ActualcordR0x2+0.04:pry4))

		when((ActualleftPoly-0.07>ActualcordR1x1)&&(ActualleftPoly-0.07<=ActualcordR0x1)&&(ActualrightPoly+0.07<ActualcordR0x2)&&(ActualrightPoly+0.07<ActualcordR1x2)&&(ActualcordR0x2>=ActualcordR1x2)
		pntss=list(ActualcordR1x1-0.04:pry1 ActualcordR1x1-0.04:pry2 ActualcordR0x2+0.04:pry3 ActualcordR0x2+0.04:pry4))

		when((ActualleftPoly-0.07>ActualcordR1x1)&&(ActualleftPoly-0.07>ActualcordR0x1)&&(ActualcordR1x1<ActualcordR0x1)&&(ActualrightPoly+0.07<ActualcordR0x2)&&(ActualrightPoly+0.07<ActualcordR1x2)&&(ActualcordR0x2>=ActualcordR1x2)
		pntss=list(ActualcordR1x1-0.04:pry1 ActualcordR1x1-0.04:pry2 ActualcordR0x2+0.04:pry3 ActualcordR0x2+0.04:pry4))

		when((ActualleftPoly-0.07>ActualcordR1x1)&&(ActualleftPoly-0.07>ActualcordR0x1)&&(ActualcordR1x1>=ActualcordR0x1)&&(ActualrightPoly+0.07<ActualcordR0x2)&&(ActualrightPoly+0.07<ActualcordR1x2)&&(ActualcordR0x2>=ActualcordR1x2)
		pntss=list(ActualcordR0x1-0.04:pry1 ActualcordR0x1-0.04:pry2 ActualcordR0x2+0.04:pry3 ActualcordR0x2+0.04:pry4))
		dbDeleteObject(prbound)
		dbCreatePRBoundary(CellView pntss)
		)

	foreach(VIA CellView->vias
		(let (vx1 vy1 bx1 by1 bx2 by2 vx1 vy1 orgn)
		bx1=caar(VIA~>bBox)
		by1=cadar(VIA~>bBox)
		bx2=caadr(VIA~>bBox)
		by2=cadadr(VIA~>bBox)		
		vx1=car(VIA~>origin)
		vy1=cadr(VIA~>origin)
		when(((VIA~>viaHeader~>viaDefName=="M2_M1")||(VIA~>viaHeader~>viaDefName=="M3_M2"))&&(vx1==1.465)&&(VIA~>net~>name=="_RESET")
		orgn=(cordResx2-0.055:vy1)
		dbSet(VIA orgn "origin"))
		when((VIA~>viaHeader~>viaDefName=="M2_M1")&&(vx1==1.595)&&(VIA~>net~>name=="_Reset")
		when((Actualcordrvx2-0.2)<=1.565
		orgn=(cordResx2+0.075:vy1))
		when((Actualcordrvx2-0.2)>1.565
		orgn=(Actualcordrvx2-0.2:vy1))
		dbSet(VIA orgn "origin"))
		when((VIA~>viaHeader~>viaDefName=="M2_M1")&&(vx1==1.74)&&(VIA~>net~>name=="_r.1")
		when((Actualcordrvx2-0.2)<=1.565
		orgn=(cordResx2+0.205:vy1))
		when((Actualcordrvx2-0.2)>1.565
		orgn=(Actualcordrvx2-0.07:vy1))
		dbSet(VIA orgn "origin"))
		when(((VIA~>viaHeader~>viaDefName=="M2_M1")||(VIA~>viaHeader~>viaDefName=="M3_M2"))&&(vx1==2.18)
		orgn=(ActualrightPoly:vy1)
		dbSet(VIA orgn "origin"))
		when((VIA~>viaHeader~>viaDefName=="M2_M1")&&(vx1==0.495)
		orgn=(ActualcordLex1+0.07:vy1)
		dbSet(VIA orgn "origin"))
		when((VIA~>viaHeader~>viaDefName=="M2_M1")&&(vx1==0.305)
		when((ActualcordLex1+0.07)>0.435
		orgn=(vx1:vy1))
		when((ActualcordLex1+0.07)<=0.435
		orgn=(ActualcordLex1-0.06:vy1))
		dbSet(VIA orgn "origin"))
		when(((VIA~>viaHeader~>viaDefName=="M2_M1")||(VIA~>viaHeader~>viaDefName=="M3_M2"))&&(vx1==0.11)
		orgn=(ActualleftPoly:vy1)
		dbSet(VIA orgn "origin"))
		when((VIA~>viaHeader~>viaDefName=="M2_M1")&&((vx1==0.95&&vy1==5.85)||(vx1==0.95&&vy1==5.59))
		when(((cordR1y2==5.81)||(cordR1y2==5.68))&&(vx1==0.95&&vy1==5.85)
		dbDeleteObject(VIA))
		when(((cordR1y2==5.55)||(cordR1y2==5.42))&&((vx1==0.95&&vy1==5.85)||(vx1==0.95&&vy1==5.59))
		dbDeleteObject(VIA)))
		when((VIA~>viaHeader~>viaDefName=="M2_M1")&&((vx1==0.95&&vy1==0.91)||(vx1==0.95&&vy1==0.65))
		when(((cordR0y2==0.87)||(cordR0y2==0.74))&&(vx1==0.95&&vy1==0.91)
		dbDeleteObject(VIA))
		when(((cordR0y2==0.61)||(cordR0y2==0.48))&&((vx1==0.95&&vy1==0.91)||(vx1==0.95&&vy1==0.65))
		dbDeleteObject(VIA)))
		when((VIA~>viaHeader~>viaDefName=="M2_M1")&&((vx1==0.95&&vy1==2.47)||(vx1==0.95&&vy1==2.73))
		when(((cordLey2==2.69)||(cordLey2==2.56))&&(vx1==0.95&&vy1==2.73)
		dbDeleteObject(VIA))
		when(((cordLey2==2.43)||(cordLey2==2.3))&&((vx1==0.95&&vy1==2.47)||(vx1==0.95&&vy1==2.73))
		dbDeleteObject(VIA)))
		when((VIA~>viaHeader~>viaDefName=="M2_M1")&&((vx1==0.495&&vy1==2.77)||(vx1==0.495&&vy1==2.58)||(vx1==0.495&&vy1==2.36))
		when((cordLey2==2.82)&&(vx1==0.495&&vy1==2.77)
		dbDeleteObject(VIA))
		when(((cordLey2==2.69)||(cordLey2==2.56))&&((vx1==0.495&&vy1==2.77)||(vx1==0.495&&vy1==2.58))
		dbDeleteObject(VIA))
		when(((cordLey2==2.43)||(cordLey2==2.3))&&((vx1==0.495&&vy1==2.77)||(vx1==0.495&&vy1==2.58)||(vx1==0.495&&vy1==2.36))
		dbDeleteObject(VIA)))

	))
	))
(defun BufferCreator ( CellName ViewName )
    let( ( k l m n y s z ce  cel piecess piecesss cv ncv mcv lcv vcv TemplateCellName pieces LibName Temp LeftLeaf RightLeaf pntss)
        pieces=parseString( CellName "." )
        LibName=nth( 0 pieces )
	k="0"
	l="0"
	m="0"
	n="0"
        for( n 1 length(pieces)-3
            LibName = strcat( LibName "." nth( n pieces )))
        cv=nrOpenCellViewReadable( LibName CellName ViewName )
        TemplateCellName = "lib.buffer.half.BUF_1of2.buftemplate"
	mcv=nrOpenCellViewReadable( LibName TemplateCellName "Template")

        when( cv != nil
            printf( "BUF_1of2 type already exists %s\n" CellName)
            )
	when( mcv == nil
	    printf("Template Cell doesnot exists %s\n" TemplateCellName))
         when( ((cv == nil)&&(mcv != nil))
            printf("yes\n")
            betterCopyCellView(mcv LibName CellName "scratch" nil nil t)
            ncv = nrOpenCellViewWritable( LibName CellName "scratch" )
	    MakeBuffer( ncv ViewName)
		dbSave(ncv)
		dbPurge(ncv)
            lcv = nrOpenCellViewWritable( LibName CellName ViewName )
		CopyBuffer( lcv ViewName LibName CellName ))
		))

(defun CopyBuffer ( CellView ViewName LibName CellName )
		(let (xy1 xy2 XY bx1 bx2 by1 by2 buf rightcord RightCord pnts SHAPE) 
		when(minusp(LeftLeaf)
		buf = dbCreateInstByMasterName(CellView LibName CellName "scratch" "buf" 0.04:0 "R0"))
		when(plusp(LeftLeaf)
		buf = dbCreateInstByMasterName(CellView LibName CellName "scratch" "buf" LeftLeaf:0 "R0"))
		when(LeftLeaf==0
		buf = dbCreateInstByMasterName(CellView LibName CellName "scratch" "buf" 0.04:0 "R0"))
		foreach(INST CellView~>instances
		xy1=car(INST~>xy)
		xy2=cadr(INST~>xy)	
		bx1=caar(INST~>bBox)
		by1=cadar(INST~>bBox)
		bx2=caadr(INST~>bBox)
		by2=cadadr(INST~>bBox)
		XY=(-bx1:xy2)	
		dbSet(INST XY "xy"))
		dbFlattenInst(buf 1)	
		rightcord=round(((RightLeaf/0.26)/0.5)*0.5+0.5)
		RightCord=0.26*rightcord
		pnts=list(0:by1+0.13 0:by2-0.13 RightCord:by2-0.13 RightCord:by1+0.13)
		dbCreatePRBoundary(CellView pnts)
	foreach(INST CellView->instances
		(let (xy1 xy2 XY bx1 bx2 by1 by2)
		xy1=car(INST~>xy)
		xy2=cadr(INST~>xy)	
		bx1=caar(INST~>bBox)
		by1=cadar(INST~>bBox)
		bx2=caadr(INST~>bBox)
		by2=cadadr(INST~>bBox)
		when((INST->cellName=="stack.PPLUG.0")
		XY=(RightCord-0.165:xy2)
		dbSet(INST XY "xy"))
		))
	foreach(SHAPE CellView->shapes
		(let (bx1 bx2 by1 by2 x1 x2 x3 x4 x5 x6 x7 x8 y1 y2 y3 y4 y5 y6 y7 y8 xy1 xy2 pnts bbox orgn)
		bx1=caar(SHAPE~>bBox)
		by1=cadar(SHAPE~>bBox)
		bx2=caadr(SHAPE~>bBox)
		by2=cadadr(SHAPE~>bBox)
		xy1=car(SHAPE~>xy)
		xy2=cadr(SHAPE~>xy)	
		x1=caar(SHAPE~>points)
		y1=cadar(SHAPE~>points)
		x2=caadr(SHAPE~>points)
		y2=cadadr(SHAPE~>points)
		x3=caaddr(SHAPE~>points)
		y3=car(cdaddr(SHAPE~>points))
		x4=car(cadddr(SHAPE~>points))
		y4=cadr(cadddr(SHAPE~>points))
		x5=caar(cddddr(SHAPE~>points))
		y5=cadar(cddddr(SHAPE~>points))
		x6=caadr(cddddr(SHAPE~>points))
		y6=cadadr(cddddr(SHAPE~>points))
		x7=caaddr(cddddr(SHAPE~>points))
		y7=car(cdaddr(cddddr(SHAPE~>points)))
		x8=car(cadddr(cddddr(SHAPE~>points)))
		y8=cadr(cadddr(cddddr(SHAPE~>points)))
		when(((SHAPE~>layerName=="NW")||(SHAPE~>layerName=="NP"))&&((by2==6.37&&by1==6.07)||(by2==6.37&&by1==6.11)||(by2==0.13&&by1==-0.13)||(by2==0.17&&by1==-0.13))
		bbox=list(0:by1 RightCord:by2)
		dbSet(SHAPE bbox "bBox")) ; moves the NW/NP layers on top and bottom
		when(((SHAPE~>layerName=="NW")||(SHAPE~>layerName=="PM"))&&by2==6.11&&by1==0.13
		bbox=list(bx1:by1 RightCord:by2)
		dbSet(SHAPE bbox "bBox")) ; moves the NW/PM layer on the right side of the leaf cell
		when((SHAPE~>layerName=="PP")&&by2==6.07&&by1==0.17
		pnts=list(RightCord:y1 RightCord-0.27:y2 RightCord-0.27:y3 RightCord:y4 RightCord:y5 x6:y6 x7:y7 RightCord:y8)
		dbSet(SHAPE pnts "points")) ; moves the PP layer on the right side of the leaf cell
		when((SHAPE~>layerName=="NP")&&by2==3.385&&by1==2.86
		bbox=list(RightCord-0.06:by1 RightCord:by2)
		dbSet(SHAPE bbox "bBox")) ; moves the NP layer on the right side of the leaf cell associated with PPLUG
		when((SHAPE~>layerName=="M3")&&((bx1==0&&by1==0.025)||(bx1==0&&by1==1.465)||(bx1==0&&by1==1.585)||(bx1==0&&by1==3.025)||(bx1==0&&by1==3.145)||(bx1==0&&by1==4.585)||(bx1==0&&by1==4.705)||(bx1==0&&by1==6.145))
		bbox=list(bx1:by1 RightCord:by2)
		dbSet(SHAPE bbox "bBox")) ;moves the M3 power grid(GND/Vdd) wires
		when((SHAPE~>layerName=="M3")&&((bx1!=0.04&&by1==0.295&&by2==0.355)||(bx1!=0.04&&by1==2.895&&by2==2.955)||(bx1!=0.04&&by1==4.975&&by2==5.035))
		bbox=list(bx1:by1 RightCord-0.04:by2)
		dbSet(SHAPE bbox "bBox")) ;moves the M3 R.o/R.1/R.e pins on the left side of the leaf cell
		when((SHAPE~>layerName=="M3")&&bx1==0.04&&by1==3.415
		bbox=list(bx1:by1 RightCord-0.04:by2)
		dbSet(SHAPE bbox "bBox")) ;moves the M3 _RESET pin
		when((SHAPE~>layerName=="M1")&&by2==3.065&&by1==2.83
		pnts=list(x1:y1 RightCord-0.17:y2 RightCord-0.17:y3)
		dbSet(SHAPE pnts "points")) ; moves the M1 connecting the PPLUG to Vdd
		))
		DeleteGridPoly(?CellView CellView)
		DrawGridPoly(?CellView CellView)
		ddDeleteObj(ddGetObj(LibName CellName "scratch"))
		dbSave(CellView)
		dbPurge(CellView)
 
 )	
)

(defun Parameter ( CellName)
	(let (fl pieces piece specname initialpath InitialPath IntermediatePath finalpath k text Variable1 cst FinalPath variable1 variable2 variable3 variable4 Variable3 Variable4 var3 var4 Var3 varb4 Varb3 Var4 Outfl)
	println(CellName)
	pieces=parseString( CellName "." )
	specname=nth( 4 pieces )
	cst=".cast"
	IntermediatePath="/lib/buffer/half/BUF_1of2/"
	initialpath=ConfigFileGetValue(TheCDSConfigTable "CAST_PATH")
	piece=parseString( initialpath ":" )
	InitialPath=nth( 1 piece )
	finalpath=strcat(InitialPath IntermediatePath specname cst)
	fl = infile(finalpath)
	Outfl = outfile("Parameter.txt" "w")
	while( gets(k fl) != nil
  	text = parseString( k " \n")
	Variable1 = nth(0 text)
	when((Variable1=="XReset")||(Variable1=="X_Reset")||(Variable1=="XL.e")||(Variable1=="XR.0")||(Variable1=="XR.1")
	variable1=car(parseString( Variable1 "X" ))
	variable2=nth(6 text)
	Variable3=nth(7 text)
	Var3=parseString( Variable3 "=" )
	Varb3=nth( 1 Var3)
	var3=parseString( Varb3 "u" )
	variable3=nth( 0 var3)
	Variable4=nth(8 text)
	Var4=parseString( Variable4 "=" )
	Varb4=nth( 1 Var4)
	var4=parseString( Varb4 "u" )
	variable4=nth( 0 var4)
	fprintf(Outfl "%s" variable1 )
	fprintf(Outfl " %s" variable2 )
	fprintf(Outfl " %s" variable3 )
	fprintf(Outfl " %s\n" variable4 )
	)
	when((Variable1=="X_r.0")||(Variable1=="X_r.1")
	variable1=car(parseString( Variable1 "X" ))
	variable2=nth(8 text)
	Variable3=nth(9 text)
	Var3=parseString( Variable3 "=" )
	Varb3=nth( 1 Var3)
	var3=parseString( Varb3 "u" )
	variable3=nth( 0 var3)
	Variable4=nth(10 text)
	Var4=parseString( Variable4 "=" )
	Varb4=nth( 1 Var4)
	var4=parseString( Varb4 "u" )
	variable4=nth( 0 var4)
	fprintf(Outfl "%s" variable1 )
	fprintf(Outfl " %s" variable2 )
	fprintf(Outfl " %s" variable3 )
	fprintf(Outfl " %s\n" variable4 )
	)
	when((Variable1=="Xrv")
	variable1=car(parseString( Variable1 "X" ))
	variable2=nth(8 text)
	Variable3=nth(9 text)
	Var3=parseString( Variable3 "=" )
	Varb3=nth( 1 Var3)
	var3=parseString( Varb3 "u" )
	variable3=nth( 0 var3)
	Variable4=nth(10 text)
	Var4=parseString( Variable4 "=" )
	Varb4=nth( 1 Var4)
	var4=parseString( Varb4 "u" )
	variable4=nth( 0 var4)
	fprintf(Outfl "%s" variable1 )
	fprintf(Outfl " %s" variable2 )
	fprintf(Outfl " %s" variable3 )
	fprintf(Outfl " %s\n" variable4 )
	)

)
	close(Outfl)
	close(fl)
	))

(defun BC2 ( CellName)

;	fl = infile("buf.txt")
;	while( gets(k fl) != nil
;  	text = parseString( k " \n") 
;  	CellName = nth(0 text)
	printf("Cell is %s\n" CellName)
	Parameter(CellName)
  	BufferCreator(CellName "layout")
)
