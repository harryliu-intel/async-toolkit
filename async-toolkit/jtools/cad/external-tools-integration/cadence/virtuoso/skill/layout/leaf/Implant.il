(defvar Scratch0LPP ( list "y0" "drawing" ) )
(defvar Scratch1LPP ( list "y1" "drawing" ) )
(defvar Scratch2LPP ( list "y5" "drawing" ) )
(defvar Scratch3LPP ( list "y6" "drawing" ) )
(defvar Scratch4LPP ( list "y7" "drawing" ) )
(defvar Scratch5LPP ( list "y8" "drawing" ) )
(defun SetImpProp (obj) 
	 (when obj
		(dbReplaceProp obj "Imp_prop" "boolean" t)	        	
	 )
)

;;;;;;;;Deletes Implants in a leaf cell;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun DeleteImplant ( @key (CellView (geGetEditCellView)) )
 let( (CellView shape)
	CellView=geGetEditCellView() 
        foreach(shape CellView-> shapes 
	    	  (when (dbGetPropByName shape "Imp_prop")->value=="TRUE"
		  dbDeleteObject(shape)
                ))
	))


;;;;;;;;Draws Implants in a leaf cell;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun Implant ( @key (CellView (geGetEditCellView)) )
 let( (CellView left bx1 bx2 by1 by2 right top bottom ixy1 toplimit rightlimit lastPolyx initialPP initialPP1 initialPP2 initialNP initialNP1 initialNP2 i j k l x y y1 x1 newshape overlapNP overlapPP overlapNP2 overlapPP2 overlapNP3 overlapPP3 overlapNPBoth overlapPPBoth  overlapNPBoth2 overlapPPBoth2 overlapSCN3 overlapSCP3 overlapSCN32 overlapSCP32 overlapSCN33 overlapSCP33 item SHP Flip Flip1 Flip2 NPimp NPimp1 PPimp PPimp1 NWimp PMimp Imp1 Imp2 Imp3 Imp4 imp1 imp2 imp3 imp4 Shape1 Shape2 Shape3 overlapSC3NP2 overlapSC3NP1 overlapSC3PP2 overlapSC3PP1 overlpy1 overlpy2 overlny1 overlny2 layer)

	CellView=geGetEditCellView() 
	i=0
	j=0
	k=0
	l=0
	ixy1=0
	declare(anx[10])
	declare(bnx[10])
	declare(any[10])
	declare(bny[10])
	declare(apx[10])
	declare(bpx[10])
	declare(apy[10])
	declare(bpy[10])
	overlapNP=nil
	overlapPP=nil
	overlapNP2=nil
	overlapPP2=nil
	overlapNP3=nil
	overlapPP3=nil
	overlapNPBoth=nil
	overlapPPBoth=nil
	overlapNPBoth2=nil
	overlapPPBoth2=nil
	overlapSCN3=nil
	overlapSCN32=nil
	overlapSCN33=nil
	overlapSCP3=nil
	overlapSCP32=nil
	overlapSCP33=nil



  ;locating PrBound
	GetPrbound(CellView)
	left=caar(GetPrbound(CellView)->bBox)
	bottom=cadar(GetPrbound(CellView)->bBox)
	right=caadr(GetPrbound(CellView)->bBox)
	top=cadadr(GetPrbound(CellView)->bBox)

  ;locating the left most instance type
    foreach(INST CellView->instances
        (let (cordNx cordNx1 cordNx2 cordPx cordPx1 cordPx2 xy1 xy2 XY diff Instname pieces piecess LibName libname chainname stackname)
        xy1=car(INST~>xy)
        xy2=cadr(INST~>xy)   
        bx1=caar(INST~>bBox)
        by1=cadar(INST~>bBox)
        bx2=caadr(INST~>bBox)
        by2=cadadr(INST~>bBox)
        Instname=INST->cellName
            pieces=parseString( Instname "." )
            LibName=nth( 0 pieces )
        when((LibName=="stack")
        libname=nth( 1 pieces )
        piecess=parseString( libname "_" )
        chainname=nth(0 piecess)
            stackname = strcat( LibName "." chainname ))
        when((stackname=="stack.NMOS")||(LibName=="gate")||(stackname=="stack.PMOS")
        when((LibName=="gate")&&(ixy1==0)
	when((INST~>orient=="R0")||(INST~>orient=="MX")
	Flip = 0)	
	when((INST~>orient=="R180")||(INST~>orient=="MY")
	Flip = 1)		
        ixy1=xy1)
        when((LibName=="gate")&&(ixy1!=0)
        when(xy1<ixy1
	when((INST~>orient=="R0")||(INST~>orient=="MX")
	Flip = 0)	
	when((INST~>orient=="R180")||(INST~>orient=="MY")
	Flip = 1)	
        ixy1=xy1))
        when((stackname=="stack.NMOS")&&(ixy1==0)
	Flip = 0
        ixy1=xy1)
        when((stackname=="stack.NMOS")&&(ixy1!=0)
        when(xy1<ixy1
	Flip = 0
        ixy1=xy1))
        when((stackname=="stack.PMOS")&&(ixy1==0)
	Flip = 1
        ixy1=xy1)
        when((stackname=="stack.PMOS")&&(ixy1!=0)
        when(xy1<ixy1
	Flip = 1
        ixy1=xy1))
        )
	when(stackname=="stack.NPLUG"
		dbCreateRect(CellView Scratch3LPP list( bx1:by1 bx2:by2) )
		anx[i]=bx1
		bnx[i]=bx2
		any[i]=by1
		bny[i]=by2
	i = i + 1)
	when(stackname=="stack.PPLUG"
		dbCreateRect(CellView Scratch3LPP list( bx1:by1 bx2:by2) )
		apx[j]=bx1
		bpx[j]=bx2
		apy[j]=by1
		bpy[j]=by2
	j = j + 1)
	))


  ;adding Scratch1 and Scratch2 layers inplace of NP/PP layers
	toplimit =  top - 0.26
	x = left + 0.0
	initialNP = 0
	initialPP = 0
	while( x < right-0.005
	x1 = x
	l = 0
	k = 0
	y = bottom + 0.13
       	while( y < toplimit
	when(Flip==0
	when(initialNP==1
		initialNP=0
		x=x-0.065)
	    newshape=dbCreateRect(CellView Scratch1LPP  
		list( x: y x+0.065: y + toplimit ) )

        foreach(shape CellView-> shapes 
	    	  (when (dbGetPropByName shape "Imp_prop")->value=="TRUE"
		  dbDeleteObject(shape)
                ))

  foreach(shape CellView-> shapes 
	when( (shape->lpp== Scratch1LPP)&&(shape==newshape)
        bx1=caar(shape~>bBox)
        by1=cadar(shape~>bBox)
        bx2=caadr(shape~>bBox)
        by2=cadadr(shape~>bBox)	
		overlapNP=dbGetTrueOverlaps(CellView list(bx1+0.005:by1+0.005 bx2-0.005:by2-0.025) PImplantLPP 2)	
		overlapNPBoth=dbGetTrueOverlaps(CellView list(bx1+0.005:by1+0.005 bx2-0.005:by2-0.025) NImplantLPP 2)	
		overlapSCN3=dbGetTrueOverlaps(CellView list(bx1+0.005:by1+0.005 bx2-0.005:by2-0.025) Scratch3LPP 0)
		when(overlapSCN3!=nil
		overlny1=cadar(car(overlapSCN3~>bBox))
		overlny2=cadadr(car(overlapSCN3~>bBox))
		overlapSC3PP1=dbGetTrueOverlaps(CellView list(bx1+0.005:0.13 bx2-0.005:overlny1-0.025) PImplantLPP 2)
		overlapSC3PP2=dbGetTrueOverlaps(CellView list(bx1+0.005:overlny2+0.005 bx2-0.005:toplimit) PImplantLPP 2)
		)
		; checks for overlaps of both NP/PP layer with Scratch 1 layer and switches the Scratch layer drawing to 0.065 pitch
		when(((overlapNP!=nil)&&(overlapSCN3==nil)&&(overlapNPBoth!=nil))||((overlapSCN3!=nil)&&((overlapSC3PP1!=nil)||(overlapSC3PP2!=nil)))
         		dbDeleteObject(shape)
			Flip1=0
			y = bottom + 0.13
    			while( y < toplimit+0.13
    			when(Flip1==0
			when(initialNP1==1
				initialNP1=0
			        y=y-0.065)
        			newshape=dbCreateRect(CellView Scratch1LPP 
			        list( x: y x+0.065: y + 0.065 ) )
				foreach(shape CellView-> shapes
				when( shape->lpp== Scratch1LPP
			        bx1=caar(shape~>bBox)
			        by1=cadar(shape~>bBox)
			        bx2=caadr(shape~>bBox)
			        by2=cadadr(shape~>bBox)   
			        overlapNP2=dbGetTrueOverlaps(CellView list(bx1+0.005:by1+0.005 bx2-0.005:by2-0.025) PImplantLPP 2) 
			        overlapNPBoth2=dbGetTrueOverlaps(CellView list(bx1+0.005:by1+0.005 bx2-0.005:by2-0.025) NImplantLPP 2)   
			        overlapSCN32=dbGetTrueOverlaps(CellView list(bx1+0.005:by1+0.005 bx2-0.005:by2-0.025) Scratch3LPP 0)
        			when((overlapNP2!=nil)&&(overlapSCN32==nil)&&(overlapNPBoth2!=nil)
            				dbDeleteObject(shape)
					Flip2=0
					layer=0
					while( x1<x+0.064
					y1 = y
    					while( y1 < toplimit+0.13
    					when(Flip2==0
					when(initialNP2==1
					initialNP2=0
				        y1=y1-0.065)
        				newshape=dbCreateRect(CellView Scratch1LPP 
				        list( x1: y1 x1+0.005: y1 + 0.065 ) )
					foreach(shape CellView-> shapes
					when( shape->lpp== Scratch1LPP
				        bx1=caar(shape~>bBox)
				        by1=cadar(shape~>bBox)
				        bx2=caadr(shape~>bBox)
			        	by2=cadadr(shape~>bBox)  
				        overlapNP3=dbGetTrueOverlaps(CellView list(bx1+0.001:by1+0.005 bx2-0.001:by2-0.025) PImplantLPP 2)   
				        overlapSCN33=dbGetTrueOverlaps(CellView list(bx1+0.001:by1+0.005 bx2-0.001:by2-0.025) Scratch3LPP 0)
        				when((overlapNP3!=nil)&&(overlapSCN33==nil)
            				Flip2 = 1
            				initialPP2 = 1
	            			dbDeleteObject(shape)
	            			))))
    					when(Flip2==1
        				newshape=dbCreateRect(CellView Scratch2LPP 
        				list( x1: y1 x1+0.005: y1 + 0.065 ) )
					foreach(shape CellView-> shapes
					when( shape->lpp== Scratch2LPP
				        bx1=caar(shape~>bBox)
				        by1=cadar(shape~>bBox)
				        bx2=caadr(shape~>bBox)
			        	by2=cadadr(shape~>bBox)   
				        overlapPP3=dbGetTrueOverlaps(CellView list(bx1+0.001:by1+0.005 bx2-0.001:by2-0.025) NImplantLPP 2)   
				        overlapSCP33=dbGetTrueOverlaps(CellView list(bx1+0.001:by1+0.005 bx2-0.001:by2-0.025) Scratch3LPP 0)
				        when((overlapPP3!=nil)&&(overlapSCP33==nil)
            				Flip2 = 0
		 	        	initialNP2 = 1
				        dbDeleteObject(shape)
				        ))))
				when(y1==0.13
				Shape1=newshape)
				when(y1==0.195
				Shape2=newshape)
				when((y1==0.26)&&(newshape~>lpp==Scratch2LPP)
				layer=1
				dbCreateRect(CellView Scratch2LPP 
        				list( x1: 0.13 x1+0.005: 0.195 ) )
				dbCreateRect(CellView Scratch2LPP 
        				list( x1: 0.195 x1+0.005: 0.26 ) )
				)
				when((y1==0.26)&&(newshape~>lpp==Scratch1LPP)
				layer=2
				dbCreateRect(CellView Scratch1LPP 
        				list( x1: 0.13 x1+0.005: 0.195 ) )
				dbCreateRect(CellView Scratch1LPP 
        				list( x1: 0.195 x1+0.005: 0.26 ) )
				)
					y1 = y1 + 0.065)
					x1 = x1 + 0.005)
					when(((layer==1)&&(Shape1==Scratch1LPP)&&(Shape1==Scratch1LPP))||((layer==2)&&(Shape1==Scratch2LPP)&&(Shape1==Scratch2LPP))
				        dbDeleteObject(Shape1)
				        dbDeleteObject(Shape2))
					y = toplimit+0.13

				)


        			when((overlapNP2!=nil)&&(overlapSCN32==nil)&&(overlapNPBoth2==nil)
            			Flip1 = 1
            			initialPP1 = 1
            			dbDeleteObject(shape)
            			))))
    				when(Flip1==1
        			newshape=dbCreateRect(CellView Scratch2LPP 
        			list( x: y x+0.065: y + 0.065 ) )
				foreach(shape CellView-> shapes
				when( shape->lpp== Scratch2LPP
			        bx1=caar(shape~>bBox)
			        by1=cadar(shape~>bBox)
			        bx2=caadr(shape~>bBox)
			        by2=cadadr(shape~>bBox)   
			        overlapPP2=dbGetTrueOverlaps(CellView list(bx1+0.005:by1+0.005 bx2-0.005:by2-0.025) NImplantLPP 2)  
			        overlapPPBoth2=dbGetTrueOverlaps(CellView list(bx1+0.005:by1+0.005 bx2-0.005:by2-0.025) PImplantLPP 2)    
			        overlapSCP32=dbGetTrueOverlaps(CellView list(bx1+0.005:by1+0.005 bx2-0.005:by2-0.025) Scratch3LPP 0)
			        when((overlapPP2!=nil)&&(overlapSCP32==nil)&&(overlapPPBoth2!=nil)
            				dbDeleteObject(shape)
					Flip2=0
					layer=0
					while( x1<x+0.064
					y1 = y
    					while( y1 < toplimit+0.13
    					when(Flip2==0
					when(initialNP2==1
					initialNP2=0
				        y1=y1-0.065)
        				newshape=dbCreateRect(CellView Scratch1LPP 
				        list( x1: y1 x1+0.005: y1 + 0.065 ) )
					foreach(shape CellView-> shapes
					when( shape->lpp== Scratch1LPP
				        bx1=caar(shape~>bBox)
				        by1=cadar(shape~>bBox)
				        bx2=caadr(shape~>bBox)
			        	by2=cadadr(shape~>bBox)  
				        overlapNP3=dbGetTrueOverlaps(CellView list(bx1+0.001:by1+0.005 bx2-0.001:by2-0.025) PImplantLPP 2)   
				        overlapSCN33=dbGetTrueOverlaps(CellView list(bx1+0.001:by1+0.005 bx2-0.001:by2-0.025) Scratch3LPP 0)
        				when((overlapNP3!=nil)&&(overlapSCN33==nil)
            				Flip2 = 1
            				initialPP2 = 1
	            			dbDeleteObject(shape)
	            			))))
    					when(Flip2==1
        				newshape=dbCreateRect(CellView Scratch2LPP 
        				list( x1: y1 x1+0.005: y1 + 0.065 ) )
					foreach(shape CellView-> shapes
					when( shape->lpp== Scratch2LPP
				        bx1=caar(shape~>bBox)
				        by1=cadar(shape~>bBox)
				        bx2=caadr(shape~>bBox)
			        	by2=cadadr(shape~>bBox)   
				        overlapPP3=dbGetTrueOverlaps(CellView list(bx1+0.001:by1+0.005 bx2-0.001:by2-0.025) NImplantLPP 2)   
				        overlapSCP33=dbGetTrueOverlaps(CellView list(bx1+0.001:by1+0.005 bx2-0.001:by2-0.025) Scratch3LPP 0)
				        when((overlapPP3!=nil)&&(overlapSCP33==nil)
            				Flip2 = 0
		 	        	initialNP2 = 1
				        dbDeleteObject(shape)
				        ))))
				when(y1==0.13
				Shape1=newshape)
				when(y1==0.195
				Shape2=newshape)
				when((y1==0.26)&&(newshape~>lpp==Scratch2LPP)
				layer=1
				dbCreateRect(CellView Scratch2LPP 
        				list( x1: 0.13 x1+0.005: 0.195 ) )
				dbCreateRect(CellView Scratch2LPP 
        				list( x1: 0.195 x1+0.005: 0.26 ) )
				)
				when((y1==0.26)&&(newshape~>lpp==Scratch1LPP)
				layer=2
				dbCreateRect(CellView Scratch1LPP 
        				list( x1: 0.13 x1+0.005: 0.195 ) )
				dbCreateRect(CellView Scratch1LPP 
        				list( x1: 0.195 x1+0.005: 0.26 ) )
				)

					y1 = y1 + 0.065)
					x1 = x1 + 0.005)
					when(((layer==1)&&(Shape1==Scratch1LPP)&&(Shape1==Scratch1LPP))||((layer==2)&&(Shape1==Scratch2LPP)&&(Shape1==Scratch2LPP))
				        dbDeleteObject(Shape1)
				        dbDeleteObject(Shape2))
					y = toplimit+0.13

				)

			        when((overlapPP2!=nil)&&(overlapSCP32==nil)&&(overlapPPBoth2==nil)
            			Flip1 = 0
		 	        initialNP1 = 1
			        dbDeleteObject(shape)
			        ))))
				when(y==0.13
				Shape1=newshape)
				when(y==0.195
				Shape2=newshape)
				when((y==0.26)&&(newshape~>lpp==Scratch2LPP)
			        dbDeleteObject(Shape1)
			        dbDeleteObject(Shape2)
				dbCreateRect(CellView Scratch2LPP 
        				list( x: 0.13 x+0.065: 0.195 ) )
				dbCreateRect(CellView Scratch2LPP 
        				list( x: 0.195 x+0.065: 0.26 ) )
				)
				when((y==0.26)&&(newshape~>lpp==Scratch1LPP)
			        dbDeleteObject(Shape1)
			        dbDeleteObject(Shape2)
				dbCreateRect(CellView Scratch1LPP 
        				list( x: 0.13 x+0.065: 0.195 ) )
				dbCreateRect(CellView Scratch1LPP 
        				list( x: 0.195 x+0.065: 0.26 ) )
				)
				y = y + 0.065)
				)
		when((overlapNP!=nil)&&(overlapSCN3==nil)&&(overlapNPBoth==nil)
			Flip = 1
			initialPP = 1
			dbDeleteObject(shape)
			))))
; starts drawing Scratch 2 layer
	when(Flip==1
		newshape=dbCreateRect(CellView Scratch2LPP  
		list( x: y x+0.065: y + toplimit ) )
  foreach(shape CellView-> shapes 
	when( (shape->lpp== Scratch2LPP)&&(shape==newshape)
        bx1=caar(shape~>bBox)
        by1=cadar(shape~>bBox)
        bx2=caadr(shape~>bBox)
        by2=cadadr(shape~>bBox)	
		overlapPP=dbGetTrueOverlaps(CellView list(bx1+0.005:by1+0.005 bx2-0.005:by2-0.025) NImplantLPP 2)
		overlapPPBoth=dbGetTrueOverlaps(CellView list(bx1+0.005:by1+0.005 bx2-0.005:by2-0.025) PImplantLPP 2)		
		overlapSCP3=dbGetTrueOverlaps(CellView list(bx1+0.005:by1+0.005 bx2-0.005:by2-0.025) Scratch3LPP 0)
		when(overlapSCP3!=nil
		overlpy1=cadar(car(overlapSCP3~>bBox))
		overlpy2=cadadr(car(overlapSCP3~>bBox))
		overlapSC3NP1=dbGetTrueOverlaps(CellView list(bx1+0.005:0.13 bx2-0.005:overlpy1-0.025) NImplantLPP 2)
		overlapSC3NP2=dbGetTrueOverlaps(CellView list(bx1+0.005:overlpy2+0.005 bx2-0.005:toplimit) NImplantLPP 2)
		)
		when(((overlapPP!=nil)&&(overlapSCP3==nil)&&(overlapPPBoth!=nil))||((overlapSCP3!=nil)&&((overlapSC3NP1!=nil)||(overlapSC3NP2!=nil)))
          		dbDeleteObject(shape)	
			Flip1=1
			y = bottom + 0.13
    			while( y < toplimit+0.13
    			when(Flip1==0
			when(initialNP1==1
				initialNP1=0
			        y=y-0.065)
        			newshape=dbCreateRect(CellView Scratch1LPP 
			        list( x: y x+0.065: y + 0.065 ) )
				foreach(shape CellView-> shapes
				when( shape->lpp== Scratch1LPP
			        bx1=caar(shape~>bBox)
			        by1=cadar(shape~>bBox)
			        bx2=caadr(shape~>bBox)
			        by2=cadadr(shape~>bBox)   
			        overlapNP2=dbGetTrueOverlaps(CellView list(bx1+0.005:by1+0.005 bx2-0.005:by2-0.025) PImplantLPP 2)
			        overlapNPBoth2=dbGetTrueOverlaps(CellView list(bx1+0.005:by1+0.005 bx2-0.005:by2-0.025) NImplantLPP 2)   
			        overlapSCN32=dbGetTrueOverlaps(CellView list(bx1+0.005:by1+0.005 bx2-0.005:by2-0.025) Scratch3LPP 0)
        			when((overlapNP2!=nil)&&(overlapSCN32==nil)&&(overlapNPBoth2!=nil)
            				dbDeleteObject(shape)
					Flip2=0
					layer=0
					while( x1<x+0.064
					y1 = y
    					while( y1 < toplimit+0.13
    					when(Flip2==0
					when(initialNP2==1
					initialNP2=0
				        y1=y1-0.065)
        				newshape=dbCreateRect(CellView Scratch1LPP 
				        list( x1: y1 x1+0.005: y1 + 0.065 ) )
					foreach(shape CellView-> shapes
					when( shape->lpp== Scratch1LPP
				        bx1=caar(shape~>bBox)
				        by1=cadar(shape~>bBox)
				        bx2=caadr(shape~>bBox)
			        	by2=cadadr(shape~>bBox)  
				        overlapNP3=dbGetTrueOverlaps(CellView list(bx1+0.001:by1+0.005 bx2-0.001:by2-0.025) PImplantLPP 2)   
				        overlapSCN33=dbGetTrueOverlaps(CellView list(bx1+0.001:by1+0.005 bx2-0.001:by2-0.025) Scratch3LPP 0)
        				when((overlapNP3!=nil)&&(overlapSCN33==nil)
            				Flip2 = 1
            				initialPP2 = 1
	            			dbDeleteObject(shape)
	            			))))
    					when(Flip2==1
        				newshape=dbCreateRect(CellView Scratch2LPP 
        				list( x1: y1 x1+0.005: y1 + 0.065 ) )
					foreach(shape CellView-> shapes
					when( shape->lpp== Scratch2LPP
				        bx1=caar(shape~>bBox)
				        by1=cadar(shape~>bBox)
				        bx2=caadr(shape~>bBox)
			        	by2=cadadr(shape~>bBox)   
				        overlapPP3=dbGetTrueOverlaps(CellView list(bx1+0.001:by1+0.005 bx2-0.001:by2-0.025) NImplantLPP 2)   
				        overlapSCP33=dbGetTrueOverlaps(CellView list(bx1+0.001:by1+0.005 bx2-0.001:by2-0.025) Scratch3LPP 0)
				        when((overlapPP3!=nil)&&(overlapSCP33==nil)
            				Flip2 = 0
		 	        	initialNP2 = 1
				        dbDeleteObject(shape)
				        ))))
				when(y1==0.13
				Shape1=newshape)
				when(y1==0.195
				Shape2=newshape)
				when((y1==0.26)&&(newshape~>lpp==Scratch2LPP)
				layer=1
				dbCreateRect(CellView Scratch2LPP 
        				list( x1: 0.13 x1+0.005: 0.195 ) )
				dbCreateRect(CellView Scratch2LPP 
        				list( x1: 0.195 x1+0.005: 0.26 ) )
				)
				when((y1==0.26)&&(newshape~>lpp==Scratch1LPP)
				layer=2
				dbCreateRect(CellView Scratch1LPP 
        				list( x1: 0.13 x1+0.005: 0.195 ) )
				dbCreateRect(CellView Scratch1LPP 
        				list( x1: 0.195 x1+0.005: 0.26 ) )
				)
					y1 = y1 + 0.065)
					x1 = x1 + 0.005)
					when(((layer==1)&&(Shape1==Scratch1LPP)&&(Shape1==Scratch1LPP))||((layer==2)&&(Shape1==Scratch2LPP)&&(Shape1==Scratch2LPP))
				        dbDeleteObject(Shape1)
				        dbDeleteObject(Shape2))
					y = toplimit+0.13
				)

        			when((overlapNP2!=nil)&&(overlapSCN32==nil)&&(overlapNPBoth2==nil)
            			Flip1 = 1
            			initialPP1 = 1
            			dbDeleteObject(shape)
            			))))
    			when(Flip1==1
        			newshape=dbCreateRect(CellView Scratch2LPP 
        			list( x: y x+0.065: y + 0.065 ) )
				foreach(shape CellView-> shapes
				when( shape->lpp== Scratch2LPP
			        bx1=caar(shape~>bBox)
			        by1=cadar(shape~>bBox)
			        bx2=caadr(shape~>bBox)
			        by2=cadadr(shape~>bBox)   
			        overlapPP2=dbGetTrueOverlaps(CellView list(bx1+0.005:by1+0.005 bx2-0.005:by2-0.025) NImplantLPP 2) 
			        overlapPPBoth2=dbGetTrueOverlaps(CellView list(bx1+0.005:by1+0.005 bx2-0.005:by2-0.025) PImplantLPP 2)   
			        overlapSCP32=dbGetTrueOverlaps(CellView list(bx1+0.005:by1+0.005 bx2-0.005:by2-0.025) Scratch3LPP 0)
			        when((overlapPP2!=nil)&&(overlapSCP32==nil)&&(overlapPPBoth2!=nil)
            				dbDeleteObject(shape)
					Flip2=0
					layer=0
					while( x1<x+0.064
					y1 = y
    					while( y1 < toplimit+0.13
    					when(Flip2==0
					when(initialNP2==1
					initialNP2=0
				        y1=y1-0.065)
        				newshape=dbCreateRect(CellView Scratch1LPP 
				        list( x1: y1 x1+0.005: y1 + 0.065 ) )
					foreach(shape CellView-> shapes
					when( shape->lpp== Scratch1LPP
				        bx1=caar(shape~>bBox)
				        by1=cadar(shape~>bBox)
				        bx2=caadr(shape~>bBox)
			        	by2=cadadr(shape~>bBox)  
				        overlapNP3=dbGetTrueOverlaps(CellView list(bx1+0.001:by1+0.005 bx2-0.001:by2-0.025) PImplantLPP 2)   
				        overlapSCN33=dbGetTrueOverlaps(CellView list(bx1+0.001:by1+0.005 bx2-0.001:by2-0.025) Scratch3LPP 0)
        				when((overlapNP3!=nil)&&(overlapSCN33==nil)
            				Flip2 = 1
            				initialPP2 = 1
	            			dbDeleteObject(shape)
	            			))))
    					when(Flip2==1
        				newshape=dbCreateRect(CellView Scratch2LPP 
        				list( x1: y1 x1+0.005: y1 + 0.065 ) )
					foreach(shape CellView-> shapes
					when( shape->lpp== Scratch2LPP
				        bx1=caar(shape~>bBox)
				        by1=cadar(shape~>bBox)
				        bx2=caadr(shape~>bBox)
			        	by2=cadadr(shape~>bBox)   
				        overlapPP3=dbGetTrueOverlaps(CellView list(bx1+0.001:by1+0.005 bx2-0.001:by2-0.025) NImplantLPP 2)   
				        overlapSCP33=dbGetTrueOverlaps(CellView list(bx1+0.001:by1+0.005 bx2-0.001:by2-0.025) Scratch3LPP 0)
				        when((overlapPP3!=nil)&&(overlapSCP33==nil)
            				Flip2 = 0
		 	        	initialNP2 = 1
				        dbDeleteObject(shape)
				        ))))
				when(y1==0.13
				Shape1=newshape)
				when(y1==0.195
				Shape2=newshape)
				when((y1==0.26)&&(newshape~>lpp==Scratch2LPP)
				layer=1
				dbCreateRect(CellView Scratch2LPP 
        				list( x1: 0.13 x1+0.005: 0.195 ) )
				dbCreateRect(CellView Scratch2LPP 
        				list( x1: 0.195 x1+0.005: 0.26 ) )
				)
				when((y1==0.26)&&(newshape~>lpp==Scratch1LPP)
				layer=2
				dbCreateRect(CellView Scratch1LPP 
        				list( x1: 0.13 x1+0.005: 0.195 ) )
				dbCreateRect(CellView Scratch1LPP 
        				list( x1: 0.195 x1+0.005: 0.26 ) )
				)
					y1 = y1 + 0.065)
					x1 = x1 + 0.005)
					when(((layer==1)&&(Shape1==Scratch1LPP)&&(Shape1==Scratch1LPP))||((layer==2)&&(Shape1==Scratch2LPP)&&(Shape1==Scratch2LPP))
				        dbDeleteObject(Shape1)
				        dbDeleteObject(Shape2))
					y = toplimit+0.13
				)

			        when((overlapPP2!=nil)&&(overlapSCP32==nil)&&(overlapPPBoth2==nil)
            			Flip1 = 0
		 	        initialNP1 = 1
			        dbDeleteObject(shape)
			        ))))
				when(y==0.13
				Shape1=newshape)
				when(y==0.195
				Shape2=newshape)
				when((y==0.26)&&(newshape~>lpp==Scratch2LPP)
			        dbDeleteObject(Shape1)
			        dbDeleteObject(Shape2)
				dbCreateRect(CellView Scratch2LPP 
        				list( x: 0.13 x+0.065: 0.195 ) )
				dbCreateRect(CellView Scratch2LPP 
        				list( x: 0.195 x+0.065: 0.26 ) )
				)
				when((y==0.26)&&(newshape~>lpp==Scratch1LPP)
			        dbDeleteObject(Shape1)
			        dbDeleteObject(Shape2)
				dbCreateRect(CellView Scratch1LPP 
        				list( x: 0.13 x+0.065: 0.195 ) )
				dbCreateRect(CellView Scratch1LPP 
        				list( x: 0.195 x+0.065: 0.26 ) )
				)
				y = y + 0.065)
				)
		when((overlapPP!=nil)&&(overlapSCP3==nil)&&(overlapPPBoth==nil)
			Flip = 0
			initialNP = 1
			dbDeleteObject(shape)
			))))
	y = y + toplimit)
	x = x + 0.065)
  NPimp1=leLayerAndNot(CellView Scratch1LPP Scratch2LPP Scratch4LPP)
  NPimp=leLayerAndNot(CellView Scratch4LPP Scratch3LPP NImplantLPP)
  PPimp1=leLayerAndNot(CellView Scratch2LPP Scratch1LPP Scratch5LPP)
  PPimp=leLayerAndNot(CellView Scratch5LPP Scratch3LPP PImplantLPP)
  NWimp=leLayerAndNot(CellView Scratch2LPP Scratch1LPP NWellLPP)
  PMimp=leLayerAndNot(CellView Scratch2LPP Scratch1LPP PMetLPP)
 	foreach( item NPimp
		SetImpProp(item)
	 )	
 	foreach( item PPimp
		SetImpProp(item)
	 )	
 	foreach( item NWimp
		SetImpProp(item)
	 )
 	foreach( item PMimp
		SetImpProp(item)
	 )
        foreach(shape CellView-> shapes 
        	when( shape->lpp== Scratch1LPP || shape->lpp== Scratch2LPP || shape->lpp== Scratch3LPP || shape->lpp== Scratch4LPP || shape->lpp== Scratch5LPP
		  dbDeleteObject(shape)
                ))
	Imp1=dbCreateRect(CellView NImplantLPP list(left:bottom-0.13 right:bottom+0.13))
	Imp2=dbCreateRect(CellView NWellLPP list(left:bottom-0.13 right:bottom+0.13))
	Imp3=dbCreateRect(CellView NImplantLPP list(left:top+0.13 right:top-0.13))
	Imp4=dbCreateRect(CellView NWellLPP list(left:top+0.13 right:top-0.13))
	imp1=list(Imp1)
	imp2=list(Imp2)
	imp3=list(Imp3)
	imp4=list(Imp4)
 	foreach( item imp1
		SetImpProp(item)
	 )	
 	foreach( item imp2
		SetImpProp(item)
	 )
 	foreach( item imp3
		SetImpProp(item)
	 )	
 	foreach( item imp4
		SetImpProp(item)
	 )

	)
	)


