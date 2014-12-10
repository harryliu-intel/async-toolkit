(defun MakeHVT (CellView ViewName)

;  dbCopyCellView(cv LibName newCellName ViewName)
;  ncv = nrOpenCellViewWritable( LibName newCellName ViewName )  
  vthn_shapes=setof( shape CellView->shapes 
    ( shape->lpp==NImplantLPP || shape->lpp==PImplantLPP  )&& 
      cond( ( shape->objType=="polygon" && shape->nPoints > 4 shape->lpp==NImplantLPP )
            (t dbGetTrueOverlaps(CellView list(leftEdge(shape->bBox)+0.005:bottomEdge(shape->bBox)+0.005 rightEdge(shape->bBox)-0.005:topEdge(shape->bBox)-0.005 ) NWellLPP 1)==nil )
      )
    )
  foreach( shape vthn_shapes
    newshape=dbCopyShape(shape CellView)
    dbSetq(newshape "VTH_N" layerName)
    SetImpProp(newshape) 
  )
  vthp_shapes=setof( shape CellView->shapes (shape->lpp==PImplantLPP || shape->lpp==NImplantLPP ) &&
      cond( (shape->objType=="polygon" && shape->nPoints > 4 shape->lpp==PImplantLPP )
            (t dbGetTrueOverlaps(CellView list(leftEdge(shape->bBox)+0.005:bottomEdge(shape->bBox)+0.005 rightEdge(shape->bBox)-0.005:topEdge(shape->bBox)-0.005 ) NWellLPP 1)!=nil )
       )
   )
  foreach( shape vthp_shapes
    newshape=dbCopyShape(shape CellView)
    dbSetq(newshape "VTH_P" layerName)
    SetImpProp(newshape) 
  )
    




  foreach(inst CellView->instances
	when( inst->libName=="gate"
    cell = inst->cellName
    pieces = parseString( cell "." )
    newcell = strcat( "gate" "." nth(1 pieces) "." "0-L3_3-R" )
		(leReplaceAnyInstMaster inst inst->libName newcell nil )	
 )
)

  foreach(inst CellView->instances
	when( inst->cellName=="M1_SUBmin"
	c=inst->bBox
	dbCreateRect(CellView "VTH_N" c)	
 )
)
  foreach(inst CellView->instances
	when( inst->cellName=="M1_SUB"
	c=inst->bBox
	dbCreateRect(CellView "VTH_N" c)	
 )
)
  foreach(inst CellView->instances
	when( inst->cellName=="M1_NWmin"
	c=inst->bBox
	dbCreateRect(CellView "VTH_P" c)	
 )
)

  foreach(inst CellView->instances
	when( inst->cellName=="M1_NW"
	c=inst->bBox
	dbCreateRect(CellView "VTH_P" c)	
 )
)

  foreach(inst CellView->instances
	when( inst->libName=="stack"
    cell = inst->cellName
    pieces = parseString( cell "." )
    newcell = strcat( "stack" "." nth(1 pieces) "." "0-L3-R" )
		(leReplaceAnyInstMaster inst inst->libName newcell nil )	
 )
)

   dbSave( CellView )
  dbClose( CellView )

)

(defun MakeHSVT (CellView ViewName)

           
  foreach(shape CellView->shapes
	when( shape->lpp==NImplantLPP
  newshape=dbCopyShape(shape CellView)
  dbSetq(newshape "VTH_N" layerName)
  SetImpProp(newshape) 
	    )
	)

  foreach(inst CellView->instances
	when( inst->libName=="gate"
    cell = inst->cellName
    pieces = parseString( cell "." )
    newcell = strcat( "gate" "." nth(1 pieces) "." "0-L3_1-R" )
		(leReplaceAnyInstMaster inst inst->libName newcell nil )	
 )
)

  foreach(inst CellView->instances
	when( inst->libName=="stack"
    cell = inst->cellName
    pieces = parseString( cell "." )
    x=nth(1 pieces)
    y = parseString(x "_")
    when(nth(0 y)=="NMOS"
    newcell = strcat( "stack" "." nth(1 pieces) "." "0-L3-R" )
		(leReplaceAnyInstMaster inst inst->libName newcell nil ))	
 )
)

   dbSave( CellView )
  dbClose( CellView )

)

(defun MakeHLVT (CellView ViewName)

           
  foreach(shape CellView->shapes
	when( shape->lpp==NImplantLPP
  newshape=dbCopyShape(shape CellView)
  dbSetq(newshape "VTH_N" layerName)
  SetImpProp(newshape) 
	    )
	when( shape->lpp==PImplantLPP
  newshape=dbCopyShape(shape CellView)
  dbSetq(newshape "VTL_P" layerName)
  SetImpProp(newshape)
	    )
	)

  foreach(inst CellView->instances
	when( inst->libName=="gate"
    cell = inst->cellName
    pieces = parseString( cell "." )
    newcell = strcat( "gate" "." nth(1 pieces) "." "0-L3_2-R" )
		(leReplaceAnyInstMaster inst inst->libName newcell nil )	
 )
)

  foreach(inst CellView->instances
	when( inst->libName=="stack"
    cell = inst->cellName
    pieces = parseString( cell "." )
    x=nth(1 pieces)
    y = parseString(x "_")
    when(nth(0 y)=="NMOS"
    newcell = strcat( "stack" "." nth(1 pieces) "." "0-L3-R" )
		(leReplaceAnyInstMaster inst inst->libName newcell nil ))	
    when(nth(0 y)=="PMOS"
    newcell = strcat( "stack" "." nth(1 pieces) "." "0-L2-R" )
		(leReplaceAnyInstMaster inst inst->libName newcell nil ))	
 )
)

   dbSave( CellView )
  dbClose( CellView )

)

(defun MakeLHVT (CellView ViewName)

           
  foreach(shape CellView->shapes
	when( shape->lpp==NImplantLPP
  newshape=dbCopyShape(shape CellView)
  dbSetq(newshape "VTL_N" layerName)
  SetImpProp(newshape) 
	    )
	when( shape->lpp==PImplantLPP
  newshape=dbCopyShape(shape CellView)
  dbSetq(newshape "VTH_P" layerName)
  SetImpProp(newshape)
	    )
	)

  foreach(inst CellView->instances
	when( inst->libName=="gate"
    cell = inst->cellName
    pieces = parseString( cell "." )
    newcell = strcat( "gate" "." nth(1 pieces) "." "0-L2_3-R" )
		(leReplaceAnyInstMaster inst inst->libName newcell nil )	
 )
)

  foreach(inst CellView->instances
	when( inst->libName=="stack"
    cell = inst->cellName
    pieces = parseString( cell "." )
    x=nth(1 pieces)
    y = parseString(x "_")
    when(nth(0 y)=="NMOS"
    newcell = strcat( "stack" "." nth(1 pieces) "." "0-L2-R" )
		(leReplaceAnyInstMaster inst inst->libName newcell nil ))	
    when(nth(0 y)=="PMOS"
    newcell = strcat( "stack" "." nth(1 pieces) "." "0-L3-R" )
		(leReplaceAnyInstMaster inst inst->libName newcell nil ))	
 )
)

   dbSave( CellView )
  dbClose( CellView )

)

(defun MakeLSVT (CellView ViewName)

           
  foreach(shape CellView->shapes
	when( shape->lpp==NImplantLPP
  newshape=dbCopyShape(shape CellView)
  dbSetq(newshape "VTL_N" layerName)
  SetImpProp(newshape) 
	    )
	)

  foreach(inst CellView->instances
	when( inst->libName=="gate"
    cell = inst->cellName
    pieces = parseString( cell "." )
    newcell = strcat( "gate" "." nth(1 pieces) "." "0-L2_1-R" )
		(leReplaceAnyInstMaster inst inst->libName newcell nil )	
 )
)

  foreach(inst CellView->instances
	when( inst->libName=="stack"
    cell = inst->cellName
    pieces = parseString( cell "." )
    x=nth(1 pieces)
    y = parseString(x "_")
    when(nth(0 y)=="NMOS"
    newcell = strcat( "stack" "." nth(1 pieces) "." "0-L2-R" )
		(leReplaceAnyInstMaster inst inst->libName newcell nil ))	
 )
)

   dbSave( CellView )
  dbClose( CellView )

)


(defun MakeSHVT (CellView ViewName)

           
  foreach(shape CellView->shapes
	when( shape->lpp==PImplantLPP
  newshape=dbCopyShape(shape CellView)
  dbSetq(newshape "VTH_P" layerName)
  SetImpProp(newshape) 
	    )
	)

  foreach(inst CellView->instances
	when( inst->libName=="gate"
    cell = inst->cellName
    pieces = parseString( cell "." )
    newcell = strcat( "gate" "." nth(1 pieces) "." "0-L1_3-R" )
		(leReplaceAnyInstMaster inst inst->libName newcell nil )	
 )
)

  foreach(inst CellView->instances
	when( inst->libName=="stack"
    cell = inst->cellName
    pieces = parseString( cell "." )
    x=nth(1 pieces)
    y = parseString(x "_")
    when(nth(0 y)=="PMOS"
    newcell = strcat( "stack" "." nth(1 pieces) "." "0-L3-R" )
		(leReplaceAnyInstMaster inst inst->libName newcell nil ))	
 )
)

   dbSave( CellView )
  dbClose( CellView )

)

(defun MakeSLVT (CellView ViewName)

           
  foreach(shape CellView->shapes
	when( shape->lpp==PImplantLPP
  newshape=dbCopyShape(shape CellView)
  dbSetq(newshape "VTL_P" layerName)
  SetImpProp(newshape) 
	    )
	)

  foreach(inst CellView->instances
	when( inst->libName=="gate"
    cell = inst->cellName
    pieces = parseString( cell "." )
    newcell = strcat( "gate" "." nth(1 pieces) "." "0-L1_2-R" )
		(leReplaceAnyInstMaster inst inst->libName newcell nil )	
 )
)

  foreach(inst CellView->instances
	when( inst->libName=="stack"
    cell = inst->cellName
    pieces = parseString( cell "." )
    x=nth(1 pieces)
    y = parseString(x "_")
    when(nth(0 y)=="PMOS"
    newcell = strcat( "stack" "." nth(1 pieces) "." "0-L2-R" )
		(leReplaceAnyInstMaster inst inst->libName newcell nil ))	
 )
)

   dbSave( CellView )
  dbClose( CellView )

)


(defun MakeLVT (CellView ViewName)
           
  foreach(shape CellView->shapes
	when( shape->lpp==NImplantLPP
  newshape=dbCopyShape(shape CellView)
  dbSetq(newshape "VTL_N" layerName)
  SetImpProp(newshape) 
	    )
	when( shape->lpp==PImplantLPP
  newshape=dbCopyShape(shape CellView)
  dbSetq(newshape "VTL_P" layerName)
  SetImpProp(newshape)
	    )
	)

  foreach(inst CellView->instances
	when( inst->libName=="gate"
    cell = inst->cellName
    pieces = parseString( cell "." )
    newcell = strcat( "gate" "." nth(1 pieces) "." "0-L2_2-R" )
		(leReplaceAnyInstMaster inst inst->libName newcell nil )	
 )
)

  foreach(inst CellView->instances
	when( inst->libName=="stack"
    cell = inst->cellName
    pieces = parseString( cell "." )
    newcell = strcat( "stack" "." nth(1 pieces) "." "0-L2-R" )
		(leReplaceAnyInstMaster inst inst->libName newcell nil )	
 )
)

   dbSave( CellView )
  dbClose( CellView )

)

(defun MakeSVT (CellView ViewName)

  foreach(shape CellView->shapes
	when( shape->lpp==NVtlLPP
  newshape=dbCopyShape(shape CellView)
  dbSetq(newshape "NP" layerName)
  DeleteImpl() 
  SetImpProp(newshape) 
	    )
	when( shape->lpp==NVhtLPP
  newshape=dbCopyShape(shape CellView)
  dbSetq(newshape "NP" layerName)
  DeleteImpl() 
  SetImpProp(newshape) 
	    )
	when( shape->lpp==PVtlLPP
  newshape=dbCopyShape(shape CellView)
  dbSetq(newshape "PP" layerName)
  DeleteImpl() 
  SetImpProp(newshape)
	    )
	when( shape->lpp==PVhtLPP
  newshape=dbCopyShape(shape CellView)
  dbSetq(newshape "PP" layerName)
  DeleteImpl() 
  SetImpProp(newshape)
	    )
	)

  foreach(inst CellView->instances
	when( inst->libName=="gate"
    cell = inst->cellName
    pieces = parseString( cell "." )
    newcell = strcat( "gate" "." nth(1 pieces) "." "0-L1_1-R" )
		(leReplaceAnyInstMaster inst inst->libName newcell nil )	
 )
)

  foreach(inst CellView->instances
	when( inst->libName=="stack"
    cell = inst->cellName
    pieces = parseString( cell "." )
    newcell = strcat( "stack" "." nth(1 pieces) "." "0-L1-R" )
		(leReplaceAnyInstMaster inst inst->libName newcell nil )	
 )
)

   dbSave( CellView )
  dbClose( CellView )

)

; defining a implant property
(defun SetImpProp (obj) 
	 (when obj
		(dbReplaceProp obj "impl" "boolean" t)	        	
	 )
)
                                                 
(defun IsImpObject (obj)
	  (let (isImpl)
	    	isImpl = nil
		(when obj
	    	  (when (dbGetPropByName obj "impl")->value=="TRUE"
		   isImpl=t
	     	  )
		)
	   	isImpl

	  )
)



;deletes implants

(defun DeleteImpl () 
  	   (let (view shapes)
   		 view = (geGetEditCellView)
   		 shapes = (setof obj ncv->shapes (IsImpObject obj))
  		 (foreach shape shapes (dbDeleteObject shape))
  	   )
 	 t
 )

(defun DELCell (CellName ViewName)
	let((shape inst cv ncv mcv lcv newCellName pieces LibName) 
	pieces=parseString( CellName "." )
        LibName=nth( 0 pieces )
        for( n 1 length(pieces)-3
            LibName = strcat( LibName "." nth( n pieces )))
        cv=nrOpenCellViewReadable( LibName CellName ViewName )
        newCellName = strcat(CellName "_hvt") 
	mcv=nrOpenCellViewReadable( LibName newCellName ViewName)
	lcv=nrOpenCellViewWritable( LibName newCellName ViewName)
        when( cv == nil
            printf( "Cannot open %s\n" CellName)
            )
        when( mcv == nil
            printf( "Cannot open HVT subtype %s\n" CellName)
            )
        when( lcv == nil
            printf( "HVT subtype already checked in %s\n" CellName)
            )
         when( ((cv != nil)&&(mcv != nil)&&(lcv != nil))
;	foreach(shape mcv->shapes (dbDeleteObject shape))
;	foreach(inst cv->instances (dbDeleteObject inst))
        dbCopyCellView(cv LibName newCellName ViewName nil nil t)
            ncv = nrOpenCellViewWritable( LibName newCellName ViewName )
            MakeHVT( ncv ViewName)
)))
     					      
(defun MakeHVTCell ( CellName ViewName )
    let( ( k l m n y s z ce  cel piecess piecesss cv ncv mcv newCellName pieces LibName )
        pieces=parseString( CellName "." )
        LibName=nth( 0 pieces )
	k="0"
	l="0"
	m="0"
	n="0"
        for( n 1 length(pieces)-3
            LibName = strcat( LibName "." nth( n pieces )))
        cv=nrOpenCellViewReadable( LibName CellName ViewName )
        newCellName = strcat(CellName "_hvt") 
	mcv=nrOpenCellViewReadable( LibName newCellName ViewName)
	foreach(x cv->shapes
	when(((x->lpp==PVtlLPP)||(x->lpp==NVtlLPP))
		k="1"))
	foreach(y cv->instances
	when((y->libName=="gate")
	ce = y->cellName
	piecess = parseString(ce ".")
	when(((nth(2 piecess)=="0-L2_2-R")||(nth(2 piecess)=="0-L2_1-R")||(nth(2 piecess)=="0-L1_2-R")||(nth(2 piecess)=="0-L1_3-R")||(nth(2 piecess)=="0-L3_1-R"))
		l="1")))
	foreach(z cv->instances
	when((z->libName=="stack")
	cel = z->cellName
	piecesss = parseString(cel ".")
	when((nth(2 piecesss)=="0-L2-R")
		m="1"	)))
        when( cv == nil
            printf( "Cannot open %s\n" CellName)
            )
	when( mcv != nil
	    printf("HVT subtype already exists %s\n" newCellName))
	when( ((k=="1")||(l=="1")||(m=="1"))
		printf("Cannot open %s. Cell has both lvt and hvt instances \n" CellName))
         when( ((cv != nil)&&(mcv == nil)&&(k!="1")&&(l!="1")&&(m!="1"))
            printf("yes\n")
            dbCopyCellView(cv LibName newCellName ViewName nil nil t)
            ncv = nrOpenCellViewWritable( LibName newCellName ViewName )
            MakeHVT( ncv ViewName)
            )
    )
 )

(defun MakeHSVTCell ( CellName ViewName )
    let( ( cv ncv newCellName pieces LibName )
        pieces=parseString( CellName "." )
        LibName=nth( 0 pieces )
        for( n 1 length(pieces)-3
            LibName = strcat( LibName "." nth( n pieces )))
        cv=nrOpenCellViewReadable( LibName CellName ViewName )
        newCellName = strcat(CellName "_hsvt") 
        when( cv == nil
            printf( "Cannot open %s\n" CellName)
            )
         when( cv != nil
            printf("yes\n")
            dbCopyCellView(cv LibName newCellName ViewName nil nil t)
            ncv = nrOpenCellViewWritable( LibName newCellName ViewName )
            MakeHSVT( ncv ViewName)
            )
    )
 )

(defun MakeSHVTCell ( CellName ViewName )
    let( ( cv ncv newCellName pieces LibName )
        pieces=parseString( CellName "." )
        LibName=nth( 0 pieces )
        for( n 1 length(pieces)-3
            LibName = strcat( LibName "." nth( n pieces )))
        cv=nrOpenCellViewReadable( LibName CellName ViewName )
        newCellName = strcat(CellName "_shvt") 
        when( cv == nil
            printf( "Cannot open %s\n" CellName)
            )
         when( cv != nil
            printf("yes\n")
            dbCopyCellView(cv LibName newCellName ViewName nil nil t)
            ncv = nrOpenCellViewWritable( LibName newCellName ViewName )
            MakeSHVT( ncv ViewName)
            )
    )
 )

(defun MakeSLVTCell ( CellName ViewName )
    let( ( cv ncv newCellName pieces LibName )
        pieces=parseString( CellName "." )
        LibName=nth( 0 pieces )
        for( n 1 length(pieces)-3
            LibName = strcat( LibName "." nth( n pieces )))
        cv=nrOpenCellViewReadable( LibName CellName ViewName )
        newCellName = strcat(CellName "_slvt") 
        when( cv == nil
            printf( "Cannot open %s\n" CellName)
            )
         when( cv != nil
            printf("yes\n")
            dbCopyCellView(cv LibName newCellName ViewName nil nil t)
            ncv = nrOpenCellViewWritable( LibName newCellName ViewName )
            MakeSLVT( ncv ViewName)
            )
    )
 )

(defun MakeLSVTCell ( CellName ViewName )
    let( ( cv ncv newCellName pieces LibName )
        pieces=parseString( CellName "." )
        LibName=nth( 0 pieces )
        for( n 1 length(pieces)-3
            LibName = strcat( LibName "." nth( n pieces )))
        cv=nrOpenCellViewReadable( LibName CellName ViewName )
        newCellName = strcat(CellName "_lsvt") 
        when( cv == nil
            printf( "Cannot open %s\n" CellName)
            )
         when( cv != nil
            printf("yes\n")
            dbCopyCellView(cv LibName newCellName ViewName nil nil t)
            ncv = nrOpenCellViewWritable( LibName newCellName ViewName )
            MakeLSVT( ncv ViewName)
            )
    )
 )

(defun MakeHLVTCell ( CellName ViewName )
    let( ( cv ncv newCellName pieces LibName )
        pieces=parseString( CellName "." )
        LibName=nth( 0 pieces )
        for( n 1 length(pieces)-3
            LibName = strcat( LibName "." nth( n pieces )))
        cv=nrOpenCellViewReadable( LibName CellName ViewName )
        newCellName = strcat(CellName "_hlvt") 
        when( cv == nil
            printf( "Cannot open %s\n" CellName)
            )
         when( cv != nil
            printf("yes\n")
            dbCopyCellView(cv LibName newCellName ViewName nil nil t)
            ncv = nrOpenCellViewWritable( LibName newCellName ViewName )
            MakeHLVT( ncv ViewName)
            )
    )
 )

(defun MakeLHVTCell ( CellName ViewName )
    let( ( cv ncv newCellName pieces LibName )
        pieces=parseString( CellName "." )
        LibName=nth( 0 pieces )
        for( n 1 length(pieces)-3
            LibName = strcat( LibName "." nth( n pieces )))
        cv=nrOpenCellViewReadable( LibName CellName ViewName )
        newCellName = strcat(CellName "_lhvt") 
        when( cv == nil
            printf( "Cannot open %s\n" CellName)
            )
         when( cv != nil
            printf("yes\n")
            dbCopyCellView(cv LibName newCellName ViewName nil nil t)
            ncv = nrOpenCellViewWritable( LibName newCellName ViewName )
            MakeLHVT( ncv ViewName)
            )
    )
 )

(defun MakeLVTCell ( CellName ViewName )
    let( ( k l y  z ce cv cel piecess piecesss ncv newCellName pieces LibName )
        pieces=parseString( CellName "." )
        LibName=nth( 0 pieces )
	k="0"
	l="0"
	m="0"
        for( n 1 length(pieces)-3
            LibName = strcat( LibName "." nth( n pieces )))
        cv=nrOpenCellViewReadable( LibName CellName ViewName )
        newCellName = strcat(CellName "_lvt") 
	foreach(x cv->shapes
	when(((x->lpp==PVhtLPP)||(x->lpp==NVhtLPP))
		k="1"))
	foreach(y cv->instances
	when((y->libName=="gate")
	ce = y->cellName
	piecess = parseString(ce ".")
	when(((nth(2 piecess)=="0-L3_3-R")||(nth(2 piecess)=="0-L3_1-R")||(nth(2 piecess)=="0-L1_3-R")||(nth(2 piecess)=="0-L1_2-R")||(nth(2 piecess)=="0-L2_1-R"))
		l="1")))
	foreach(z cv->instances
	when((z->libName=="stack")
	cel = z->cellName
	piecesss = parseString(cel ".")
	when((nth(2 piecesss)=="0-L3-R")
		m="1"	)))
        when( cv == nil
            printf( "Cannot open %s\n" CellName)
            )
	when( ((k=="1")||(l=="1")||(m=="1"))
		printf("Cannot open %s. Cell has both lvt and hvt instances \n" CellName))
         when( ((cv != nil)&&(k!="1")&&(l!="1")&&(m!="1"))
            printf("yes\n")
            dbCopyCellView(cv LibName newCellName ViewName nil nil t)
            ncv = nrOpenCellViewWritable( LibName newCellName ViewName )
            MakeLVT( ncv ViewName)
            )
    )
 )

(defun MakeSVTCell ( CellName ViewName )
    let( ( cv ncv newCellName pieces LibName CName)
        pieces=parseString( CellName "." )
	CNames=parseString( CellName "_" )
	CName=nth( 0 CNames )
	for( n 1 length(CNames)-2
	CName = strcat( CName "_" nth( n CNames)))
        LibName=nth( 0 pieces )
        for( n 1 length(pieces)-3
            LibName = strcat( LibName "." nth( n pieces )))
        cv=nrOpenCellViewReadable( LibName CellName ViewName )
        newCellName = strcat(CName "_svt") 
        when( cv == nil
            printf( "Cannot open %s\n" CellName)
            )
         when( cv != nil
            printf("yes\n")
            dbCopyCellView(cv LibName newCellName ViewName nil nil t)
            ncv = nrOpenCellViewWritable( LibName newCellName ViewName )
            MakeSVT( ncv ViewName)
            )
    )
 )

(defun hvt ()
	(let (text CellName)
	fl = infile("list_1.txt")
	while( gets(k fl) != nil
  	text = parseString( k " \n") 
  	CellName = nth(0 text)
  	MakeHVTCell(CellName "layout")
)))

(defun hvtdel ()
	(let (text CellName)
	fl = infile("list_1.txt")
	while( gets(k fl) != nil
  	text = parseString( k " \n") 
  	CellName = nth(0 text)
  	DELCell(CellName "layout")
)))

(defun hlvt ()
	(let (text CellName)
	fl = infile("/home/user/vjain/alpine/MakeVTH.list")
	while( gets(k fl) != nil
  	text = parseString( k "\n") 
  	CellName = nth(0 text)
  	MakeHLVTCell(CellName "layout")
)))

(defun lhvt ()
	(let (text CellName)
	fl = infile("/home/user/vjain/alpine/MakeVTH.list")
	while( gets(k fl) != nil
  	text = parseString( k "\n") 
  	CellName = nth(0 text)
  	MakeLHVTCell(CellName "layout")
)))

(defun shvt ()
	(let (text CellName)
	fl = infile("/home/user/vjain/alpine/MakeVTH.list")
	while( gets(k fl) != nil
  	text = parseString( k "\n") 
  	CellName = nth(0 text)
  	MakeSHVTCell(CellName "layout")
)))

(defun hsvt ()
	(let (text CellName)
	fl = infile("/home/user/vjain/alpine/MakeVTH.list")
	while( gets(k fl) != nil
  	text = parseString( k "\n") 
  	CellName = nth(0 text)
  	MakeHSVTCell(CellName "layout")
)))

(defun lsvt ()
	(let (text CellName)
	fl = infile("/home/user/vjain/alpine/MakeVTH.list")
	while( gets(k fl) != nil
  	text = parseString( k "\n") 
  	CellName = nth(0 text)
  	MakeLSVTCell(CellName "layout")
)))

(defun slvt ()
	(let (text CellName)
	fl = infile("/home/user/vjain/alpine/MakeVTH.list")
	while( gets(k fl) != nil
  	text = parseString( k "\n") 
  	CellName = nth(0 text)
  	MakeSLVTCell(CellName "layout")
)))

(defun lvt ()
	(let (text CellName)
	fl = infile("/home/user/vjain/alpine/MakeVTH.list")
	while( gets(k fl) != nil
  	text = parseString( k "\n") 
  	CellName = nth(0 text)
  	MakeLVTCell(CellName "layout")
)))

(defun svt ()
	(let (text CellName)
	fl = infile("/home/user/vjain/alpine/MakeVTH.list")
	while( gets(k fl) != nil
  	text = parseString( k "\n") 
  	CellName = nth(0 text)
  	MakeSVTCell(CellName "layout")
)))


