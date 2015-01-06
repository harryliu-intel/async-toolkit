(defvar Scratch1LPP ( list "y1" "drawing" ) )
(defvar Scratch3LPP ( list "y6" "drawing" ) )
(defvar Scratch4LPP ( list "y7" "drawing" ) )
(defvar Scratch5LPP ( list "y8" "drawing" ) )
(defvar Scratch6LPP ( list "y9" "drawing" ) )

(defun DrawGndSheild ()
	(let (CellView INST bfr obstr overlapy5)
	CellView=geGetEditCellView()
	bfr=0
	obstr=0
	overlapy5=nil
  ;locating instances like MBUFS and RESET cells
   	foreach(INST CellView->instances		
	(let (xy1 xy2 bx1 by1 bx2 by2 Instname pieces piecess LibName cbls)
        xy1=car(INST~>xy)
        xy2=cadr(INST~>xy)   
        bx1=caar(INST~>bBox)
        by1=cadar(INST~>bBox)
        bx2=caadr(INST~>bBox)
        by2=cadadr(INST~>bBox)
        Instname=INST->cellName
            pieces=parseString( Instname "." )
            LibName=nth( 0 pieces )
        when((LibName=="standard") || (LibName=="lib") || (Instname=="chip.rrc.util.sync.SBUF_A12.1000b") || (Instname=="chip.rrc.util.sync.SBUF_A12.1000a")
	dbCreateRect(CellView Scratch5LPP list( bx1:by1 bx2:by2) )
	))
	)
        foreach(shape CellView-> shapes
	when( (cadr(shape->lpp)== "bus") && (shape~>BufferHops!= nil)
	CreateCableBoundary(?CableShape shape)))

        leLayerOr(CellView Scratch5LPP list("OBS" "bus") Scratch3LPP )
        leLayerAndNot(CellView list("prBoundary" "drawing") Scratch3LPP Scratch6LPP) 
	leLayerAndNot(CellView Scratch6LPP list("y5" "drawing") Scratch1LPP)

        foreach(shape CellView-> shapes
	when(shape~>lpp==Scratch1LPP	
	(let (xy1 xy2 bx1 by1 bx2 by2 diffx diffy numx numy inst gridCV overlapM4 overlapM5)	
        bx1=caar(shape~>bBox)
        by1=cadar(shape~>bBox)
        bx2=caadr(shape~>bBox)
        by2=cadadr(shape~>bBox)
	overlapM4=dbGetTrueOverlaps(CellView list(bx1+0.005:by1+0.005 bx2-0.005:by2-0.025) list("M4" "drawing") 1)
	overlapM5=dbGetTrueOverlaps(CellView list(bx1+0.005:by1+0.005 bx2-0.005:by2-0.025) list("M5" "drawing") 1)
	overlapM6=dbGetTrueOverlaps(CellView list(bx1+0.005:by1+0.005 bx2-0.005:by2-0.025) list("M6" "drawing") 1)
	overlapM7=dbGetTrueOverlaps(CellView list(bx1+0.005:by1+0.005 bx2-0.005:by2-0.025) list("M7" "drawing") 1)
	when((overlapM4==nil)&&(overlapM6==nil)&&(overlapM5 !=nil|| overlapM7 !=nil)
		;println("Vertical")
		diffx=bx2-bx1
		diffy=by2-by1
		numx=round(diffx/3.12)
		numy=round(diffy/3.12)
	        gridCV = (nrOpenCellViewReadable "globals" "globals.wires.VERTICAL_STRUTS_M246" "layout")
		if( numx>0 && numy>0 then dbCreateSimpleMosaic( CellView gridCV nil bx1:by1 "MXR90" numx numy 3.12 3.12))
		)
	when((overlapM5==nil)&&(overlapM7==nil)&&(overlapM4 !=nil|| overlapM6 !=nil)
		;println("Horizontal")
		diffx=bx2-bx1
		diffy=by2-by1
		numx=round(diffx/3.12)
		numy=round(diffy/3.12)
	        gridCV = (nrOpenCellViewReadable "globals" "globals.wires.HORIZONTAL_STRUTS_M357" "layout")
		if( numx>0 && numy>0 then dbCreateSimpleMosaic( CellView gridCV nil bx1:by1 "MXR90" numx numy 3.12 3.12))
		)	
	when((overlapM4!=nil || overlapM6!=nil )&&(overlapM5!=nil || overlapM7!=nil)
		printf("WEIRD %L\n" list(bx1+0.005:by1+0.005 bx2-0.005:by2-0.025)))
	when((overlapM4==nil)&&(overlapM5==nil)&&(overlapM6==nil)&&(overlapM7==nil)
		printf("STILL WEIRD %L\n" list(bx1+0.005:by1+0.005 bx2-0.005:by2-0.025)))
	)
	))
        foreach( shape CellView->shapes 
          if( shape->lpp==Scratch1LPP || shape->lpp==Scratch3LPP || shape->lpp==Scratch4LPP || shape->lpp==Scratch5LPP || shape->lpp==Scratch6LPP then dbDeleteObject(shape))
        )
        t
))
