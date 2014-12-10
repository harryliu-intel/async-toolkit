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
        when((LibName=="standard") || (LibName=="lib")
	dbCreateRect(CellView Scratch5LPP list( bx1:by1 bx2:by2) )
	))
	)
        foreach(shape CellView-> shapes
	when( (cadr(shape->lpp)== "bus") && (shape~>BufferHops!= nil)
	CreateCableBoundary(?CableShape shape)))
        foreach(shape CellView-> shapes
	when((shape~>lpp==Scratch5LPP)||(car(shape~>lpp)=="OBS")
	when(shape~>lpp==Scratch5LPP
	leLayerXor(CellView list("y8" "drawing") list("prBoundary" "drawing") list("y6" "drawing"))
	bfr=1)
	when(car(shape~>lpp)=="OBS"
	leLayerXor(CellView list("OBS" "bus") list("prBoundary" "drawing") list("y7" "drawing"))
	obstr=1)
	))	
	when((obstr==1)&&(bfr==1)
	leLayerAnd(CellView list("y7" "drawing") list("y6" "drawing") list("y9" "drawing")))
	when((obstr==1)&&(bfr==0)
	leLayerAnd(CellView list("y7" "drawing") list("prBoundary" "drawing") list("y9" "drawing")))
	when((obstr==0)&&(bfr==1)
	leLayerAnd(CellView list("y6" "drawing") list("prBoundary" "drawing") list("y9" "drawing")))
        foreach(shape CellView-> shapes
	(let (xy1 xy2 bx1 by1 bx2 by2)
	when(shape~>lpp==Scratch6LPP
        bx1=caar(shape~>bBox)
        by1=cadar(shape~>bBox)
        bx2=caadr(shape~>bBox)
        by2=cadadr(shape~>bBox)
	overlapy5=dbGetTrueOverlaps(CellView list(bx1+0.005:by1+0.005 bx2-0.005:by2-0.025) list("y5" "drawing") 1)
	when(overlapy5==nil
	dbCreateRect(CellView Scratch1LPP list(bx1:by1 bx2:by2)))
	;leLayerAnd(CellView list("y9" "drawing") list("prBoundary" "drawing") list("y1" "drawing")))
	when(overlapy5!=nil
	leLayerXor(CellView list("y9" "drawing") list("y5" "drawing") list("y1" "drawing"))
	overlapy5==nil)
	)
	))
        foreach(shape CellView-> shapes
	when(shape~>lpp==Scratch1LPP	
	(let (xy1 xy2 bx1 by1 bx2 by2 diffx diffy numx numy inst gridCV overlapM4 overlapM5)	
        bx1=caar(shape~>bBox)
        by1=cadar(shape~>bBox)
        bx2=caadr(shape~>bBox)
        by2=cadadr(shape~>bBox)
	overlapM4=nil
	overlapM5=nil
	overlapM4=dbGetTrueOverlaps(CellView list(bx1+0.005:by1+0.005 bx2-0.005:by2-0.025) list("M4" "drawing") 1)
	overlapM5=dbGetTrueOverlaps(CellView list(bx1+0.005:by1+0.005 bx2-0.005:by2-0.025) list("M5" "drawing") 1)
	when((overlapM4!=nil)&&(overlapM5==nil)
		;println("Vertical")
		diffx=bx2-bx1
		diffy=by2-by1
		numx=round(diffx/3.12)
		numy=round(diffy/3.12)
	        gridCV = (nrOpenCellViewReadable "globals" "globals.wires.HORIZONTAL_STRUTS_M357" "layout")
		dbCreateSimpleMosaic( CellView gridCV nil bx1:by1 "R0" numy numx 3.12 3.12)
		)
	when((overlapM4==nil)&&(overlapM5!=nil)
		;println("Horizontal")
		diffx=bx2-bx1
		diffy=by2-by1
		numx=round(diffx/3.12)
		numy=round(diffy/3.12)
	        gridCV = (nrOpenCellViewReadable "globals" "globals.wires.VERTICAL_STRUTS_M246" "layout")
		dbCreateSimpleMosaic( CellView gridCV nil bx1:by1 "R0" numy numx 3.12 3.12)
		)	
	when((overlapM4!=nil)&&(overlapM5!=nil)
		println("WEIRD"))
	when((overlapM4==nil)&&(overlapM5==nil)
		println("STILL WEIRD"))
	)
	))
))
