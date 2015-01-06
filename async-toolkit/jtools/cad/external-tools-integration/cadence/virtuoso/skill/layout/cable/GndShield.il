(defvar Scratch1LPP ( list "y1" "drawing" ) )
(defvar Scratch2LPP ( list "y2" "drawing" ) )
(defvar Scratch3LPP ( list "y6" "drawing" ) )
(defvar Scratch4LPP ( list "y7" "drawing" ) )
(defvar Scratch5LPP ( list "y8" "drawing" ) )
(defvar Scratch6LPP ( list "y9" "drawing" ) )

(defun DrawGndShield ( CellView @key ( deleteScratch t) )
	(let ( INST bfr obstr overlapy5)
	bfr=0
	obstr=0
	overlapy5=nil

     foreach( shape CellView->shapes 
            if( shape->lpp==Scratch1LPP || shape->lpp==Scratch2LPP || shape->lpp==Scratch3LPP || shape->lpp==Scratch4LPP || shape->lpp==Scratch5LPP || shape->lpp==Scratch6LPP then dbDeleteObject(shape))
     )

  ; find PrBound
        PrBound=GetPrbound(CellView)
        if( PrBound->objType=="PRBoundary" then
          dbCreatePolygon(CellView Scratch4LPP PrBound->points)
        else
          leLayerOr(CellView list("prBoundary" "drawing") list("prBoundary" "drawing") Scratch4LPP)
        )
        prBounds=setof( shape CellView->shapes shape~>lpp==Scratch4LPP)

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

        cable_width=16
        foreach( shape CellView-> shapes
          if( shape->lpp==list("y5" "drawing") then
            cable_width=caadr(shape~>bBox)-caar(shape~>bBox)+1.0
          )
        )

	; Scratch3LPP = OBS + BUF CELLS (obstructions)
        BlockageShapes=setof( shape CellView->shapes shape~>lpp==list("OBS" "bus") || shape~>lpp==Scratch5LPP )
        foreach( shape BlockageShapes
          bx1=caar(shape~>bBox)
          by1=cadar(shape~>bBox)
          bx2=caadr(shape~>bBox)
          by2=cadadr(shape~>bBox)
          length=bx2-bx1 height=by2-by1
          if( length>cable_width then length=cable_width)
          if( height>cable_width then height=cable_width)
          bxx1=bx1-abs(cable_width-length) bxx2=bx2+abs(cable_width-length)
          byy1=by1-abs(cable_width-height) byy2=by2+abs(cable_width-height)
   	  Rect1=dbCreateRect(CellView Scratch3LPP list( bxx1:byy1 bxx2:byy2) )
          RectRet1=car(dbLayerAnd( CellView Scratch2LPP list(Rect1) prBounds ))
          RectRetBBox=RectRet1~>bBox
          r1=nil r2=nil
          if(caar(RectRetBBox)>bxx1+0.005 || caadr(RectRetBBox)<bxx2-0.005 then
            bx1=caar(RectRetBBox) bx2=caadr(RectRetBBox) r1=t
          )
          if(cadar(RectRetBBox)>byy1+0.005 || cadadr(RectRetBBox)<byy2-0.005 then
            by1=cadar(RectRetBBox) by2=cadadr(RectRetBBox) r2=t
          )
;          if( r1 && r2 printf("%L %L\n" list( bxx1:byy1 bxx2:byy2) RectRetBBox))
          dbDeleteObject(Rect1)
          dbDeleteObject(RectRet1)
	  dbCreateRect(CellView Scratch3LPP list( bx1:by1 bx2:by2) )
        )

        ; Scratch6LPP = PrBound - obstructions
        leLayerAndNot(CellView Scratch4LPP Scratch3LPP Scratch6LPP)

        ; y5 = corner square (no shields), Scratch1LPP = final shield areas
	leLayerAndNot(CellView Scratch6LPP list("y5" "drawing") Scratch1LPP)

        foreach(shape CellView-> shapes
	when(shape~>lpp==Scratch1LPP	
	(let (xy1 xy2 bx1 by1 bx2 by2 diffx diffy numx numy inst gridCV overlapM4 overlapM5)	
        bx1=caar(shape~>bBox)
        by1=cadar(shape~>bBox)
        bx2=caadr(shape~>bBox)
        by2=cadadr(shape~>bBox) 
        bx1=ceiling(bx1/3.12-0.001)*3.12
        by1=ceiling(by1/3.12-0.001)*3.12
        bx2=floor(bx2/3.12+0.001)*3.12
        by2=floor(by2/3.12+0.001)*3.12
        ; test if bBox still has area after snapping to 3.12 grid
        if(by2-by1>0.1 && bx2-bx1>0.1 then 
  	  overlapM2=dbGetTrueOverlaps(CellView list(bx1+0.005:by1+0.005 bx2-0.005:by2-0.025) list("M2" "drawing") 1)
	  overlapM3=dbGetTrueOverlaps(CellView list(bx1+0.005:by1+0.005 bx2-0.005:by2-0.025) list("M3" "drawing") 1)
	  overlapM4=dbGetTrueOverlaps(CellView list(bx1+0.005:by1+0.005 bx2-0.005:by2-0.025) list("M4" "drawing") 1)
	  overlapM5=dbGetTrueOverlaps(CellView list(bx1+0.005:by1+0.005 bx2-0.005:by2-0.025) list("M5" "drawing") 1)
	  overlapM6=dbGetTrueOverlaps(CellView list(bx1+0.005:by1+0.005 bx2-0.005:by2-0.025) list("M6" "drawing") 1)
	  overlapM7=dbGetTrueOverlaps(CellView list(bx1+0.005:by1+0.005 bx2-0.005:by2-0.025) list("M7" "drawing") 1)
	  when((overlapM2==nil)&&(overlapM4==nil)&&(overlapM6==nil)&&(overlapM3 !=nil||overlapM5 !=nil|| overlapM7 !=nil)
		;println("Vertical")
		diffx=bx2-bx1
		diffy=by2-by1
		numx=round(diffx/3.12)
		numy=round(diffy/3.12)
	        gridCV = (nrOpenCellViewReadable "globals" "globals.wires.VERTICAL_STRUTS_M246" "layout")
		if( numx>0 && numy>0 then dbCreateSimpleMosaic( CellView gridCV nil bx1:by1 "MXR90" numx numy 3.12 3.12))
		)
	  when((overlapM3==nil)&&(overlapM5==nil)&&(overlapM7==nil)&&(overlapM2 !=nil||overlapM4 !=nil|| overlapM6 !=nil)
		;println("Horizontal")
		diffx=bx2-bx1
		diffy=by2-by1
		numx=round(diffx/3.12)
		numy=round(diffy/3.12)
	        gridCV = (nrOpenCellViewReadable "globals" "globals.wires.HORIZONTAL_STRUTS_M357" "layout")
		if( numx>0 && numy>0 then dbCreateSimpleMosaic( CellView gridCV nil bx1:by1 "MXR90" numx numy 3.12 3.12))
		)	
	  when((overlapM2!=nil ||overlapM4!=nil || overlapM6!=nil )&&(overlapM3!=nil ||overlapM5!=nil || overlapM7!=nil)
		printf("WEIRD %L\n" list(bx1+0.005:by1+0.005 bx2-0.005:by2-0.005)))
	  when((overlapM2==nil)&&(overlapM3==nil)&&(overlapM4==nil)&&(overlapM5==nil)&&(overlapM6==nil)&&(overlapM7==nil)
		printf("STILL WEIRD %L\n" list(bx1+0.005:by1+0.005 bx2-0.005:by2-0.025)))
          )
	)
	))
        if( deleteScratch then
          foreach( shape CellView->shapes 
            if( shape->lpp==Scratch1LPP || shape->lpp==Scratch2LPP || shape->lpp==Scratch3LPP || shape->lpp==Scratch4LPP || shape->lpp==Scratch5LPP || shape->lpp==Scratch6LPP then dbDeleteObject(shape))
          )
        )
        t
))

defun( DeleteGndShield ( CellView )
  let( (shape) 
     foreach( shape CellView->shapes 
            if( shape->lpp==Scratch1LPP || shape->lpp==Scratch2LPP || shape->lpp==Scratch3LPP || shape->lpp==Scratch4LPP || shape->lpp==Scratch5LPP || shape->lpp==Scratch6LPP then dbDeleteObject(shape))
     )
     foreach( inst CellView->instances
       if( inst~>cellName=="globals.wires.HORIZONTAL_STRUTS_M357" || inst~>cellName == "globals.wires.VERTICAL_STRUTS_M246" then
         dbDeleteObject( inst )
       )
     )
  t
  )

)
