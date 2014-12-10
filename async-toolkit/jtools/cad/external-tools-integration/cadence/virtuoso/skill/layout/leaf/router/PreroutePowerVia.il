; Copyright 2008 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

defun( PreroutePowerVia ( CellView )
  prog((prBoundShapes shape y1 y2 x1 x2 via23CellViewMaster M3GNDShapeBottom M3GNDShapeTop M2GNDShapes)
    prBoundShapes= setof( shape CellView~>shapes shape~>layerName=="prBoundary" && shape~>objType=="rect")
    shape=car(prBoundShapes)
    prboundBottom=bottomEdge(shape~>bBox)
    prboundTop=topEdge(shape~>bBox)
    M3GNDShapeBottom=car(setof( shape CellView~>shapes 
               shape~>lpp==Metal3LPP && 
               shape~>net~>name==GNDNetName && 
               bottomEdge(shape)-prboundBottom<0.01 ))
    M3GNDShapeTop=car(setof( shape CellView~>shapes 
               shape~>lpp==Metal3LPP && 
               shape~>net~>name==GNDNetName && 
               prboundTop-topEdge(shape)<0.01 ))
    M2GNDShapes=setof( shape CellView~>shapes shape~>lpp==Metal2LPP && shape~>net~>name==GNDNetName )
    via23CellViewMaster=nrOpenCellViewReadable( TechLibName "M3_M2_H" "layout") 
    if( M3GNDShapeBottom then 
      foreach( shape M2GNDShapes 
        y1=bottomEdge(shape~>bBox)
        y2=topEdge(M3GNDShapeBottom)
        if( y1<y2 then
          x1=leftEdge(shape)
          x2=rightEdge(shape)
          y1=prboundBottom+0.06
          shape~>bBox=list( x1:y1 x2:topEdge(shape) )
          printf("%f %f %f %f\n" x1 x2 y1 y2 )
   	  dbCreateParamInst(CellView via23CellViewMaster nil 
	    ((x1+x2)/2:(y1+y2)/2) "R0" 1 
	    list(
		    list("column" "int" 2) 
		    list("row" "int" 1) 
                    list( "layer1Direction" "string" "XY")
                    list( "layer1YEnclosure" "float" 0.01)
                    list( "layer1XEnclosure" "float" 0.04)
                    list( "layer2Direction" "string" "XY")
                    list( "layer2YEnclosure" "float" 0.01)         
                    list( "layer2XEnclosure" "float" 0.04)         
	    )
	  )
        )
      )
    )
    if( M3GNDShapeTop then 
      foreach( shape M2GNDShapes 
        y1=bottomEdge(M3GNDShapeTop)
        y2=topEdge(shape~>bBox)
        if( y1<y2 then
          x1=leftEdge(shape)
          x2=rightEdge(shape)
          y2=prboundTop-0.06
          shape~>bBox=list( x1:bottomEdge(shape) x2:y2 )
          printf("%f %f %f %f\n" x1 x2 y1 y2 )
   	  dbCreateParamInst(CellView via23CellViewMaster nil 
	    ((x1+x2)/2:(y1+y2)/2) "R0" 1 
	    list(
		    list("column" "int" 2) 
		    list("row" "int" 1) 
                    list( "layer1Direction" "string" "XY")
                    list( "layer1YEnclosure" "float" 0.01)
                    list( "layer1XEnclosure" "float" 0.04)
                    list( "layer2Direction" "string" "XY")
                    list( "layer2YEnclosure" "float" 0.01)         
                    list( "layer2XEnclosure" "float" 0.04)         
	    )
	  )
        )
      )
    )


  )
)
