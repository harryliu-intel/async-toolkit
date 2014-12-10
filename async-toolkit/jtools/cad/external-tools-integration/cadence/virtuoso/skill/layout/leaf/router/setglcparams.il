; Copyright 2007 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

defun( SetGLCParams ( CellView glc )
  let( (instances inst )
    instances=cadr( NameFilterInstances( CellView~>instances 
                 append( GateLibCellPairRegExs StackLibCellPairRegExs )))
    foreach( inst instances
      dbReplaceProp(inst "glc" "boolean" glc)
    )
    dbSave( CellView )
  )
)

defun( GrowPrbound ( CellView size )
  let((prBoundShapes shape x1 x2 y1 y2)
    prBoundShapes= setof( shape CellView~>shapes shape~>layerName=="prBoundary" && shape~>objType=="rect")
    foreach(shape prBoundShapes
      x1=leftEdge(shape~>bBox)
      x2=rightEdge(shape~>bBox)
      y1=bottomEdge(shape~>bBox)
      y2=topEdge(shape~>bBox)
      shape~>bBox=list(x1:y1-size x2:y2+size)
    )
  )
)

defun( AddPrboundBoundary ( CellView size )
  ; add prbound boundary for PO to route within NP, PP
  let((prBoundShapes shape x1 x2 y1 y2)
    prBoundShapes= setof( shape CellView~>shapes shape~>layerName=="prBoundary" && shape~>purpose=="drawing" && shape~>objType=="rect")
    if( prBoundShapes then
      shape=car( prBoundShapes )
      x1=leftEdge(shape~>bBox)
      x2=rightEdge(shape~>bBox)
      y1=bottomEdge(shape~>bBox)
      y2=topEdge(shape~>bBox)
      dbCreateRect( CellView list("prBoundary" "boundary" )
        list(x1:y1-size x2:y2+size)
      )
    )
  )
)

defun( DeletePrboundBoundary ( CellView )
  let((prBoundShapes shape)
    prBoundShapes= setof( shape CellView~>shapes shape~>layerName=="prBoundary" && shape~>purpose=="boundary")
    foreach( shape prBoundShapes
      dbDeleteObject( shape )
    )  
  )
)
