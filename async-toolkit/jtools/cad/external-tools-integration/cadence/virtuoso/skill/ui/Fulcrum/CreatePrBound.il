; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id: //depot/sw/cad/external-tools-integration/cadence/virtuoso/main/skill/ui/Fulcrum/CreateCDL.il#11 $
; $DateTime: 2004/08/21 16:39:54 $
; $Author: clayton $


( list "Create prBound" "UICreatePrBound" )

( UICreateMenuItemCallBack 
  `UICreatePrBoundCB
  (lambda ()
    (let (
          ( CellView ( UIGetCellView ) ) 
          lpps
          bBox
          x1 x2 y1 y2
          Grid
         )
      Grid=ManufacturingGrid*MicronsPerMeter
      bBox=CellView~>bBox
      x1=min(caar(bBox) caadr(bBox))
      x2=max(caar(bBox) caadr(bBox))
      y1=min(cadar(bBox) cadadr(bBox))
      y2=max(cadar(bBox) cadadr(bBox))
      x1=floor(x1/Grid)*Grid
      y1=floor(y1/Grid)*Grid
      x2=ceiling(x2/Grid)*Grid
      y2=ceiling(y2/Grid)*Grid
      bBox=list( x1:y1 x2:y2 )
      lpps=setof( lpp CellView~>lpps lpp~>layerName==car(BoundaryLPP) )
      foreach( shape CellView->shapes
        when( shape->lpp==BoundaryLPP
          dbDeleteObject( shape )
        )
      )
      llcorner=car(CellView->bBox)
      urcorner=cadr(CellView->bBox)
      ulcorner=(list car(llcorner) cadr(urcorner)  )
      lrcorner=(list car(urcorner) cadr(llcorner)  )         
      dbCreatePRBoundary( CellView list(llcorner lrcorner urcorner ulcorner) )
					 	        
      dbSave( CellView )
    )
  )
)

