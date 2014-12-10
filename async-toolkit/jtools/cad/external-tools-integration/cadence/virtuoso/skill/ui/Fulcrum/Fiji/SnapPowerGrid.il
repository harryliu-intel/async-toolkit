; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id: //depot/sw/cad/external-tools-integration/cadence/virtuoso/main/skill/ui/Fulcrum/Pins/PreroutePins.il#26 $
; $DateTime: 2005/06/17 14:39:47 $
; $Author: khe $

( list "Snap Leaf M3 Power Grid" "UISnapPowerGrid" )

( UICreateMenuItemCallBack 
  `UISnapPowerGridCB
  (lambda ()
    let( (CellView)
      CellView= UIGetCellView()
      if( nrOpenCellViewWritable(CellView~>libName CellView~>cellName CellView~>viewName) then 
        SnapM3PowerGrid( CellView ) 
      else
        printf("Unable to open CellView for write.\n")
      )
    )
) )

        
defun( SnapM3PowerGrid ( CellView )
  let((n lpp prbound shape w1 w2 h1 h2 x1 x2 y1 y2 m3power m3shape m3label move)
    prbound=car( car( setof( lpp CellView~>lpps lpp~>layerName=="prBoundary"))~>shapes)
    w1=leftEdge(prbound~>bBox)
    w2=rightEdge(prbound~>bBox)
    heigth=topEdge(prbound~>bBox)
    for( n 0 floor(heigth/2.88)
      h1=n*2.88-0.18 h2=n*2.88+0.18
      bBox=list(w1:h1 w2:h2)
      m3power=dbGetOverlaps( CellView bBox Metal3LPP)
      m3label=car(setof( shape m3power shape~>objType=="label" ))
      m3shape=setof( shape m3power shape~>objType!="label" )
      if( length(m3shape)==1 then 
        shape=car(m3shape)
        x1=leftEdge(shape~>bBox)
        x2=rightEdge(shape~>bBox)
        y1=bottomEdge(shape~>bBox)
        y2=topEdge(shape~>bBox)
        move=h1-y1
        if( abs(h1-y1)<abs(h2-y2) then move=h2-y2 )
        if(abs(x1-w1)>1e-6 || abs(x2-w2)>1e-6 || abs(y1-h1)>1e-6 || abs(y2-h2)>1e-6 then
          if( shape~>objType=="rect" then 
            via2shape=dbGetOverlaps( CellView shape~>bBox Via23LPP )
            shape~>bBox=bBox
          else 
            via2shape=dbGetOverlaps( CellView bBox Via23LPP )
            dbMoveFig( shape CellView list( list(0 move) "R0")))
          printf("M3 Power Grid moved to: %L\n" bBox)
          foreach( shape via2shape dbMoveFig( shape CellView list( list(0 move) "R0")))
          if( m3label then m3label~>xy=list((w1+w2)/2.0 (h1+h2)/2.0) )
        )
        if(n == floor(n/2)*2 then     net=dbMakeNet( CellView "GND" )
        else net=dbMakeNet( CellView "Vdd" ))
        dbCreatePin( net car(m3shape)) 
      )
      if( length(m3shape)>1 then 
        printf("Error: Power Grid %L can not be created from multiple overlapping m3 shapes: %L\n" bBox m3shape~>bBox) 
      )

    )

  )
)
