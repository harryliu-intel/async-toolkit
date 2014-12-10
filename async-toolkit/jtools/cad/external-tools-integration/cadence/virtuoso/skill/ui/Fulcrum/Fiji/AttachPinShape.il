; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id: //depot/sw/cad/external-tools-integration/cadence/virtuoso/main/skill/ui/Fulcrum/Pins/PreroutePins.il#26 $
; $DateTime: 2005/06/17 14:39:47 $
; $Author: khe $

( list "Attach Pin Shape" "UIAttachPinShape" )

( UICreateMenuItemCallBack 
  `UIAttachPinShapeCB
  (lambda ()
    let(( CellView )  
      CellView=UIGetCellView()   
      fxAttachPinShapes(CellView)
;      dbSave(CellView) 
    )
  ) 
)
    
defun( fxGetLabelFromPinShape ( pinBBox labels )
  let((x1 x2 y1 y2 x y label)
    x1=leftEdge(pinBBox)
    x2=rightEdge(pinBBox)
    y1=bottomEdge(pinBBox)
    y2=topEdge(pinBBox)
    foreach( mapcan label labels
      x=car(label~>xy)
      y=cadr(label~>xy)
      if( x>=x1 && x<=x2 && y>=y1 && y<=y2 then list(label)
      else nil
      )
    )
  )
)

defun( fxAttachPinShapes ( CellView )
  let((pinShapes pinLabels labels label shape shapes net 
       n lpp prbound shape w1 w2 h1 h2 x1 x2 y1 y2 m3power m3shape)
    pinShapes=setof( shape CellView~>shapes shape~>purpose=="pin")
    pinLabels=setof( shape CellView~>shapes shape~>objType=="label")
    foreach( pinShape pinShapes 
      labels=setof( shapes fxGetLabelFromPinShape( pinShape~>bBox pinLabels) 
          shapes~>objType=="label" && shapes~>layerName==pinShape~>layerName )
      if( length( labels ) != 1 then
        printf("Warning: error pin shape found at %L\n" pinShape~>bBox )
      else
        label=car( labels )
        net=dbMakeNet( CellView label~>theLabel )
        dbCreatePin( net pinShape ) 
        label~>parent=pinShape
        if( label~>theLabel=="Vdd" then
          if( bottomEdge(pinShape~>bBox)<=2.7 && bottomEdge(pinShape~>bBox)>=0.18 then
            dbMoveFig( pinShape CellView list( list( 0 2.88-(bottomEdge(pinShape~>bBox)+topEdge(pinShape~>bBox))/2) "R0")) 
          )
        )
        if( label~>theLabel=="GND" then
          if( bottomEdge(pinShape~>bBox)<=-0.18 && bottomEdge(pinShape~>bBox)>=-2.7 then
            dbMoveFig( pinShape CellView list( list( 0 -(bottomEdge(pinShape~>bBox)+topEdge(pinShape~>bBox))/2) "R0")) 
          )
        )
      )
    )
    unconnectLabels=setof( shape CellView~>shapes shape~>objType=="label" && shape~>parent==nil)
    foreach( label unconnectLabels
      pinShapes=dbGetTrueOverlaps( CellView 
        list( label~>xy label~>xy )
        label~>lpp
        1 
      )
      foreach(pinShape pinShapes
        net=dbMakeNet( CellView label~>theLabel )
        dbCreatePin( net pinShape ) 
        label~>parent=pinShape
        if( label~>theLabel=="Vdd" then
          if( bottomEdge(pinShape~>bBox)<=2.7 && bottomEdge(pinShape~>bBox)>=0.18 then
            dbMoveFig( pinShape CellView list( list( 0 2.88-(bottomEdge(pinShape~>bBox)+topEdge(pinShape~>bBox))/2) "R0")) 
          )
        )
        if( label~>theLabel=="GND" then
          if( bottomEdge(pinShape~>bBox)<=-0.18 && bottomEdge(pinShape~>bBox)>=-2.7 then
            dbMoveFig( pinShape CellView list( list( 0 -(bottomEdge(pinShape~>bBox)+topEdge(pinShape~>bBox))/2) "R0")) 
          )
        )
      )
    )

    ; Attach Power Grid Pin
    prbound=car( car( setof( lpp CellView~>lpps lpp~>layerName=="prBoundary"))~>shapes)
    w1=leftEdge(prbound~>bBox)
    w2=rightEdge(prbound~>bBox)
    height=topEdge(prbound~>bBox)
    net=dbMakeNet( CellView "GND" )
    for( n 0 floor(height/5.76)
      h1=n*5.76-0.18 h2=n*5.76+0.18
      bBox=list(w1:h1 w2:h2)
      m3power=dbGetOverlaps( CellView bBox Metal3LPP)
      m3shape=setof( shape m3power shape~>objType!="label" )
      if( length(m3shape)==1 then 
        dbCreatePin( net car(m3shape)) 
      )
    )
    net=dbMakeNet( CellView "Vdd" )
    for( n 0 floor(height/5.76)
      h1=n*5.76+2.88-0.18 h2=n*5.76+2.88+0.18
      bBox=list(w1:h1 w2:h2)
      m3power=dbGetOverlaps( CellView bBox Metal3LPP)
      m3shape=setof( shape m3power shape~>objType!="label" )
      if( length(m3shape)==1 then 
        dbCreatePin( net car(m3shape)) 
      )
    )
    unconnectLabels=setof( shape CellView~>shapes shape~>objType=="label" && shape~>parent==nil)
    if( unconnectLabels then
      printf( "Labels not attached to pin layers: %L " unconnectLabels~>theLabel )
    )
  )
)
