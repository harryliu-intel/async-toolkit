; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

; Function: SwapVias
; Parameter: CellView (CVId)
; Return: CellView (CVId)
;
; Swap vias

defun( hkBBoxEnclose ( bBox1 bBox2 )
       leftEdge(bBox1)<=leftEdge(bBox2) &&
       rightEdge(bBox1)>=rightEdge(bBox2) &&
       bottomEdge(bBox1)<=bottomEdge(bBox2) &&
       topEdge(bBox1)>=topEdge(bBox2)
)

defun( hkSnapXToGrid (x Grid)
  if( x floor(x/Grid+0.5)*Grid nil )
)

defun( fixBoundaryVia23 ( CellView )
  ;fix the M3_M2c via that at the boundary that violates M3 space to boundray 0.05 frc rule
  ; by (1) make M3 extension vertical
  ; or (2) move M3_M2c via inside by 0.02
  let((prBoundShapes shape x1 x2 y1 y2 allvia23 badvia23_1 badvia23_2 inst via1 via2)
    prBoundShapes= setof( shape CellView~>shapes shape~>layerName=="prBoundary" && shape~>objType=="rect")
    shape=car(prBoundShapes)
    x1=leftEdge(shape~>bBox)
    x2=rightEdge(shape~>bBox)
    y1=bottomEdge(shape~>bBox)
    y2=topEdge(shape~>bBox)
    allvia23=setof( inst CellView~>instances inst~>cellName=="M3_M2c")
    badvia23_1=setof( inst allvia23 car(inst~>xy)<x1+0.14 )
    badvia23_2=setof( inst allvia23 car(inst~>xy)>x2-0.14 )
    badvia23_3=setof( inst allvia23 cadr(inst~>xy)>y2-0.14 )
    via1=nrOpenCellViewReadable( TechLibName "M3_M2_V" "layout") 
    via2=nrOpenCellViewReadable( TechLibName "M3_M2_H" "layout") 
    ; fix M3_M2c via on the left side  
    foreach( inst badvia23_1
      if( length(dbGetTrueOverlaps( CellView 
                   list( car(inst~>xy)-0.09:cadr(inst~>xy)-0.21 
                         car(inst~>xy)+0.09:cadr(inst~>xy)+0.21 )
                   Metal3LPP 
                   0 ))>1 then
        inst~>xy=list(car(inst~>xy)+0.02 cadr(inst~>xy))
      else
        dbReplaceProp( inst "layer1Direction" "string" "XY")
        dbReplaceProp( inst "layer1YEnclosure" "float" 0.04)
        dbReplaceProp( inst "layer2Direction" "string" "XY")
        dbReplaceProp( inst "layer2YEnclosure" "float" 0.04)         
        dbReplaceProp( inst "layer2XEnclosure" "float" 0.01)         
        inst~>master=via1
      ) 
    )
    ; fix M3_M2c via on the right side  
    foreach( inst badvia23_2
      if( length(dbGetTrueOverlaps( CellView 
                   list( car(inst~>xy)-0.09:cadr(inst~>xy)-0.21 
                         car(inst~>xy)+0.09:cadr(inst~>xy)+0.21 )
                   Metal3LPP 
                   0 ))>1 then
        inst~>xy=list(car(inst~>xy)-0.02 cadr(inst~>xy))
      else
        dbReplaceProp( inst "layer1Direction" "string" "XY")
        dbReplaceProp( inst "layer1YEnclosure" "float" 0.04)
        dbReplaceProp( inst "layer2Direction" "string" "XY")
        dbReplaceProp( inst "layer2YEnclosure" "float" 0.04)         
        dbReplaceProp( inst "layer2XEnclosure" "float" 0.01)         
        inst~>master=via1
      ) 
    )
    ; fix M3_M2c via on the top side  
    foreach( inst badvia23_3
      if( length(dbGetTrueOverlaps( CellView 
                   list( car(inst~>xy)-0.21:cadr(inst~>xy)-0.09 
                         car(inst~>xy)+0.21:cadr(inst~>xy)+0.09 )
                   Metal3LPP 
                   0 ))>1 then
        inst~>xy=list(car(inst~>xy) cadr(inst~>xy)-0.02)
      else
        dbReplaceProp( inst "layer1Direction" "string" "XY")
        dbReplaceProp( inst "layer1YEnclosure" "float" 0.01)
        dbReplaceProp( inst "layer1XEnclosure" "float" 0.04)         
        dbReplaceProp( inst "layer2Direction" "string" "XY")
        dbReplaceProp( inst "layer2YEnclosure" "float" 0.01)         
        dbReplaceProp( inst "layer2XEnclosure" "float" 0.04) 
        inst~>orient="R0"        
        inst~>master=via2
      ) 
    )

  )
)

defun( SwapVias ( CellView )
  ; replace double via M2_M1min with
  ; (1) M2_M1c if M1 below has enough room for horizontal extension
  ; or (2) M2_M1_V if M1 below does not have enough room for horizontal extension.
  let((inst viaInst m1shape m2shape shape x left right via1)
    viaInsts=setof( inst CellView~>instances inst~>cellName=="M2_M1min" ) 
    via1=nrOpenCellViewReadable( TechLibName "M2_M1c" "layout")   
    via2=nrOpenCellViewReadable( TechLibName "M2_M1_V" "layout")   
    foreach( inst viaInsts
      m1shape=car( setof( shape dbGetTrueOverlaps( CellView inst~>bBox list("M1" "drawing") 0 ) hkBBoxEnclose( shape~>bBox inst~>bBox )))~>bBox 
      m2shape=car( setof( shape dbGetTrueOverlaps( CellView inst~>bBox list("M2" "drawing") 0 ) hkBBoxEnclose( shape~>bBox inst~>bBox )))~>bBox
      left=0.0 right=0.0
      if( m1shape && m2shape then
        left=max( leftEdge(m1shape)+0.03 leftEdge(m2shape))
        right=min( rightEdge(m1shape)-0.03 rightEdge(m2shape)))
      if(right-left>=0.32 then
        ; M1 below has enough room for horizontal extension
        ; replace with M2_M1c
        x=hkSnapXToGrid((right+left)/2 0.005)
        dbReplaceProp( inst "layer1Direction" "string" "XY")
        dbReplaceProp( inst "layer1XEnclosure" "float" 0.04)
        dbReplaceProp( inst "layer2Direction" "string" "XY")
        dbReplaceProp( inst "layer2YEnclosure" "float" 0.04)
        inst~>xy=list( x cadr(inst~>xy) )
        inst~>master=via1
      else
          ; M1 below does not have enough room for horizontal extension
          ; replace with M2_M1_V
        dbReplaceProp( inst "layer1YEnclosure" "float" 0.04)
        dbReplaceProp( inst "layer2Direction" "string" "XY")
        dbReplaceProp( inst "layer2YEnclosure" "float" 0.04)         
        inst~>master=via2
      )
    )
;    SwapSchematicMasters(
;      CellView 
;      ViaMappings )

    dbSave( CellView )
  )
)

defun( fixFatPolyContact ( CellView )
  let((polyContacts inst)
    polyContacts=setof( inst CellView~>instances 
      inst~>cellName=="M1_POLY" )
    foreach( inst polyContacts
      if( dbGetPropByName(inst "layer1XDefOverride")~>value && dbGetPropByName(inst "layer1XDefOverride")~>value>0.04 then
        dbReplaceProp( inst "layer1XDefOverride" "float" 0.04)
        printf("Fixing Poly Contact %L \n" inst~>xy)
      )
    )
    t
  )
)

defun( fixNegativeExtension ( CellView )
  let((polyContacts M2_M1c inst)
    polyContacts=setof( inst CellView~>instances 
      inst~>cellName=="M1_POLY" )
    foreach( inst polyContacts
      if( dbGetPropByName(inst "layer2XDefOverride")~>value && dbGetPropByName(inst "layer2XDefOverride")~>value<0.0 then
        dbReplaceProp( inst "layer2XDefOverride" "float" 0.04)
        dbReplaceProp( inst "layer2XEnclosure" "float" 0.04)
        printf("Fixing Poly Contact %L \n" inst~>xy)
      )
    )
    M2_M1c=setof( inst CellView~>instances 
      inst~>cellName=="M2_M1c" )
    foreach( inst M2_M1c
      if( dbGetPropByName(inst "layer1XDefOverride")~>value && dbGetPropByName(inst "layer1XDefOverride")~>value<0.0 then
        dbReplaceProp( inst "layer1XDefOverride" "float" 0.04)
        dbReplaceProp( inst "layer1XEnclosure" "float" 0.04)
        printf("Fixing M2_M1c %L \n" inst~>xy)
      )
    )
    t
  )
)
