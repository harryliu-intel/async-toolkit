; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id: //depot/sw/cad/external-tools-integration/cadence/virtuoso/main/skill/ui/Fulcrum/Pins/DrawPins.il#24 $
; $DateTime: 2004/12/10 10:42:22 $
; $Author: rliu $

( list "Draw Reset Pins" "UIDrawResetPins" )

( UICreateMenuItemCallBack 
  `UIDrawResetPinsCB
  (lambda ()
    
    ( hiDisplayForm UIDrawResetPins_Form ) ) )

( UICreateAction
  'UIDrawResetPinsFormCB
  (lambda () 
    let( (CellView xMin xMax yMin yMax 
          xPitch yPitch xOffset yOffset xWidth yWidth
          m n x y resetNet path rect)
      CellView=( UIGetCellView )
      xMin=1e12 xMax=-1e12 yMin=1e12 yMax=-1e12
      xPitch=UIDrawResetPins_Form->UIDrawResetPins_Form_xPitch->value
      yPitch=UIDrawResetPins_Form->UIDrawResetPins_Form_yPitch->value
      xOffset=UIDrawResetPins_Form->UIDrawResetPins_Form_xOffset->value
      yOffset=UIDrawResetPins_Form->UIDrawResetPins_Form_yOffset->value
      xWidth=UIDrawResetPins_Form->UIDrawResetPins_Form_xWidth->value
      yWidth=UIDrawResetPins_Form->UIDrawResetPins_Form_yWidth->value
      xMetalLPP=list(UIDrawResetPins_Form->UIDrawResetPins_Form_xMetalName->value "drawing")
      xMetalPin=list(UIDrawResetPins_Form->UIDrawResetPins_Form_xMetalName->value "pin")
      yMetalLPP=list(UIDrawResetPins_Form->UIDrawResetPins_Form_yMetalName->value "drawing")
      yMetalPin=list(UIDrawResetPins_Form->UIDrawResetPins_Form_yMetalName->value "pin")
      resetNet=dbMakeNet( CellView UIDrawResetPins_Form->UIDrawResetPins_Form_NetName~>value )

      prBound=setof( lpp CellView~>lpps lpp~>layerName==car(BoundaryLPP) && lpp~>purpose==cadr(BoundaryLPP))
      if( prBound==nil then bBox=CellView~>bBox
          xMin=min(xMin caar(bBox))
          yMin=min(yMin cadar(bBox))
          xMax=max(xMax caadr(bBox))
          yMax=max(yMax cadadr(bBox))
      else prBound=car(prBound)
        foreach( shape prBound~>shapes
          bBox=shape~>bBox
          xMin=min(xMin caar(bBox))
          yMin=min(yMin cadar(bBox))
          xMax=max(xMax caadr(bBox))
          yMax=max(yMax cadadr(bBox))
        )
        bBox=list( xMin:yMin xMax:yMax )
      )
      
      for( m ceiling((xMin-xOffset)/xPitch) floor((xMax-xOffset)/xPitch)
        x=m*xPitch+xOffset
        path=dbCreatePath( CellView 
                      xMetalLPP
                      list( x:yMin+DefaultWireSpacing*MicronsPerMeter+yWidth/2
                            x:yMax-DefaultWireSpacing*MicronsPerMeter-yWidth/2
                          )
                      yWidth
        ) 
        dbSetq( path "varExtendExtend" pathStyle )
        dbSetq( path  yWidth/2 beginExt )
        dbSetq( path  yWidth/2 endExt ) 
        rect=dbCreateRect( CellView
                      xMetalPin
                      list( x-yWidth/2:yMin+DefaultWireSpacing*MicronsPerMeter
                            x+yWidth/2:yMax-DefaultWireSpacing*MicronsPerMeter
                          )         
        )
        dbCreatePin( resetNet path )
        dbCreatePin( resetNet rect )
      )
      for( n ceiling((yMin-yOffset)/yPitch) floor((yMax-yOffset)/yPitch)
        y=n*yPitch+yOffset
        path=dbCreatePath( CellView 
                      yMetalLPP
                      list( xMin+DefaultWireSpacing*MicronsPerMeter+xWidth/2:y
                            xMax-DefaultWireSpacing*MicronsPerMeter-xWidth/2:y
                          )
                      xWidth
        ) 
        dbSetq( path "varExtendExtend" pathStyle )
        dbSetq( path  xWidth/2 beginExt )
        dbSetq( path  xWidth/2 endExt ) 
        rect=dbCreateRect( CellView
                      yMetalPin
                      list( xMin+DefaultWireSpacing*MicronsPerMeter:y-xWidth/2
                            xMax-DefaultWireSpacing*MicronsPerMeter:y+xWidth/2
                          )         
        )
        dbCreatePin( resetNet path )
        dbCreatePin( resetNet rect )        
      )
      for( m ceiling((xMin-xOffset)/xPitch) floor((xMax-xOffset)/xPitch)
        for( n ceiling((yMin-yOffset)/yPitch) floor((yMax-yOffset)/yPitch)
          x=m*xPitch+xOffset
          y=n*yPitch+yOffset
          leCreateContact( CellView 
                  UIDrawResetPins_Form->UIDrawResetPins_Form_ViaName->value 
                  list(x y) 
                  "R0" 
                  UIDrawResetPins_Form->UIDrawResetPins_Form_ViaWidth->value 
                  UIDrawResetPins_Form->UIDrawResetPins_Form_ViaWidth->value 
                  UIDrawResetPins_Form->UIDrawResetPins_Form_ViaRows->value 
                  UIDrawResetPins_Form->UIDrawResetPins_Form_ViaCols->value 
                  UIDrawResetPins_Form->UIDrawResetPins_Form_ViaSpacing->value 
                  UIDrawResetPins_Form->UIDrawResetPins_Form_ViaSpacing->value 
                  "center" "center" 
          )
        )
      )

    )
  )
)

      

( UICreateComponent
  "UIDrawResetPins_Form"
  ( UICreateConstructor 
    (lambda ( Children )
      (let (
            ( Fields
              ( list
                ( hiCreateStringField
                  ?name `UIDrawResetPins_Form_NetName
                  ?prompt "Reset Net Name"
                  ?defValue "_RESET" )
                ( hiCreateCyclicField
                  ?name `UIDrawResetPins_Form_xMetalName
                  ?prompt "Vertical Strip Metal Layer" 
                  ?choices foreach( mapcar lpp MetalLPPs car(lpp)) 
                  ?defValue car(Metal4LPP) )
                ( hiCreateFloatField
                  ?name `UIDrawResetPins_Form_xPitch
                  ?prompt "Vertical Strip Pitch"
                  ?defValue 4.8 )
                ( hiCreateFloatField
                  ?name `UIDrawResetPins_Form_xWidth
                  ?prompt "Vertical Strip Width"
                  ?defValue 0.72 )
                ( hiCreateFloatField
                  ?name `UIDrawResetPins_Form_xOffset
                  ?prompt "Vertical Strip Offset"
                  ?defValue 1.08 )
                ( hiCreateCyclicField
                  ?name `UIDrawResetPins_Form_yMetalName
                  ?prompt "Horizontal Strip Metal Layer" 
                  ?choices foreach( mapcar lpp MetalLPPs car(lpp)) 
                  ?defValue car(Metal5LPP) )
                ( hiCreateFloatField
                  ?name `UIDrawResetPins_Form_yPitch
                  ?prompt "Horizontal Strip Pitch"
                  ?defValue 4.8 )
                ( hiCreateFloatField
                  ?name `UIDrawResetPins_Form_yWidth
                  ?prompt "Horizontal Strip Width"
                  ?defValue 0.72 )
                ( hiCreateFloatField
                  ?name `UIDrawResetPins_Form_yOffset
                  ?prompt "Horizontal Strip Offset"
                  ?defValue 1.08 )
                ( hiCreateStringField
                  ?name `UIDrawResetPins_Form_ViaName
                  ?prompt "Via Type"
                  ?defValue "M5_M4L" )
                ( hiCreateFloatField
                  ?name `UIDrawResetPins_Form_ViaWidth
                  ?prompt "Via Width"
                  ?defValue 0.19 )
                 ( hiCreateFloatField
                  ?name `UIDrawResetPins_Form_ViaSpacing
                  ?prompt "Via Spacing"
                  ?defValue 0.22 )
                 ( hiCreateIntField
                  ?name `UIDrawResetPins_Form_ViaCols
                  ?prompt "Via Columns"
                  ?defValue 2 )
                 ( hiCreateIntField
                  ?name `UIDrawResetPins_Form_ViaRows
                  ?prompt "Via Rows"
                  ?defValue 2 )
               ) ) )
        ( hiCreateAppForm
          ?name `UIDrawResetPins_Form
          ?formTitle "Draw Pins"  
          ?fields Fields
          ?callback "( UIDrawResetPinsFormCB )"
          ) ) )
    ) )
        
 
