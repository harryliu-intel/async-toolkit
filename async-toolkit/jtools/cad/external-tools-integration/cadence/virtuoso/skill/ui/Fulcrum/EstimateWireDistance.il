; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id: //depot/sw/cad/external-tools-integration/cadence/virtuoso/main/skill/ui/Fulcrum/Pins/PreroutePins.il#26 $
; $DateTime: 2005/06/17 14:39:47 $
; $Author: khe $

( list "Estimate Pin Wire Distance" "UIEstimateWireDistance" )

defun( flEstimateWireDistance (CellView)
  let((Msg pinDistanceTable pins pin pinFigs instPinBBoxList instPinBBox 
       pinBBox x1 x2 y1 y2 pinDistanceList)
    printf("Processing cell %s :\n" CellView~>cellName)
    Msg=sprintf(nil "Processing cell %s :\n" CellView~>cellName)
    pinDistanceTable=makeTable("myTable" 0.0)
    pins=setof( pin CellView~>terminals !member( pin~>name list("Vdd" "GND")))
    foreach( pin pins
      pinFigs=pin~>pins~>fig~>bBox
      instPinBBoxList=foreach( mapcan instTerm pin~>net~>instTerms
        foreach( mapcar bBox instTerm~>term~>pins~>fig~>bBox
          geTransformUserBBox( bBox instTerm~>inst~>transform )
        )
      )
      cond( 
        (pinFigs==nil 
           Msg=sprintf(nil "%s Error: Unbound Pin Found: %s \n" Msg pin~>name)
        )         
        ( instPinBBoxList==nil pinDistanceTable[pin~>name]=0.0 )
        ( t 
           foreach( instPinBBox instPinBBoxList
             x1=(leftEdge(instPinBBox)+rightEdge(instPinBBox))/2.0
             y1=(topEdge(instPinBBox)+bottomEdge(instPinBBox))/2.0
             foreach( pinBBox pinFigs
               x2=(leftEdge(pinBBox)+rightEdge(pinBBox))/2.0
               y2=(topEdge(pinBBox)+bottomEdge(pinBBox))/2.0
               distance=abs(x1-x2)+abs(y1-y2)
               pinDistanceTable[pin~>name]=max(pinDistanceTable[pin~>name] distance)
             )
           )
         )
      )
    )
    pinDistanceList=sort( tableToList(pinDistanceTable) lambda((x y) cadr(x)>cadr(y)))
    if( pinDistanceList printf("Longest Wire is %s : %f \n" caar(pinDistanceList) cadar(pinDistanceList)))
    Msg=sprintf(nil "%sListing Pins in order: \n" Msg)
    foreach(pin pinDistanceList
      Msg=sprintf(nil "%s Pin %s : %f \n" Msg car(pin) cadr(pin))
    )
    Msg
  )
)

( UICreateMenuItemCallBack 
  `UIEstimateWireDistanceCB
  (lambda ()    
    ( hiDisplayForm UIEstimateWireDistance_Form ) ) )

( UICreateAction
  'UIEstimateWireDistanceFormCB
  (lambda ()
    (let ( CellView Msg p)
      CellView=UIGetCellView()
      Msg= flEstimateWireDistance(CellView)
      p=outfile(UIEstimateWireDistance_Form->UIEstimateWireDistance_Filename->value)
      if( p then
        fprintf(p "%s" Msg)
        close(p)
      else
        printf("Fail to write to file %s/n" UIEstimateWireDistance_Form->UIEstimateWireDistance_Filename->value)
      )

    ) 
  ) 
)
    
( UICreateComponent
  "UIEstimateWireDistance_Form"
  ( UICreateConstructor 
    (lambda ( Children )
      (let (
            ( Fields
              ( list
                ( hiCreateStringField
                  ?name `UIEstimateWireDistance_Filename
                  ?prompt "Output Filename"
                  ?defValue "PinWireEstimate.txt" )
                ) ) )
        ( hiCreateAppForm
          ?name `UIEstimateWireDistance_Form
          ?formTitle "Estimate Wire Distance"  
          ?fields Fields
          ?callback "( UIEstimateWireDistanceFormCB )"
          ) ) )
    ) )
        
 
