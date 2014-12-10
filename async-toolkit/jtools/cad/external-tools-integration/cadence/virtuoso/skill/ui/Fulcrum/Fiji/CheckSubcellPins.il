; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id: //depot/sw/cad/external-tools-integration/cadence/virtuoso/main/skill/ui/Fulcrum/Pins/PreroutePins.il#26 $
; $DateTime: 2005/06/17 14:39:47 $
; $Author: khe $

( list "Check Subcell Pins" "UICheckSubcellPins" )

( UICreateMenuItemCallBack 
  `UICheckSubcellPinsCB
  (lambda ()
    let( (cellName bufferString)
      CheckSubcellPinsFailedInstances=fjCheckSubcellPins( UIGetCellView())
      if( CheckSubcellPinsFailedInstances then
        bufferString=""
        foreach( cellName CheckSubcellPinsFailedInstances~>cellName
          bufferString=sprintf( nil "%s%s\n"  bufferString cellName)
        )
        UICheckSubcellPins_Form->UICheckSubcellPins_Subcells->value= bufferString
        hiDisplayForm( UICheckSubcellPins_Form )
      else printf("Success: All subcell labels have pins.\n")
      ) 
    )
) )
( UICreateAction
  'UICheckSubcellPinsFormCB
  (lambda ()
    (let (bufferString outPort inPort subcellCellView)
       foreach( inst CheckSubcellPinsFailedInstances
         subcellCellView=nrOpenCellViewWritable( inst~>master~>libName inst~>master~>cellName inst~>master~>viewName)
         if( subcellCellView then
           if( UICheckSubcellPins_Form->UICheckSubcellPins_AttachPins->value then 
             fxAttachPinShapes(subcellCellView))
           if( UICheckSubcellPins_Form->UICheckSubcellPins_SnapPowerGrid->value then 
             SnapM3PowerGrid(subcellCellView ))
           dbSave( subcellCellView )
         else
           printf("Error: unable to open %s %s(%s) for write.\n" inst~>master~>libName inst~>master~>cellName inst~>master~>viewName)
         )
       )
    )
  ) 
)
( UICreateComponent
  "UICheckSubcellPins_Form"
  ( UICreateConstructor 
    (lambda ( Children )
      (let (
            ( Fields
              ( list
                ( hiCreateMLTextField
                  ?name `UICheckSubcellPins_Subcells
                  ?prompt "Failed Subcells"
                  ?editable nil
                  ?defValue "" )
                ( hiCreateBooleanButton
                  ?name `UICheckSubcellPins_AttachPins
                  ?buttonText "Run Subcell Attach Pin Shapes"
                  ?defValue t )
                ( hiCreateBooleanButton
                  ?name `UICheckSubcellPins_SnapPowerGrid
                  ?buttonText "Run Snap M3 Power Grid"
                  ?defValue t )
                ( hiCreateButton
		  ?name	`UICheckSubcellPins_OK
		  ?buttonText	"Run on Above Subcells"
		  ?callback	"hiFormDone(UICheckSubcellPins_Form)"
		  )
                ) ) )
        ( hiCreateAppForm
          ?name `UICheckSubcellPins_Form
          ?formTitle "Check Subcell Pins"  
          ?fields Fields
          ?callback "( UICheckSubcellPinsFormCB )"
          ) ) )
) )




defun( fjCheckSubcellPins ( CellView )
  let((uniqueInstances inst shape unconnectLabels failedInstances)
    uniqueInstances=nil
    foreach(inst CellView~>instances 
      if( inst~>libName!=TechLibName && !member( inst~>cellName uniqueInstances~>cellName) then
        uniqueInstances=cons( inst uniqueInstances)
      )
    )
    failedInstances=nil
    foreach( inst uniqueInstances
      unconnectLabels=setof( shape inst~>master~>shapes shape~>objType=="label" && shape~>parent==nil)
      if( unconnectLabels then
        printf( "Error: %s Labels not attached to pin layers: %L \n" inst~>cellName unconnectLabels~>theLabel )
        failedInstances=cons( inst failedInstances)
      )
    )
    failedInstances
  )
)
