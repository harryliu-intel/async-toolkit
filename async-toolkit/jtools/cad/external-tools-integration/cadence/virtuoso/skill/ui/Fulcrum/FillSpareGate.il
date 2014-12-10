; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id: //depot/sw/cad/external-tools-integration/cadence/virtuoso/main/skill/ui/Fulcrum/Pins/PreroutePins.il#26 $
; $DateTime: 2005/06/17 14:39:47 $
; $Author: khe $

( list "Fill Spare Gate & Metal" "UIFillSpareGate" )


( UICreateMenuItemCallBack 
  `UIFillSpareGateCB
  (lambda () 
    let((cellView cellBase cellSubType)
      cellView = UIGetCellView()
      when( cellView 
        cellBase = cadr( reverse( parseString( cellView->cellName "." ) ))
        cellSubtype = car( reverse( parseString( cellView->cellName "." ) ))
        UIFillSpareGate_Form->UIFillSpareGate_FillCellName->value = 
            strcat( cellView->libName ".wires." cellBase "_" cellSubtype "_fill")
      ) 
      hiDisplayForm( UIFillSpareGate_Form ) 
    ) 
  ) 
)

( UICreateAction
  'UIFillSpareGateFormCB
  (lambda ()
    (let ( CellView Msg p )
      CellView=UIGetCellView()
      if(AutoSpareCell( CellView UIFillSpareGate_Form->UIFillSpareGate_FillCellName->value 
                               ?fillViewName "layout" 
                               ?PowerGridSize 5.76
                               ?FillWithSpareCell UIFillSpareGate_Form->UIFillSpareGate_ODM2Fill->value=="spare gates" 
                               ?FillWithDcapCell UIFillSpareGate_Form->UIFillSpareGate_ODM2Fill->value=="DCAP" 
                               ?FillWithM2M7 UIFillSpareGate_Form->UIFillSpareGate_M2M7Fill->value=="M2M7"
                               ?SpareCellLibName UIFillSpareGate_Form->UIFillSpareGate_SpareCellLibName->value 
                               ?LargeSpareCellName UIFillSpareGate_Form->UIFillSpareGate_LargeSpareCellName->value
                               ?SmallSpareCellName UIFillSpareGate_Form->UIFillSpareGate_SmallSpareCellName->value
                               ?LargeSpaceCellGridX 3 
                               ?LargeSpaceCellGridY 4
                               ?FillCellLibName UIFillSpareGate_Form->UIFillSpareGate_FillCellLibName->value 
                               ?DcapFillCellName UIFillSpareGate_Form->UIFillSpareGate_DcapCellName->value
                               ?ODPOM1FillCellName UIFillSpareGate_Form->UIFillSpareGate_ODPOM1CellName->value
                               ?ODM1FillCellName UIFillSpareGate_Form->UIFillSpareGate_ODM1CellName->value
                               ?ODPOFillCellName UIFillSpareGate_Form->UIFillSpareGate_ODPOCellName->value
                               ?ODSMALLFillCellName UIFillSpareGate_Form->UIFillSpareGate_ODSMALLCellName->value
                               ?M2FillCellName "globals.fill.M2"
                               ?M3FillCellName "globals.fill.M3"
                               ?M4FillCellName "globals.fill.M4"
                               ?M5FillCellName "globals.fill.M5"
                               ?M6FillCellName "globals.fill.M6"
                               ?M7FillCellName "globals.fill.M7"
                               ?M23FillCellName "globals.fill.M23"
                               ?M34FillCellName "globals.fill.M34"
                               ?M45FillCellName "globals.fill.M45"
                               ?M56FillCellName "globals.fill.M56"
                               ?M67FillCellName "globals.fill.M67"
                               ?LeaveMess UIFillSpareGate_Form->UIFillSpareGate_Debug->value
                               )
      dbCreateInstByMasterName( CellView CellView~>libName
                   UIFillSpareGate_Form->UIFillSpareGate_FillCellName->value
                   "layout" "fill" (list 0 0) "R0")
      )
    )
  )
)
    
( UICreateComponent
  "UIFillSpareGate_Form"
  ( UICreateConstructor 
    (lambda ( Children )
      (let (
            ( Fields
              ( list
                ( hiCreateStringField
                  ?name `UIFillSpareGate_FillCellName
                  ?prompt "Fill Cell Name"
                  ?defValue "fill" )
                ( hiCreateRadioField
                  ?name `UIFillSpareGate_ODM2Fill
                  ?prompt "Fill OD to M2 with"
                  ?choices list( "DCAP" "spare gates" "none")
                  ?defValue "DCAP" )
                ( hiCreateRadioField
                  ?name `UIFillSpareGate_M2M7Fill
                  ?prompt "Fill M2 to M7 with"
                  ?choices list( "M2M7" "none" )
                  ?defValue "M2M7" )
                ( hiCreateStringField
                  ?name `UIFillSpareGate_SpareCellLibName
                  ?prompt "Spare Cell Lib Name"
                  ?defValue "lib.util.spare" )
                ( hiCreateStringField
                  ?name `UIFillSpareGate_LargeSpareCellName
                  ?prompt "Large Spare Cell Name"
                  ?defValue "lib.util.spare.LARGE_SPARE_GATES.1000" )
                ( hiCreateStringField
                  ?name `UIFillSpareGate_SmallSpareCellName
                  ?prompt "Small Spare Cell Name"
                  ?defValue "lib.util.spare.SMALL_SPARE_GATES.1000" )
                ( hiCreateStringField
                  ?name `UIFillSpareGate_FillCellLibName
                  ?prompt "Fill Cell Lib Name"
                  ?defValue "globals" )
                ( hiCreateStringField
                  ?name `UIFillSpareGate_DcapCellName
                  ?prompt "DCAP Cell Name"
                  ?defValue "globals.fill.DCAP" )
                ( hiCreateStringField
                  ?name `UIFillSpareGate_ODPOM1CellName
                  ?prompt "ODPOM1 Cell Name"
                  ?defValue "globals.fill.ODPOM1" )
                ( hiCreateStringField
                  ?name `UIFillSpareGate_ODM1CellName
                  ?prompt "ODM1 Cell Name"
                  ?defValue "globals.fill.ODM1" )
                ( hiCreateStringField
                  ?name `UIFillSpareGate_ODPOCellName
                  ?prompt "ODPO Cell Name"
                  ?defValue "globals.fill.ODPO" )
                ( hiCreateStringField
                  ?name `UIFillSpareGate_ODSMALLCellName
                  ?prompt "OD SMALL Cell Name"
                  ?defValue "globals.fill.OD_SMALL" )
                ( hiCreateBooleanButton
                  ?name `UIFillSpareGate_Debug
                  ?buttonText "Debug (leave Assura Run)"
                  ?defValue nil )
                ) ) )
        ( hiCreateAppForm
          ?name `UIFillSpareGate_Form
          ?formTitle "Fill Spare Gate & Metal"
          ?fields Fields
          ?callback "( UIFillSpareGateFormCB )"
          ) ) )
    ) )
        
 
