; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id: //depot/sw/cad/external-tools-integration/cadence/virtuoso/main/skill/ui/Fulcrum/Pins/AutoPowerGrid.il#26 $
; $DateTime: 2005/06/17 14:39:47 $
; $Author: khe $

( list "Auto PowerGrid" "UIAutoPowerGrid" )


( UICreateMenuItemCallBack 
  `UIAutoPowerGridCB
  (lambda ()    
    let((CellView powerCellName OBSCellName Components RComponents)
       CellView= UIGetCellView()
       Components= parseString( CellView~>cellName "." )
       RComponents= reverse( Components )
       if( length(RComponents)>=3 then
         powerCellName=sprintf( nil "%s_%s" cadr(RComponents) car(RComponents)) 
         OBSCellName=sprintf( nil "%s.wires.%s_OBS" car( NameParseCellName( CellView~>cellName )) powerCellName )
         powerCellName=sprintf( nil "%s.wires.%s_POWER_GRID_TIEOFF" car( NameParseCellName( CellView~>cellName )) powerCellName )
         UIAutoPowerGrid_Form->UIAutoPowerGrid_Form_OBSCellName->value = OBSCellName
         UIAutoPowerGrid_Form->UIAutoPowerGrid_Form_PowerGridCellName->value = powerCellName
       )
    ( hiDisplayForm UIAutoPowerGrid_Form ) ) ) )


( UICreateAction
  'UIAutoPowerGridFormCB
  (lambda ()
    ( createDir ( ConfigFileGetValue TheCDSConfigTable "TEMP" ) )
 
    (let (CellView powerGridView)
      CellView = UIGetCellView( )
        powerGridView=AutoGeneratePowerGrid( CellView
           UIAutoPowerGrid_Form->UIAutoPowerGrid_Form_PowerGridCellName->value
           ?OBSCellName UIAutoPowerGrid_Form->UIAutoPowerGrid_Form_OBSCellName->value
           ?OBSViewName UIAutoPowerGrid_Form->UIAutoPowerGrid_Form_OBSViewName->value
           ?powerViewName UIAutoPowerGrid_Form->UIAutoPowerGrid_Form_PowerGridViewName->value
        )
        if( powerGridView then
          dbCreateInst( CellView powerGridView "tiehilo" list(0 0) "R0" ) 
          dbSave( CellView )
        else
          printf("Error: AutoGeneratePowerGrid failed.\n")
        ) 
    ) 
  ) 
)

    
( UICreateComponent
  "UIAutoPowerGrid_Form"
  ( UICreateConstructor 
    (lambda ( Children )
      (let ( Fields )
            Fields = list(
                ( hiCreateStringField
                  ?name `UIAutoPowerGrid_Form_PowerGridCellName
                  ?prompt "Power Grid Cell Name" 
                  ?defValue "powerGridTieOff" )
                ( hiCreateStringField
                  ?name `UIAutoPowerGrid_Form_PowerGridViewName
                  ?prompt "Power Grid View Name" 
                  ?defValue "layout" )
                ( hiCreateStringField
                  ?name `UIAutoPowerGrid_Form_OBSCellName
                  ?prompt "Routing Obstruction Cell Name" 
                  ?defValue "OBS" )
                ( hiCreateStringField
                  ?name `UIAutoPowerGrid_Form_OBSViewName
                  ?prompt "Routing Obstruction View Name" 
                  ?defValue "layout" )
                ( hiCreateFormButton
                  ?name `UIAutoPowerGrid_Help
                  ?buttonText "Tool Help"
                  ?callback "nrOpenHtml( \"Create_Power_Grid_Cell_help.html\" )" )
                )
        ( hiCreateAppForm
          ?name `UIAutoPowerGrid_Form
          ?formTitle "Create Power Grid Cell"  
          ?fields Fields
          ?callback "( UIAutoPowerGridFormCB )"
        ) 
      ) 
    )
  ) 
)
        
 
