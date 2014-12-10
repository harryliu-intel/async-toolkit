; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id: //depot/sw/cad/external-tools-integration/cadence/virtuoso/main/skill/ui/Nano/CreateFloorplan.il#13 $
; $DateTime: 2004/11/24 12:59:08 $
; $Author: khe $

;
; menu item Nano/Floorplan
;
( list "Create Floorplan Views From Prelayout" "UICreateFloorplan" )

( UICreateMenuItemCallBack
  `UICreateFloorplanCB
  (lambda ()
    hiDisplayForm( UICreateFloorplan_Form ) 
  ) 
)


 
( UICreateAction
  'UICreateFloorplanFormCB
  (lambda ()
    let( (CellView)
    CellView= UIGetCellView()
    doneCellList=nil
    nrCreateFloorplan(CellView~>libName CellView~>cellName CellView~>viewName 
        UICreateFloorplan_Form->UICreateFloorplan_ViewName->value
        ?Verbose UICreateFloorplan_Form->UICreateFloorplan_Verbose->value
    )
    )
  )
)

( UICreateComponent
  "UICreateFloorplan_Form"
  ( UICreateConstructor 
    (lambda ( Children )
      (let ( Fields )
         Fields = list(
           ( hiCreateStringField
             ?name `UICreateFloorplan_ViewName
             ?prompt "Floorplan View Name"
             ?defValue "floorplan" )
           ( hiCreateBooleanButton
             ?name `UICreateFloorplan_Verbose
             ?buttonText "Verbose"
             ?defValue nil )
          )
        ( hiCreateAppForm
          ?name `UICreateFloorplan_Form
          ?formTitle "Create Floorplan Views"  
          ?fields Fields
          ?callback "( UICreateFloorplanFormCB )"
        ) 
      ) 
    ) 
  ) 
)
