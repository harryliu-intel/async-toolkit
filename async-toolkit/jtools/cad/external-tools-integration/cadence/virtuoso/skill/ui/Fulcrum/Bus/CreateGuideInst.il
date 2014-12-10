; Copyright 2006 Fulcrum Microsystems.  All rights reserved.
; $Id: $
; $DateTime: $
; $Author: $

;
; menu item Fulcrum/Bus
( list "Create Guide Instance" "UICreateGuideInst" )


( UICreateMenuItemCallBack
  `UICreateGuideInstCB
  (lambda ()
    let((CellView)
      CellView = UIGetCellView()
      CellBase = cadr( reverse( parseString( CellView~>cellName "." ) ))
      UICreateGuideInst_Form->UICreateGuideInst_cellname->value =
        strcat( CellView~>libName ".wires." CellBase "_buswires")
    hiDisplayForm( UICreateGuideInst_Form ) 
  ))
)


( UICreateComponent
  "UICreateGuideInst_Form"
  ( UICreateConstructor 
    (lambda ( Children )
      (let ( Fields )
         Fields = list(
           ( hiCreateStringField
             ?name `UICreateGuideInst_cellname
             ?prompt "Cell Name"
             ?defValue "" )
           ( hiCreateStringField
             ?name `UICreateGuideInst_instname
             ?prompt "Instance Name"
             ?defValue "buswires" )
           ( hiCreateBooleanButton
             ?name `UICreateGuideInst_wires
             ?buttonText "Create wires.il"
             ?defValue t )
           ( hiCreateBooleanButton
             ?name `UICreateGuideInst_defchans
             ?buttonText "Include global DefChans in wires.il"
             ?defValue t )
         )
        ( hiCreateAppForm
          ?name `UICreateGuideInst_Form
          ?formTitle "Create Guide Instance"  
          ?fields Fields
          ?callback "( UICreateGuideInstFormCB )"
        ) 
      ) 
    ) 
  ) 
)


( UICreateAction
  'UICreateGuideInstFormCB
  (lambda ()
    let((CellView)
      CellView = UIGetCellView()
    MakeGuideInst( UICreateGuideInst_Form->UICreateGuideInst_cellname->value 
                   UICreateGuideInst_Form->UICreateGuideInst_instname->value )
    when( UICreateGuideInst_Form->UICreateGuideInst_wires->value 
      MakeWiresSkill( CellView
                      UICreateGuideInst_Form->UICreateGuideInst_instname->value
                      UICreateGuideInst_Form->UICreateGuideInst_defchans->value )
    )
  ))
)


