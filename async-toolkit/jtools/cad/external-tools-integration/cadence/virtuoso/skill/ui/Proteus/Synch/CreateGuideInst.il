; Copyright 2006 Fulcrum Microsystems.  All rights reserved.
; $Id: $
; $DateTime: $
; $Author: $

;
; menu item Synch/Bus
( list "2.Create Guide Instance,,," "UISyCreateGuideInst" )


( UICreateMenuItemCallBack
  `UISyCreateGuideInstCB
  (lambda ()
    let((CellView)
      CellView = UIGetCellView()
      CellBase = cadr( reverse( parseString( CellView~>cellName "." ) ))
      UISyCreateGuideInst_Form->UISyCreateGuideInst_cellname->value =
        strcat( CellView~>libName ".wires." CellBase "_buswires")
    hiDisplayForm( UISyCreateGuideInst_Form ) 
  ))
)


( UICreateComponent
  "UISyCreateGuideInst_Form"
  ( UICreateConstructor 
    (lambda ( Children )
      (let ( Fields )
         Fields = list(
           ( hiCreateStringField
             ?name `UISyCreateGuideInst_cellname
             ?prompt "Cell Name"
             ?defValue "" )
           ( hiCreateStringField
             ?name `UISyCreateGuideInst_instname
             ?prompt "Instance Name"
             ?defValue "buswires" )
           ( hiCreateBooleanButton
             ?name `UISyCreateGuideInst_wires
             ?buttonText "Create wires.il"
             ?defValue t )
           ( hiCreateBooleanButton
             ?name `UISyCreateGuideInst_defchans
             ?buttonText "Include global DefChans in wires.il"
             ?defValue t )
         )
        ( hiCreateAppForm
          ?name `UISyCreateGuideInst_Form
          ?formTitle "Create Guide Instance"  
          ?fields Fields
          ?callback "( UISyCreateGuideInstFormCB )"
        ) 
      ) 
    ) 
  ) 
)


( UICreateAction
  'UISyCreateGuideInstFormCB
  (lambda ()
    MakeGuideInst( UISyCreateGuideInst_Form->UISyCreateGuideInst_cellname->value 
                   UISyCreateGuideInst_Form->UISyCreateGuideInst_instname->value )
    when( UISyCreateGuideInst_Form->UISyCreateGuideInst_wires->value 
      MakeWiresSkill( UISyCreateGuideInst_Form->UISyCreateGuideInst_instname->value
                      UISyCreateGuideInst_Form->UISyCreateGuideInst_defchans->value )
    )
  ) 
)


