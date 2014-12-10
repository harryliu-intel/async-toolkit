; Copyright 2006 Fulcrum Microsystems.  All rights reserved.
; $Id: $
; $DateTime: $
; $Author: $

;
; menu item Nano/Preroute
;
; reference function from layout/bus/bus.il
;
( list "Set Bus Wire Pattern..." "UISetBusPattern" )


( UICreateMenuItemCallBack
  `UISetBusPatternCB
  (lambda ()
    hiDisplayForm( UISetBusPattern_Form ) 
  ) 
)


( UICreateComponent
  "UISetBusPattern_Form"
  ( UICreateConstructor 
    (lambda ( Children )
      (let ( Fields )
         Fields = list(
           ( hiCreateStringField
             ?name `UISetBusPattern_Pattern
             ?prompt "Bus Pattern"
             ?defValue "" )
           ( hiCreateFormButton
             ?name `UISetBusPattern_Help
             ?buttonText "Tool Help"
             ?callback "nrOpenHtml( \"Preroute_Nets_help.html\" )" )
          )
        ( hiCreateAppForm
          ?name `UISetBusPattern_Form
          ?formTitle "Set Bus Wire Pattern"  
          ?fields Fields
          ?callback "( UISetBusPatternFormCB )"
        ) 
      ) 
    ) 
  ) 
)


( UICreateAction
  'UISetBusPatternFormCB
  (lambda ()
    SetPattern( UISetBusPattern_Form->UISetBusPattern_Pattern->value )
  )
)

