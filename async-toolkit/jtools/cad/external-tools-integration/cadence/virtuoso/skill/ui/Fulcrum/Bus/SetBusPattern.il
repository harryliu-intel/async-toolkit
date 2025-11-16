; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

; Copyright 2006 Fulcrum Microsystems.  All rights reserved.
; $Id: $
; $DateTime: $
; $Author: $

;
; menu item Fulcrum/Bus
;
; reference function from layout/bus/bus.il
;
( list "Set Bus Wire Pattern  Ctrl-b" "UISetBusPattern" )


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
             ?defValue "e1of4" )
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

