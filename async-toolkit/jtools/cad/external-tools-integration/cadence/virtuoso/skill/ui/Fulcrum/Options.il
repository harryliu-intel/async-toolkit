; Copyright 2004 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

( list "Max Heap Size" "UIOptions" )

( UICreateMenuItemCallBack
  `UIOptionsCB
  (lambda ()
    ( hiDisplayForm UIOptions_Form ) ) )

( UICreateComponent
  "UIOptions_Form"
  ( UICreateConstructor
    (lambda ( Children )
      (let (
            ( Fields
              ( list
                ( hiCreateIntField
                  ?name `UIOptions_Form_MaxHeapSize
                  ?prompt "Max Heap Size (M)"
                  ?defValue 1800 )
                ( hiCreateStringField
                  ?name `UIOptions_Form_JavaArguments
                  ?prompt "Additional Java Arguments"
                  ?defValue "" ) ) ) )
  ( hiCreateAppForm
    ?name `UIOptions_Form
    ?formTitle "Java Options"
    ?fields Fields
    ) ) ) ) )
