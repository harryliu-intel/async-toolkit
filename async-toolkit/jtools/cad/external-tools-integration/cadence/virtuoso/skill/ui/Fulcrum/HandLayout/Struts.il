; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

( list "Struts" "UIStruts" )

( UICreateMenuItemCallBack 
  `UIStrutsCB
  (lambda ()    
    ( hiDisplayForm UIStruts_Form ) ) )


( UICreateAction
  'UIStruts_FormCB
  (lambda () nil ) )

( UICreateAction
  'UIDrawVStrutsCB
  (lambda ()
    (VStrutsDrawStruts
         UIStruts_Form->UIStruts_Form_GND->value 
         UIStruts_Form->UIStruts_Form_Vdd->value 
         UIStruts_Form->UIStruts_Form_Extent->value)
) )


( UICreateAction
  'UIDrop12ContactsCB
  (lambda ()
    (VStrutsDropM12Vias 
         UIStruts_Form->UIStruts_Form_GND->value 
         UIStruts_Form->UIStruts_Form_Vdd->value 
         UIStruts_Form->UIStruts_Form_Extent->value)
 ))

( UICreateAction
  'UIDrop23ContactsCB
  (lambda ()
    (VStrutsDropM23Vias 
         UIStruts_Form->UIStruts_Form_GND->value 
         UIStruts_Form->UIStruts_Form_Vdd->value )
 ))

( UICreateAction
  'UIDeleteObjectsCB
  (lambda ()
    (VStrutsDeleteObjects)
 ))


( UICreateComponent
  "UIStruts_Form"
  ( UICreateConstructor 
    (lambda ( Children )
      (let (
            ( Fields
              ( list
                ( hiCreateBooleanButton
                  ?name `UIStruts_Form_GND
                  ?buttonText "GND"
                  ?defValue t )
                ( hiCreateBooleanButton
                  ?name `UIStruts_Form_Vdd
                  ?buttonText "Vdd"
                  ?defValue t )
                ( hiCreateCyclicField
                  ?name `UIStruts_Form_Extent
                  ?prompt "Which Instances"
                  ?choices ( list "All" "Selected" ) )
                ( hiCreateButton
                  ?name `UIStruts_Form_DrawVStruts
                  ?buttonText "Draw VStruts"
                  ?callback "( UIDrawVStrutsCB )" )
                ( hiCreateButton
                  ?name `UIStruts_Form_Drop12Contacts
                  ?buttonText "Drop M12 Contacts"
                  ?callback "( UIDrop12ContactsCB )" )
                ( hiCreateButton
                  ?name `UIStruts_Form_Drop23Contacts
                  ?buttonText "Drop M23 Contacts"
                  ?callback "( UIDrop23ContactsCB )" )
                ( hiCreateButton
                  ?name `UIStruts_Form_DeleteObjects
                  ?buttonText "Delete All"
                  ?callback "( UIDeleteObjectsCB )" )
                ) ) )
        ( hiCreateAppForm
          ?name `UIStruts_Form
          ?formTitle "Struts"  
          ?fields Fields
          ?callback "( UIStruts_FormCB )"
          ) ) )
    ) )
