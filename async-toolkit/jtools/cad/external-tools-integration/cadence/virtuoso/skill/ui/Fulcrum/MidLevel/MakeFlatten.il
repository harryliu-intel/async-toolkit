; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id: //depot/sw/intel/cad/external-tools-integration/cadence/virtuoso/skill/ui/Fulcrum/Pins/DrawPins.il#4 $
; $DateTime: 2012/07/11 17:39:18 $
; $Author: pankala $

( list "2. Make Flatten" "UIMakeFlatten" )


( UICreateMenuItemCallBack 
  `UIMakeFlattenCB
  (lambda ()
    ( hiDisplayForm UIMakeFlatten_Form ) )
)


( UICreateAction
  'UIMakeFlattenFormCB
  (lambda ()
          ( MakeFlatten
              ?RegenerateFromCast UIMakeFlatten_Form->UIMakeFlatten_Form_RegenerateCast->value
              ?doFillPoly UIMakeFlatten_Form->UIMakeFlatten_Form_FillPoly->value
              ?doPowerGridAbstract UIMakeFlatten_Form->UIMakeFlatten_Form_PGAbstract->value ) 
    )
)
    


( UICreateComponent
  "UIMakeFlatten_Form"
  ( UICreateConstructor 
    (lambda ( Children )
      (let (
            ( Fields
              ( list
                ( hiCreateBooleanButton
                  ?name `UIMakeFlatten_Form_RegenerateCast
                  ?buttonText "Force Cast Refresh"
                  ?defValue nil )
                ( hiCreateBooleanButton
                  ?name `UIMakeFlatten_Form_FillPoly
                  ?buttonText "Poly Fill"
                  ?defValue t )
                ( hiCreateBooleanButton
                  ?name `UIMakeFlatten_Form_PGAbstract
                  ?buttonText "Power Grid Abstract"
                  ?defValue t )
                ) ) )
        ( hiCreateAppForm
          ?name `UIMakeFlatten_Form
          ?formTitle "Make Flatten"  
          ?fields Fields
          ?callback "( UIMakeFlattenFormCB )"
          ) ) )
   ) )


