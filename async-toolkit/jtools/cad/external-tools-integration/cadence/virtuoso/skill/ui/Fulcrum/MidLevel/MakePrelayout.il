; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id: //depot/sw/intel/cad/external-tools-integration/cadence/virtuoso/skill/ui/Fulcrum/Pins/DrawPins.il#4 $
; $DateTime: 2012/07/11 17:39:18 $
; $Author: pankala $

( list "1. Make Prelayout" "UIMakePrelayout" )


( UICreateMenuItemCallBack 
  `UIMakePrelayoutCB
  (lambda ()
    ( hiDisplayForm UIMakePrelayout_Form ) )
)


( UICreateAction
  'UIMakePrelayoutFormCB
  (lambda ()
          ( MakePrelayout
              ?RegenerateFromCast UIMakePrelayout_Form->UIMakePrelayout_Form_RegenerateCast->value
              ?Overwrite UIMakePrelayout_Form->UIMakePrelayout_Form_Overwrite->value
              ?SyncToNetlist UIMakePrelayout_Form->UIMakePrelayout_Form_SyncToNetlist->value ) 
    )
)
    


( UICreateComponent
  "UIMakePrelayout_Form"
  ( UICreateConstructor 
    (lambda ( Children )
      (let (
            ( Fields
              ( list
                ( hiCreateBooleanButton
                  ?name `UIMakePrelayout_Form_RegenerateCast
                  ?buttonText "Force Cast Refresh"
                  ?defValue nil )
                ( hiCreateBooleanButton
                  ?name `UIMakePrelayout_Form_Overwrite
                  ?buttonText "Overwrite Existing Views"
                  ?defValue t )
                ( hiCreateBooleanButton
                  ?name `UIMakePrelayout_Form_SyncToNetlist
                  ?buttonText "Sync To Netlist"
                  ?defValue t )
                ) ) )
        ( hiCreateAppForm
          ?name `UIMakePrelayout_Form
          ?formTitle "Make Prelayout"  
          ?fields Fields
          ?callback "( UIMakePrelayoutFormCB )"
          ) ) )
   ) )


