; Copyright 2004 Fulcrum Microsystems.  All rights reserved.
; $Id: //depot/sw/main/cad/external-tools-integration/cadence/virtuoso/skill/ui/Fulcrum/LeafAbstractList.il#1 $
; $DateTime: 2009/10/05 15:21:21 $
; $Author: stevemc $

( list "Set Cell Align Property" "UISetAlignment" )

( UICreateMenuItemCallBack
  `UISetAlignmentCB
  (lambda ()
    ( hiDisplayForm UISetAlignment_Form ) ) )

( UICreateAction
  'UISetAlignmentFormCB
  (lambda ()
    (let (leaflist abstracts overwrite layouts)
      (SetAlignmentUI)
    )
  )
)


( UICreateComponent
  "UISetAlignment_Form"
  ( UICreateConstructor
    (lambda ( Children )
      (let (
            ( Fields
              ( list
                ( hiCreateFloatField
                  ?name `UISetAlignment_Form_XAlign
                  ?prompt "X Alignment Value"
                  ?defValue (GetXAlignmentProp (geGetWindowCellView)) )
                ( hiCreateFloatField
                  ?name `UISetAlignment_Form_YAlign
                  ?prompt "Y Alignment Value"
                  ?defValue 5.76 )
               )
             ))


  ( hiCreateAppForm
    ?name `UISetAlignment_Form
    ?formTitle "Attach Cell Alignment Property"
    ?fields Fields
    ?callback "(UISetAlignmentFormCB)"
    ) ) ) ) )

