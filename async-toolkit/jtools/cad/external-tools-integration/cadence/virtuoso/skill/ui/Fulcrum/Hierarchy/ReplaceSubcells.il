; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id: //depot/sw/main/cad/external-tools-integration/cadence/virtuoso/skill/ui/Fulcrum/Hierarchy/ReplaceSubcells.il#1 $
; $DateTime: 2005/11/07 15:21:21 $
; $Author: stevemc $


( list "Recursively Replace Subcell Views" "UIReplaceSubcells" )

( UICreateMenuItemCallBack 
  `UIReplaceSubcellsCB
  (lambda ()
    ( hiDisplayForm UIReplaceSubcells_Form ) ) 
)

( UICreateAction
  'UIReplaceSubcellsFormCB
  (lambda ()
      ( RecursiveReplaceSubcells
        ( UIReplaceSubcells_Form->UIReplaceSubcells_Form_FromViewName->value )
        ( UIReplaceSubcells_Form->UIReplaceSubcells_Form_ToViewName->value )        
       ) )
)


( UICreateComponent
  "UIReplaceSubcells_Form"
  ( UICreateConstructor 
    (lambda ( Children )
      (let (
            ( Fields
              ( list
                ( hiCreateStringField
                  ?name `UIReplaceSubcells_Form_FromViewName
                  ?prompt "From View Name"
                  ?defValue "floorplan" )
                ( hiCreateStringField
                  ?name `UIReplaceSubcells_Form_ToViewName
                  ?prompt "To View Name"
                  ?defValue "layout" )
                ) ) )
        ( hiCreateAppForm
          ?name `UIReplaceSubcells_Form
          ?formTitle "Replace Subcells"  
          ?fields Fields
          ?callback "( UIReplaceSubcellsFormCB )"
          ) ) )
    )
)
        
 
