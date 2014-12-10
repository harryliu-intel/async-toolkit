; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$


( list "Inline" "UIInline" )

( UICreateMenuItemCallBack 
  `UIInlineCB
  (lambda ()
    
    UIInline_Form->UIInline_Form_ConnectivityViewName->value =
    ( getq ( UIGetCellView ) viewName )
    
    ( hiDisplayForm UIInline_Form ) ) )

( UICreateAction
  'UIInlineFormCB
  (lambda ()
    (let (
          ( SelectedInstances
            ( setof Obj ( geGetSelectedSet ) ( equal ( getq Obj objType ) "inst" ) ) ) )      
      ( InlineInstances
        ( UIGetCellView )
        ( UIGetCellView )
        SelectedInstances
        UIInline_Form->UIInline_Form_ConnectivityViewName->value
        UIInline_Form->UIInline_Form_UpdateConnectivityCellView->value
        FoldableCellLibCellPairRegExs
        SuperStackLibCellPairRegExs
        ?Verbose UIInline_Form->UIInline_Form_Verbose->value
        ?CopyPins UIInline_Form->UIInline_Form_CopyPins->value
       ) ) ) )

( UICreateComponent
  "UIInline_Form"
  ( UICreateConstructor 
    (lambda ( Children )
      (let (
            ( Fields
              ( list
                ( hiCreateStringField
                  ?name `UIInline_Form_ConnectivityViewName
                  ?prompt "Connectivity View Name"
                  ?defValue "" )
                ( hiCreateBooleanButton
                  ?name `UIInline_Form_UpdateConnectivityCellView
                  ?buttonText "Update Connectivity CellView"
                  ?defValue nil )
               ( hiCreateBooleanButton
                  ?name `UIInline_Form_Verbose
                  ?buttonText "Verbose"
                  ?defValue t )
               ( hiCreateBooleanButton
                  ?name `UIInline_Form_CopyPins
                  ?buttonText "Copy Pins"
                  ?defValue nil )
                
                ) ) )
        ( hiCreateAppForm
          ?name `UIInline_Form
          ?formTitle "Inline"  
          ?fields Fields
          ?callback "( UIInlineFormCB )"
          ) ) )
    ) )
        
 
