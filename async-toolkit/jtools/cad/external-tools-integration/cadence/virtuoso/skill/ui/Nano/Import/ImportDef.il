; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

;
; menu item Nano/Import
;
( list "1.Import Routed DEF..." "UIImportRoutedDef" )


( UICreateMenuItemCallBack
  `UIImportRoutedDefCB
  (lambda ()
    hiDisplayForm( UIImportRoutedDef_Form ) 
  ) 
)

 
( UICreateAction
  'UIImportRoutedDefFormCB
  (lambda ()
    let( (CellView Command RoutedFileName DefNetsFileName RoutedCellView Msg ChildId 
          Status CellMsg LogFile SumFile InPort )
      CellView = UIGetCellView()

      ; check that we are properly in the flatten view
      when( CellView->viewName!="flatten"
        UIPopUpDialog("You should probably be running this menu from a \"flatten\" view.")
      )

      NanoRouteImport( ?CV CellView 
                       ?importView UIImportRoutedDef_Form->UIImportRoutedDef_routedView->value
                       ?deleteGNDShield UIImportRoutedDef_Form->UIImportRoutedDef_deletetiedown->value
      )
    )
  )
)


( UICreateComponent
  "UIImportRoutedDef_Form"
  ( UICreateConstructor 
    (lambda ( Children )
      let(( Fields )
        Fields = list(
          ( hiCreateStringField
            ?name `UIImportRoutedDef_routedView
            ?prompt "Routed View Name" 
            ?defValue "layout_pg" )
          ( hiCreateBooleanButton
            ?name `UIImportRoutedDef_deletetiedown
            ?buttonText "Delete GNDShield Cell"
            ?defValue t )
          ( hiCreateFormButton
            ?name `UIImportRoutedDef_Help
            ?buttonText "Tool Help"
            ?callback "nrOpenHtml( \"Import_Routed_DEF_help.html\" )" )
        )
        ( hiCreateAppForm
          ?name `UIImportRoutedDef_Form
          ?formTitle "Import Routed DEF"  
          ?fields Fields
          ?callback "( UIImportRoutedDefFormCB )"
        ) 
      ) 
    ) 
  ) 
)



