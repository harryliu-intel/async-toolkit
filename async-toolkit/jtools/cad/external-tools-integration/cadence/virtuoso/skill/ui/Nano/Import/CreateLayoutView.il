; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

;
; menu item Nano/Import
;
( list "2.Create Layout Cellview..." "UICreateLayoutView" )


( UICreateMenuItemCallBack
  `UICreateLayoutViewCB
  (lambda ()
    UICreateLayoutView_Form->UICreateLayoutView_RoutedView->value = UIGetCellView()~>viewName
    hiDisplayForm( UICreateLayoutView_Form ) 
  ) 
)

 
( UICreateComponent
  "UICreateLayoutView_Form"
  ( UICreateConstructor 
    (lambda ( Children )
      (let ( Fields )
        Fields = list(
          ( hiCreateStringField
            ?name `UICreateLayoutView_RoutedView
            ?prompt "Routed View Name" 
            ?defValue "layout_pg" )
          ( hiCreateStringField
            ?name `UICreateLayoutView_LayoutView
            ?prompt "Layout View Name" 
            ?defValue "layout" )
          ( hiCreateBooleanButton
            ?name `UICreateLayoutView_Overwrite
            ?buttonText "Overwrite Layout View"
            ?defValue t )
          ( hiCreateFormButton
            ?name `UICreateLayoutView_Help
            ?buttonText "Tool Help"
            ?callback "nrOpenHtml( \"Create_Layout_Cellview_help.html\" )" )
        )
        ( hiCreateAppForm
          ?name `UICreateLayoutView_Form
          ?formTitle "Create Layout Cellview"  
          ?fields Fields
          ?callback "( UICreateLayoutViewFormCB )"
        ) 
      ) 
    ) 
  ) 
)


( UICreateAction
  'UICreateLayoutViewFormCB
  (lambda ()
    let(( CellView LayoutCellView PowerGrid Msg Inst NumDeleted )
      CellView = UIGetCellView()

      ; copy source cellview to layout cellview
      LayoutCellView = dbCopyCellView( CellView CellView~>libName CellView~>cellName 
        UICreateLayoutView_Form->UICreateLayoutView_LayoutView->value
        nil nil 
        UICreateLayoutView_Form->UICreateLayoutView_Overwrite->value
      )

      if( LayoutCellView then
        dbSave(LayoutCellView)

        ; delete power grid (and routing obstructions since it should be a subcell)
        PowerGrid = dbGetInstanceByName( LayoutCellView "tiehilo" )
        if( PowerGrid then
          dbDeleteObject(PowerGrid)
          dbSave(LayoutCellView)
        else
          Msg = sprintf( nil "ERROR: Unable to delete Power Grid instance from %s cellview."
                         UICreateLayoutView_Form->UICreateLayoutView_LayoutView->value )
          nrToolStatus( "Create Layout View" Msg )
        )

        ; delete any buswires cellviews
        NumDeleted = 0
        foreach( Inst CellView~>instances
          if( Inst~>viewName == "buswires" then
            printf( "Deleting %s (%s).\n" Inst~>cellName Inst~>viewName )
            dbDeleteObject( Inst )
            NumDeleted = NumDeleted + 1
          )
        )
        printf( "Found and deleted %d buswire instances.\n" NumDeleted)
      else
        Msg = sprintf( nil "WARNING: Can not overwrite existing cellview %s (%s).\n" 
                       CellView~>cellName 
                       UICreateLayoutView_Form->UICreateLayoutView_LayoutView->value)
        nrToolStatus( "Create Layout Cellview" Msg )
      )    
    )
  )
)

