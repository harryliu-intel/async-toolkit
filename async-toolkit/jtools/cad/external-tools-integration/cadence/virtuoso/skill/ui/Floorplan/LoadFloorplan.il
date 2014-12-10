; Copyright 2006 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

( list "3.Load Floorplan" "UILoadFloorplan" )
( UICreateMenuItemCallBack
  `UILoadFloorplanCB
  (lambda ()
     let(()
       UILoadFloorplan_Form->UILoadFloorplan_FileName->value=sprintf( nil "%s.autosave" UIGetCellView()~>cellName)
       hiDisplayForm( UILoadFloorplan_Form )
    ) 
  ) 
)

( UICreateAction
  'UILoadFloorplanFormCB
  (lambda ()
    let( (CellView p s SkipCellList)
    SkipCellList=nil
    if( UILoadFloorplan_Form->UILoadFloorplan_SkipCellFileName->value!="" then
      p=infile(UILoadFloorplan_Form->UILoadFloorplan_SkipCellFileName->value)
      if( p then
        while( fscanf( p "%s" s) SkipCellList=cons( s SkipCellList ))
      else
        SkipCellList=nil
      )
      SkipCellList=nil
    )
    CellView= UIGetCellView()
    flLoadCoordinates( CellView 
           UILoadFloorplan_Form->UILoadFloorplan_FileName->value
           ?SkipCellList SkipCellList 
           ?BlockLayer list("LOGO" "drawing") 
           ?SaveCellView UILoadFloorplan_Form->UILoadFloorplan_SaveCellView->value )
    )
  )
)

( UICreateComponent
  "UILoadFloorplan_Form"
  ( UICreateConstructor 
    (lambda ( Children )
      (let ( Fields )
         Fields = list(
                 ( hiCreateStringField
                  ?name `UILoadFloorplan_SkipCellFileName
                  ?prompt "SkipCellFileName" 
                  ?defValue "skipCell.list" )
                 ( hiCreateStringField
                  ?name `UILoadFloorplan_FileName
                  ?prompt "FileName" 
                  ?defValue "autosave.sav" )
                 ( hiCreateBooleanButton
                  ?name `UILoadFloorplan_SaveCellView
                  ?buttonText "Save All CellViews" 
                  ?defValue t )
                 ) 
        ( hiCreateAppForm
          ?name `UILoadFloorplan_Form
          ?formTitle "Load Floorplan"  
          ?fields Fields
          ?callback "( UILoadFloorplanFormCB )"
          ) 
      ) 
    ) 
  ) 
)
