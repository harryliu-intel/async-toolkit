; Copyright 2006 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

( list "2.Save Floorplan" "UISaveFloorplan" )
( UICreateMenuItemCallBack
  `UISaveFloorplanCB
  (lambda ()
     let(()
       UISaveFloorplan_Form->UISaveFloorplan_FileName->value=sprintf( nil "%s.sav" UIGetCellView()~>cellName)
       hiDisplayForm( UISaveFloorplan_Form )
    ) 
  ) 
)

( UICreateAction
  'UISaveFloorplanFormCB
  (lambda ()
    let( (CellView p s SkipCellList)
    SkipCellList=nil
    if( UISaveFloorplan_Form->UISaveFloorplan_SkipCellFileName->value!="" then
      p=infile(UISaveFloorplan_Form->UISaveFloorplan_SkipCellFileName->value)
      if( p then
        while( fscanf( p "%s" s) SkipCellList=cons( s SkipCellList ))
      else
        SkipCellList=nil
      )
      SkipCellList=nil
    )
    CellView= UIGetCellView()
    flSaveCoordinates( CellView 
           UISaveFloorplan_Form->UISaveFloorplan_FileName->value
           ?SkipCellList SkipCellList 
           ?BlockLayer list("LOGO" "drawing")  )
    )
  )
)

( UICreateComponent
  "UISaveFloorplan_Form"
  ( UICreateConstructor 
    (lambda ( Children )
      (let ( Fields )
         Fields = list(
                 ( hiCreateStringField
                  ?name `UISaveFloorplan_SkipCellFileName
                  ?prompt "SkipCellFileName" 
                  ?defValue "skipCell.list" )
                 ( hiCreateStringField
                  ?name `UISaveFloorplan_FileName
                  ?prompt "FileName" 
                  ?defValue "autosave.sav" )
                 ) 
        ( hiCreateAppForm
          ?name `UISaveFloorplan_Form
          ?formTitle "Save Floorplan"  
          ?fields Fields
          ?callback "( UISaveFloorplanFormCB )"
          ) 
      ) 
    ) 
  ) 
)
