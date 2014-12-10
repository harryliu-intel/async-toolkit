; Copyright 2006 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

( list "4.Init Floorplan" "UIInitFloorplan" )
( UICreateMenuItemCallBack
  `UIInitFloorplanCB
  (lambda ()
     let(()
       if( !geGetSelectedSet() then UIInitFloorplan_Form->UIInitFloorplan_DoNotMoveExistingInstances->value=nil )
       hiDisplayForm( UIInitFloorplan_Form )
    ) 
  ) 
)

( UICreateAction
  'UIInitFloorplanFormCB
  (lambda ()
    let( (CellView)

    TEMP=ConfigFileGetValue( TheCDSConfigTable "TEMP" )
    CellView= UIGetCellView()
    flInitFloorplan( CellView~>libName CellView~>cellName CellView~>viewName 
                      ?Verbose UIInitFloorplan_Form->UIInitFloorplan_Verbose->value 
                      ?RegenerateFromCast UIInitFloorplan_Form->UIInitFloorplan_RegenerateFromCast->value
                      ?SnapXGrid  UIInitFloorplan_Form->UIInitFloorplan_SnapXGrid->value
                      ?ScriptFileName UIInitFloorplan_Form->UIInitFloorplan_ScriptFileName->value
                      ?MaxHeapSize nrGetMaxHeapSize( )
                      ?DoNotMoveExistingInstances UIInitFloorplan_Form->UIInitFloorplan_DoNotMoveExistingInstances->value
           )
    )
  )
)

( UICreateComponent
  "UIInitFloorplan_Form"
  ( UICreateConstructor 
    (lambda ( Children )
      (let ( Fields )
         Fields = list(
                ( hiCreateBooleanButton
                  ?name `UIInitFloorplan_DoNotMoveExistingInstances
                  ?buttonText "Do Not Move Existing Instances" 
                  ?defValue t )
                ( hiCreateBooleanButton
                  ?name `UIInitFloorplan_RegenerateFromCast
                  ?buttonText "Regenerate From Cast" 
                  ?defValue nil )
                ( hiCreateFloatField
                  ?name `UIInitFloorplan_SnapXGrid
                  ?prompt "Snap X to Grid"
                  ?defValue 0.01 )
                ( hiCreateBooleanButton
                  ?name `UIInitFloorplan_Verbose
                  ?buttonText "Verbose" 
                  ?defValue nil )
                ( hiCreateStringField
                  ?name `UIInitFloorplan_ScriptFileName
                  ?prompt "Filter Script" 
                  ?defValue "" )
                ( hiCreateFormButton
                  ?name `UIInitFloorplan_Help
                  ?buttonText "Tool Help"
                  ?callback "nrOpenHtml( \"Resolve_Overlaps_help.html\" )" )
                 ) 
        ( hiCreateAppForm
          ?name `UIInitFloorplan_Form
          ?formTitle "Init Floorplan"  
          ?fields Fields
          ?callback "( UIInitFloorplanFormCB )"
          ) 
      ) 
    ) 
  ) 
)
