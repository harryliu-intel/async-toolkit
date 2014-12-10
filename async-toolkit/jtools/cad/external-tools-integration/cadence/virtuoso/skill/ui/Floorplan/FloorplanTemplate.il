; Copyright 2006 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

( list "5.Template Floorplan" "UITemplateFloorplan" )
( UICreateMenuItemCallBack
  `UITemplateFloorplanCB
  (lambda ()
     let((cellName)
       cellName=UIGetCellView()~>cellName
       UITemplateFloorplan_Form->UITemplateFloorplan_TemplateCellName->value=cellName
       UITemplateFloorplan_Form->UITemplateFloorplan_TemplateLibName->value=car( NameParseCellName( cellName ))
       UITemplateFloorplan_Form->UITemplateFloorplan_AutoSaveFileName->value=sprintf( nil "%s.autosave" UIGetCellView()~>cellName)
       hiDisplayForm( UITemplateFloorplan_Form )
    ) 
  ) 
)

( UICreateAction
  'UITemplateFloorplanFormCB
  (lambda ()
    let( ()
      if( UITemplateFloorplan_Form->UITemplateFloorplan_AutoSave->value
          flSaveCoordinates( UIGetCellView() 
                                          UITemplateFloorplan_Form->UITemplateFloorplan_AutoSaveFileName->value 
                                          ?SkipCellList nil 
                                          ?BlockLayer list("LOGO" "drawing")
                                          )
          )
      flFloorplanTemplating( UIGetCellView() 
                        UITemplateFloorplan_Form->UITemplateFloorplan_TemplateCellName->value
                        ?srcLibName UITemplateFloorplan_Form->UITemplateFloorplan_TemplateLibName->value
                        ?srcViewName UITemplateFloorplan_Form->UITemplateFloorplan_TemplateViewName->value
                        ?ResolveOverlap UITemplateFloorplan_Form->UITemplateFloorplan_ResolveOverlap->value
                        ?SnapXGrid UITemplateFloorplan_Form->UITemplateFloorplan_SnapXGrid->value
                        ?SnapXInst UITemplateFloorplan_Form->UITemplateFloorplan_SnapXInst->value
                        ?Recursive UITemplateFloorplan_Form->UITemplateFloorplan_Recursive->value
                        ?DeletePins UITemplateFloorplan_Form->UITemplateFloorplan_DeletePins->value
                        )
    )
  )
)

( UICreateComponent
  "UITemplateFloorplan_Form"
  ( UICreateConstructor 
    (lambda ( Children )
      (let ( Fields )
         Fields = list(
                 ( hiCreateStringField
                  ?name `UITemplateFloorplan_TemplateLibName
                  ?prompt "Template LibName" 
                  ?defValue "" ) 
                 ( hiCreateStringField
                  ?name `UITemplateFloorplan_TemplateCellName
                  ?prompt "Template CellName" 
                  ?defValue "" )
                 ( hiCreateStringField
                  ?name `UITemplateFloorplan_TemplateViewName
                  ?prompt "Template ViewName" 
                  ?defValue "floorplan" ) 
                 ( hiCreateStringField
                  ?name `UITemplateFloorplan_AutoSaveFileName
                  ?prompt "AutoSave FileName" 
                  ?defValue "" )
                 ( hiCreateBooleanButton
                   ?name `UITemplateFloorplan_AutoSave
                   ?buttonText "AutoSave"
                   ?defValue nil )
                 ( hiCreateBooleanButton
                   ?name `UITemplateFloorplan_Recursive
                   ?buttonText "Recursive"
                   ?defValue nil )
                 ( hiCreateBooleanButton
                  ?name `UITemplateFloorplan_ResolveOverlap
                  ?buttonText "Resolve Overlaps" 
                  ?defValue nil )
                 ( hiCreateBooleanButton
                  ?name `UITemplateFloorplan_DeletePins
                  ?buttonText "Delete Pins and Wires" 
                  ?defValue t )
                 ( hiCreateFloatField
                  ?name `UITemplateFloorplan_SnapXGrid
                  ?prompt "Snap X to Grid"
                  ?defValue 0.01 )
                 ( hiCreateBooleanButton
                  ?name `UITemplateFloorplan_SnapXInst
                  ?buttonText "Snap X to Instance Alignment Property"
                  ?defValue nil )
               ) 
        ( hiCreateAppForm
          ?name `UITemplateFloorplan_Form
          ?formTitle "Template Floorplan"  
          ?fields Fields
          ?callback "( UITemplateFloorplanFormCB )"
          ) 
      ) 
    ) 
  ) 
)
