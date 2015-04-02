; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id: //depot/sw/cad/external-tools-integration/cadence/virtuoso/main/skill/ui/Fulcrum/Pins/PreroutePins.il#26 $
; $DateTime: 2005/06/17 14:39:47 $
; $Author: khe $

( list "Shrink Bali To Fiji Prelayout" "UIShrinkPrelayout" )

( UICreateMenuItemCallBack 
  `UIShrinkPrelayoutCB
  (lambda ()
    let( (CellName LibName LibName130 ViewName130)
      CellName= UIGetCellView()~>cellName
      LibName= car( NameParseCellName( CellName ) ) 
      LibName130=strcat( "130" LibName )
      ViewName130=UIGetCellView()~>viewName
      UIShrinkPrelayout_Form->UIShrinkPrelayout_CellName->value=UIGetCellView()~>cellName
      UIShrinkPrelayout_Form->UIShrinkPrelayout_LibName65->value=LibName
      UIShrinkPrelayout_Form->UIShrinkPrelayout_LibName130->value=LibName130
      hiDisplayForm( UIShrinkPrelayout_Form ) 
    )
) )

( UICreateAction
  'UIShrinkPrelayoutFormCB
  (lambda ()
    (let ( )
      ProcessedCellList=nil
      fjShrinkPrelayout( UIShrinkPrelayout_Form->UIShrinkPrelayout_CellName->value
              ?LibName130 UIShrinkPrelayout_Form->UIShrinkPrelayout_LibName130->value
              ?LibName65 UIShrinkPrelayout_Form->UIShrinkPrelayout_LibName65->value
              ?ViewName130 UIShrinkPrelayout_Form->UIShrinkPrelayout_ViewName130->value
              ?ViewName65 UIShrinkPrelayout_Form->UIShrinkPrelayout_ViewName65->value
              ?ratio UIShrinkPrelayout_Form->UIShrinkPrelayout_Ratio->value
              ?Grid UIShrinkPrelayout_Form->UIShrinkPrelayout_Grid->value
              ?DeleteWires UIShrinkPrelayout_Form->UIShrinkPrelayout_DeleteWires->value
              ?OverwritePrelayout UIShrinkPrelayout_Form->UIShrinkPrelayout_OverwritePrelayout->value
      )
    )
  ) 
)
    
( UICreateComponent
  "UIShrinkPrelayout_Form"
  ( UICreateConstructor 
    (lambda ( Children )
      (let (
            ( Fields
              ( list
                ( hiCreateStringField
                  ?name `UIShrinkPrelayout_CellName
                  ?prompt "CellName"
                  ?defValue "" )
                ( hiCreateStringField
                  ?name `UIShrinkPrelayout_LibName130
                  ?prompt "LibName 130n"
                  ?defValue "" )
                ( hiCreateStringField
                  ?name `UIShrinkPrelayout_ViewName130
                  ?prompt "ViewName 130n"
                  ?defValue "prelayout" )
                ( hiCreateStringField
                  ?name `UIShrinkPrelayout_LibName65
                  ?prompt "LibName 65n"
                  ?defValue "" )
                ( hiCreateStringField
                  ?name `UIShrinkPrelayout_ViewName65
                  ?prompt "ViewName 65n"
                  ?defValue "prelayout" )
                ( hiCreateFloatField
                  ?name `UIShrinkPrelayout_Ratio
                  ?prompt "Shrink Ratio"
                  ?defValue 0.6 )
                ( hiCreateFloatField
                  ?name `UIShrinkPrelayout_Grid
                  ?prompt "Snap To Grid"
                  ?defValue 0.005 )
                ( hiCreateBooleanButton
                  ?name `UIShrinkPrelayout_DeleteWires
                  ?buttonText "Delete Wires"
                  ?defValue t )
                ( hiCreateBooleanButton
                  ?name `UIShrinkPrelayout_OverwritePrelayout
                  ?buttonText "Overwrite Existing Prelayout"
                  ?defValue t )
                ) ) ) 
        ( hiCreateAppForm
          ?name `UIShrinkPrelayout_Form
          ?formTitle "Go To Location"  
          ?fields Fields
          ?callback "( UIShrinkPrelayoutFormCB )"
          ) ) )
) )
        

defun( fjShrinkPrelayout ( CellName
              @key
              (LibName65 "")
              (LibName130 "")
              (ViewName130 "prelayout")
              (ViewName65 "prelayout")
              (ratio 0.6)
              (Grid 0.005)
              (DeleteWires t)
              (OverwritePrelayout t)
              )
  let((temp CellView130 CellView65 DFIIDir)

    if( LibName65=="" then LibName65=car( NameParseCellName( CellName ) ))
    DFIIDir=ConfigFileGetValue( TheCDSConfigTable "DFII_DIR" )
    GDSIIHierMakeLibrary( LibName65 DFIIDir "tsmc65" )
    if( LibName130=="" then LibName130=strcat( "130" LibName65 ))
    if( nrOpenCellViewWritable( LibName65 CellName ViewName65 )==nil then
      printf("Prelayout %s %s(%s) was not writable, ignore OverwritePrelayout.\n"
        LibName65 CellName ViewName65)
      CellView65=nrOpenCellViewReadable( LibName65 CellName ViewName65 )
      ProcessedCellList=cons( CellView65 ProcessedCellList)      
    )

    if( member( CellName ProcessedCellList~>cellName ) then
      CellView65=car(setof( temp ProcessedCellList temp~>cellName==CellName ))
    else
      printf("Processing %s ...\n" CellName )
      CellView130=nrOpenCellViewReadable( LibName130 CellName ViewName130 )
      if( CellView130 == nil then 
         printf("Unable to open CellView %s %s(%s) for read."  LibName130 CellName ViewName130)
      )
      CellView65=betterCopyCellView( CellView130 LibName65 CellName ViewName65 nil nil t)
      if( CellView65 == nil then 
         printf("Unable %s %s(%s) for write"  LibName130 CellName ViewName130)
      ) 
      foreach(inst CellView65~>instances  
        if( inst~>libName=="tsmc13lg" then
          if( DeleteWires then dbDeleteObject(inst)
          else dbFlattenInst( inst 32 t t t))
        )
      ) 
      dbSave(CellView65)
      if( DeleteWires then
        foreach( shape CellView65~>shapes
          if(shape~>objType!="label" && shape~>layerName!="prBoundary" && shape~>pin==nil then
            dbDeleteObject(shape)
          )
        )
        foreach( inst cadr( NameFilterInstances( CellView65~>instances WiringCellLibCellPairRegExs ))
          dbDeleteObject(inst)
        )          
      )
      dbSave(CellView65)
      foreach( shape CellView65~>shapes
        case( shape~>objType
          ("rect"
            shape~>bBox=fxShrinkPointListToGrid(shape~>bBox ratio 0.01)  
          )
          ("polygon"
            shape~>points=fxShrinkPointListToGrid(shape~>points ratio Grid)  
          )
          ("path"
            shape~>points=fxShrinkPointListToGrid(shape~>points ratio Grid)
            shape~>width=fxShrinkXToGrid( shape~>width ratio 0.01)  
            shape~>beginExt=fxShrinkXToGrid( shape~>beginExt ratio Grid)  
            shape~>endExt=fxShrinkXToGrid( shape~>endExt ratio Grid)  
          )
          ("label"
            shape~>xy=fxShrinkPointToGrid(shape~>xy ratio Grid)  
            shape~>height=0.06
          )
          ("dot"
            shape~>xy=fxShrinkPointToGrid(shape~>xy ratio Grid)  
          )
          ( t
             printf("Warning: unknown objType %s \n" shape~>objType)
          )
        )
      )
      fxFixCellViewViaSize( CellView65 )
      foreach(inst CellView65~>instances
        inst~>xy=fxShrinkPointToGrid(inst~>xy ratio Grid) 
        cond( 
          ( temp=nrOpenCellViewReadable( inst~>libName inst~>cellName "prelayout" )
             if( OverwritePrelayout then temp=fjShrinkPrelayout( inst~>cellName
                                     ?LibName65 ""
                                     ?LibName130 ""
                                     ?ViewName130 ViewName130 
                                     ?ViewName65 ViewName65
                                     ?ratio ratio
                                     ?Grid Grid 
                                     ?DeleteWires DeleteWires 
                                     ?OverwritePrelayout OverwritePrelayout 
                                    )
               )
               inst~>master=temp )
          ( temp=nrOpenCellViewReadable( inst~>libName inst~>cellName "layout" )
               inst~>master=temp )
          ( temp=nrOpenCellViewReadable( inst~>libName inst~>cellName "shrink" )
               inst~>master=temp )
          ( t temp=fjShrinkPrelayout( inst~>cellName
                                     ?LibName65 ""
                                     ?LibName130 ""
                                     ?ViewName130 ViewName130 
                                     ?ViewName65 ViewName65
                                     ?ratio ratio
                                     ?Grid Grid 
                                     ?DeleteWires DeleteWires 
                                     ?OverwritePrelayout OverwritePrelayout 
                                    )
               inst~>master=temp )
        )     
      )
      dbSave(CellView65)
      ProcessedCellList=cons( CellView65 ProcessedCellList)
    )
    CellView65
  )
)

