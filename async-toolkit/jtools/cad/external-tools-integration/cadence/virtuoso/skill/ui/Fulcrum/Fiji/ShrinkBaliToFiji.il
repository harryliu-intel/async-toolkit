; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id: //depot/sw/cad/external-tools-integration/cadence/virtuoso/main/skill/ui/Fulcrum/Pins/PreroutePins.il#26 $
; $DateTime: 2005/06/17 14:39:47 $
; $Author: khe $

( list "Shrink Bali To Fiji" "UIShrinkBaliToFiji" )

( UICreateMenuItemCallBack 
  `UIShrinkBaliToFijiCB
  (lambda ()
    let( (CellName LibName LibName130 ViewName130)
      CellName= UIGetCellView()~>cellName
      LibName= car( NameParseCellName( CellName ) ) 
      LibName130=strcat( "130" LibName )
      ViewName130=UIGetCellView()~>viewName
      UIShrinkBaliToFiji_Form->UIShrinkBaliToFiji_CellName->value=UIGetCellView()~>cellName
      UIShrinkBaliToFiji_Form->UIShrinkBaliToFiji_LibName65->value=LibName
      UIShrinkBaliToFiji_Form->UIShrinkBaliToFiji_LibName130->value=LibName130
      UIShrinkBaliToFiji_Form->UIShrinkBaliToFiji_ViewName130->value=ViewName130
      hiDisplayForm( UIShrinkBaliToFiji_Form ) 
    )
) )

( UICreateAction
  'UIShrinkBaliToFijiFormCB
  (lambda ()
    (let ( )
      fjShrinkCellView( UIShrinkBaliToFiji_Form->UIShrinkBaliToFiji_CellName->value
              ?LibName130 UIShrinkBaliToFiji_Form->UIShrinkBaliToFiji_LibName130->value
              ?LibName65 UIShrinkBaliToFiji_Form->UIShrinkBaliToFiji_LibName65->value
              ?ViewName130 UIShrinkBaliToFiji_Form->UIShrinkBaliToFiji_ViewName130->value
              ?ViewName65 UIShrinkBaliToFiji_Form->UIShrinkBaliToFiji_ViewName65->value
              ?ratio UIShrinkBaliToFiji_Form->UIShrinkBaliToFiji_Ratio->value
              ?Grid UIShrinkBaliToFiji_Form->UIShrinkBaliToFiji_Grid->value
              ?DeleteWires UIShrinkBaliToFiji_Form->UIShrinkBaliToFiji_DeleteWires->value
      )
    )
  ) 
)
    
( UICreateComponent
  "UIShrinkBaliToFiji_Form"
  ( UICreateConstructor 
    (lambda ( Children )
      (let (
            ( Fields
              ( list
                ( hiCreateStringField
                  ?name `UIShrinkBaliToFiji_CellName
                  ?prompt "CellName"
                  ?defValue "" )
                ( hiCreateStringField
                  ?name `UIShrinkBaliToFiji_LibName130
                  ?prompt "LibName 130n"
                  ?defValue "" )
                ( hiCreateStringField
                  ?name `UIShrinkBaliToFiji_ViewName130
                  ?prompt "ViewName 130n"
                  ?defValue "" )
                ( hiCreateStringField
                  ?name `UIShrinkBaliToFiji_LibName65
                  ?prompt "LibName 65n"
                  ?defValue "" )
                ( hiCreateStringField
                  ?name `UIShrinkBaliToFiji_ViewName65
                  ?prompt "ViewName 65n"
                  ?defValue "shrink" )
                ( hiCreateFloatField
                  ?name `UIShrinkBaliToFiji_Ratio
                  ?prompt "Shrink Ratio"
                  ?defValue 0.6 )
                ( hiCreateFloatField
                  ?name `UIShrinkBaliToFiji_Grid
                  ?prompt "Snap To Grid"
                  ?defValue 0.005 )
                ( hiCreateBooleanButton
                  ?name `UIShrinkBaliToFiji_DeleteWires
                  ?buttonText "Delete Wires"
                  ?defValue t )
                ) ) ) 
        ( hiCreateAppForm
          ?name `UIShrinkBaliToFiji_Form
          ?formTitle "Go To Location"  
          ?fields Fields
          ?callback "( UIShrinkBaliToFijiFormCB )"
          ) ) )
) )
        
 
defun( fxSnapXToGrid (x Grid)
  if( x floor(x/Grid+0.5)*Grid nil )
)
defun( fxSnapPointToGrid (point Grid)
  let((x)
    foreach( mapcar x point fxSnapXToGrid( x Grid))
  )
)
defun( fxSnapPointListToGrid (pointList Grid)
  let((point)
    foreach( mapcar point pointList fxSnapPointToGrid( point Grid))
  )
)
defun( fxShrinkXToGrid (x ratio Grid)
  if( x floor(x*ratio/Grid+0.5)*Grid nil )
)

defun( fxShrinkPointToGrid ( point ratio Grid)
  let((x)
    foreach( mapcar x point fxShrinkXToGrid( x ratio Grid))
  )
)

defun( fxShrinkPointListToGrid (pointList ratio Grid)
  let((point)
    foreach( mapcar point pointList fxShrinkPointToGrid( point ratio Grid))
  )
)
defun( fxViaSize (bBox size
         @key
         (Grid 0.005)
         )
  prog((x1 y1 x2 y2 x y)
    x1=leftEdge(bBox)
    x2=rightEdge(bBox)
    y1=bottomEdge(bBox)
    y2=topEdge(bBox)
    if(abs(x1-y1)==size && abs(x2-y2)==size then return bBox)
    x=fxSnapXToGrid((x1+x2)/2 Grid)
    y=fxSnapXToGrid((y1+y2)/2 Grid)
    return(list( x-size/2:y-size/2 x+size/2:y+size/2 ))
  )
)
defun( fxFixCellViewViaSize ( CellView )
  let((shape)
    foreach( shape CellView~>shapes
      if( shape~>purpose=="drawing" && shape~>objType=="rect" then
        case( shape~>layerName
          ("CO"
             shape~>bBox=fxViaSize( shape~>bBox 0.09 )
          )     
          ("VIA1"
             shape~>bBox=fxViaSize( shape~>bBox 0.10 )
          )     
          ("VIA2"
             shape~>bBox=fxViaSize( shape~>bBox 0.10 )
          )     
          ("VIA3"
             shape~>bBox=fxViaSize( shape~>bBox 0.10 )
          )     
          ("VIA4"
             shape~>bBox=fxViaSize( shape~>bBox 0.10 )
          )     
          ("VIA5"
             shape~>bBox=fxViaSize( shape~>bBox 0.10 )
          )     
          ("VIA6"
             shape~>bBox=fxViaSize( shape~>bBox 0.10 )
          )     
          ("VIA7"
             shape~>bBox=fxViaSize( shape~>bBox 0.20 )
          )     
          ("VIA8"
             shape~>bBox=fxViaSize( shape~>bBox 0.20 )
          )     
        )
      )
    )
    CellView
  )
)

procedure( fjShrinkCellView( CellName 
              @key
              (LibName65 "")
              (LibName130 "")
              (ViewName130 "layout")
              (ViewName65 "shrink")
              (ratio 0.6)
              (Grid 0.005)
              (DeleteWires nil)
              )
  let((DFIIDir CellView130 CellView65 inst shape temp)
    if( LibName65=="" then LibName65=car( NameParseCellName( CellName ) ))
    DFIIDir=ConfigFileGetValue( TheCDSConfigTable "DFII_DIR" )
    GDSIIHierMakeLibrary( LibName65 DFIIDir "tsmc65" )
    if( LibName130=="" then LibName130=strcat( "130" LibName65 ))
    CellView130=nrOpenCellViewReadable( LibName130 CellName ViewName130 )
    if( CellView130 == nil then 
       printf("Error: Unable to open CellView %s %s(%s) for read."  LibName130 CellName ViewName130)
       return() 
    )
    CellView65=betterCopyCellView( CellView130 LibName65 CellName ViewName65 nil nil t)
    if( CellView65 == nil then 
       printf("Error: Unable %s %s(%s) for write"  LibName130 CellName ViewName130)
       return() 
    ) 
    foreach(inst CellView65~>instances  
      if( inst~>libName=="tsmc13lg" then
        if( DeleteWires then dbDeleteObject(inst)
        else dbFlattenInst( inst 32 t t t))
      )
    ) 
    dbSave(CellView65)
    foreach(inst CellView65~>instances
      inst~>xy=fxShrinkPointToGrid(inst~>xy ratio Grid) 
      if( temp=nrOpenCellViewReadable( inst~>libName inst~>cellName "shrink" ) then
        inst~>master=temp
        else printf("Error: %s %s (shrink or layout) view not found" inst~>libName inst~>cellName)
      )     
    )
    if( DeleteWires then
      foreach( shape CellView65~>shapes
;        if(shape~>objType!="label" && shape~>purpose!="pin" then
        if(shape~>objType!="label" && shape~>layerName!="prBoundary" && shape~>pin==nil then
          dbDeleteObject(shape)
        )
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
           printf("Error: unknown objType %s \n" shape~>objType)
        )
      )
    )
    fxFixCellViewViaSize( CellView65 )
    dbSave(CellView65)
    CellView65
  )
)
