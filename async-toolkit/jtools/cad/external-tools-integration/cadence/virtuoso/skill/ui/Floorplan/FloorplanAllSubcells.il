; Copyright 2006 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

( list "6.Init Floorplan All Subcells" "UIFloorplanAllSubcells" )


defun( flCompareCellNameSubType ( cellName1 cellName2 )
  prog((cellRootName1 cellRootName2 subType1 subType2)
    rexCompile(".[0-9]*$")
    cellRootName1=rexReplace( cellName1 "" 0)
    cellRootName2=rexReplace( cellName2 "" 0)
    if( strcmp( cellRootName1 cellRootName2 )!=0 then
      return( strcmp( cellRootName1 cellRootName2 ))
    )
    rexCompile(sprintf(nil "^%s." cellRootName1))
    subType1=rexReplace( cellName1 "" 0 )
    subType2=rexReplace( cellName2 "" 0 )
    return( cond( 
              (evalstring(subType1)>evalstring(subType2) 1) 
              (evalstring(subType1)<evalstring(subType2) -1) 
              (t 0)
            )
    )
  )
)

defun( flFloorplanSubcells ( CellView
                             @key
                             (SubType "500")
                             (SubTypeEnd "9999")
                             (viewName "floorplan")
                             (SnapXGrid 0.001)
                             (ResolveOverlap t)
                             (SkipCellList nil)
                           )
 ;  processCellList is used as a global variable, and will need to be initialized to nil
  prog((cellName1 cellName2 cellView2 subcellView cellRootName libName name cellNameList uniqueInstList inst)
    cellName1=CellView~>cellName
    if( member( cellName1 processedCellList ) || CellView~>instances==nil then 
      return(t)
    else 
      processedCellList=cons( cellName1 processedCellList )
    )

    ; find the lowest sybtype number.
    libName=car( NameParseCellName( cellName1 ) ) 
    cellNameList=ddGetObj(libName)~>cells~>name
    rexCompile(".[0-9]*$")
    cellRootName=rexReplace( cellName1 "" 0)
    cellNameList=rexMatchList( sprintf(nil "^%s.[0-9]*$" cellRootName ) cellNameList )
    cellNameList=setof( cellName2 cellNameList flCompareCellNameSubType( cellName2 sprintf( nil "%s.%s" cellRootName SubType ))>=0 && flCompareCellNameSubType( cellName2 sprintf( nil "%s.%s" cellRootName SubTypeEnd ))<0 && strcmp(cellName1 cellName2)!=0)
    cellNameList=sort( cellNameList nil)

    cellView2=nil
    while( cellNameList && cellView2==nil
      cellName2=car(cellNameList) 
      cellView2=nrOpenCellViewReadable( libName cellName2 viewName )
      cellNameList=cdr(cellNameList)
    )

    if( cellView2 then
      printf("Running FloorplanTemplating: %s(%s) %s \n" CellView~>libName CellView~>cellName CellView~>viewName )
      flFloorplanTemplating( CellView 
                        cellName2
                        ?srcLibName libName
                        ?srcViewName viewName
                        ?SnapXGrid SnapXGrid
                        ?Recursive t
                        ?DeletePins t
                        ?SkipCellList SkipCellList
                        ?ResolveOverlap ResolveOverlap
                        )
    else    
      uniqueInstList=nil
      foreach( inst CellView~>instances
        if( !member( inst~>cellName uniqueInstList~>cellName ) && inst~>libName !="gate" && inst~>libName !="stack" && inst~>libName != TechLibName then
          uniqueInstList=cons( inst uniqueInstList)
        )
      )
      foreach( inst uniqueInstList
        subcellView=nrOpenCellViewWritable( inst~>libName inst~>cellName inst~>viewName )
        if(subcellView then 
          flFloorplanSubcells( subcellView 
                               ?SubType SubType
                               ?SnapXGrid SnapXGrid
                               ?viewName viewName
                               ?ResolveOverlap ResolveOverlap
                               ?SkipCellList SkipCellList
                             )
        else
          printf("Error: cell view %s(%s) %s not writable.\n" inst~>cellName inst~>libName inst~>viewName )
        )
      )
      printf("Running InitFloorplan: %s(%s) %s \n" CellView~>libName CellView~>cellName CellView~>viewName )
      flInitFloorplan( CellView~>libName CellView~>cellName CellView~>viewName 
                      ?Verbose UIFloorplanAllSubcells_Form->UIFloorplanAllSubcells_Verbose->value 
                      ?RegenerateFromCast UIFloorplanAllSubcells_Form->UIFloorplanAllSubcells_RegenerateFromCast->value
                      ?SnapXGrid  SnapXGrid
                      ?ScriptFileName UIFloorplanAllSubcells_Form->UIFloorplanAllSubcells_ScriptFileName->value
                      ?MaxHeapSize nrGetMaxHeapSize( )
           )
      return(t)
    )
  )
)


( UICreateMenuItemCallBack
  `UIFloorplanAllSubcellsCB
  (lambda ()
     let(()
       UIFloorplanAllSubcells_Form->UIFloorplanAllSubcells_AutoSaveFileName->value=sprintf( nil "%s.autosave" UIGetCellView()~>cellName)
       hiDisplayForm( UIFloorplanAllSubcells_Form )
    ) 
  ) 
)

( UICreateAction
  'UIFloorplanAllSubcellsFormCB
  (lambda ()
    let( (CellView processedCellList)
     
      CellView= UIGetCellView()
      processedCellList=nil
      SkipCellList=nil
      if( UIFloorplanAllSubcells_Form->UIFloorplanAllSubcells_SkipCellFileName->value!="" then
        p=infile(UIFloorplanAllSubcells_Form->UIFloorplanAllSubcells_SkipCellFileName->value)
        if( p then
          while( fscanf( p "%s" s) SkipCellList=cons( s SkipCellList ))
        else
          SkipCellList=nil
        )
        SkipCellList=nil
      )

      if( UIFloorplanAllSubcells_Form->UIFloorplanAllSubcells_AutoSave->value then
         flSaveCoordinates( CellView 
                         UIFloorplanAllSubcells_Form->UIFloorplanAllSubcells_AutoSaveFileName->value 
                         ?SkipCellList SkipCellList
                         ?BlockLayer list("LOGO" "drawing")
                       )
      )
      flFloorplanSubcells( CellView 
                         ?SubType UIFloorplanAllSubcells_Form->UIFloorplanAllSubcells_SubTypeStart->value
                         ?SubTypeEnd UIFloorplanAllSubcells_Form->UIFloorplanAllSubcells_SubTypeEnd->value
                         ?viewName UIFloorplanAllSubcells_Form->UIFloorplanAllSubcells_ViewName->value
                         ?SnapXGrid  UIFloorplanAllSubcells_Form->UIFloorplanAllSubcells_SnapXGrid->value
                         ?ResolveOverlap  UIFloorplanAllSubcells_Form->UIFloorplanAllSubcells_ResolveOverlap->value
                         ?SkipCellList SkipCellList 
      )
    )
  )
)

( UICreateComponent
  "UIFloorplanAllSubcells_Form"
  ( UICreateConstructor 
    (lambda ( Children )
      (let ( Fields )
         Fields = list(
                ( hiCreateBooleanButton
                  ?name `UIFloorplanAllSubcells_RegenerateFromCast
                  ?buttonText "Regenerate From Cast" 
                  ?defValue nil )
                ( hiCreateBooleanButton
                  ?name `UIFloorplanAllSubcells_Verbose
                  ?buttonText "Verbose" 
                  ?defValue nil )
                ( hiCreateStringField
                  ?name `UIFloorplanAllSubcells_SubTypeStart
                  ?prompt "Template From Subtype" 
                  ?defValue "1000" )
                ( hiCreateStringField
                  ?name `UIFloorplanAllSubcells_SubTypeEnd
                  ?prompt "Template To Subtype" 
                  ?defValue "9999" )
                ( hiCreateStringField
                  ?name `UIFloorplanAllSubcells_ViewName
                  ?prompt "Template From ViewName" 
                  ?defValue "floorplan" )
                ( hiCreateBooleanButton
                  ?name `UIFloorplanAllSubcells_ResolveOverlap
                  ?buttonText "Resolve Overlap" 
                  ?defValue t )
                ( hiCreateStringField
                  ?name `UIFloorplanAllSubcells_ScriptFileName
                  ?prompt "Filter Script" 
                  ?defValue "" )
                ( hiCreateStringField
                  ?name `UIFloorplanAllSubcells_SkipCellFileName
                  ?prompt "Skip Cell File Name" 
                  ?defValue "" )
                ( hiCreateBooleanButton
                  ?name `UIFloorplanAllSubcells_AutoSave
                  ?buttonText "AutoSave" 
                  ?defValue nil )
                ( hiCreateStringField
                  ?name `UIFloorplanAllSubcells_AutoSaveFileName
                  ?prompt "AutoSaveFileName" 
                  ?defValue "autosave.sav" )
                ( hiCreateFloatField
                  ?name `UIFloorplanAllSubcells_SnapXGrid
                  ?prompt "Snap X to Grid"
                  ?defValue 0.01 ))
        ( hiCreateAppForm
          ?name `UIFloorplanAllSubcells_Form
          ?formTitle "Init Floorplan All Subcells"  
          ?fields Fields
          ?callback "( UIFloorplanAllSubcellsFormCB )"
          ) 
      ) 
    ) 
  ) 
)
