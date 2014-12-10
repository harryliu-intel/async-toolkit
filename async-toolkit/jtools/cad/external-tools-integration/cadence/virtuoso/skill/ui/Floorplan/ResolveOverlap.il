; Copyright 2006 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

( list "1.Resolve Overlap" "UIResolveOverlap" )
( UICreateMenuItemCallBack
  `UIResolveOverlapCB
  (lambda ()
     let(()
       if(UIResolveOverlap_Form->UIResolveOverlap_ProjectName->value== "default" then
         UIResolveOverlap_Form->UIResolveOverlap_ProjectName->value=UIGetCellView()~>cellName)
       UIResolveOverlap_Form->UIResolveOverlap_AutoSaveFileName->value=sprintf( nil "%s.autosave" UIGetCellView()~>cellName)
       hiDisplayForm( UIResolveOverlap_Form )
    ) 
  ) 
)

defun( addInstToList (instance)
  let((vName instcv inst)
    LibCellsToIgnore = append( WiringCellLibCellPairRegExs
              append( TechLibCellPairRegExs
              append( GateLibCellPairRegExs StackLibCellPairRegExs ) ) )
    vName=sprintf( nil "%s|%s" instance~>cellView~>cellName instance~>name )    
    if( !member( vName FreezeCellList ) then
      FreezeCellList=cons( vName FreezeCellList )
      instcv=nrOpenCellViewReadable( instance~>libName instance~>cellName instance~>viewName)
      foreach( inst car( NameFilterInstances( instcv~>instances LibCellsToIgnore ))
        addInstToList( inst )
      )
    )    
  )
)

( UICreateAction
  'UIResolveOverlapFormCB
  (lambda ()
    let( (CellView projectName xtotal)

    TEMP=ConfigFileGetValue( TheCDSConfigTable "TEMP" )
    CellView= UIGetCellView()
    projectName=UIResolveOverlap_Form->UIResolveOverlap_ProjectName->value
    SkipCellList=nil
    if( UIResolveOverlap_Form->UIResolveOverlap_SkipCellFileName->value!="" then
      p=infile(UIResolveOverlap_Form->UIResolveOverlap_SkipCellFileName->value)
      if( p then
        while( fscanf( p "%s" s) SkipCellList=cons( s SkipCellList ))
      else
        printf("Skip Cell File %s does not exist.\n" UIResolveOverlap_Form->UIResolveOverlap_SkipCellFileName->value!="") 
        SkipCellList=nil
      )
    else
      SkipCellList=nil
    ) 
    FreezeCellList=nil
    if( UIResolveOverlap_Form->UIResolveOverlap_Scope->value=="Selected" then
      if( geGetSelectedSet() then
        foreach( inst setof( inst CellView~>instances !member(inst geGetSelectedSet()))
          addInstToList( inst )
        )
      else
        printf("No instance is selected. Run resolve overlap on whole cellview.\n")
      )
    ) 
    createDir( sprintf( nil "%s/glpsol/" TEMP))
    xtotal=flResolveOverlap( CellView 
           ?SkipCellList SkipCellList
           ?LPMacroFileName sprintf( nil "%s/glpsol/%s.lpt" TEMP projectName) 
           ?LPOutputFileName sprintf( nil "%s/glpsol/%s.out" TEMP projectName)
           ?LPLogFileName sprintf( nil "%s/glpsol/%s.log" TEMP projectName)
           ?LPmemoFileName sprintf( nil "%s/glpsol/%s.memo" TEMP projectName)
           ?SaveFileName UIResolveOverlap_Form->UIResolveOverlap_AutoSaveFileName->value
           ?BlockLayer list("LOGO" "drawing")
           ?SaveCellView UIResolveOverlap_Form->UIResolveOverlap_SaveCellView->value
           ?AbutCellKeepOrder UIResolveOverlap_Form->UIResolveOverlap_AbutCellKeepOrder->value
           ?SingleHierarchy UIResolveOverlap_Form->UIResolveOverlap_SingleHierarchy->value
           ?SnapXGrid UIResolveOverlap_Form->UIResolveOverlap_SnapXGrid->value
           ?FreezeCellList FreezeCellList
           )
    printf("Total Width: %f\n" xtotal)
    if( xtotal!=0 && (UIResolveOverlap_Form->UIResolveOverlap_Mode->value!="No" ) then
      if( UIResolveOverlap_Form->UIResolveOverlap_Mode->value=="Least Move" then
        flResolveOverlap( CellView 
           ?SkipCellList SkipCellList
           ?LPMacroFileName sprintf( nil "%s/glpsol/%s.lpt" TEMP projectName) 
           ?LPOutputFileName sprintf( nil "%s/glpsol/%s.out" TEMP projectName)
           ?LPLogFileName sprintf( nil "%s/glpsol/%s.log" TEMP projectName)
           ?LPmemoFileName sprintf( nil "%s/glpsol/%s.memo" TEMP projectName)
           ?SaveFileName UIResolveOverlap_Form->UIResolveOverlap_AutoSaveFileName->value
           ?BlockLayer list("LOGO" "drawing")
           ?SaveCellView UIResolveOverlap_Form->UIResolveOverlap_SaveCellView->value
           ?AbutCellKeepOrder UIResolveOverlap_Form->UIResolveOverlap_AbutCellKeepOrder->value
           ?SingleHierarchy UIResolveOverlap_Form->UIResolveOverlap_SingleHierarchy->value
           ?xtotal xtotal
           ?Mode 1
           ?SnapXGrid UIResolveOverlap_Form->UIResolveOverlap_SnapXGrid->value
           ?FreezeCellList FreezeCellList
           )
       else
        flResolveOverlap( CellView 
           ?SkipCellList SkipCellList
           ?LPMacroFileName sprintf( nil "%s/glpsol/%s.lpt" TEMP projectName) 
           ?LPOutputFileName sprintf( nil "%s/glpsol/%s.out" TEMP projectName)
           ?LPLogFileName sprintf( nil "%s/glpsol/%s.log" TEMP projectName)
           ?LPmemoFileName sprintf( nil "%s/glpsol/%s.memo" TEMP projectName)
           ?SaveFileName UIResolveOverlap_Form->UIResolveOverlap_AutoSaveFileName->value
           ?BlockLayer list("LOGO" "drawing")
           ?SaveCellView UIResolveOverlap_Form->UIResolveOverlap_SaveCellView->value
           ?AbutCellKeepOrder UIResolveOverlap_Form->UIResolveOverlap_AbutCellKeepOrder->value
           ?SingleHierarchy UIResolveOverlap_Form->UIResolveOverlap_SingleHierarchy->value
           ?xtotal xtotal
           ?Mode 0
           ?SnapXGrid UIResolveOverlap_Form->UIResolveOverlap_SnapXGrid->value
           ?FreezeCellList FreezeCellList
           )
       )
    
    )
    )
  )
)

( UICreateComponent
  "UIResolveOverlap_Form"
  ( UICreateConstructor 
    (lambda ( Children )
      (let ( Fields )
         Fields = list(
                ( hiCreateRadioField
                  ?name `UIResolveOverlap_Scope
                  ?prompt "Scope: "
                  ?choices list( "CellView" "Selected")
                  ?defValue "CellView" )
                ( hiCreateStringField
                  ?name `UIResolveOverlap_ProjectName
                  ?prompt "glpsol Project Name" 
                  ?defValue "default" )
                ( hiCreateStringField
                  ?name `UIResolveOverlap_SkipCellFileName
                  ?prompt "SkipCellFileName" 
                  ?defValue "skipCell.list" )
                ( hiCreateStringField
                  ?name `UIResolveOverlap_AutoSaveFileName
                  ?prompt "AutoSaveFileName" 
                  ?defValue "autosave.sav" )
                ( hiCreateBooleanButton
                  ?name `UIResolveOverlap_AbutCellKeepOrder
                  ?buttonText "Y-Abutting Cells Keep Their Relative X Order" 
                  ?defValue t )
                ( hiCreateBooleanButton
                  ?name `UIResolveOverlap_SingleHierarchy
                  ?buttonText "Run on Just Top One Hierarchy" 
                  ?defValue t )
                ( hiCreateRadioField
                  ?name `UIResolveOverlap_Mode
                  ?prompt "Run Second Pass: "
                  ?choices list( "No" "Compact Left" "Least Move")
                  ?defValue "No" )
                ( hiCreateBooleanButton
                  ?name `UIResolveOverlap_SaveCellView
                  ?buttonText "Save All CellViews" 
                  ?defValue t )
                ( hiCreateFloatField
                  ?name `UIResolveOverlap_SnapXGrid
                  ?prompt "Snap X to Grid"
                  ?defValue 0.0 )
                ( hiCreateFormButton
                  ?name `UIResolveOverlap_Help
                  ?buttonText "Tool Help"
                  ?callback "nrOpenHtml( \"Resolve_Overlaps_help.html\" )" )
                 ) 
        ( hiCreateAppForm
          ?name `UIResolveOverlap_Form
          ?formTitle "Resolve Overlaps"  
          ?fields Fields
          ?callback "( UIResolveOverlapFormCB )"
          ) 
      ) 
    ) 
  ) 
)
