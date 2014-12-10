; Copyright 2011 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$


defun( SyncNetlistGetInstanceParametersToSet ( ConnectivityInstanceName InstanceMap InstanceTable )
  let( ( Ret CurrInstances ConnecitivityInstanceParamPairs ParamName ParamValue ParaRet TotalWidth ExistingParam)
      Ret=nil
      CurrInstances=NameGetInstancesForCanonicalName( InstanceMap ConnectivityInstanceName )
      ConnecitivityInstanceParamPairs=InstanceTable["p"]
      foreach( ParamName ConnecitivityInstanceParamPairs
         ParamValue= ConnecitivityInstanceParamPairs[ParamName]
         ParamRet=nil
         ParamValueNum=if( stringp( ParamValue ) then 
                       StringUtilParseLengthString( ParamValue ) else ParamValue )
         cond( 
            ; if chain width parameter, calculate total of all folds
            (  rexMatchp( "^[NP]*[Ww]+[0-9]*$" ParamName )
               ParamRet= if( CurrInstances 
                 then
                 TotalWidth= PDKCombineFolds(
                   mapcar(
                     (lambda ( CurrInstance )
                        (let ( WidthValue )
                           WidthValue=dbSearchPropByName( CurrInstance ParamName ) ~>value
                             if( WidthValue then 
                               if( stringp( WidthValue ) 
                               then StringUtilParseLengthString( WidthValue )
                               else WidthValue )
                             else -1e9 ) 
                        ) 
                     )
                     CurrInstances 
                   ) 
                 )  
                 if( abs( TotalWidth - ParamValueNum )> 1e-10 
                   then list( ConnectivityInstanceName ParamName ParamValueNum/length(CurrInstances) TotalWidth )
                 )
                 else list( ConnectivityInstanceName ParamName ParamValueNum ) 
               )
            )
            ; all other parameter, compare value 
            ( t
               if( CurrInstances then
                 foreach( CurrInstance CurrInstances 
                   ExistingParam=dbSearchPropByName( CurrInstance ParamName )
                   ParamRet= if( ExistingParam then
                     ExistingValue = ExistingParam~>value
                     ExistingValueNum = if( stringp( ExistingValue )
                                        then StringUtilParseLengthString( ExistingValue )
                                        else ExistingValue )
                     if( abs( ExistingValueNum - ParamValueNum ) > 1e-10 
                       then list( ConnectivityInstanceName ParamName ParamValueNum ExistingValueNum )
                       else ParamRet 
                     ) 
                   else list( ConnectivityInstanceName ParamName ParamValueNum ) 
                   ) 
                 )
               else ParamRet = list( ConnectivityInstanceName ParamName ParamValueNum ) ) 
            ) 
         )
         if( ParamRet Ret=cons( ParamRet Ret ) ) 
      )
      Ret 
  ) 
)

defun( SyncNetlistCompareCellInstances ( Instances InstanceMap )
  let( ( inst InstanceTable InstanceName InstanceNameExist InstancesToDelete InstancesToCreate InstancesToFix ParametersToSet InstancesToChange )

    InstanceNameExist=nil
    InstancesTable=NetListTable["i"]
    InstancesToDelete=nil
    InstancesToCreate=nil
    InstancesToFix=nil
    ParametersToSet=nil
    InstancesToChange=nil

    foreach( Instance Instances
      InstanceName=Instance~>name
      CanonicalInstanceName = NameGetCanonicalInstanceNameForInstance( InstanceMap Instance )
      InstanceTable=InstancesTable[CanonicalInstanceName]
      if( tablep( InstanceTable ) then
        InstanceNameExist=cons( InstanceName InstanceNameExist ) 
        printf("Found %s\n" InstanceName)
        if( Instance~>libName != InstanceTable["ln"] || Instance~>cellName != InstanceTable["cn"] then
          InstancesToChange=cons( list( CanonicalInstanceName InstanceTable["ln"] InstanceTable["cn"] Instance Instance~>transform nil ) InstancesToChange )
        )
        ParametersToSet=append( ParametersToSet SyncNetlistGetInstanceParametersToSet( InstanceName InstanceMap InstanceTable ))
      else
        printf("Deleting %s\n" InstanceName)
        InstancesToDelete=cons(Instance InstancesToDelete)
      )
    )
    foreach( InstanceName InstancesTable
      if( !member( InstanceName InstanceNameExist ) then
        InstanceTable=InstancesTable[InstanceName]
        printf("Creating %s\n" InstanceName)
        InstancesToCreate=cons( list(InstanceName InstanceTable["ln"] InstanceTable["cn"] nil) InstancesToCreate)
        ParametersToSet=append( ParametersToSet SyncNetlistGetInstanceParametersToSet( InstanceName InstanceMap InstanceTable ))
      )
    )
    when( or( InstancesToDelete
                 InstancesToChange
                 InstancesToCreate 
                 InstancesToFix
                 ParametersToSet )
      if( InstancesToCreate printf( "%L\n" InstancesToCreate ) )
      list(
          InstancesToDelete
          InstancesToChange
          InstancesToCreate 
          InstancesToFix
          ParametersToSet ) 
    )  
  )
)

defun( SyncNetlistFindNetByInstTerm ( Instance InstTermName )
  let((netName instTerm)
    netName=nil
    foreach( instTerm Instance~>instTerms
      if( instTerm~>name == InstTermName then netName=instTerm~>net~>name )
    )
    netName
  )
)
  
defun( SyncNetlistCompareCellConnectivity ( CellView InstanceMap )
  let( ( ConnectionsToDelete ConnectionsToCreate TerminalsToDelete TerminalsToCreate NetsToDelete NetsToCreate NetsToChange
       net term NetName TermName CurrentConnectionsToCreate ConnectionsToCreate)
    NetsTable=NetListTable["n"]
    InstancesTable=NetListTable["i"]
    TerminalsList = setof( NetName NetsTable car(NetsTable[NetName] ))
    ConnectionsToDelete=nil
    ConnectionsToCreate=nil
    foreach( InstanceName InstancesTable
      CurrConnectivityInstTerms=InstancesTable[InstanceName]["c"]
      CurrInstances = NameGetInstancesForCanonicalName( InstanceMap InstanceName )
      CurrentConnectionsToCreate=nil
      CurrentConnectionsToDelete=nil
      if( CurrInstances then
        foreach( CurrInstance CurrInstances
          ; CurrentConnectionsToCreate
          foreach( TermName InstancesTable[InstanceName]["c"] 
            NetName=SyncNetlistFindNetByInstTerm(CurrInstance TermName) 
            ConnectivityNetName=CurrConnectivityInstTerms[TermName]
            if( !NetName || NetName!=ConnectivityNetName then 
              CurrentConnectionsToCreate=cons( list( CurrInstance~>name TermName ConnectivityNetName ) CurrentConnectionsToCreate )
            )
          )
          ; CurrentConnectionsToDelete
          CurrentConnectionsToDelete=setof( instTerm CurrInstance~>instTerm !CurrConnectivityInstTerms[instTerm~>name])
        )
      else
        foreach( TermName CurrConnectivityInstTerms 
          CurrentConnectionsToCreate=cons( list( InstanceName TermName CurrConnectivityInstTerms[TermName] ) CurrentConnectionsToCreate )
        )
      )  
      ConnectionsToDelete=append( ConnectionsToDelete CurrentConnectionsToDelete )
      ConnectionsToCreate=append( ConnectionsToCreate CurrentConnectionsToCreate )
    )
    TerminalsToDelete=setof( term CellView~>terminals !member( term~>name TerminalsList ))
    TerminalsToCreate=setof( TermName TerminalsList !dbFindTermByName( CellView TermName ))
    NetsToDelete = setof( net CellView~>nets !NetsTable[net~>name] )
    NetsToCreate = setof( NetName NetsTable !dbFindNetByName( CellView NetName ))
    NetsToChange = nil ; wirelength and wirewidth parameteres are outdated
    when( or(
            ConnectionsToDelete
            ConnectionsToCreate
            TerminalsToDelete
            TerminalsToCreate
            NetsToDelete
            NetsToCreate
            NetsToChange
            )
      list(
        list(
          ConnectionsToDelete
          ConnectionsToCreate )
        list(
          TerminalsToDelete
          TerminalsToCreate )
        list(
          NetsToDelete
          NetsToCreate
          NetsToChange )
      ) 
    )
  )
)

; To compare cell height and width in 
defun( SyncNetlistCompareCellBoundary ( CellView LayoutViewName UserUnitsPerMeter LockBound )
  let( ( Shapes ExistingBoundaryBBox LibName CellName ViewName HeightOfExistingBoundary WidthOfExistingBoundary ConnectivityCellHeight ConnectivityCellWidth LayoutCellView LayoutBoundaryBBox )
    Shapes=CellView->shapes
    ExistingBoundaryBBox = CellView~>prBoundary~>bBox 
    LibName=CellView->libName
    CellName=CellView->cellName
    ViewName=CellView->viewName
    unless( LockBound && ExistingBoundaryBBox 
       HeightOfExistingBoundary = when( ExistingBoundaryBBox RectGetHeight( ExistingBoundaryBBox )/float(UserUnitsPerMeter) )
       WidthOfExistingBoundary = when( ExistingBoundaryBBox RectGetWidth( ExistingBoundaryBBox )/float(UserUnitsPerMeter) )
       PositionOfExistingBoundary = when( ExistingBoundaryBBox list( leftEdge( ExistingBoundaryBBox )/float(UserUnitsPerMeter) bottomEdge(ExistingBoundaryBBox)/float(UserUnitsPerMeter)))
       ConnectivityCellHeight = NetlistTableGetCellHeight( NetlistTable )
       ConnectivityCellWidth = NetlistTableGetCellWidth( NetlistTable )
       ConnectivityBoundaryPosition = DirectivesTable["boundary_position"]
       ConnectivityCellHeightWarning = DirectivesTable["boundary_height_warning"]
       ConnectivityCellWidthWarning = DirectivesTable["boundary_width_warning"]
       ConnectivityBoundaryPositionWarning = DirectivesTable["boundary_position_warning"]
       LayoutCellView=nrOpenCellViewReadable( LibName CellName LayoutViewName ) 
       LayoutBoundaryBBox= when( LayoutCellView  LayoutCellView~>prBoundary~>bBox )
       if( LayoutBoundaryBBox then
         NewHeight=RectGetHeight(LayoutBoundaryBBox)/float(UserUnitsPerMeter)
         NewWidth=RectGetWidth(LayoutBoundaryBBox)/float(UserUnitsPerMeter)
         NewBoundaryPosition=list( leftEdge(LayoutBoundaryBBox)/float(UserUnitsPerMeter) bottomEdge(LayoutBoundaryBBox)/float(UserUnitsPerMeter))
       else
         NewHeight=ConnectivityCellHeight
         NewWidth=ConnectivityCellWidth
         NewBoundaryPosition=ConnectivityBoundaryPosition
       )
       HeightCompareResult= 
         if( HeightOfExistingBoundary && NewHeight then
           when( abs( NewHeight - HeightOfExistingBoundary ) > 1e-10                
             list( NewHeight HeightOfExistingBoundary ConnectivityCellHeightWarning )
           )
         else
           when( NewHeight list( NewHeight nil ConnectivityCellHeightWarning ))
         )
       WidthCompareResult= 
         if( WidthOfExistingBoundary && NewWidth then
           when( abs( NewWidth - WidthOfExistingBoundary ) > 1e-10                
             list( NewWidth WidthOfExistingBoundary ConnectivityCellWidthWarning )
           )
         else
           when( NewWidth list( NewWidth nil ConnectivityCellWidthWarning ))
         )
       PositionCompareResult=
         if( PositionOfExistingBoundary then
           when( abs( car(NewBoundaryPosition) - car(PositionOfExistingBoundary) ) > 1e-10 
               || abs( cadr(NewBoundaryPosition) - cadr(PositionOfExistingBoundary) ) > 1e-10            
           list( NewBoundaryPosition PositionOfExistingBoundary ConnectivityBoundaryPositionWarning )
           )
         else
           list( NewBoundaryPosition nil ConnectivityBoundaryPositionWarning )
         )
                                    

        when( HeightCompareResult || WidthCompareResult || PositionCompareResult 
            list(
              if( HeightCompareResult
                  HeightCompareResult
                  list( ConnectivityCellHeight nil ConnectivityCellHeightWarning ) )
              if( WidthCompareResult
                  WidthCompareResult
                  list( ConnectivityCellWidth nil ConnectivityCellWidthWarning ) )
              if( PositionCompareResult
                  PositionCompareResult
                  list( ConnectivityBoundaryPosition nil ConnectivityBoundaryPositionWarning ) )
              CellView~>prBoundary  ) )

    )
  )
)

defun( SyncNetlistComparePins ( CompareResult SuppressPins ForcePins )
 nil
)

(defun SyncNetlistCompareCellView ( CellViewToCheck
                                      LibCellExpressionPairsToIgnore
                                      FoldableLibCellExpressionPairs
                                      InstanceMap
                                      Instances
                                      BoundaryLayerLPP
                                      UserUnitsPerMeter
                                      LockBound
                                      SuppressPins
                                      ForcePins
                                    )
  (let ( isLeaf InstanceName InstanceTable )

     isLeaf=nil
     if( rexMatchp("floorplan" CellViewToCheck~>viewName) then
        foreach( InstanceName NetListTable["i"]
          InstanceTable=NetListTable["i"][InstanceName]
          if( InstanceTable["ln"] == "gate" || InstanceTable["ln"] == "stack" then isLeaf=t )
        )
     )
     if(isLeaf then 
       NetListTable["i"]=makeTable( "tableOfInstancesTables" nil )
     )
     printf("Running SyncNetlistCompareCellInstances... isLeaf=%L\n" isLeaf)
     InstancesCompareResult = SyncNetlistCompareCellInstances( Instances InstanceMap ) 
     println("Done: Running SyncNetlistCompareCellInstances.")
     printf("Running SyncNetlistCompareCellConnectivity... isLeaf=%L\n" isLeaf)
     ConnectivityCompareResult = SyncNetlistCompareCellConnectivity( CellViewToCheck InstanceMap ) 
     println("Done: Running SyncNetlistCompareCellConnectivity.")
     if( CellViewToCheck->cellViewType == "maskLayout" && isLeaf then
       println("Running SyncNetlistCompareCellBoundary...")
       BoundaryCompareResult = SyncNetlistCompareCellBoundary( CellViewToCheck "layout" UserUnitsPerMeter LockBound )
       println("Done: Running SyncNetlistCompareCellBoundary.")
     else BoundaryCompareResult = nil )
     PinCompareResult = nil
      (when ( or
              InstancesCompareResult
              ConnectivityCompareResult 
              BoundaryCompareResult
              PinCompareResult )
        ( list
          InstancesCompareResult
          ConnectivityCompareResult
          BoundaryCompareResult
          PinCompareResult
          CellViewToCheck~>viewName  
          CellViewToCheck~>cellName
          ) ) ) )


(defun SyncNetlistGenFromSource ( TargetCellView
                                    LibCellExpressionPairsToIgnore
                                    FoldableLibCellExpressionPairs
                                    BoundaryLPP
                                    UserUnitsPerMeter
                                    @key
                                    ( DoNotMoveExistingInstances t )
                                    ( Verbose t ) 
                                 )
  
  (let ( Ret InstanceMap Istances )
    Ret= nil 
    InstanceMap= ( NameMakeEmptyInstanceMap ) 
    Instances= (when TargetCellView
                      ( UpdateNetlistFilterInstances
                        TargetCellView
                        LibCellExpressionPairsToIgnore ) )  
    NamePopulateInstanceMapWithInstances(
        InstanceMap
        Instances
        FoldableLibCellExpressionPairs )
    CompareResult= SyncNetlistCompareCellView( TargetCellView 
                                    LibCellExpressionPairsToIgnore
                                    FoldableLibCellExpressionPairs
                                    InstanceMap Instances
                                    BoundaryLPP
                                    UserUnitsPerMeter
                                    nil nil nil
                                    )
    if( CompareResult || geGetSelectedSet() then
       if( Verbose then println("Running UpdateNetlistPrintDifferences...") UpdateNetlistPrintDifferences( CompareResult poport ) )
       println("Running UpdateNetlistUpdateCellViewWithCompareResult...")
       Ret = UpdateNetlistUpdateCellViewWithCompareResult(
              TargetCellView
              CompareResult
              InstanceMap
              BoundaryLPP
              UserUnitsPerMeter 
              ?DoNotMoveExistingInstances DoNotMoveExistingInstances
              )
       println("Done: UpdateNetlistUpdateCellViewWithCompareResult")
       if( Ret then printf( "Error: %L\n" Ret ) ) 
    )
    Ret 
   ) 
) 


(defun SyncNetlistGenFromSkillNetlistUsingPDKInfo ( TargetCellView
                                                      SkillNetlistDir
                                                      SkillDirectivesDir
                                                      @key
                                                      ( LibCellsToIgnore nil )
                                                      ( DoNotMoveExistingInstances t )
                                                      )
  let( ( ) 
    NetlistTable = NetlistTableGetSkillNetlistTableForCellName( TargetCellView~>cellName SkillNetlistDir ) 
    if( tablep( NetlistTable ) then
      DirectivesTable= CellInfoGetTableForCellName( TargetCellView~>cellName SkillDirectivesDir )
      if( tablep( DirectivesTable ) then
         TransformedNetlistTable=InlineConnectionsHierTransformTable(
                        NetlistTable
                        UpdateNetlistGetDefaultShouldInLineConnectionsFunc( )
                        list( DirectivesTable SkillDirectivesDir ) 
                        SkillNetlistDir ) 
         TransformedNetlistsTablesTable=makeTable( "tableOfNetlistTables" nil )
         TransformedNetlistsTablesTable[TargetCellView~>viewName]=TransformedNetlistTable
         InstancesTable=NetListTable["i"] 
         NetsTable=NetListTable["n"] 
         StatisticsTable=NetListTable["s"]
         ; if viewName=="floorplan" && subcell libName == "gate" || "stack", delete all subcells from NetListTable
         LibCellExpressionPairsToIgnore=  
           append( LibCellsToIgnore
             append( WiringCellLibCellPairRegExs 
                   TechLibExceptTransistorsLibCellPairRegExs
             ) )
         FoldableLibCellExpressionPairs=FoldableCellLibCellPairRegExs
         SyncNetlistGenFromSource(
                    TargetCellView
                    LibCellExpressionPairsToIgnore
                    FoldableLibCellExpressionPairs
                    BoundaryLPP
                    UserUnitsPerMeter 
                    ?DoNotMoveExistingInstances DoNotMoveExistingInstances
                    ) 
            
      else DirectivesTable )
    else NetlistTable ) 
  )
)

defun( SyncNetlistInitFloorplan ( CellView InstancesToCreate InstanceMap                                                       
                                  @key
                                  ( DoNotMoveExistingInstances t )
                                )
  let( (InstanceToCreate InstName InstLibName InstCellName InstTransform InstancesTable) 
    InstancesTable=car( InstanceMap )
    GatesToCreate = nil
    PStacksToCreate = nil
    NStacksToCreate = nil
    SubcellsToCreate = nil
    println(InstancesToCreate)
    foreach( InstanceToCreate InstancesToCreate
      InstName = UpdateNetlistGetInstanceNameFromInstanceToCreate( InstanceToCreate )
      InstLibName = UpdateNetlistGetLibNameFromInstanceToCreate( InstanceToCreate )
      InstCellName = UpdateNetlistGetCellNameFromInstanceToCreate( InstanceToCreate )
      InstTransform = UpdateNetlistGetInstanceTransformFromInstanceToCreate( InstanceToCreate )
      cond(
        ( InstLibName == "gate" 
           GatesToCreate=cons( InstancesTable[InstName] GatesToCreate ) 
        )
        ( InstLibName == "stack"
           cond( 
             ( rexMatchp( "NMOS" InstCellName ) NStacksToCreate=cons( InstancesTable[InstName] NStacksToCreate )) 
             ( rexMatchp( "PMOS" InstCellName ) PStacksToCreate=cons( InstancesTable[InstName] PStacksToCreate )) 
             ( t GatesToCreate=cons( InstancesTable[InstName] GatesToCreate )) 
           )
        )
        ( t
           SubcellsToCreate=cons( InstName SubcellsToCreate )  
        )
      )
    )

    if( GatesToCreate || PStacksToCreate || NStacksToCreate then
      CurrX=rightEdge(CellView~>bBox)+6.0
      CurrY=0.0
      foreach( instances PStacksToCreate
        foreach( inst instances
          inst~>xy=list( CurrX CurrY )
          CurrY=topEdge(inst~>bBox)+0.04
        )
      )
      CurrX=rightEdge(CellView~>bBox)+6.0
      CurrY=0.0
      foreach( instances NStacksToCreate
        foreach( inst instances
          inst~>xy=list( CurrX CurrY )
          CurrY=topEdge(inst~>bBox)+0.04
        )
      )
      CurrX=rightEdge(CellView~>bBox)+6.0
      CurrY=0.0
      foreach( instances GatesToCreate
        foreach( inst instances
          inst~>xy=list( CurrX CurrY )
          CurrY=topEdge(inst~>bBox)+0.04
        )
      )
    else
      ; flInitFloorplan
      CurrX=if( DoNotMoveExistingInstances rightEdge(CellView~>bBox) 0.0)
      CurrY=0.0
      flInitFloorplanWithInstOrderList( CellView~>libName CellView~>cellName CellView~>viewName
        ?SubcellsToCreate SubcellsToCreate
        ?DoNotMoveExistingInstances DoNotMoveExistingInstances
        ?x_offset CurrX
        ?y_offset CurrY
        ?RunSyncToNetlist nil
      )
    )
  )
)
