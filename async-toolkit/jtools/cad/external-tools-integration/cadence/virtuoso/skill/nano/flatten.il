; Copyright 2004 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$


defun( nrFlattenView ( CellView flattenViewName 
                        powerCellName
                        powerViewName
                        celllist_filename 
                        @key
                        ( OBSCellName "" )
                        ( OBSViewName "layout" )
                        ( BusWireCellName "" )
                        ( BusWireViewName "layout" )
                        ( Verbose nil )
                        ( DeleteWires t )
                        ( DeleteObs t )
                        ( RegenerateFromCast nil )
                        ( CheckAlignment t )
                        ( MaxHeapSize (nrGetMaxHeapSize) )
                      )
  let((FlattenView FlattenViewTemp temp instances uniqueInstances routedDirectiveList 
         prbounds ignore_align ignore_prbound ignore_routed floorplans prelayouts pout)

  LibCellsToIgnore = append( WiringCellLibCellPairRegExs
              append( TechLibCellPairRegExs
              append( GateLibCellPairRegExs StackLibCellPairRegExs ) ) )

  ;check if this cell is actually supposed to be routed
  ignore_routed=t
  (when !(nrIsRoutedCell CellView->libName CellView->cellName CellView->viewName)
     ignore_routed = hiDisplayAppDBox(
        ?name 'RoutedErrorDialogBox
        ?dboxText "The current cell is not set to be routed.\nDo you want to proceed anyway?"
        ?dboxBanner "Routed Directive Error"
        ?dialogType     hicQuestionDialog
        ?dialogStyle    'modal
        ?buttonLayout   'YesNo
        ?defaultButton 2
    )
 )
 (when ignore_routed
  ;continue

  ;check if subcells are aligned
  ignore_align=t
  when( CheckAlignment
  when( CheckRoutedAlignment(CellView) 
    ignore_align = hiDisplayAppDBox(
        ?name 'FlattenErrorDialogBox
        ?dboxText "One or more cells are misaligned.\nDo you want to proceed anyway?"
        ?dboxBanner "Alignment Errors"
        ?dialogType     hicQuestionDialog
        ?dialogStyle    'modal
        ?buttonLayout   'YesNo
        ?defaultButton 2
    )
  ))

  when( ignore_align 
   ; do the rest of the steps

    ;look for prbound
    ignore_prbound=t
    when( !GetPrbound(CellView)
      ignore_prbound = hiDisplayAppDBox(
        ?name 'PrboundErrorDialogBox
        ?dboxText "No top-level prbound present.\nThis is likely to make the power grid abstract incorrect.\nDo you want to proceed anyway?"
        ?dboxBanner "No Prbound"
        ?dialogType     hicQuestionDialog
        ?dialogStyle    'modal
        ?buttonLayout   'YesNo
        ?defaultButton 2
      )
    ) 
    
    when( ignore_prbound
    ; do the rest of the steps

  
  FlattenView= betterCopyCellView( CellView CellView~>libName CellView~>cellName flattenViewName nil nil t) 
  dbSave( FlattenView )
  instances = t
  routedDirectiveList=nrCastQuery( FlattenView ?RegenerateFromCast RegenerateFromCast ?MaxHeapSize MaxHeapSize)

  while( instances != nil
    instances= setof( inst FlattenView~>instances inst->libName!=TechLibName)
    instances= car( NameFilterInstances( instances LibCellsToIgnore ))
    instances= setof( inst instances    
      !nrIsRoutedCell( inst~>libName 
                       inst~>cellName 
                       inst~>viewName 
                       ?routedDirectiveList routedDirectiveList
                       ?RegenerateFromCast RegenerateFromCast
                       ?Verbose Verbose
                       ?MaxHeapSize MaxHeapSize
                     )
    )
    if( instances then
    println( foreach( mapcar inst instances inst~>cellName ))
    InlineInstances( FlattenView
                  FlattenView
                  instances
                  CellView~>viewName
                  nil
                  nil
                  nil
                  ?CopyPins nil
                  ?Verbose nil
                  ?CopyFigs t
                )
    )

    ; merge any flattened prbounds
    prbounds = nil
    foreach( shape FlattenView~>shapes
      when( car(shape->lpp)=="prBoundary"
        prbounds = cons( shape prbounds )
      )
    )
    leMergeShapes( prbounds )

    dbSave( FlattenView )
  ) 
  if(DeleteWires then
    if( Verbose printf("Deleting existing wirings...\n"))
    instances= setof( inst FlattenView~>instances inst->libName!=TechLibName)
    instances= setof( inst cadr( NameFilterInstances( instances WiringCellLibCellPairRegExs )) inst~>cellName!=powerCellName)
    if( instances then
      if( Verbose then printf("Deleting wiring instances:") println( instances~>cellName ))
      foreach( inst instances dbDeleteObject( inst ) )    
    )
    nrDeleteLayers( FlattenView Metal1LPP )
    nrDeleteLayers( FlattenView Metal2LPP )
    nrDeleteLayers( FlattenView Metal3LPP )
    nrDeleteLayers( FlattenView Metal4LPP )
    nrDeleteLayers( FlattenView Metal5LPP )
    nrDeleteLayers( FlattenView Metal6LPP )
    nrDeleteLayers( FlattenView Metal7LPP )
    if( Verbose printf("Deleting existing vias...\n"))
    instances = setof( inst FlattenView~>instances inst->libName==TechLibName)
    foreach( inst instances dbDeleteObject(inst))
    dbSave( FlattenView )
  )
  if( Verbose printf("%s flatten done.\n" CellView~>cellName))
 )

  pout=outfile(celllist_filename)
  if( pout then
    foreach( inst uniqueInstances
      fprintf( pout "%s %s %s\n" inst~>libName inst~>cellName "abstract")
    )
    close(pout)
  )

  ; post-flatten check for missing layout views
  prelayouts=nil
  floorplans=nil
  foreach( inst FlattenView->instances
    when( inst->viewName=="prelayout"
      prelayouts=t
    )
    when( inst->viewName=="floorplan"
      floorplans=t
    )
  )
  (unless ExpertMode
    (when prelayouts
      UIPopUpDialog( "Instances found with prelayout view.\n" )
      )
    (when floorplans
      UIPopUpDialog( "Instances found with floorplan view.\n" )
      )
    )

    ; switch current window
    win = (hiGetCurrentWindow)
    (when win (geOpen ?window win
                      ?lib  FlattenView->libName
                      ?cell FlattenView->cellName
                      ?view FlattenView->viewName
                      ?viewType "maskLayout"
                      ?mode "a")
          )

 FlattenView
))))


defun( nrFlattenViewPG ( CellView flattenViewName 
                        powerCellName
                        powerViewName
                        celllist_filename 
                        @key
                        ( OBSCellName "" )
                        ( OBSViewName "layout" )
                        ( BusWireCellName "" )
                        ( BusWireViewName "layout" )
                        ( Verbose nil )
                        ( DeleteWires t )
                        ( DeleteObs t )
                        ( RegenerateFromCast nil )
                        ( MaxHeapSize (nrGetMaxHeapSize) )
                        ( overhang t )
                      )
  let((FlattenView FlattenViewTemp temp instances uniqueInstances routedDirectiveList 
        prbounds xalign tiehilo GND Vdd instGND instVdd x0 y0 pout )

  FlattenView=dbOpenCellViewByType( CellView~>libName CellView~>cellName flattenViewName "maskLayout" "a" )
  unless( FlattenView error("Flatten View (%s) does not exist!\n" flattenViewName ) )

  ; regenerate power grid, or open existing one read only
  powerGridView=AutoGeneratePowerGrid( FlattenView powerCellName 
         ?OBSCellName OBSCellName 
         ?OBSViewName OBSViewName 
         ?powerViewName powerViewName 
         ?overhang overhang
         )
  unless( powerGridView
          printf( "WARNING: using read-only power grid %s %s\n" powerCellName powerViewName)
          powerGridView=dbOpenCellViewByType( CellView~>libName powerCellName powerViewName
                                                                "maskLayout" "r" )
          )
  unless( powerGridView error("Error: AutoGeneratePowerGrid failed.\n") )

  instances= setof( inst FlattenView~>instances inst->libName!=TechLibName)
  instances= cadr( NameFilterInstances( instances PowerGridCellPairRegExs ))
  foreach( inst instances if( inst~>objType=="mosaicInst" dbDeleteObject(inst~>mosaic) dbDeleteObject(inst) ) )

  ; draw power pins
  x0 = (car  (car FlattenView->bBox))
  y0 = (cadr (car FlattenView->bBox))
  x0 = (ceiling (x0/PowerGridPitch))*PowerGridPitch
  y0 = (ceiling (y0/(2*PowerGridPitch)))*(2*PowerGridPitch)
  CreatePowerPins( FlattenView "M7" x0:y0 t )

  ; instantiate tiehilo
  tiehilo = dbCreateInst( FlattenView powerGridView "tiehilo" list(0 0) "R0" )
  AnchorInstance( tiehilo )
  GND = dbFindNetByName( FlattenView "GND")
  Vdd = dbFindNetByName( FlattenView "Vdd")
  instGND = dbFindTermByName( powerGridView "GND")
  instVdd = dbFindTermByName( powerGridView "Vdd")
  dbCreateInstTerm( GND tiehilo instGND)
  dbCreateInstTerm( Vdd tiehilo instVdd)

  xalign=GetXAlignmentProp(CellView)
  if( xalign then
    when( xalign<TrackPitch
      SetXAlignmentProp( CellView TrackPitch )
      SetXAlignmentProp( FlattenView TrackPitch )
    )
  else
      SetXAlignmentProp( CellView TrackPitch )
      SetYAlignmentProp( CellView GridPitch )
      SetXAlignmentProp( FlattenView TrackPitch )
      SetYAlignmentProp( FlattenView GridPitch )
  )
  dbSave( CellView )
  dbSave( FlattenView )

  instances= setof( inst FlattenView~>instances inst->libName!=TechLibName)
  instances= append( car( NameFilterInstances( instances LibCellsToIgnore ))
                     cadr( NameFilterInstances( instances PowerGridCellPairRegExs )))
  uniqueInstances=nil
  foreach( inst instances
    if( !member( inst~>cellName uniqueInstances~>cellName ) then 
      uniqueInstances=cons( inst uniqueInstances )
    )
  )
  pout=outfile(celllist_filename)
  if( pout then
    foreach( inst uniqueInstances
      fprintf( pout "%s %s %s\n" inst~>libName inst~>cellName "abstract")
    )
    close(pout)
  )

  dbSave( FlattenView )

  ; switch current window
  win = (hiGetCurrentWindow)
  (when win (geOpen ?window win
                    ?lib  FlattenView->libName
                    ?cell FlattenView->cellName
                    ?view FlattenView->viewName
                    ?viewType "maskLayout"
                    ?mode "a")
        )

  FlattenView
))


;attempt to compare floorplan to prelayout placements
(defun ComparePlacement (cv)
 (let (floorplan prelayout flattenlist flat1 flat2 cells1 cells2
       x y oldy1 oldy2
      )
  floorplan=(nrOpenCellViewReadable cv->libName cv->cellName "floorplan")
  prelayout=(nrOpenCellViewReadable cv->libName cv->cellName "prelayout")
  flattenlist = (strcat cv->cellName ".flatten_cells_list")
  flat1=(nrFlattenView floorplan nil nil flattenlist)
  flat2=(nrFlattenView prelayout nil nil flattenlist)

  cells1=flat1->instances
  cells2=flat2->instances
  (foreach inst cells1
    x=(car inst->xy)
    y=(cadr inst->xy)
    (when !(IsInList y oldy1)
      oldy1=(cons oldy1 y)
    )
  )

 )
)
