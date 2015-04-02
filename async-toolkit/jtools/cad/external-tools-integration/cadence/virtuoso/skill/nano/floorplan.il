
defun( nrCreateFloorplan (libName cellName viewName floorplanViewName
                         @key
                         ( Verbose nil )
                         )
  prog( (CellView FloorplanView LayoutView LibCellsToIgnore routedStatus 
        transform InstName NewMasterCellView)
    LibCellsToIgnore = append( WiringCellLibCellPairRegExs
              append( TechLibCellPairRegExs
              append( GateLibCellPairRegExs StackLibCellPairRegExs ) ) )
    FloorplanView=nrOpenCellViewWritable(libName cellName floorplanViewName)
    if( FloorplanView==nil then 
      printf("Error: CellView %s(%s) is not writable.\n" cellName floorplanViewName) 
      FloorplanView=nrOpenCellViewReadable(libName cellName floorplanViewName)
      return(FloorplanView) 
    )
    if( viewName==floorplanViewName then 
      return(FloorplanView) 
    )
    if( member( cellName doneCellList ) then
      return(FloorplanView)
    )
    doneCellList=cons( cellName doneCellList )
    if( nrIsLeafCell( libName cellName viewName) then
      CellView=nrOpenCellViewReadable(libName cellName viewName)
      FloorplanView=betterCopyCellView( CellView libName cellName floorplanViewName nil nil t) 
      foreach( inst FloorplanView~>instances 
          if( inst~>objType=="mosaicInst" dbDeleteObject(inst~>mosaic) dbDeleteObject(inst) )
      )
      foreach( lpp FloorplanView~>lpps
        if( lpp~>layerName!="prBoundary" then
          foreach( shape lpp~>shapes if( dbIsId(shape) then dbDeleteObject(shape))) 
        )
      )
      dbSave(FloorplanView)
      if( Verbose then printf("Done Creating leaf cell: %s(%s)...\n" FloorplanView~>cellName FloorplanView~>viewName))
    else
      if( nrIsRoutedCell(libName cellName viewName) then
        CellView=nrOpenCellViewReadable(libName cellName "prelayout")
        when( !CellView 
          printf("Can't find view: %s(%s)...\n" cellName "prelayout")
          CellView=nrOpenCellViewReadable(libName cellName viewName)
        )
        if( Verbose then printf("Found routed mid cell: %s(%s)...\n" FloorplanView~>cellName FloorplanView~>viewName))
      else
        CellView=nrOpenCellViewReadable(libName cellName viewName)
      )
      FloorplanView=betterCopyCellView(CellView libName cellName floorplanViewName nil nil t)
      if( Verbose then printf("Creating mid cell: %s(%s)...\n" FloorplanView~>cellName FloorplanView~>viewName))
      instances= car( NameFilterInstances( FloorplanView~>instances LibCellsToIgnore ))
      if( instances then nrDeleteLayers( FloorplanView BoundaryLPP ))
      foreach( inst instances
        InstName=inst~>name
        transform=inst~>transform
        NewMasterCellView=nrCreateFloorplan(inst~>libName inst~>cellName inst~>viewName floorplanViewName
                         ?Verbose Verbose
                         )
        dbDeleteObject(inst)
        dbCreateInst(
              FloorplanView
              NewMasterCellView
              InstName
              car( transform )
              cadr( transform ) )
      )
      dbSave(FloorplanView)
      foreach( inst cadr( NameFilterInstances( FloorplanView~>instances LibCellsToIgnore ))
        if( inst~>objType=="mosaicInst" dbDeleteObject(inst~>mosaic) dbDeleteObject(inst) )
      )
      foreach( lpp FloorplanView~>lpps
        if( lpp~>layerName!="prBoundary" then
          foreach( shape lpp~>shapes if( dbIsId(shape) then dbDeleteObject( shape ))) 
        )
      )
      dbSave(FloorplanView)
      if( Verbose then printf("Done Creating mid cell: %s(%s)...\n" FloorplanView~>cellName FloorplanView~>viewName))
    )
    return(FloorplanView) 
  )
)
