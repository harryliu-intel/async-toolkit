; Copyright 2006 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

defun( AutoSpareCell ( CellView fillCellName 
                               @key
                               (fillViewName "layout") 
                               (PowerGridSize 5.76)
                               (PowerGridOffset 0.24) 
                               (FillWithSpareCell t) 
                               (FillWithDcapCell nil) 
                               (FillWithM2M7 t) 
                               (SpareCellLibName "lib.util.spare") 
                               (LargeSpareCellName "lib.util.spare.LARGE_SPARE_GATES.500")
                               (SmallSpareCellName "lib.util.spare.SMALL_SPARE_GATES.500") 
                               (LargeSpaceCellGridX 3) 
                               (LargeSpaceCellGridY 4) 
                               (FillCellLibName "globals") 
                               (DcapFillCellName "globals.fill.DCAP")
                               (ODPOM1FillCellName "globals.fill.ODPOM1")
                               (ODM1FillCellName "globals.fill.ODM1")
                               (ODPOFillCellName "globals.fill.ODPO")
                               (ODSMALLFillCellName "globals.fill.OD_SMALL")
                               (M2FillCellName "globals.fill.M2")
                               (M3FillCellName "globals.fill.M3")
                               (M4FillCellName "globals.fill.M4")
                               (M5FillCellName "globals.fill.M5")
                               (M6FillCellName "globals.fill.M6")
                               (M7FillCellName "globals.fill.M7")
                               (M23FillCellName "globals.fill.M23")
                               (M34FillCellName "globals.fill.M34")
                               (M45FillCellName "globals.fill.M45")
                               (M56FillCellName "globals.fill.M56")
                               (M67FillCellName "globals.fill.M67")
                               (LeaveMess nil)
                               )
  prog(( fillTempView fillGridView x1 x2 x1s x2s y1 y2 y1s y2s x y i j ci 
        AssuraLayerMappings AssuraRuleFile AssuraSets libName)
    ; create table of fill cell mappings to generated layers
    ReplaceCellNameTable=makeTable("a" "")
    ReplaceCellNameTable[car(NImplantLPP)]=SmallSpareCellName
    ReplaceCellNameTable[car(PImplantLPP)]=LargeSpareCellName
    ReplaceCellNameTable[car(PolyLPP)]=DcapFillCellName
    ReplaceCellNameTable[car(NWellLPP)]=ODSMALLFillCellName
    ReplaceCellNameTable[car(DiffLPP)]=ODPOFillCellName
    ReplaceCellNameTable[car(Metal1LPP)]=ODM1FillCellName
    ReplaceCellNameTable[car(Metal2LPP)]=M2FillCellName
    ReplaceCellNameTable[car(Metal3LPP)]=M3FillCellName
    ReplaceCellNameTable[car(Metal4LPP)]=M4FillCellName
    ReplaceCellNameTable[car(Metal5LPP)]=M5FillCellName
    ReplaceCellNameTable[car(Metal6LPP)]=M6FillCellName
    ReplaceCellNameTable[car(Metal7LPP)]=M7FillCellName
    ReplaceCellNameTable[car(Via23LPP)]=M23FillCellName
    ReplaceCellNameTable[car(Via34LPP)]=M34FillCellName
    ReplaceCellNameTable[car(Via45LPP)]=M45FillCellName
    ReplaceCellNameTable[car(Via56LPP)]=M56FillCellName
    ReplaceCellNameTable[car(Via67LPP)]=M67FillCellName
    ci=setof( in CellView~>instances in~>cellName == fillCellName)
    if(ci
      foreach( i ci
        dbDeleteObject( i )
        printf("Warning: %s exists in %s, removing\n" fillCellName CellView~>cellName)
      )
    )
;    fillGridView=ddGetObj( CellView~>libName fillCellName "layout" )
;    if( fillGridView != nil then
;      ddDeleteObj(fillGridView)
;      printf("Warning: Deleted cell %s\n" fillCellName)
;    )
    ; check for p4Bound
    if( setof( lpp CellView~>lpps lpp~>layerName=="prBoundary" ) == nil then
      printf("Error: There is no prBoundary layer defined in cellview %s. \n" CellView~>cellName)
      return(nil)
    )
    ; delete existing temp cell if it exists
    fillTempView=ddGetObj( CellView~>libName CellView~>cellName "autoFillGrid_temp")
    if( fillTempView != nil ddDeleteObj(fillTempView) )
    ; create new temp cell
    fillTempView=dbOpenCellViewByType( CellView~>libName CellView~>cellName "autoFillGrid_temp" "maskLayout" "w" )
    if( fillTempView == nil then
      printf("Error: Can not open cellview %s (autoFillGrid_temp) for write. \n" CellView~>cellName)
      return(nil)
    )
    ; create new the fill cell
    fillGridView=nrOpenCellViewWritable(CellView~>libName fillCellName fillViewName)
    if( fillGridView == nil then
      printf("Error: Can not open cellview %s (%s) for write. \n" fillCellName fillViewName)
      return(nil)
    )
    ; get the small fill cell
    smallGridCellView=nrOpenCellViewReadable(SpareCellLibName SmallSpareCellName "layout")
    if( smallGridCellView == nil then
      printf("Error: Can not open cellview %s for read. \n" SmallSpaceCellName)
      return(nil)
    )
    ; get the large fill cell
    largeGridCellView=nrOpenCellViewReadable(SpareCellLibName LargeSpareCellName "layout")
    if( largeGridCellView == nil then
      printf("Error: Can not open cellview %s for read. \n" LargeSpaceCellName)
      return(nil)
    )

    ; instantiate the cell to be filled in the temp view
    dbCreateInst( fillTempView CellView "I0" list(0 0) "R0" ) 
    ; ??? shouldn' this be dbSave( fillTempView ) ???
    dbSave( CellView )
    ; Draw 5.76 x 5.76 rectangle grid
    x1=leftEdge( fillTempView~>bBox )
    x2=rightEdge( fillTempView~>bBox )
    y1=bottomEdge( fillTempView~>bBox )
    y2=topEdge( fillTempView~>bBox )
    x1s=floor(x1/(PowerGridSize/2))
    x1=floor(x1/PowerGridSize)
    y1s=floor((y1+PowerGridOffset)/(PowerGridSize/2))
    y1=floor((y1+PowerGridOffset)/PowerGridSize)
    x2s=floor(x2/(PowerGridSize/2))
    x2=floor(x2/PowerGridSize)
    y2s=floor((y2+PowerGridOffset-0.005)/(PowerGridSize/2))
    y2=floor((y2+PowerGridOffset-0.005)/PowerGridSize)
    ; create a pg size grid
    for( i x1 x2
      x=i*PowerGridSize
      for( j y1 y2
        y=j*PowerGridSize-PowerGridOffset
        ; rectangles are slightly small so they are maintained as rectangles thru the Assura processing!
        ; the only thing important is the lower left corner
        dbCreateRect(fillTempView list("y0" "drawing") list(x:y x+PowerGridSize-0.005:y+PowerGridSize-0.005))
      )
    )
    ; create a large cell size grid
    for( i floor(x1/LargeSpaceCellGridX) floor(x2/LargeSpaceCellGridX)
      x=i*PowerGridSize*LargeSpaceCellGridX
      for( j floor(y1/LargeSpaceCellGridY) floor(y2/LargeSpaceCellGridY)
        y=j*PowerGridSize*LargeSpaceCellGridY-PowerGridOffset
        ; rectangles are slightly small so they are maintained as rectangles thru the Assura processing!
        dbCreateRect(fillTempView list("y1" "drawing") list(x:y x+PowerGridSize*LargeSpaceCellGridX-0.005:y+PowerGridSize*LargeSpaceCellGridY-0.005))
      )
    )
    ; create a 1/2 pg size grid for OD_SMALL
    for( i x1s x2s
      x=i*PowerGridSize/2
      for( j y1s y2s
        y=j*PowerGridSize/2-PowerGridOffset
        ; rectangles are slightly small so they are maintained as rectangles thru the Assura processing!
        ; the only thing important is the lower left corner
        ; y2 is a block layer, do not use y2
        dbCreateRect(fillTempView list("y3" "drawing") list(x:y x+PowerGridSize/2-0.005:y+PowerGridSize/2-0.005))
      )
    )
    dbSave( fillTempView )
    printf("Creating %s layout\n" fillCellName )
    ; run layer processing
    AssuraLayerMappings = (list (list "smallFill" NImplantLPP)
                                (list "largeFill" PImplantLPP)
                                (list "dcapFill" PolyLPP)
                                (list "odsmallFill" NWellLPP)
                                (list "odFill" DiffLPP)
                                (list "m1Fill" Metal1LPP)
                                (list "m2Fill" Metal2LPP)
                                (list "m3Fill" Metal3LPP)
                                (list "m4Fill" Metal4LPP)
                                (list "m5Fill" Metal5LPP)
                                (list "m6Fill" Metal6LPP)
                                (list "m7Fill" Metal7LPP)
                                (list "via23Fill" Via23LPP)
                                (list "via34Fill" Via34LPP)
                                (list "via45Fill" Via45LPP)
                                (list "via56Fill" Via56LPP)
                                (list "via67Fill" Via67LPP)
                          )
    AssuraSets = append( 
                   append( 
                     if( FillWithSpareCell list("SPAREGATE") nil ) 
                     if( FillWithDcapCell list("DCAP") nil )
                   ) 
                   if( FillWithM2M7 list("M2M7") nil ) 
                 )
    AssuraRuleFile = sprintf( nil "%s/share/Fulcrum/filler/%s" 
                       ConfigFileGetValue( TheCDSConfigTable "FULCRUM_PDK_ROOT")
                       "generateFill.rul")
    AssuraRunLog = sprintf( nil "%s/%s.fill_grid.log" 
                       ConfigFileGetValue( TheCDSConfigTable "TEMP") 
                       CellView~>cellName )
    ErrorStr = AssuraRunAssuraLayerProcessor( 
                       fillTempView CellView~>libName fillCellName fillViewName
                       AssuraRuleFile
                       ConfigFileGetValue( TheCDSConfigTable "TEMP") 
                       AssuraLayerMappings 
                       nil
                       ?AssuraRunLog AssuraRunLog
                       ?LeaveMess LeaveMess
                       ?AssuraSets AssuraSets
                       )
    ddDeleteObj(ddGetObj(fillTempView~>libName fillTempView~>cellName fillTempView~>viewName))
    fillGridView=nrOpenCellViewWritable(CellView~>libName fillCellName fillViewName)
    
    count=0
    foreach( lpp fillGridView~>lpps
      if(ReplaceCellNameTable[lpp~>layerName]!="" then
        if( lpp~>layerName==car(NImplantLPP) || lpp~>layerName==car(PImplantLPP) then
          libName=SpareCellLibName
        else
          libName=FillCellLibName
        )
        fillMosaicView=nrOpenCellViewReadable( libName ReplaceCellNameTable[lpp~>layerName] "layout" )
        if( fillMosaicView == nil then printf("(%s) (%s)\n" libName lpp~>layerName))
        x1=0
        foreach( shape lpp~>shapes
          if( shape~>objType == "rect" || (shape~>objType == "polygon" && shape~>nPoints==5) then
            x2=leftEdge(shape~>bBox)
            ; small cells, do not offset
            if(ReplaceCellNameTable[lpp~>layerName] == "globals.fill.OD_SMALL"
              y2=bottomEdge(shape~>bBox)
              y2=bottomEdge(shape~>bBox)+PowerGridOffset
            )
            ; suggested alternative, to be tested
            ; if( abs(x2-floor(x2/PowerGridSize+0.0001)*PowerGridSize)<0.001 && abs(y2-floor(y2/PowerGridSize+0.0001)*PowerGridSize)<0.001 then
            ; I think the following condition is always true!, see above
            if( abs(x2-(x2/PowerGridSize)*PowerGridSize)<0.001 && abs(y2-(y2/PowerGridSize)*PowerGridSize)<0.001 then
              dbCreateInst(fillGridView fillMosaicView sprintf( nil "F%d" count ) list(x2 y2) "R0" )            
              count=count+1 
              x1=x1+1
            )
          else
            printf("Warning: no rectangle %s shape found: %L\n" shape~>layerName shape~>points)
          )         
          dbDeleteObject(shape)
        )
        printf("%s count: %d \n" ReplaceCellNameTable[lpp~>layerName] x1)
      )
    )
    printf("Done: %s (%s) created. \n" fillCellName fillViewName)
    dbSave(fillGridView)
    return(fillGridView)
  )
)


