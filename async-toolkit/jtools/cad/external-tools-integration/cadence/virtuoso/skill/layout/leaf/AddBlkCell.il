
defun( AddBlkCell ( CellView )
  let((ColumnList prBound Params blkCellName)  
    ColumnList = caar(PlacementRegionsGetCellRegionDataColumnList( CellRegionData ))
    prBound=( PlacementRegionsGetCellRegionDataBBox CellRegionData )

    blkCellNameTable=makeTable(`blkCellName)
    blkCellNameTable[2]="globals.wires.start_np_blk"
    blkCellNameTable[3]="globals.wires.start_npn_blk"
    blkCellNameTable[4]="globals.wires.start_npnp_blk"
    blkCellName=blkCellNameTable[length(ColumnList)-1]
    Params=list(
        list( "height" "int" ceiling((topEdge(prBound)-bottomEdge(prBound))/3.12 ))
        list( "width" "int" ceiling((rightEdge(prBound)-leftEdge(prBound)-2.08)/0.26) )
        list( "metal_implant_guide" "boolean" nil )
        list( "PO_GRID" "boolean" nil )
        list( "PO_BLOCKAGE" "boolean" nil )
        list( "M3_BLOCKAGE" "boolean" nil )
        list( "implants" "boolean" nil )
        

    )

    Column1=car(ColumnList)
    ColumnList=cdr(ColumnList)

    Params=cons( list( "firstsetm2move" "int" ceiling((rightEdge(Column1)-0.325)/0.13) ) Params)

    
    dbCreateParamInstByMasterName( 
         CellView 
         "globals"
         blkCellName
         "layout" 
         "start_blk"
         list(leftEdge(prBound) bottomEdge(prBound))
         "R0"
         1
         Params
    
    )
  )
)

defun( AddLeafBlockageCell ( CellView )
  let(( prBound Params blkCellName)  
    prBound=CellView->prBoundary->bBox
    blkCellName="globals.wires.leaf_blockage"
    Params=list(
        list( "height" "int" ceiling((topEdge(prBound)-bottomEdge(prBound))/3.12 ))
        list( "width" "int" ceiling((rightEdge(prBound)-leftEdge(prBound)-2.08)/0.26) )
        list( "PO_GRID" "boolean" nil )
        list( "PO_BLOCKAGE" "boolean" t )
        list( "M3_BLOCKAGE" "boolean" t )
        

    )

    dbCreateParamInstByMasterName( 
         CellView 
         "globals"
         blkCellName
         "layout" 
         "leaf_blockage"
         list(leftEdge(prBound) bottomEdge(prBound))
         "R0"
         1
         Params
    
    )
  )
)
defun( DeleteLeafBlockageCell ( CellView )
  let((inst)
    inst=dbFindAnyInstByName( CellView "leaf_blockage")
    if( inst then dbDeleteObject( inst ))
  )
)

defun( SnapGatesStacksToPitch ( CellView )
  let((x y inst)
    foreach( inst CellView~>instances
      if( inst~>libName=="gate" || inst~>libName=="stack" then
        y=cadr(inst->xy)
        x=car(inst->xy)
        y=0.13*floor(y/0.13)
        inst->xy=list(x y)
      )
    )
  )
)
