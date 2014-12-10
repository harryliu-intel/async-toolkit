; Copyright 2006 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

defun( AutoGeneratePowerGrid ( CellView powerCellName 
                               @key
                               (powerViewName "layout") 
                               (powerAbstractViewName "abstract") 
                               (OBSCellName "") 
                               (OBSViewName "layout") 
                               (overhang t) ; add an extra grid around the outside
                               )
  prog(( powerTempView powerGridView x1 x2 y1 y2 x y x1box x2box y1box y2box
        AssuraLayerMappings AssuraRuleFile powerGridView layoutWithPowerGridView powerGridAbstractView
        count lpp shape ReplaceCellNameTable pinLayerName pinLayerPurpose tmp OBSView blockShapes)
    PowerGridSize=2*PowerGridPitch
    PowerGridLib="globals"
    ReplaceCellNameTable=makeTable("a" "")
    ReplaceCellNameTable["M3"]="globals.wires.POWER_GRID_M34567"
    ReplaceCellNameTable["M4"]="globals.wires.POWER_GRID_M4567"
    ReplaceCellNameTable["M5"]="globals.wires.POWER_GRID_M567"
    ReplaceCellNameTable["M6"]="globals.wires.POWER_GRID_M67"
    ReplaceCellNameTable["M7"]="globals.wires.POWER_GRID_M78"
    ReplaceCellNameTable["M8"]="globals.wires.POWER_GRID_M8"
    if( setof( lpp CellView~>lpps lpp~>layerName=="prBoundary" ) == nil then
      printf("Error: There is no prBoundary layer defined in cellview %s. \n" CellView~>cellName)
      return(nil)
    )
    powerTempView=dbOpenCellViewByType( CellView~>libName CellView~>cellName "autoPowerGrid_temp" "maskLayout" "w" )
    if( powerTempView == nil then
      printf("Error: Can not open cellview %s (autoPowerGrid_temp) for write. \n" CellView~>cellName)
      return(nil)
    )
    powerGridView=nrOpenCellViewWritable(CellView~>libName powerCellName powerViewName)
    if( powerGridView == nil then
      printf("Error: Can not open cellview %s (%s) for write. \n" powerCellName powerViewName)
      return(nil)
    )
    dbCreateInst( powerTempView CellView "I0" list(0 0) "R0" ) 

    ; Draw 5.76 x 5.76 rectangle grid
    x1box=leftEdge( powerTempView~>bBox )
    x2box=rightEdge( powerTempView~>bBox )
    y1box=bottomEdge( powerTempView~>bBox )
    y2box=topEdge( powerTempView~>bBox )

    ; minimum sized power grid
    x1=floor(x1box/PowerGridSize)
    y1=floor(y1box/PowerGridSize)
    x2=floor(x2box/PowerGridSize)
    y2=floor(y2box/PowerGridSize)

    when( modulo( floor(x2box*1000) floor(PowerGridSize*1000) )==0
      x2=x2-1
    )
    when( modulo( floor(y2box*1000) floor(PowerGridSize*1000) )==0
      y2=y2-1
    )

    ; expand the power grid 
    when( overhang
          x1 = x1-1
          y1 = y1-1
          x2 = x2+1
          y2 = y2+1
    )

    ; create rectangles for Assura processing
    for( i x1 x2
      x=i*PowerGridSize
      for( j y1 y2
        y=j*PowerGridSize
        dbCreateRect(powerTempView list("y0" "drawing") list(x:y x+PowerGridSize-0.005:y+PowerGridSize-0.005))
      )
    )

    dbSave( powerTempView )
    printf("Creating %s layout\n" powerCellName )
    ; run layer processing
    AssuraLayerMappings = (list 
                                (list "power345678" (list "M3" "block"))
                                (list "power45678" (list "M4" "block"))
                                (list "power5678" (list "M5" "block"))
                                (list "power678" (list "M6" "block"))
                                (list "power78" (list "M7" "block"))
                                (list "power8" (list "M8" "block"))
                                (list "m1Keepout" (list "M1" "boundary"))
                                (list "m1Keepout" (list "M1" "boundary"))
                                (list "m2Keepout" (list "M2" "boundary"))
                                (list "m3Keepout" (list "M3" "boundary"))
                                (list "m4Keepout" (list "M4" "boundary"))
                                (list "m5Keepout" (list "M5" "boundary"))
                                (list "m6Keepout" (list "M6" "boundary"))
                                (list "m7Keepout" (list "M7" "boundary"))
                                (list "m8Keepout" (list "M8" "boundary"))
                          )
    AssuraRuleFile = sprintf( nil "%s/share/Fulcrum/cell_automation/%s" 
                       ConfigFileGetValue( TheCDSConfigTable "FULCRUM_PDK_ROOT")
                       "generatePowerGrid.rul")
    AssuraRunLog = sprintf( nil "%s/%s.power_grid.log" 
                       ConfigFileGetValue( TheCDSConfigTable "TEMP") 
                       CellView~>cellName )
    ErrorStr = AssuraRunAssuraLayerProcessor( 
                       powerTempView CellView~>libName powerCellName powerViewName
                       AssuraRuleFile
                       ConfigFileGetValue( TheCDSConfigTable "TEMP") 
                       AssuraLayerMappings 
                       nil
                       ?AssuraRunLog AssuraRunLog
                       ?LeaveMess nil
                       )
    ddDeleteObj(ddGetObj(powerTempView~>libName powerTempView~>cellName powerTempView~>viewName))
    powerGridView=nrOpenCellViewWritable(CellView~>libName powerCellName powerViewName)
    
    count=0
    x1=nil 
    y1=nil
    foreach( lpp powerGridView~>lpps
      if(ReplaceCellNameTable[lpp~>layerName]!="" && lpp~>purpose=="block" then
        powerMosaicView=nrOpenCellViewReadable( PowerGridLib ReplaceCellNameTable[lpp~>layerName] "layout" )
        foreach( shape lpp~>shapes
          if( shape~>objType == "rect" || (shape~>objType == "polygon" && shape~>nPoints==5) then
            x2=leftEdge(shape~>bBox)
            y2=bottomEdge(shape~>bBox)
            dbCreateInst(powerGridView powerMosaicView
                 sprintf( nil "I%d" count ) 
                 list(x2 y2) 
                 "R0" )
            dbDeleteObject(shape)
            count=count+1 
            cond((y1==nil y1=y2 x1=x2)
                 (y2<y1 y1=y2 x1=x2)
                 (y2==y1 if(x2<x1 then x1=x2))
                )
          else
            printf("Error: no rectangle %s shape found\n" shape~>layerName)
          )         
        )
      )
    )
    if( x1!=nil && y1!=nil then
      CreatePowerPins( powerGridView "M7" list(x1 y1) nil )
    else
      printf("Error: no shape created in PowerGrid View.\n")
      return(nil)
    )

    foreach( shape CellView->shapes
      when( car(shape->lpp)=="prBoundary" && shape->objType=="rect"
        dbCreateRect( powerGridView shape->lpp shape->bBox)
      )
      when( car(shape->lpp)=="prBoundary" && shape->objType=="polygon"
        dbCreatePolygon( powerGridView shape->lpp shape->points)
      )
    )

    dbSave(powerGridView)
    OBSView=nrOpenCellViewReadable( powerGridView->libName OBSCellName OBSViewName )
    if( OBSView then
      dbCreateInst( powerGridView OBSView "obs" list(0 0) "R0" ) 
      dbSave(powerGridView)      
      printf("OBS %s instantiated in powerGrid. \n" OBSCellName )
    else
      if( OBSCellName != "" then
        printf("Info: OBS view %s (%s) does not exist. \n" OBSCellName OBSViewName )
      )
    )
    blockShapes= (setof shape CellView~>shapes
                        (or shape~>purpose=="boundary"
                            (and shape~>layerName=="prBoundary"
                                 shape~>purpose=="block")))
    foreach( shape blockShapes dbCopyFig( shape powerGridView))
    foreach( shape blockShapes dbDeleteObject(shape))
    printf("Done: %s (%s) created. \n" powerCellName powerViewName)
    return(powerGridView)
  )
)

defun( AutoGeneratePowerGridAbstract ( CellView powerCellName
                               @key
                               (powerViewName "layout") 
                               (powerAbstractViewName "abstract") 
                               (OBSCellName "") 
                               (OBSViewName "layout") 
                               (M3PowerPins t)
                               (M5PowerPins nil)
                               (M7PowerPins nil)
                               (ResolveMacroBlockageConflict nil)
                               (M1M2_Ring t)
                               (Class "block")
                               )

  prog(( powerGridView powerGridAbstractView AssuraLayerMappings 
         powerGridViewTemp NewInst InstAbstractView ViewToAssura
         AssuraRuleFile AssuraRunLog ErrorStr power_layers
         blockCells blockShapes m1points)

    ; open power grid CV
    powerGridView=nrOpenCellViewWritable(CellView~>libName powerCellName powerViewName)
    if( powerGridView == nil then
      printf("Error: Can not open cellview %s (%s) for write. \n" powerCellName powerViewName)
      return(nil)
    )
    powerGridViewTemp=nil
    ViewToAssura=powerGridView

    ; copy abstract views of subcells so stripes avoid their keepout too
    if( ResolveMacroBlockageConflict then
      powerGridViewTemp=dbCopyCellView( powerGridView CellView~>libName powerCellName
                                        strcat(powerViewName "_temp") nil nil t )
      if( powerGridViewTemp == nil then
        printf("Error: Can not open cellview %s (%s_temp) for write. \n"
               powerCellName powerViewName)
        return(nil)
      )
      foreach( inst CellView~>instances 
        when( inst~>libName!=TechLibName && inst~>master!=powerGridView
          InstAbstractView=nrOpenCellViewReadable(inst~>libName inst~>cellName "abstract_edit" )
          unless( InstAbstractView InstAbstractView=nrOpenCellViewReadable(inst~>libName inst~>cellName "abstract" ) )
          if( InstAbstractView then 
            NewInst=dbCreateInst( powerGridViewTemp InstAbstractView inst->name inst->xy inst->orient )
          else
            printf("Can't open abstract view for %s\n" inst->cellName)
          )
        )
      )
      dbSave(powerGridViewTemp)
      ViewToAssura=powerGridViewTemp
    )

    ; check writeability of target CV
    powerGridAbstractView=nrOpenCellViewWritable( CellView~>libName
                                                  powerCellName powerAbstractViewName ) 
    if( powerGridAbstractView == nil then
      printf("Error: Can not open cellview %s (%s) for write. \n"
             powerCellName powerAbstractViewName)
      return(nil)
    )

    ; run Assura abstract_PowerGrid.rul
    printf("Creating %s abstract\n" powerCellName )
    AssuraLayerMappings = (list
                                (list "prb" (list "prBoundary" "drawing"))
                                (list "m3Vdd" (list "M3" "vdd"))
                                (list "m3GND" (list "M3" "gnd"))
                                (list "m5Vdd" (list "M5" "vdd"))
                                (list "m5GND" (list "M5" "gnd"))
                                (list "m7Vdd" (list "M7" "vdd"))
                                (list "m7GND" (list "M7" "gnd"))
                                (list "m1Keepout" (list "M1" "boundary"))
                                (list "m2Keepout" (list "M2" "boundary"))
                                (list "m3Keepout" (list "M3" "boundary"))
                                (list "m4Keepout" (list "M4" "boundary"))
                                (list "m5Keepout" (list "M5" "boundary"))
                                (list "m6Keepout" (list "M6" "boundary"))
                                (list "m7Keepout" (list "M7" "boundary"))
                                (list "overlap"   (list "OVERLAP" "boundary"))
                          )
    AssuraRuleFile = sprintf( nil "%s/share/Fulcrum/cell_automation/%s" 
                       ConfigFileGetValue( TheCDSConfigTable "FULCRUM_PDK_ROOT")
                       "abstract_PowerGrid.rul")
    AssuraRunLog = sprintf( nil "%s/%s.abstract_pg.log" 
                       ConfigFileGetValue( TheCDSConfigTable "TEMP") 
                       CellView~>cellName )
    ErrorStr = AssuraRunAssuraLayerProcessor( 
                       ViewToAssura CellView~>libName powerCellName powerAbstractViewName
                       AssuraRuleFile
                       ConfigFileGetValue( TheCDSConfigTable "TEMP") 
                       AssuraLayerMappings 
                       nil
                       ?AssuraSets (when M1M2_Ring (list "M1M2_RING"))
                       ?AssuraRunLog AssuraRunLog
                       ?LeaveMess nil
                       )
    powerGridAbstractView=nrOpenCellViewWritable( CellView~>libName
                                                  powerCellName powerAbstractViewName ) 
    if( powerGridAbstractView~>shapes==nil then
      printf("Error: %s (%s) is empty. \n" powerCellName powerAbstractViewName)
      dbSave(powerGridAbstractView)
      return(nil)
    )

    ; convert shapes to rectangles
    leMergeShapes( powerGridAbstractView~>shapes )
    foreach( shape powerGridAbstractView~>shapes
      dbLayerTile(powerGridAbstractView shape~>lpp list(shape))
      dbDeleteObject(shape)
    )

    ; create power pins on specified horizontal metal layers
    when( M3PowerPins power_layers = cons( "M3" power_layers ) )
    when( M5PowerPins power_layers = cons( "M5" power_layers ) )
    when( M7PowerPins power_layers = cons( "M7" power_layers ) )
    foreach( shape setof( shape powerGridAbstractView~>shapes
                                member( shape~>layerName power_layers) &&
                                shape~>purpose=="vdd")
                   shape~>purpose="net"
                   dbCreatePin( dbMakeNet( powerGridAbstractView VddNetName ) shape )
                   )
    foreach( shape setof( shape powerGridAbstractView~>shapes
                                member( shape~>layerName power_layers) &&
                                shape~>purpose=="gnd")
                   shape~>purpose="net"
                   dbCreatePin( dbMakeNet( powerGridAbstractView GNDNetName ) shape )
                   )
    foreach( shape setof( shape powerGridAbstractView~>shapes
                                or( shape~>purpose=="vdd" shape~>purpose=="gnd"))
                   shape~>purpose="boundary"
                   )

    ; finish up
    dbCreateRect( powerGridAbstractView list("prBoundary" "boundary") powerGridAbstractView~>bBox )
    sprintf( gec3Foreign "%s %f %f" powerGridAbstractView~>cellName 
                                    caar(powerGridAbstractView~>bBox) 
                                    cadar(powerGridAbstractView~>bBox) )
    dbReplacePropList( powerGridAbstractView list( 
              list("prCellClass" "string" Class)
              list("prCellType" "string" "macro")
              list("maskLayoutSubType" "string" powerGridAbstractView~>viewName)
              list("symmetry" "string" "X Y")
              list("gec3Foreign" "string" gec3Foreign)
              )
    )
    dbSave(powerGridAbstractView)
    printf("Done: %s (%s) created. \n" powerCellName powerAbstractViewName)
    if(  powerGridViewTemp then 
      ddDeleteObj( ddGetObj( powerGridViewTemp~>libName powerGridViewTemp~>cellName powerGridViewTemp~>viewName ""))
      powerGridView=nrOpenCellViewWritable(CellView~>libName powerCellName powerViewName)      
    )
    blockCells=setof( inst powerGridView~>instances inst~>cellName==OBSCellName && inst~>viewName==OBSViewName)
    foreach( inst blockCells dbDeleteObject( inst ))
    blockShapes= setof( shape powerGridView~>shapes shape~>purpose=="boundary" ) 
    foreach( shape blockShapes dbDeleteObject(shape))
    dbSave( powerGridView )

    return(powerGridAbstractView)
  )
)



defun( IsPowerGridCell ( inst )
  let( (istrue)
    foreach( part parseString( inst->cellName "_" )
      when( part=="TIEOFF" istrue=t )
    )
    istrue
 )
)


defun( DeletePowerGridInstance ( srcView )
  let( (pwrgridcell pwrgridobj)
   foreach( inst srcView->instances
    when( IsPowerGridCell(inst)
            pwrgridcell=inst->cellName
            dbDeleteObject( inst )
    )
   )
))

defun( DeletePowerGridCell ( srcView )
  let( (pwrgridcell pwrgridobj)
   foreach( inst srcView->instances
    when( IsPowerGridCell(inst)
            pwrgridcell=inst->cellName
            dbDeleteObject( inst )
    )
   )
   when( pwrgridcell
      pwrgridobj=ddGetObj( srcView->libName pwrgridcell )
      when( pwrgridobj ddDeleteObj(pwrgridobj) )
   )
))

; instantiate the power grid in specified CV
(defun AddPowerGridInstance
  (@key (CV (geGetEditCellView))
        )
  (let (RComponenets type_sub PowerGridCellName pgCV)
    (unless (dbFindAnyInstByName CV "tiehilo")
      RComponents = (reverse (parseString CV->cellName "."))
      (when (length RComponents)<3 (error "Cell name violates CAST conventions\n"))
      type_sub = (sprintf nil "%s_%s" (cadr RComponents) (car RComponents))
      PowerGridCellName = (strcat CV->libName ".wires." type_sub "_POWER_GRID_TIEOFF")
      pgCV = (nrOpenCellViewReadable CV->libName PowerGridCellName "layout")
      (when pgCV (dbCreateInst CV pgCV "tiehilo" 0:0 "R0"))
      )
    )
  )
