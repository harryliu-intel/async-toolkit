; add a standard power grid to a cellview
(defun AddPowerGrid (@key (CellView nil) (bbox nil))
  (let (x0 y0 x1 y1 power)
    (cond (CellView==nil CellView = (geGetEditCellView)))
    (cond (bbox==nil bbox = CellView->bBox))
    gp = 2*PowerGridPitch
    x0 = (leftEdge   bbox)
    y0 = (bottomEdge bbox)
    x1 = (rightEdge  bbox)
    y1 = (topEdge    bbox)
    x0 = (floor x0/gp)
    y0 = (floor y0/gp)
    x1 = (ceiling x1/gp)
    y1 = (ceiling y1/gp)
    power = (dbOpenCellViewByType "globals" "globals.wires.POWER_GRID_M34567" "layout")
    (dbCreateSimpleMosaic CellView power "power"
                          gp*x0:gp*y0 "R0"
                          y1-y0 x1-x0
                          gp gp)
    )
  t
  )

; check if a lib/cell/view is legal
(defun ViewExists (view viewName)
  (let (views)
    views = (dbAllCellViews view->lib view->cellName)
    (setof v views v==viewName)!=nil
    )
  )

; creates a layout_nogrid view without power grid or POWER_GRID_TIEOFF subcells
; processes subcells recursively if they don't already have layout_nogrid views
(defun CreateNoGridView (@key (CellView nil))
  (let (libName cellName hasNoGridView)

    ; select view to start from
    (cond (CellView==nil CellView = (geGetEditCellView)))
    libName  = CellView->libName
    cellName = CellView->cellName

    ; recursively generate layout_nogrid views if they don't exist
    (cond ((ViewExists CellView "layout_nogrid")==nil
           (printf "Creating %s %s layout_nogrid\n",CellView->libName,CellView->cellName)
           (dbSave CellView CellView->libName CellView->cellName "layout_nogrid")
           CellView = (dbOpenCellViewByType CellView->libName CellView->cellName
                                            "layout_nogrid" nil "a")
           
           ; delete POWER_GRID mosaics
           (rexCompile "^globals\\.wires\\.POWER_GRID_.*")
           (foreach mosaic CellView->mosaics
                    (cond ((rexExecute (car mosaic->instanceList)->cellName)
                           (dbDeleteObject mosaic))))

           ; delete any *POWER_GRID* instances
           (rexCompile "POWER_GRID")
           (foreach inst CellView->instances
                    (cond ((and inst->objType=="inst" (rexExecute inst->cellName))
                           (dbDeleteObject inst))))

           ; delete any y1..y7 layers
           (foreach shape CellView->shapes
                    (cond ((or shape->layerName=="y1"
                               shape->layerName=="y2"
                               shape->layerName=="y3"
                               shape->layerName=="y4"
                               shape->layerName=="y5"
                               shape->layerName=="y6"
                               shape->layerName=="y7")
                           (dbDeleteObject shape)
                           )
                          )
                    )
                               
           ; recursively create and swap in layout_nogrid subcells
           (rexCompile "^vendor\\..*")
           (foreach inst CellView->instances
                    (cond ((and inst->objType=="inst"
                                inst->viewName=="layout"
                                !(rexExecute inst->libName)
                                !(GetProp inst->master "isSynchronous" nil)
                                inst->libName!="globals"
                                inst->libName!="gate"
                                inst->libName!="stack"
                                inst->libName!="tsmc13lg")
                           (CreateNoGridView ?CellView inst->master)
                           inst->master = (dbOpenCellViewByType inst->master->lib
                                                                inst->master->cellName
                                                                "layout_nogrid")
                           )
                          )
                    )
           
           )
          )
    (dbOpenCellViewByType libName cellName "layout_nogrid" nil "a")
    )
  )

; flatten any y1 shapes of selected view to power_grid view
(defun CreatePowerGridView (@key (CellView nil))
  (let (libName cellName AssuraLayerMappings TempDir
        AssuraRuleFile PdkRoot AssuraRunLog ErrorStr)

    ; select view to start from
    (cond (CellView==nil CellView = (geGetEditCellView)))
    libName  = CellView->libName
    cellName = CellView->cellName

    ; flatten power_grid view if it doesn't already exist
    (cond ((ViewExists CellView "power_grid")==nil
           AssuraLayerMappings = (list (list "m3keep" Metal3LPP)
                                       (list "m4keep" Metal4LPP)
                                       (list "m5keep" Metal5LPP)
                                       (list "m6keep" Metal6LPP)
                                       (list "m7keep" Metal7LPP)
                                       (list "via3keep" Via34LPP)
                                       (list "via4keep" Via45LPP)
                                       (list "via5keep" Via56LPP)
                                       (list "via6keep" Via67LPP)
                                       )
           TempDir = (ConfigFileGetValue TheCDSConfigTable "TEMP")
           AssuraRuleFile = "flattenPowerGrid.rul"
           PdkRoot = (ConfigFileGetValue TheCDSConfigTable "FULCRUM_PDK_ROOT")
           AssuraRuleFile = (sprintf nil "%s/share/Fulcrum/cell_automation/%s" 
                                     PdkRoot AssuraRuleFile)
           AssuraRunLog = (sprintf nil "%s/%s.power_grid.log" TempDir cellName )
           ErrorStr = (AssuraRunAssuraLayerProcessor 
                       CellView libName cellName "power_grid"
                       AssuraRuleFile TempDir AssuraLayerMappings nil
                       ?AssuraRunLog AssuraRunLog
                       ?LeaveMess nil
                       )
           )
          )
    (dbOpenCellViewByType libName cellName "power_grid" nil "a")
    )
  )

; create the layout_topgrid view by combining layout_nogrid and power_grid views
(defun CreateTopGridView (@key (CellView nil))
  (let (libName cellName noGridView powerGridView topGridView)

    ; select view to start from
    (cond (CellView==nil CellView = (geGetEditCellView)))
    libName  = CellView->libName
    cellName = CellView->cellName

    ; create power_grid and layout_nogrid views
    noGridView    = (CreateNoGridView    ?CellView CellView)
    powerGridView = (CreatePowerGridView ?CellView CellView)

    ; combine them into layout_topgrid view
    (dbSave noGridView libName cellName "layout_topgrid")
    topGridView = (dbOpenCellViewByType libName cellName "layout_topgrid" nil "a")
    (foreach shape powerGridView->shapes    (dbCopyFig shape topGridView))
    (foreach inst  powerGridView->instances (dbCopyFig inst  topGridView))
    topGridView
    )
  )
