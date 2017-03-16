; Scripts to reroute everything in N2 (above fragment level).  Actual
; cells and options are in the route/[leaf|mid|apr] directories
; which can be run with virtuoso_replay *.il.

(defun RouteCell (cellName topMetal @key (redoPG nil) (colorGrid t) (importOnly nil) (timingDriven t))
  (let (lCV pgCV aCV fCV cgCV libName fillCV bbox h w)
    libName = (GetCastLibName cellName)
    fCV  = (dbOpenCellViewByType libName cellName "floorplan")
    lCV  = (dbOpenCellViewByType libName cellName "layout")
    cgCV = (dbOpenCellViewByType libName (strcat cellName "_color") "layout")
    pgCV = (dbOpenCellViewByType libName (strcat cellName "_pg") "layout")
    aCV  = (dbOpenCellViewByType libName cellName "abstract")
    (when lCV  (cdsp4edit ?CV lCV))
    (when pgCV (cdsp4edit ?CV pgCV))
    (when cgCV (cdsp4edit ?CV cgCV))
    (when aCV  (cdsp4edit ?CV aCV))
    lCV = (if importOnly
            (RouteImport ?CV fCV)
            (Route ?CV fCV ?topMetal topMetal ?timingDriven timingDriven ?colorGrid colorGrid ?Mem 16384)
            )
    (when redoPG
      (MakePowerGrid ?CV lCV ?topMetal 8)
      (MakePowerGridAbstract ?CV lCV ?topMetal 8)
      (CreatePowerPins ?topMetal 8 ?CV lCV)
      (when topMetal==6
        fillCV = (dbOpenCellViewByType "globals" "globals.FILL_GRID.m78_fill" "layout")
        bbox = lCV->prBoundary->bBox
        h = (cadr (cadr bbox)) - (cadr (car bbox))
        w = (car (cadr bbox)) - (car (car bbox))
        h = (round h/(cadr gridAlignment[8]))
        w = (round w/(car  gridAlignment[8]))
        (dbCreateSimpleMosaic lCV fillCV "fill" (car bbox) "R0"
                              h w (cadr gridAlignment[8]) (car gridAlignment[8]))
        (dbSave lCV)
        )
      )
    aCV = (MakeSimpleAbstract ?CV lCV ?topMetal topMetal)
    pgCV = (dbOpenCellViewByType libName (strcat cellName "_pg") "layout")
    cgCV = (dbOpenCellViewByType libName (strcat cellName "_color") "layout")
    (when lCV (cdsp4add ?CV lCV))
    (when pgCV (cdsp4add ?CV pgCV))
    (when aCV  (cdsp4add ?CV aCV))
    (when cgCV (cdsp4add ?CV cgCV))
    lCV
    )
  )

(defun RouteLeaf (cellName topMetal)
  (RouteCell cellName topMetal ?timingDriven nil ?colorGrid nil)
  )

(defun RouteProteus (baseName targetName abstractName @key (exportOnly nil))
  (let (libName fpCV pgCV cgCV lCV)
    libName = (GetCastLibName baseName)
    fpCV  = (dbOpenCellViewByType libName baseName "floorplan")
    pgCV = (dbOpenCellViewByType libName (strcat baseName "_pg") "layout")
    cgCV = (dbOpenCellViewByType libName (strcat baseName "_color") "layout")
    (when pgCV (cdsp4edit ?CV pgCV))
    (when cgCV (cdsp4edit ?CV cgCV))
    (cond (exportOnly
           (RouteExport ?CV fpCV ?softMacro t ?targetCellName targetName ?abstractCellName abstractName)
           )
          (t
           lCV = (Route ?CV fpCV ?softMacro t ?targetCellName targetName ?abstractCellName abstractName)
           (when lCV (cdsp4add ?CV lCV))
           )
          )
    pgCV = (dbOpenCellViewByType libName (strcat baseName "_pg") "layout")
    cgCV = (dbOpenCellViewByType libName (strcat baseName "_color") "layout")
    (when pgCV (cdsp4add ?CV pgCV))
    (when cgCV (cdsp4add ?CV cgCV))
    t
    )
  )

(defun ImportCell (cellName)
  (let (libName lCV)
    libName = (GetCastLibName cellName)
    lCV = (dbOpenCellViewByType libName cellName "layout")
    (when lCV (cdsp4edit ?CV lCV))
    lCV = (RouteImport ?libName libName ?cellName cellName)
    (cdsp4add ?CV lCV)
    t
    )
  )

; translate a bunch of subcells
(defun SubstituteSubcells (translate @key (CV (geGetEditCellView)) (countOnly nil))
  (let (old new n map)
    n=0
    map = (makeTable "map" nil)
    (foreach x translate
             old = (car x)
             new = (dbOpenCellViewByType (GetCastLibName (cadr x)) (cadr x) "layout")
             map[old]=new
             )
    (foreach inst CV->instances
             (when map[inst->cellName]
               (unless countOnly inst->master=map[inst->master])
               n++
               )
             )
    n
    )
  )

; replace bdc01's with our custom versions
(defun FixBDC (@key (CV (geGetEditCellView)))
  (SubstituteSubcells (list (list "vendor.intel.d04.bdc01ld.z04" "globals.bdc01ld.z04")
                            (list "vendor.intel.d04.bdc01ld.z08" "globals.bdc01ld.z08")
                            )
                      ?CV CV)
  )

; replace I.0 library cells with K.0 equivalents
(defun FixK0 (@key (CV (geGetEditCellView)))
  (SubstituteSubcells (list (list "vendor.intel.d04.bdc01nd.z04" "vendor.intel.d04.bdck1nd.z04")
                            (list "vendor.intel.d04.bdc01nd.z08" "vendor.intel.d04.bdck1nd.z08")
                            (list "vendor.intel.d04.bdc01nd.z16" "vendor.intel.d04.bdck1nd.z16")
                            (list "vendor.intel.d04.bdc01nd.z32" "vendor.intel.d04.bdck1nd.z32")
                            (list "vendor.intel.d04.bdc01nd.z64" "vendor.intel.d04.bdck1nd.z64") 

                            (list "vendor.intel.d04.bdc01ld.z04" "vendor.intel.d04.bdck1ld.z04")
                            (list "vendor.intel.d04.bdc01ld.z08" "vendor.intel.d04.bdck1ld.z08")
                            (list "vendor.intel.d04.bdc01ld.z16" "vendor.intel.d04.bdck1ld.z16")
                            (list "vendor.intel.d04.bdc01ld.z32" "vendor.intel.d04.bdck1ld.z32")
                            (list "vendor.intel.d04.bdc01ld.z64" "vendor.intel.d04.bdck1ld.z64")

                            (list "vendor.intel.d04.bdc01wd.z04" "vendor.intel.d04.bdck1wd.z04")
                            (list "vendor.intel.d04.bdc01wd.z08" "vendor.intel.d04.bdck1wd.z08")
                            (list "vendor.intel.d04.bdc01wd.z16" "vendor.intel.d04.bdck1wd.z16")
                            (list "vendor.intel.d04.bdc01wd.z32" "vendor.intel.d04.bdck1wd.z32")
                            (list "vendor.intel.d04.bdc01wd.z64" "vendor.intel.d04.bdck1wd.z64")

                            (list "vendor.intel.d04.bar00nn.z04" "vendor.intel.d04.bark1nn.z04")
                            (list "vendor.intel.d04.bar00nn.z08" "vendor.intel.d04.bark1nn.z08")
                            (list "vendor.intel.d04.bar00nn.z16" "vendor.intel.d04.bark1nn.z16")
                            (list "vendor.intel.d04.bar00nn.z32" "vendor.intel.d04.bark1nn.z32")
                            (list "vendor.intel.d04.bar00nn.z64" "vendor.intel.d04.bark1nn.z64") 

                            (list "vendor.intel.d04.bar00ln.z04" "vendor.intel.d04.bark1ln.z04")
                            (list "vendor.intel.d04.bar00ln.z08" "vendor.intel.d04.bark1ln.z08")
                            (list "vendor.intel.d04.bar00ln.z16" "vendor.intel.d04.bark1ln.z16")
                            (list "vendor.intel.d04.bar00ln.z32" "vendor.intel.d04.bark1ln.z32")
                            (list "vendor.intel.d04.bar00ln.z64" "vendor.intel.d04.bark1ln.z64") 

                            (list "vendor.intel.d04.bar00wn.z04" "vendor.intel.d04.bark1wn.z04")
                            (list "vendor.intel.d04.bar00wn.z08" "vendor.intel.d04.bark1wn.z08")
                            (list "vendor.intel.d04.bar00wn.z16" "vendor.intel.d04.bark1wn.z16")
                            (list "vendor.intel.d04.bar00wn.z32" "vendor.intel.d04.bark1wn.z32")
                            (list "vendor.intel.d04.bar00wn.z64" "vendor.intel.d04.bark1wn.z64") 
                           )
                      ?CV CV)
  )

; Do chip-assembly steps on a top level layout cell
(defun UpdateTopCell (@key (CV (geGetEditCellView)))
  (let (TopMetal)
    TopMetal = 11
    (UpdateFloorplan ?CV CV ?recurse nil)
    (DeletePins ?CV CV)
    (DeleteAutoGen ?CV CV)
    (LabelBumps ?CV CV)
    (RefreshAllBuswires ?CV CV)
    (DrawMidLevelPins ?CV CV ?include_supply t ?supply_topMetal 8)
    (CanonicalizePins ?CV CV)
    )
  (RouteNearby ?CV CV)
  (dbSave CV)
  CV
  )
