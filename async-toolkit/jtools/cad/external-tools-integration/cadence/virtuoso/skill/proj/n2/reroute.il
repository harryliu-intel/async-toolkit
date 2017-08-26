; Scripts to reroute everything in N2 (above fragment level).  Actual
; cells and options are in the route/[leaf|mid|apr] directories
; which can be run with virtuoso_replay *.il.

(defun RouteCell (cellName topMetal @key (redoPG nil) (colorGrid t)
                           (importOnly nil) (timingDriven t) (nearby nil))
  (let (lCV pgCV aCV fCV cgCV libName fillCV bbox h w)
    libName = (GetCastLibName cellName)
    fCV  = (ReadCV libName cellName "floorplan")
    lCV  = (ReadCV libName cellName "layout")
    cgCV = (ReadCV libName (strcat cellName "_color") "layout")
    pgCV = (ReadCV libName (strcat cellName "_pg") "layout")
    aCV  = (ReadCV libName cellName "abstract")
    (when lCV  (cdsp4edit ?CV lCV))
    (when pgCV (cdsp4edit ?CV pgCV))
    (when cgCV (cdsp4edit ?CV cgCV))
    (when aCV  (cdsp4edit ?CV aCV))
    (when topMetal<2 fCV = (MakeFlatten ?CV CV ?topMetal 2 ?colorGrid colorGrid)) ; need M2 power grid
    (cond (nearby
           lCV = (MakeFlatten ?CV fCV ?topMetal topMetal)
           lCV = (betterCopyCellView lCV lCV->libName lCV->cellName "layout" nil nil t)
           (TemplateFill ?topMetal topMetal ?CV lCV)
           )
          (importOnly
           lCV = (RouteImport ?CV fCV)
           )
          (t 
           lCV = (Route ?CV fCV ?topMetal topMetal ?timingDriven timingDriven ?colorGrid colorGrid ?Mem 16384)
           )
          )
    (when redoPG
      (MakePowerGrid ?CV lCV ?topMetal 8)
      (MakePowerGridAbstract ?CV lCV ?topMetal 8)
      (CreatePowerPins ?topMetal 8 ?CV lCV)
      (when topMetal==6
        fillCV = (ReadCV "globals" "globals.FILL_GRID.m78_fill" "layout")
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
    aCV = (if topMetal==2 (MakeLeafAbstract ?CV lCV) (MakeSimpleAbstract ?CV lCV ?topMetal topMetal))
    pgCV = (ReadCV libName (strcat cellName "_pg") "layout")
    cgCV = (ReadCV libName (strcat cellName "_color") "layout")
    (when lCV (cdsp4add ?CV lCV))
    (when pgCV (cdsp4add ?CV pgCV))
    (when aCV  (cdsp4add ?CV aCV))
    (when cgCV (cdsp4add ?CV cgCV))
    lCV
    )
  )

(defun RouteLeaf (cellName topMetal @key (nearby nil))
  (RouteCell cellName topMetal ?timingDriven nil ?colorGrid nil ?nearby nearby)
  )

(defun RouteProteus (baseName targetName abstractName @key (exportOnly nil) (importOnly nil))
  (let (SPEC_DIR TEMP WDIR client libName fpCV pgCV cgCV lCV specfile)
    SPEC_DIR = (ConfigFileGetValue TheCDSConfigTable "SPEC_DIR")
    TEMP = (ConfigFileGetValue TheCDSConfigTable "TEMP")
    WDIR = (strcat TEMP "/route/" baseName)
    client = (ConfigFileGetValue TheCDSConfigTable "P4CLIENT")
    libName = (GetCastLibName baseName)
    fpCV = (ReadCV libName baseName "floorplan")
    lCV  = (ReadCV libName targetName "layout")
    pgCV = (ReadCV libName (strcat baseName "_pg") "layout")
    cgCV = (ReadCV libName (strcat baseName "_color") "layout")
    (when lCV  (cdsp4edit ?CV lCV))
    (when pgCV (cdsp4edit ?CV pgCV))
    (when cgCV (cdsp4edit ?CV cgCV))
    specfile = (GetCastCellName targetName)
    specfile = (buildString (parseString specfile ".") "/")
    specfile = (strcat SPEC_DIR "/" specfile ".cast")
    (shell (sprintf nil "p4 -c %s edit \"%s\"" client specfile))
    (cond (exportOnly
           (RouteExport ?CV fpCV ?softMacro t ?targetCellName targetName ?abstractCellName abstractName)
           )
          (importOnly
           lCV = (RouteImport ?libName (GetCastLibName targetName) ?cellName targetName
                              ?defFile (strcat WDIR "/" targetName "_routed.def")
                              )
           )
          (t
           lCV = (Route ?CV fpCV ?softMacro t ?targetCellName targetName ?abstractCellName abstractName
                        ?ecoIter 2)
           (when lCV (cdsp4add ?CV lCV))
           )
          )
    lCV  = (ReadCV libName targetName "layout")
    pgCV = (ReadCV libName (strcat baseName "_pg") "layout")
    cgCV = (ReadCV libName (strcat baseName "_color") "layout")
    (when lCV  (cdsp4add ?CV lCV))
    (when pgCV (cdsp4add ?CV pgCV))
    (when cgCV (cdsp4add ?CV cgCV))
    (shell (sprintf nil "p4 -c %s add %s" client specfile))
    t
    )
  )

(defun ImportCell (cellName)
  (let (libName lCV)
    libName = (GetCastLibName cellName)
    lCV = (ReadCV libName cellName "layout")
    (when lCV (cdsp4edit ?CV lCV))
    lCV = (RouteImport ?libName libName ?cellName cellName)
    (cdsp4add ?CV lCV)
    t
    )
  )

; translate a bunch of subcells
(defun SubstituteSubcells (translate @key (CV (geGetEditCellView)))
  (let (old new n map)
    n=0
    map = (makeTable "map" nil)
    (foreach x translate
             old = (car x)
             new = (ReadCV (GetCastLibName (cadr x)) (cadr x) "layout")
             (unless new (error "Can't open %s\n" (cadr x)))
             map[old]=new
             )
    (when (setof y CV->instances map[y->cellName])
      (cdsp4edit ?CV CV)
      (foreach inst CV->instances
               (when map[inst->cellName] inst->master=map[inst->cellName] n++)
               )
      (dbSave CV)
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

(defun FixERC (originalCell replacementCell @key (CV (geGetEditCellView)))
  (SubstituteSubcells (list (list originalCell replacementCell)
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

; use spc cells with no M0 for TOP_HALO cells
(defun FixSPC (@key (CV (geGetEditCellView)))
  (SubstituteSubcells (list (list "vendor.intel.d04.spc00nn.z01" "globals.spc00nn.z01")
                            (list "vendor.intel.d04.spc00nn.z02" "globals.spc00nn.z02")
                            (list "vendor.intel.d04.spc00nn.z03" "globals.spc00nn.z03")
                            )
                      ?CV CV)
  )

; Do chip-assembly steps on a top level layout cell
(defun UpdateTopCell (@key (CV (geGetEditCellView)))
  (let (TopMetal)
    TopMetal = 11
    (foreach inst CV->instances inst->status="locked")
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

; post-processing to remove M2 fill
(defun PatchTDL (@key (CV (geGetEditCellView)))
  (foreach x CV->shapes (when x->lpp==(list "m2" "fill") (dbDeleteObject x)))
  (dbSave CV)
  CV = (MakeLeafAbstract ?CV CV)
  )
