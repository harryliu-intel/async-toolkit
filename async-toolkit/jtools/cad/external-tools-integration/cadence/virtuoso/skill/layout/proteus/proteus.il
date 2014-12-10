; Skill scripts to construct Proteus library LEF.  Source information
; is floorplan views from supersize.  Layout views are generated and
; pins are added.  Abstract views are created from the layout views.
; LEF is written from the abstract views.
;
; Author: Andrew Lines
; Copyright 2007 Fulcrum Microsystems

; default globals
(defun ProteusDefaults ()
  ProteusScaleX = 1
  ProteusScaleY = 1
  ProteusDeleteSubcells = nil
  DefaultWiringPitchX = DefaultWiringPitch
  ProteusPinY = 8
  ProteusPinX = (list  0.5  1.5            4.5  5.5  6.5  7.5           10.5 11.5
                      12.5 13.5           16.5 17.5 18.5 19.5           22.5 23.5
                      24.5 25.5           28.5 29.5 30.5 31.5           34.5 35.5
                      36.5 37.5           40.5 41.5 42.5 43.5           46.5 47.5)
  ProteusPinVddW = 0.16
  ProteusPinGNDW = 0.16
  ProteusPinGNDX = (list 3 21 27 45)
  ProteusPinVddX = (list 9 15 33 39)
  ProteusPinCutout = 0.06
  ProteusLayoutViewName = "layout"
  ProteusAbstractViewName = "abstract"
  ProteusAbstractProteusViewName = "abstract_proteus"
  ProteusStrengths = (list 0 1 2 3 4 5 6 7 8)
  ProteusType = "qdi"
  )

(ProteusDefaults)

; do all steps
(defun ProteusAll ()
  (ProteusAutoPlace)
  (ProteusSortCells "floorplan")
  (ProteusLayoutViews)
  (ProteusPins)
  (ProteusSortCells ProteusLayoutViewName)
  (ProteusAbstract)
  (ProteusLef)
  t
  )

; just do Abstract and Lef steps again (use for final real layout)
(defun ProteusAbstractLefs ()
  (ProteusAbstract)
  (ProteusLef)
  t
  )

; sort and arrange cells by instance name
(defun SortCells (@key (CV nil))
  (let (X x0 x1 names inst)
    (cond (CV==nil CV = (geGetEditCellView)))
    names = nil
    (foreach inst CV->instances names = (cons inst->name names))
    X = 0
    (foreach name (sort names nil)
             inst = (dbFindAnyInstByName CV name)
             x0 = (car (car inst->master->bBox))
             x1 = (car (cadr inst->master->bBox))
             inst->xy = (list X-x0 0)
             X = X+x1-x0
             )
    )
  t
  )

; sort subcells of top-level cells
(defun ProteusSortCells (view @key (ProteusType ProteusType) (Strengths ProteusStrengths))
  (let (CV)
    (foreach strength Strengths
             CV = (dbOpenCellViewByType
                   (sprintf nil "synthesis.%s.sizing" ProteusType)
                   (sprintf nil "synthesis.%s.sizing.SIZE-L%d-R.%d"
                            ProteusType strength strength)
                   view nil "a")
             (SortCells ?CV CV)
             )
    )
  t
  )

; generate abstracts
(defun ProteusAbstract (@key (ProteusType ProteusType))
  (let (filename cells)
    filename = (sprintf nil "%s.gates" ProteusType)
    cells = (OpenCellViewsFromFile filename ProteusLayoutViewName "r")
    (foreach CV cells
             (MakeLeafAbstract ?CV CV
                               ?cutout ProteusPinCutout 
                               ?floodfillM1Keepout t
                               ?floodfillM2Keepout nil
                               ?floodfillM3Keepout nil
                               ?extendM3Keepout nil
                               ?extendM2Keepout nil)
             )
    )
  t
  )

; export lef
(defun ProteusLef (@key (ProteusType ProteusType))
  (let (filename cells statusLef)
    filename = (sprintf nil "%s.gates" ProteusType)
    cells = (OpenCellViewsFromFile filename ProteusAbstractViewName "r")
    (sh "mkdir -p lef")
    (foreach CV cells
             CV = (ProteusTransformAbstract ?CV CV)
             statusLef = (ldtrLefWriteOA
                          (sprintf nil "lef/%s.lef" CV->cellName)
                          CV->libName
                          CV->cellName
                          nil
                          CV->viewName
                          (sprintf nil "lef/%s.lef.log" CV->cellName)
                          t
                          "5.7")
             (unless statusLef (error "can't write lef for %s" CV->cellName))
             )
    )
  t
  )

; transform proteus abstract (swap X and Y coordinates)
(defun ProteusTransformAbstract (@key (CV (geGetEditCellView)))
  CV = (dbCopyCellView CV CV->libName CV->cellName
                       ProteusAbstractProteusViewName nil nil t)
  (foreach label (setof s CV->shapes s->objType=="label") label->parent=nil)
  (foreach shape (cons CV->prBoundary (append CV->shapes CV->blockages))
           (dbMoveFig shape CV (list 0:0 "MXR90" 1))
           )
  CV->cellType = "core" ; CLASS CORE
  CV->site = "CoreSite" ; PROPERTY site "CoreSite"
  (dbSetCellViewSymmetry CV "XY") ; SYMMETRY X Y
  (dbSave CV)
  CV
  )

; draw a single proteus pin
(defun ProteusDrawPin ( name layer width xy0 xy1 )
  (DrawSegment name layer width xy0 xy1 nil nil t)
  )

; advance to next legal xy pin location given current xy and upper right bbox
; xy is x_index:y_pitch not coordinates
(defun ProteusNextXY (xy xy1 @key (inc 2))
  (let (x y x1 y1 rx)
    x = (car xy)
    y = (cadr xy)
    x1 = (car xy1)
    y1 = (cadr xy1)
    x=x+inc
    rx = (nth x ProteusPinX)
    (when rx==nil || rx*DefaultWiringPitchX>=x1 y=y+ProteusPinY x=0)
    x:y
    )
  )

; draw a single pin on metal2 at current location, advance location
(defun ProteusPin ( name xy xy1 @key (inc 2))
  (let (x y y0 y1)
    ; draw pin
    x = (car xy)
    y = (cadr xy)
    x = (nth x ProteusPinX)*DefaultWiringPitchX
    y = y*DefaultWiringPitch
    y0 = y + DefaultWiringPitch/2
    y1 = y + ProteusPinY*DefaultWiringPitch - DefaultWiringPitch/2
    (ProteusDrawPin name Metal2LPP DefaultWiringWidth x:y0 x:y1)
    (ProteusNextXY xy xy1 ?inc inc)
    )
  )

; draw a pair of pins on metal2 at current location, advance location
(defun ProteusPinList ( names xy xy1 @key (inc 2))
  (foreach name names
           xy = (ProteusPin name xy xy1 ?inc inc)
           )
  xy
  )

; draw power pins
(defun ProteusDrawPowerPins ( bbox )
  (let (x0 y0 x1 y1)
    x0 = (car  (car  bbox))
    y0 = (cadr (car  bbox))
    x1 = (car  (cadr bbox))
    y1 = (cadr (cadr bbox))
    (foreach i ProteusPinGNDX
             x = i*DefaultWiringPitchX
             (when x>x0 && x<x1
                   (ProteusDrawPin GNDNetName Metal2LPP ProteusPinGNDW x:y0 x:y1)
                   )
             )
    (foreach i ProteusPinVddX
             x = i*DefaultWiringPitchX
             (when x>x0 && x<x1
                   (ProteusDrawPin VddNetName Metal2LPP ProteusPinVddW x:y0 x:y1)
                   )
             )
    )
  t
  )

; delete labels
(defun DeleteLabels (@key (CV nil))
  (cond (!CV CV = (geGetEditCellView)))
  (foreach obj CV->shapes
           (cond (obj->objType=="label" (dbDeleteObject obj)))
           )
  t
  )

; redraw power grid pins (used to patch final layout)
(defun ProteusRedrawPowerPins (@key (ProteusType ProteusType) (view ProteusLayoutViewName))
  (let (filename cells bbox)
    filename = (sprintf nil "%s.gates" ProteusType)
    cells = (OpenCellViewsFromFile filename view "a")
    (foreach CV (setof cell cells cell->mode=="a")
             ; open cell
             (printf "%s\n" CV->cellName)
             (geOpen ?lib CV->libName ?cell CV->cellName
                     ?view view ?viewType "maskLayout" ?mode "a")

             ; get prBoundary
             (unless CV->prBoundary (error "no prBoundary"))
             bbox = CV->prBoundary->bBox

             ; delete old power pins
             (foreach obj CV->shapes
                      (cond ((and obj->pin
                                  (or obj->pin->net->name == VddNetName
                                      obj->pin->net->name == GNDNetName))
                             (dbDeleteObject obj))
                            )
                      )

             ; draw power pins
             (ProteusDrawPowerPins bbox)

             ; regenerate labels
             (DeleteLabels ?CV CV)
             (LabelPins ?CV CV)

             ; close cell
             (dbSave CV CV->libName CV->cellName CV->viewName)
             (leCloseWindow)
             )
    )
  t
  )

; autoplace any subcells
(defun ProteusAutoPlace (@key (ProteusType ProteusType))
  (let (filename cells type)
    filename = (sprintf nil "%s.gates" ProteusType )
    cells = (OpenCellViewsFromFile filename "floorplan" "a")
    (foreach CV cells
             ; open cell
             type = CV->cellName

             ; autoplace any subcells at origin
             (foreach obj CV->instances
                      (cond (obj->name == "reg"    obj->xy=0:0  obj->orient="MX")
                            (obj->name == "reg[0]" obj->xy=0:0  obj->orient="MX")
                            (obj->name == "reg[1]" obj->xy=2*PowerGridPitch:0 obj->orient="MX")
                            (t                     obj->xy=0:0  obj->orient="R0")
                            )
                      )

             ; close cell
             (dbSave CV CV->libName CV->cellName CV->viewName)
             )
    )
  )

; place pins for Proteus logic cells
(defun ProteusPins (@key (ProteusType ProteusType))
  (let (filename cells type
        bbox xy xy0 xy1 x0 y0 x1 y1
        inputs rails bits qdi
        logic edff send recv from to buf tie inv op cfg tok scan reset)
    filename = (sprintf nil "%s.gates" ProteusType )
    cells = (OpenCellViewsFromFile filename ProteusLayoutViewName "a")
    (foreach CV (setof cell cells cell->mode=="a")
             ; open cell
             type = CV->cellName
             (printf "%s\n" type )
             (geOpen ?lib CV->libName ?cell CV->cellName
                     ?view ProteusLayoutViewName ?viewType "maskLayout" ?mode "a")

             ; save bounding box
             bbox = CV->bBox
             xy0 = (car bbox)
             xy1 = (cadr bbox)
             x0 = (car xy0)
             y0 = (cadr xy0)
             x1 = (car xy1)
             y1 = (cadr xy1)
             x0 = x0 * ProteusScaleX
             x1 = x1 * ProteusScaleX
             y0 = y0 * ProteusScaleY
             y1 = y1 * ProteusScaleY
             y0 = DefaultWiringPitch * floor(y0/DefaultWiringPitch)
             y1 = DefaultWiringPitch * ceiling(y1/DefaultWiringPitch)
             bbox = (list x0:y0 x1:y1)
             xy0 = (car bbox)
             xy1 = (cadr bbox)
             x0 = (car xy0)
             y0 = (cadr xy0)
             x1 = (car xy1)
             y1 = (cadr xy1)

             ; delete old pins and shapes
             (when CV->prBoundary       (dbDeleteObject CV->prBoundary))
             (foreach obj CV->shapes    (dbDeleteObject obj))
             (foreach obj CV->nets      (dbDeleteObject obj))
             (foreach obj CV->terminals (dbDeleteObject obj))

             ; optionally delete subcells
             (when ProteusDeleteSubcells
               (foreach obj CV->instances (dbDeleteObject obj))
               )

             ; classify library type
             qdi = ProteusType=="qdi"

             ; classify cell type
             (rexCompile "logic")  logic = (rexExecute type)
             (rexCompile "EDFF")   edff  = (rexExecute type)
             (rexCompile "SEND_")  send  = (rexExecute type)
             (rexCompile "RECV_")  recv  = (rexExecute type)
             (rexCompile "FROM_")  from  = (rexExecute type)
             (rexCompile "TO_")    to    = (rexExecute type)
             (rexCompile "BUF")    buf   = (rexExecute type)
             (rexCompile "TIE")    tie   = (rexExecute type)
             (rexCompile "INV")    inv   = (rexExecute type)
             (rexCompile "CTREE")  op    = (rexExecute type)
             (rexCompile "NOR")    op    = (or op (rexExecute type))
             (rexCompile "AND")    op    = (or op (rexExecute type))
             (rexCompile "CONFIG") cfg   = (rexExecute type)
             (rexCompile "TOK_")   tok   = (rexExecute type)
             (rexCompile "SCAN_")  scan  = (rexExecute type)
             (rexCompile "RESET_") reset = (rexExecute type)

             ; classify number of inputs
             inputs = 0
             (for i 1 6
                  (rexCompile (sprintf nil "logic%d" i))
                  (cond ((rexExecute type) inputs=i))
                  (rexCompile (sprintf nil "CTREE%d" i))
                  (cond ((rexExecute type) inputs=i))
                  (rexCompile (sprintf nil "NOR%d" i))
                  (cond ((rexExecute type) inputs=i))
                  (rexCompile (sprintf nil "AND%d" i))
                  (cond ((rexExecute type) inputs=i))
                  )

             ; classify 1ofN
             rails = 2
             bits = 1
             (rexCompile "_1of4")
             (cond ((rexExecute type) rails=4 bits=2))

             ; adjust width and height to H/W <= 3/2 aspect ratio
             (unless scan
               (cond (y1/x1>=24 y1=y1/4 x1=4*x1)
                     (y1/x1>=6  y1=y1/2 x1=2*x1)
                     )
               )

             ; adjust height to match wire pitch
             y0 = DefaultWiringPitch * floor(y0/DefaultWiringPitch)
             y1 = DefaultWiringPitch * ceiling(y1/DefaultWiringPitch)
             (when y1<ProteusPinY*DefaultWiringPitch y1=ProteusPinY*DefaultWiringPitch)

             ; redraw prBoundary
             bbox = (list x0:y0 x1:y1)
             (dbCreatePRBoundary CV (list x0:y0 x1:y0 x1:y1 x0:y1 x0:y0))
             (SetLeafProp CV t)
             xy = 0:0 ; initial pin pair location
             xy1 = (cadr bbox)

             ; draw power pins
             (ProteusDrawPowerPins bbox)

             ; LOGIC
             (cond (logic
                    (for i 0 inputs-1
                         j = (mod i 4)
                         xy = (ProteusPinList (list (sprintf nil "A[%d].0" i)
                                                    (sprintf nil "A[%d].1" i)) xy xy1 ?inc 1)
                         )
                    xy = (ProteusPinList (list "X.0" "X.1") xy xy1 ?inc 1)
                    xy = (ProteusPinList (list "en") xy xy1)
                    )
                   )
             
             ; TIE
             (cond (tie xy = (ProteusPin "ZERO" xy xy1)))

             ; INV, INVINV, RESET_INVINV
             (cond (inv xy = (ProteusPinList (list "a" "x") xy xy1)))

             ; CTREE*, RESET_CTREE*, AND*, NOR*
             (cond (op
                    (for i 0 inputs-1
                         xy = (ProteusPin (sprintf nil "a[%d]" i) xy xy1)
                         )
                    xy = (ProteusPin "x" xy xy1)
                    )
                   )

             ; qdi BUF/BUF_1of4/TOK_BUF/SCAN_BUF/SCAN_BUF_1of4
             (cond ((and qdi buf)
                    (for i 0 rails/2-1
                         xy = (ProteusPinList (list (sprintf nil "L.%d" 2*i)
                                                    (sprintf nil "L.%d" 2*i+1)
                                                    (sprintf nil "R.%d" 2*i)
                                                    (sprintf nil "R.%d" 2*i+1)) xy xy1)
                         )
                    xy = (ProteusPinList (list "L.e" "R.e" "_RESET") xy xy1)
                    )
                   )

             ; qdi TOK_EDFF
             (cond ((and qdi edff)
                    xy = (ProteusPinList (list "E.0" "E.1" "E.e" "_RESET"
                                               "L.0" "L.1" "R.0" "R.1" "R.e") xy xy1)
                    )
                   )

             ; qdi RECV/FROM
             (cond ((and qdi (or recv from))
                    (cond (recv
                           xy = (ProteusPinList (list "E.0" "E.1" "E.e") xy xy1)
                           )
                          )
                    xy = (ProteusPinList (list "_RESET" "L.e") xy xy1)
                    (for i 0 rails/2-1
                         xy = (ProteusPinList (list (sprintf nil "L.%d" 2*i)
                                                    (sprintf nil "L.%d" 2*i+1)) xy xy1)
                         )
                    (for i 0 bits-1
                         xy = (ProteusPinList (list (sprintf nil "R[%d].0" i)
                                                    (sprintf nil "R[%d].1" i)
                                                    (sprintf nil "R[%d].e" i)) xy xy1)
                         )
                    )
                   )

             ; qdi SEND/TO
             (cond ((and qdi (or send to))
                    (cond (send
                           xy = (ProteusPinList (list "E.0" "E.1" "E.e" "_RESET") xy xy1)
                           )
                          (to
                           xy = (ProteusPinList (list "L[0].e" "_RESET") xy xy1)
                           )
                          )
                    (for i 0 bits-1
                         xy = (ProteusPinList (list (sprintf nil "L[%d].0" i)
                                                    (sprintf nil "L[%d].1" i)) xy xy1)
                         )
                    (for i 0 rails/2-1
                         xy = (ProteusPinList (list (sprintf nil "R.%d" 2*i)
                                                    (sprintf nil "R.%d" 2*i+1)) xy xy1)
                         )
                    xy = (ProteusPin "R.e" xy xy1)
                    )
                   )


             ; qdi SCAN_CONFIG
             (cond ((and qdi scan cfg)
                    xy = (ProteusPinList (list "LC" "RC" "LS.0" "LS.1" "RS.0" "RS.1"
                                               "LS.e" "RS.e" "_RESET") xy xy1 ?inc 1)
                    )
                   )

             ; qdi SCAN_* additional pins
             (cond ((and qdi scan (not cfg))
                    xy = (ProteusPinList (list "C[0]" "C[1]" "C[2]" "C[3]" "LS.0" "LS.1"
                                               "RS.0" "RS.1" "LS.e" "RS.e") xy xy1)
                    )
                   )

             ; add _RESET pins to RESET_CTREE2, RESET_INVINV
             (cond ((and qdi reset)
                    xy = (ProteusPin "_RESET" xy xy1)
                    )
                   )

             ; check for overhang of bbox
             (cond ((or (car (car bbox))!=(car (car CV->bBox))
                        (car (cadr bbox))!=(car (cadr CV->bBox))
                        (cadr (car bbox))!=(cadr (car CV->bBox))
                        (cadr (cadr bbox))!=(cadr (cadr CV->bBox)))
                    (printf "WARNING: bBox is larger than prBoundary\n")
                    )
                   )

             ; close cell
             (dbSave CV CV->libName CV->cellName CV->viewName)
             (leCloseWindow)
             )
    )
  t
  )

; generate ProteusType.gates file
(defun GenerateGatesFile (@key (ProteusType ProteusType) (Strengths ProteusStrengths))
  (let (outFileName pout)
    outFileName = (sprintf nil "%s.gates" ProteusType)
    pout = (outfile outFileName)
    (foreach strength Strengths
      CV = (dbOpenCellViewByType
            (sprintf nil "synthesis.%s.sizing" ProteusType)
            (sprintf nil "synthesis.%s.sizing.SIZE-L%d-R.%d"
                     ProteusType strength strength)
            "floorplan" nil "a")
      (foreach inst CV->instances
               (fprintf pout (sprintf nil "%s\n" inst->master->cellName))
               )
      )
    (close pout)
    )
  t
  )

; copy floorplan to layout views
(defun ProteusLayoutViews (@key (ProteusType ProteusType) (Strengths ProteusStrengths))
  (let (master)
    (foreach strength Strengths
      CV = (dbOpenCellViewByType
            (sprintf nil "synthesis.%s.sizing" ProteusType)
            (sprintf nil "synthesis.%s.sizing.SIZE-L%d-R.%d"
                     ProteusType strength strength)
            "floorplan" "maskLayout" "r")
      (dbSave CV CV->libName CV->cellName ProteusLayoutViewName)
      CV = (dbOpenCellViewByType
            (sprintf nil "synthesis.%s.sizing" ProteusType)
            (sprintf nil "synthesis.%s.sizing.SIZE-L%d-R.%d"
                     ProteusType strength strength)
            ProteusLayoutViewName "maskLayout" "a")
      (foreach inst CV->instances
               (cond (inst->viewName=="floorplan"
                      master = inst->master
                      (when master
                        (dbSave master master->libName master->cellName ProteusLayoutViewName)
                        master = (dbOpenCellViewByType inst->master->lib
                                                       inst->master->cellName
                                                       ProteusLayoutViewName
                                                       "maskLayout" "r")
                        )
                      (cond (master inst->master=master)
                            (t (dbDeleteObject inst))
                            )
                      )
                     )
               )
      (dbSave CV CV->libName CV->cellName CV->viewName)
      )
    )
  t
  )

; write type.finished.gates for layout views with vias
(defun ProteusListFinishedCells (@key (ProteusType ProteusType))
  (let (filename cells out n)
    n=0
    filename = (sprintf nil "%s.gates" ProteusType)
    cells = (OpenCellViewsFromFile filename ProteusLayoutViewName "r")
    out = (outfile (sprintf nil "%s.finished.gates" ProteusType))
    (foreach CV cells
             (when CV->vias
               n++
               (fprintf out "%s\n" CV->cellName))
             )
    (close out)
    n
    )
  )
