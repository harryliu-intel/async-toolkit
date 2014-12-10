; write the standard part of an Assura rule file
(defun WriteAssuraRuleFileHeader (file)
  (fprintf file "(drcExtractRules\n")

  ; define layers
  (fprintf file "(layerDefs \"df2\"\n")
  (for n 1 8
       (fprintf file "m%d = (layer \"METAL%d\" (type \"drawing\" \"pin\"))\n"
                n n)
       )
  (for n 1 7
       (fprintf file "via%d = (layer \"VIA%d%d\" (type \"drawing\"))\n"
                n n n+1)
       )
  (for n 1 8
       (fprintf file "m%d_text = (textToPin \"METAL%d\" (type \"drawing\" \"pin\"))\n"
                n n)
       )
  (fprintf file ")\n")

  ; turn individual via shapes into via regions
  (for Num 1 7
       (fprintf out "via%d = (geomOverlap (geomAnd m%d m%d) via%d)\n"
                Num Num Num+1 Num)
       )
  
  ; define connectivity
  (fprintf file "(geomConnect\n")
  (for n 1 7
       (fprintf file "(via via%d m%d m%d)\n" n n+1 n)
       )
  (for n 1 8
       (fprintf file "(label m%d_text m%d)\n" n n)
       )
  (fprintf file ")\n")
  t
  )

; write footer
(defun WriteAssuraRuleFileFooter (file)
  (for n 1 8 (fprintf file "(keepLayer m%dkeep)\n" n))
  (for n 1 7 (fprintf file "(keepLayer via%dkeep)\n" n))
  (fprintf file ")\n")
  t
  )

; Use Assura DRC to create very detailed abstract pins.  Then launch
; Virtuoso XL to propagate net connectivity onto the attached shapes.
(defun CreateAbstractDetailedView (@key (CellView nil)
                                        (targetView "abstract_detailed"))
  (let (libName cellName in out line layers
                AssuraLayerMappings AssuraRuleFile AssuraRunLog
                ErrorStr TempDir window dstView)
    (cond (CellView==nil CellView = (geGetEditCellView)))
    libName  = CellView->libName
    cellName = CellView->cellName

    ; create layer mappings for rsf file
    AssuraLayerMappings = nil
    layers = nil
    (for Num 1 8
         layers = (cons (sprintf nil "m%d" Num) layers)
         from = (sprintf nil "m%dkeep" Num)
         to = (sprintf nil "METAL%d" Num)
         AssuraLayerMappings = (cons 
                                (list from (list to "drawing"))
                                AssuraLayerMappings
                                )
         )
    (for Num 1 7
         layers = (cons (sprintf nil "via%d" Num) layers)
         from = (sprintf nil "via%dkeep" Num)
         to = (sprintf nil "VIA%d%d" Num Num+1)
         AssuraLayerMappings = (cons 
                                (list from (list to "drawing"))
                                AssuraLayerMappings
                                )
         )

    ; create a modified rul file for this cell
    TempDir = (ConfigFileGetValue TheCDSConfigTable "TEMP")
    AssuraRuleFile = (sprintf nil "%s/%s.%s.rul" TempDir CellView->cellName targetView)
    out = (outfile AssuraRuleFile)
    (WriteAssuraRuleFileHeader out)
    (foreach layer layers
             (fprintf out "%skeep = (geomAndNot (geomGetNet %s \"*\")\n" layer layer)
             (fprintf out " (geomCat (geomGetNet %s \"Vdd*\")\n" layer)
             (fprintf out " (geomGetNet %s \"GND*\")))\n" layer)
             )
    (WriteAssuraRuleFileFooter out)
    (close out)

    ; run assura
    AssuraRunLog = (sprintf  nil "%s/%s.%s.log" TempDir CellView->cellName targetView)
    ErrorStr = (AssuraRunAssuraLayerProcessor 
                CellView
                CellView->libName
                CellView->cellName
                targetView
                AssuraRuleFile
                TempDir
                AssuraLayerMappings
                nil
                ?AssuraRunLog AssuraRunLog
                ?LeaveMess nil
                )
    
    ; open destination cell for edit
    window = (geOpen ?lib libName ?cell cellName ?view targetView ?mode "a")
    dstView = (geGetWindowCellView window)

    ; copy pins to target view
    (foreach shape CellView->shapes
             (cond (shape->pin!=nil
                    fig = (dbCopyFig shape dstView)
                    (dbCreatePin (dbMakeNet dstView shape->net->name) fig)
                    )
                   )
             )

    ; create prBoundary
    (dbCreateRect dstView (list "prBoundary" "boundary") CellView->bBox)
    )
  )

; delete any shapes that didn't get stamped by VXL, except prBoundary
(defun DeleteUnconnectedShapes (@key (view nil))
  (cond (view==nil view = (geGetEditCellView)))
  (foreach shape view->shapes
           (cond ((and shape->net==nil shape->layerName!="prBoundary")
                  (dbDeleteObject shape)
                  )
                 )
           )
  t
  )

; Convert metal shapes with connectivity into "net" pins, delete
; everything else.  Run through VXL first to propagate connectivity.
(defun FinishAbstractDetailedView (@key (view nil))
  (let (pin)
    (cond (view==nil view = (geGetEditCellView)))

    ; delete unconnected shapes and all instances
    (DeleteUnconnectedShapes ?view view)
    (foreach inst view->instances (dbDeleteObject inst))
    
    ; convert remaining metal shapes to net pins
    (foreach shape view->shapes
             (cond (shape->layerName=="prBoundary" t)
                   ((and (isMetal shape->layerName)
                         (or shape->purpose=="drawing" shape->purpose=="pin")
                         shape->net!=nil)
                    pin = (dbCreatePin shape->net shape)
                    pin->accessDir = (list "left" "right" "top" "bottom")
                    pin->term->direction = "inputOutput"
                    shape->purpose="net"
                    )
                   (t (dbDeleteObject shape))
                   )
             )

    ; delete unused nets
    (foreach net view->nets
             (cond ((and net->term!=nil net->term->pins==nil)
                    (dbDeleteObject net))
                   )
             )

    ; set necessary properties
    (dbReplacePropList view (list 
                             (list "prCellClass" "string" "block")
                             (list "prCellType" "string" "macro")
                             (list "maskLayoutSubType" "string" "abstract")
                             (list "symmetry" "string" "X Y")
                             (list "gec3Foreign" "string" 
                                   (sprintf nil "%s %f %f" view->cellName
                                            (leftEdge view->bBox) (bottomEdge view->bBox))
                                   )
                             )
                       )
    
    ; save
    (dbSave view)
    )
  t
  )
