; Much faster alternative to SyncToNetlist.  Great for supersize
; floorplanning iterations.  Includes net connectivity and pcells in
; layout leaf cells.
;
; Author: a very impatient person
;
; TODO: Place pcells in leaf cells according to better heuristic.

; user interface
(defun UpdateFloorplan
  (@key (CV (geGetEditCellView)) ; provide a CV
        (libName nil) (cellName nil) (viewName "floorplan") ; or lib/cell/view names
        (recurse t)    ; recursively update subcells
        (cast2skill t) ; run cast2skill first
        (verbose nil)  ; spam net and parameter changes too
        )
  (let (mastersVisited cmd temp dir routed LOG pcre)

    ; open CV
    (when cellName
      (unless libName libName = (GetCastLibName cellName))
      CV = (UpdateFloorplanOpenCV libName cellName viewName)
      )
    routed = (CV->viewName!="floorplan" && CV->viewName!="prelayout")

    ; call cast2skill
    temp = (ConfigFileGetValue TheCDSConfigTable "TEMP")
    dir = (sprintf nil "%s/ilnets" temp)
    cmd = (sprintf nil
                   (strcat "%s/cast2skill --max-heap-size=4G --cadence-name "
                           "--suppress-pins --suppress-wiring-directives "
                           "--function-prefix=UpdateFloorplan "
                           "--cast-path=%s --cell=%s --output-dir=%s %s "
                           " &> %s/cast2skill.log")
                   (PackageGetBinRoot)
                   (ConfigFileGetValue TheCDSConfigTable "CAST_PATH")
                   CV->cellName
                   dir
                   (if routed "--routed" "")
                   temp
                   )
    (when cast2skill (shell cmd))

    ; compile regular expression to identify arrays
    pcre = (pcreCompile "\\[[0-9]+\\][^\\[]*$")

    ; update floorplan
    LOG = (outfile (strcat temp "/UpdateFloorplan.log"))
    mastersVisited = (makeTable "mastersVisited" nil)
    (UpdateFloorplanCore CV "")
    (close LOG)
    CV
    )
  )

; process one cell
(defun UpdateFloorplanCore (CV indent)
  (let (file NetListTable rx ux uy w h instanceTable netTable termTable
             lastArrayName layoutCV points realInstances)
    (fprintf LOG "%sUpdate floorplan of %s\n" indent CV->cellName)
    mastersVisited[CV] = t

    ; coordinates for new instances
    rx = (car (cadr (CV->bBox)))
    ux = rx
    uy = 0.0

    ; track correct instances and nets and terms
    instanceTable = (makeTable "instanceTable" nil)
    netTable = (makeTable "netTable" nil)
    termTable = (makeTable "termTable" nil)
    lastArrayName = nil

    ; load netlist.il
    file = (strcat dir "/ilnets/" CV->cellName ".netlist.il")
    (when (fileTimeModified file)
      (dbReplaceProp CV "CastCell" "boolean" t)
      (load file)
      w = NetListTable["width"]
      h = NetListTable["height"]

      ; delete incorrect nets
      (foreach net CV->nets
        (unless netTable[net]
          (when verbose (fprintf LOG "%sDelete net %s\n" indent net->name))
          (dbDeleteObject net)
          )
        )

      ; delete incorrect terminals
      (foreach term CV->terminals
        (unless termTable[term]
          (when verbose (fprintf LOG "%sDelete term %s\n" indent term->name))
          (dbDeleteObject term)
          )
        )

      ; delete incorrect instances
      (foreach inst CV->instances
        (unless instanceTable[inst] || (IsWiringCell inst->master)
                (fprintf LOG "%sDelete instance %s of %s\n" indent inst->name inst->cellName)
                (dbDeleteObject inst)
                )
        )
      )

    ; update prBoundary of leaf cells, from layout view if it exists
    (unless realInstances
      (when CV->prBoundary (dbDeleteObject CV->prBoundary))
      (when CV->viewName=="floorplan" && (ddGetObj CV->libName CV->cellName "layout")
            layoutCV = (dbOpenCellViewByType CV->libName CV->cellName "layout" "maskLayout" "r")
            points = layoutCV->prBoundary->points
            )
      (unless points
        (unless w&&h (error "undetermined height or width"))
        points = (list 0:0 w:0 w:h 0:h 0:0)
        )
      (dbCreatePRBoundary CV points)
      )

    ; save
    (dbSave CV)
    )
  )

; Open a CellView and create lib if necessary
(defun UpdateFloorplanOpenCV (libName cellName viewName)
  (let (lib libDir mode)
    (unless (ddGetObj libName)
      libDir = (strcat (ConfigFileGetValue TheCDSConfigTable "DFII_DIR") "/"
                       (buildString (parseString libName ".") "/"))
      (shell (strcat "mkdir -p " libDir))
      lib = (dbCreateLib libName libDir)
      (techBindTechFile lib TechLibName "techfile.cds" t)
      )
    mode = (if (IsCellViewWritable libName cellName viewName) "a" "r")
    (dbOpenCellViewByType libName cellName viewName "maskLayout" mode)
    )
  )

; Improved function to see if a CV is writable
(defun IsCellViewWritable (libName cellName viewName)
  (let (ddObj)
    ddObj = (ddGetObj libName cellName viewName)
    !ddObj ||                                    ; assume you can create CV if it doesn't exist
    (ddIsObjWritable ddObj) &&                   ; database object must not be locked
    !(setof f ddObj->files !(ddIsObjWritable f)) ; all files must be writable
    )
  )

;;;;;;;;;;;;;;;;; functions called by cast2skill netlist.il files ;;;;;;;;;;;;;;;;;;;;

(defun UpdateFloorplanCreateInstanceInNetlist (table instName libName cellName)
  (let (inst master viewName w h x0 y0 arrayName primitive)
    primitive = libName=="gate" || libName=="stack" || libName=="resistor" || libName==TechLibName
    realInstances = realInstances || !primitive
    (when CV->viewName!="floorplan" || !primitive
          ; find existing inst and its viewName
          inst = (dbFindAnyInstByName CV instName)
          viewName = (if inst inst->viewName CV->viewName)

          ; recurse into master
          master = (dbOpenCellViewByType libName cellName viewName "maskLayout" "r")
          (when !master || recurse && !mastersVisited[master] && master->viewName==CV->viewName
                master = (UpdateFloorplanOpenCV libName cellName CV->viewName)
                (when master->mode=="a" (UpdateFloorplanCore master (strcat indent " ")))
                )
          (unless master
            (error "Unable to create libName=%s cellName=%s viewName=%s\n"
                   libName cellName viewName)
            )
          
          ; add instance
          arrayName = (pcreReplace pcre instName "" 0)
          (unless inst
            w = (car  (cadr master->bBox)) - (car  (car master->bBox))
            h = (cadr (cadr master->bBox)) - (cadr (car master->bBox))
            (cond (lastArrayName==arrayName
                   x0 = ux
                   y0 = uy
                   rx = (max ux+w rx)
                   uy = uy+h)
                  (t
                   x0 = rx
                   y0 = 0
                   ux = rx
                   uy = h
                   rx = rx+w)
                  )
            x0 = x0 - (car  (car master->bBox))
            y0 = y0 - (cadr (car master->bBox))
            (fprintf LOG "%sCreate instance %s of %s\n" indent instName cellName)
            inst = (dbCreateInst CV master instName x0:y0 "R0")
            )
          (unless inst->master==master
            (fprintf LOG "%sChange master of instance %s to %s\n" indent instName cellName)
            inst->master = master
            )
          lastArrayName = arrayName
          instanceTable[inst] = t
          (foreach term inst->terminals (dbDeleteObject term)) ; delete old connections
          )
    )
  )

(defun UpdateFloorplanCreateEmptyNetlistTable (libName cellName)
  (makeTable "NetListTable" nil)
  )

(defun UpdateFloorplanAddTerminal (table termName)
  (let (net term)
    net = (dbMakeNet CV termName)
    term = (dbFindTermByName CV termName)
    (unless term
      term=(dbCreateTerm net termName "inputOutput")
      (when verbose (fprintf LOG "%sCreate term %s\n" indent termName))
      )
    netTable[net] = t
    termTable[term] = t
    )
  )

(defun UpdateFloorplanAddNet (table netName)
  (let (net)
    net = (dbFindNetByName CV netName)
    (unless net
      net = (dbMakeNet CV netName)
      (when verbose (fprintf LOG "%sCreate net %s\n" indent netName))
      )
    netTable[net] = t
    )
  )

(defun UpdateFloorplanAddConnectionToInstanceInNetlist (table instName termName netName)
  (let (inst term net)
    ; too spammy, since I can't figure out how to detect if connection changed
    ;(when verbose
    ;  (fprintf LOG "%sAdding instance term to inst=%s term=%s net=%s\n"
    ;           indent instName termName netName)
    ;  )
    net  = (dbFindNetByName CV netName)
    inst = (dbFindAnyInstByName CV instName)
    (when inst
      term = (dbFindTermByName inst->master termName)
      (when term && net (dbCreateInstTerm net inst term))
      )
    )
  )

 (defun UpdateFloorplanAddParameterToInstanceInNetlist (table instName parmName parmValue)
   (let (inst prop)
    inst = (dbFindAnyInstByName CV instName)
    (when inst
      prop = (dbFindProp inst parmName)
      (unless prop && prop->valueType=="float" && prop->value==parmValue
              (dbReplaceProp inst parmName "float" (StringUtilParseLengthString parmValue))
              (when verbose
                (fprintf LOG "%sSet parameter %s in instance %s to %s\n"
                         indent parmName instName parmValue)
                )
              )
      )
    )
   )

(defun UpdateFloorplanSetTotalTransistorCount    (table count)  nil)
(defun UpdateFloorplanSetTotalTransistorGateArea (table area)   nil)
(defun UpdateFloorplanSetTotalTransistorWidth    (table width)  nil)
(defun UpdateFloorplanSetTotalTransistorLength   (table length) nil)
(defun UpdateFloorplanSetCellWidth  (table width)  table["width"]  = width*1e6)
(defun UpdateFloorplanSetCellHeight (table height) table["height"] = height*1e6)
