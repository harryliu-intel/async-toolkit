; Replacement for ui/Nano/Prelayout/CreatePrelayout.il
;
; Refers to nrCastQuery, nrIsRoutedCell, NameFilterInstances,
; CreateInstWithAlignmentProps, SetCellAlignment, CheckRoutedAlignment

; Top-level function to make a prelayout view and switch to it
(defun MakePrelayout
  (@key (CV (geGetEditCellView)) ; specify CellView or nil to use edit view
        (RegenerateFromCast nil) ; force CAST query
        (Overwrite t)            ; overwrite existing editable prelayout views
        (SyncToNetlist t)        ; sync-to-netlist on prelayout view
        (Verbose nil)            ; verbose
        )
  (let (routedDirectiveList LibCellsToIgnore win)
    ; check viewName
    (when CV->viewName!="floorplan" (error "Call MakePrelayout on floorplan view\n"))

    ; save floorplan view
    (dbSave CV)

    ; query routed directives
    routedDirectiveList = (nrCastQuery CV ?RegenerateFromCast RegenerateFromCast)

    ; construct list of cellName regexp's to skip
    LibCellsToIgnore =
      (append WiringCellLibCellPairRegExs
              (append TechLibCellPairRegExs
                      (append GateLibCellPairRegExs StackLibCellPairRegExs)))

    ; recurse
    CV = (MakePrelayoutRecurse CV
                               ?RegenerateFromCast RegenerateFromCast
                               ?Overwrite Overwrite
                               ?SyncToNetlist SyncToNetlist
                               ?Verbose Verbose
                               ?routedDirectiveList routedDirectiveList
                               ?LibCellsToIgnore LibCellsToIgnore
                               ?top t)

    ; add buswires instance
    (AddBusWires ?CV CV)

    ; switch current window
    win = (hiGetCurrentWindow)
    (when win
      (geOpen ?window win
              ?lib  CV->libName
              ?cell CV->cellName
              ?view CV->viewName
              ?viewType "maskLayout"
              ?mode "r")

      ; draw bus wires (specifically keepout)
      (LoadWires ?CV CV)
      (RefreshAllBuswires ?CV CV)
      )

    ; set nonleaf property
    (SetLeafProp CV nil)

    ; convert prbound
    (ConvertPrboundToObject ?CV CV)

    ; return prelayout view
    (dbSave CV)
    CV
    )
  )

; add buswires if it exists
(defun AddBusWires (@key (CV (geGetEditCellView)) (name "buswires"))
  (let (typeName buswires busCV)
    typeName = (cadr (reverse (parseString CV->cellName ".")))
    buswires = (strcat CV->libName ".wires." typeName "_" name)
    busCV = (dbOpenCellViewByType CV->libName buswires "layout" "maskLayout" "r")
    (when (and busCV (dbFindAnyInstByName CV name)==nil)
      (dbCreateInst CV busCV name 0:0 "R0")
      )
    )
  t
  )

; recursively create prelayout views
(defun MakePrelayoutRecurse (CV @key
                                (mapToPrelayout (makeTable "mapToPrelayout" nil))
                                (RegenerateFromCast t) (Overwrite t)
                                (SyncToNetlist t) (Verbose nil)
                                (routedDirectiveList nil) (LibCellsToIgnore nil)
                                (top nil))
  (let (prelayoutCV instances instName transform instCV)

   ; terminate on routed=true cells or recurse
   (cond (mapToPrelayout[CV] ; already created
          prelayoutCV = mapToPrelayout[CV]
          )
         (!top && (nrIsRoutedCell CV->libName CV->cellName CV->viewName
                                  ?routedDirectiveList routedDirectiveList
                                  ?Verbose Verbose
                                  ?RegenerateFromCast RegenerateFromCast)
          ; return CV for layout, prelayout, or floorplan in that order
          prelayoutCV = (dbOpenCellViewByType CV->libName CV->cellName
                         "layout"    "maskLayout" "r") ||
                        (dbOpenCellViewByType CV->libName CV->cellName
                         "prelayout" "maskLayout" "r") ||
                        (dbOpenCellViewByType CV->libName CV->cellName
                         "floorplan" "maskLayout" "r")
          )
         (t
          ; create new prelayout view
          prelayoutCV = (betterCopyCellView CV CV->libName CV->cellName
                                        "prelayout" nil nil Overwrite)
          (cond (prelayoutCV
                 (DeletePins ?CV prelayoutCV)
                 (CopyAlignmentProps CV prelayoutCV)
                 ; flatten mosaics before other processing (HACK?)
                 (foreach inst (setof x prelayoutCV->instances x->objType=="mosaic")
                          (dbFlattenInst inst 1)
                          )
                 instances = (setof inst prelayoutCV->instances inst->libName!=TechLibName)
                 instances = (car (NameFilterInstances instances LibCellsToIgnore))
                 ; recurse on each instance
                 (foreach inst (setof x instances x->viewName!="layout")
                          instName  = inst->name
                          transform = inst->transform
                          instCV = (MakePrelayoutRecurse
                                    inst->master->cellView
                                    ?mapToPrelayout mapToPrelayout
                                    ?routedDirectiveList routedDirectiveList
                                    ?Verbose Verbose
                                    ?RegenerateFromCast RegenerateFromCast
                                    ?Overwrite Overwrite
                                    ?LibCellsToIgnore LibCellsToIgnore
                                    ?SyncToNetlist SyncToNetlist)
                          inst->master = instCV
                          (SetInstAlignmentProps inst)
                          )
                 (when SyncToNetlist (UpdateFloorplan ?CV prelayoutCV))
                 (dbSave prelayoutCV)
                 (printf "Created %s(%s)...\n"
                         prelayoutCV->cellName prelayoutCV->viewName)
                 )
                (t
                 ; unwritable prelayout view
                 prelayoutCV = (dbOpenCellViewByType CV->libName CV->cellName
                                                     "prelayout" "maskLayout" "r")
                 )
                )

          ; check alignments
          (SetCellAlignment prelayoutCV)
          (when (CheckRoutedAlignment prelayoutCV)
            (printf "WARNING: Instances misaligned in %s(%s)\n"
                    prelayoutCV->cellName prelayoutCV->viewName)
            )
          )
         )

   ; save mapping and return prelayoutCV
   mapToPrelayout[CV] = prelayoutCV
   prelayoutCV
   )
  )
