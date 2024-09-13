; Get CopyExact or GDS2 cellName
(defun GetCopyExact (@key (CV (geGetEditCellView)))
  (let (CID copyexact)
    copyexact = (dbGetPropByName CV "CopyExact")->value
    (unless copyexact
      CID = (NameStartRename "cell" "cadence" "gds2")
      copyexact = (NameDoRename CID CV->cellName)
      (NameStopRename CID)
      )
    copyexact
    )
  )

; Write cellmap to rename cells.  CopyExact cells get renamed to their
; original name as saved in the CopyExact property.  Other cells get
; GDS names.
(defun WriteCellmap (cellMapFile @key (CV (geGetEditCellView)) (renameTop t) (targetNamespace "gds2"))
  (let (CID fmap topName)
    (unless CV->objType=="cellView" (error "CV not valid"))
    CID = (NameStartRename "cell" "cadence" targetNamespace)
    fmap = (outfile cellMapFile)
    (unless fmap (error "can't write %s\n" cellMapFile))
    gdsName=(RecurseWriteCellmap fmap ?CV CV ?renameTop renameTop)
    (close fmap)
    (NameStopRename CID)
    gdsName
    )
  )

; recursively process all cellView used in this design
(defun RecurseWriteCellmap (fmap @key     (CV (geGetEditCellView))
                                 (renameTop t)
                                 (visited (makeTable "visited" nil))
                                 (used    (makeTable "used" nil)))
  (let (name)
    visited[CV]=t
    name = (if renameTop && (dbGetPropByName CV "CopyExact")
               (dbGetPropByName CV "CopyExact")->value
               (NameDoRename CID CV->cellName)
               )
    (when used[name] name=(strcat name "_" CV->viewName)) ; disambiguate
    (when used[name] (error "duplicate %s %s %s\n" CV->libName CV->cellName CV->viewName))
    (printf "%s\n" name)
    (fprintf fmap "%s %s %s %s\n" CV->libName CV->cellName CV->viewName name)
    used[name]=t
    (foreach inst CV->instHeaders
             (unless visited[inst->master] (RecurseWriteCellmap fmap ?visited visited ?used used ?CV inst->master))
             )
    name
    )
  )
