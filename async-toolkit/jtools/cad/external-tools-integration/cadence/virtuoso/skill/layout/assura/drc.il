; Batch and user friendly wrapper to run DRC.  By default runs full
; DRC rule deck.  Or change the ?rules open to "m2-8", "frc",
; "proteus_frc", "fillCheck", "keepoutCheck", "powerCheck",
; "wellnotch" to run smaller decks.

; Run DRC
(defun AssuraDRC
  (@key (CV (geGetEditCellView)) ; specify CellView or nil to use edit view
        (rules nil) ; string to pick rule deck, or nil for all default rules
        (dir "drc") ; run directory
        (doDensity nil) ; do density checks
        )
  (let (TEMP PDKRoot AssuraRuleFile AssuraSets status)
    TEMP = (ConfigFileGetValue TheCDSConfigTable "TEMP")
    PDKRoot = (ConfigFileGetValue TheCDSConfigTable "FULCRUM_PDK_ROOT")
    AssuraRuleFile = (if rules (sprintf nil "%s/share/Fulcrum/drc/%s.rul" PDKRoot rules)
                       (sprintf nil "%s/share/Fulcrum/assura/drc.rul" PDKRoot))
    (when !(isFile AssuraRuleFile) (error "Can't read %s\n" AssuraRuleFile))
    AssuraSets = nil
    (unless doDensity AssuraSets = (cons "SKIP_DENSITY" AssuraSets))
    (dbSave CV)
    status = (AssuraRunDRC CV AssuraRuleFile AssuraSets TEMP dir)
    (printf "DRC: %s\n" (if status "PASS" "FAIL"))
    status
    )
  )

; Shortcut for FRC
(defun AssuraFRC
  (@key (CV (geGetEditCellView)))
  (AssuraDRC ?CV CV ?rules "frc" ?dir "frc")
  )

; Run DRC on a scratch cell that overlays an abstract view
(defun AssuraCheckAbstract
  (@key (CV (geGetEditCellView))                  ; specify CellView
        (views (list "abstract_edit" "abstract")) ; overlay views, use first one found
        (rules "abstractCheck")                   ; string to pick rule deck
        (dir "drc")                               ; run directory
        )
  (let (scratchCV overlayCV status)

    ; copy CV to scratch_overlay view
    (when CV->viewName=="scratch_overlay" (error "Can't run from scratch_overlay view\n"))
    scratchCV = (betterCopyCellView CV CV->libName CV->cellName "scratch_overlay" nil nil t)
    (unless scratchCV (error "Can't write scratch %s %s\n" CV->cellName CV->viewName))

    ; copy shapes from abstract view
    (foreach view views
             (unless overlayCV
               overlayCV = (nrOpenCellViewReadable CV->libName CV->cellName view)
               )
             )
    (unless overlayCV (error "Can't read %s %s overlay view\n" CV->libName CV->cellName))
    (foreach shape overlayCV->shapes
             (dbCopyFig shape scratchCV (list 0:0 "R0"))
             )

    ; add power grid if it exists
    (AddPowerGridInstance ?CV scratchCV)

    ; run Assura
    status = (AssuraDRC ?CV scratchCV ?rules rules ?dir dir)

    ; delete scratch view if it PASS'es
    (when status
      (ddDeleteObj (ddGetObj scratchCV->libName scratchCV->cellName scratchCV->viewName))
      )

    ; return status
    status
    )
  )

; Check abstracts of all routed subcells
(defun AssuraCheckAbstracts
  (@key (CV (geGetEditCellView))                  ; specify CellView
        (views (list "abstract_edit" "abstract")) ; overlay views, use first one found
        (rules "abstractCheck")                   ; string to pick rule deck
        (dir "drc")                               ; run directory
        )
  (let (s (status t))
    (foreach subCV (ListLefSubcells ?CV CV)
             (printf "Checking %s... " subCV->cellName)
             s = (AssuraCheckAbstract ?CV subCV ?views views ?rules rules ?dir dir)
             status = (and status s)
             )
    status
    )
  )
