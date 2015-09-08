; Simpler flow to import GDS to DFII.  Streams in GDS to a temporary
; library, copies all cells to the desired destination libName,
; appends a prefix and suffix to create CAST-style cellName's, and
; updates the instance masters.  Then applies a user-defined
; postprocessing function to all cells, such as to rename instances
; and nodes.

; Import a GDS file and rename all subcells to CAST conventions
(defun ImportGDS (gdsFileName libName cellName
                              @key
                              (prefix (strcat libName "."))
                              (suffix ".0")
                              (action importPostProcessAction)
                              )
  (let (PDK_DIR cmd status CV)
    ; stream in GDS into import_gds_temp library
    PDK_DIR = (ConfigFileGetValue TheCDSConfigTable "FULCRUM_PDK_ROOT")
    cmd = (strcat "strmin \\\n"
                  "-attachTechFileOfLib \"" TechLibName "\" \\\n"
                  "-layerMap \"" PDK_DIR "/share/Fulcrum/stream/strmin.layermap\" \\\n"
                  "-objectMap \"" PDK_DIR "/share/Fulcrum/stream/objectmap\" \\\n"
                  "-library \"import_gds_temp\" \\\n"
                  "-strmFile \"" gdsFileName "\"")
    status = (shell cmd)
    (unless status (error "strmin failed\n"))
    (ddUpdateLibList)

    ; create destination library if necessary
    (UpdateFloorplanCreateLib libName)

    ; copy cells to destination library
    CV = (dbOpenCellViewByType "import_gds_temp" cellName "layout")
    (unless CV (error "unable to open import_gds_temp %s layout\n" cellName))
    (CopyToNewLib libName ?CV CV ?prefix prefix ?suffix suffix)
    cellName = (sprintf nil "%s%s%s" prefix cellName suffix)

    ; delete import_gds_temp library
    (ddDeleteObj (ddGetObj "import_gds_temp"))
    (shell "rm -rf import_gds_temp")

    ; FindSubcells to recursively rename stuff
    CV = (dbOpenCellViewByType libName cellName "layout" nil "a")
    (when action (FindSubcells ?action action ?CV CV))

    ; return top-level CV
    (dbReplaceProp CV "CastCell" "boolean" t) ; top-level only
    CV
    )
  )

; FindSubcells postprocessing action suitable for WPRD SRAM's
importPostProcessAction =
  (lambda (CV)
    (let (text pcre1 pcre2 pcre3 pcre4)
      pcre1 = (pcreCompile "<")
      pcre2 = (pcreCompile ">")
      pcre3 = (pcreCompile "/")
      pcre4 = (pcreCompile "^M.ROUT$") ; delete these from SRAM
      (foreach label (setof x CV->shapes x->objType=="label")
               text = label->theLabel
               (cond ((pcreExecute pcre4 text) (dbDeleteObject label))
                     (t 
                      text = (car (parseString text ":"))   ; delete :* suffix
                      text = (pcreReplace pcre1 text "[" 0) ; replace < with [
                      text = (pcreReplace pcre2 text "]" 0) ; replace > with ]
                      text = (pcreReplace pcre3 text "." 0) ; replace / with .
                      label->theLabel = text
                      label->height = 0.02
                      label->justify = "centerCenter"
                      )
                     )
               )
      (foreach inst CV->instances
               text = (pcreReplace pcre3 inst->name "." 0) ; replace / with .
               (when text!=inst->name inst->name=text)
               )
      (when (setof x CV->shapes x->layerName=="ndiff")!=nil
            (LaygenLabelPwell ?CV CV ?netName "vss")
            )
      )
    t
    )

; Recursively change libName of subcells.  Recurses only to instances
; with the same libName as the parent to avoid renaming technology
; cells like vias.  Adds optional prefix and suffix to cellNames.
(defun CopyToNewLib (libName @key (CV (geGetEditCellView))
                             (visited (makeTable "visited" nil))
                             (prefix "")
                             (suffix "")
                             )
  (let (newCV)
    newCV = visited[CV]
    (unless newCV
      (printf "CopyToNewLib: %s %s %s\n" CV->libName CV->cellName CV->viewName)
      newCV = (betterCopyCellView CV libName (strcat prefix CV->cellName suffix)
                                  CV->viewName nil nil t)
      (when newCV
        visited[CV] = newCV
        (foreach inst (setof x newCV->instances x->libName==CV->libName)
                 inst->master = (CopyToNewLib libName
                                              ?CV inst->master ?visited visited
                                              ?prefix prefix
                                              ?suffix suffix)
                 )
        (dbSave newCV)
        )
      )
    newCV
    )
  )
