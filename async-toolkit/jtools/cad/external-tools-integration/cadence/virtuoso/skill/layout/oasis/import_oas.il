; Simpler flow to import GDS to DFII.  Streams in GDS to a temporary
; library, copies all cells to the desired destination libName,
; appends a prefix and suffix to create CAST-style cellName's, and
; updates the instance masters.  Then applies a user-defined
; postprocessing function to all cells, such as to rename instances
; and nodes.

; Import a OAS file and rename all subcells to CAST conventions
(defun ImportOAS (oasFileName libName cellName viewName
       		 @key (CV (geGetEditCellView))
                      (layerMapFile nil)
                      (propMapFile nil)
                      (enableColoring t)
                      (setCopyExact nil) ; lamda on CV to decide when to set CopyExact property
                      )
  (let (PDK_DIR cmd status CV)
    ; stream in OAS into import_oas_temp library
    PDK_DIR = (ConfigFileGetValue TheCDSConfigTable "FULCRUM_PDK_ROOT")
    (unless layerMapFile (sprintf layerMapFile "%s/share/Fulcrum/stream/strmout.layermap" PDK_DIR))
    (unless propMapFile (sprintf propMapFile "%s/share/Fulcrum/stream/oasis_prop.map" PDK_DIR))

    cmd = (strcat "oasisin "
                  "-library \"" libName "\" "
		  "-topCell \"" cellName "\" "
                  "-view \"" viewName "\" "
                  "-oasisFile \"" oasFileName "\" "
                  "-logFile \"oasisIn.log\" "
                  "-attachTechFileOfLib \"" TechLibName "\" "
                  "-layerMap \"" layerMapFile "\" "
                  "-propMap \"" propMapFile "\" "
                  "-objectMap \"" PDK_DIR "/share/Fulcrum/stream/objectmap\" "
		  "-textHeight \"0.01\" "
	          (if enableColoring " -enableColoring" "")
		  )
    status = (shell cmd)
    (unless status (error "strmin failed\n"))
    (ddUpdateLibList)

    ; open imported top CV
    CV = (dbOpenCellViewByType libName cellName viewName)
    (unless CV (error "unable to open %s %s %s\n" libName cellName viewName))

    ; set CopyExact property
    (when setCopyExact
      (FindSubcells ?CV CV ?filter setCopyExact
                    ?action (lambda (CV) (dbReplaceProp CV "CopyExact" "string" CV->cellName) (dbSave CV)))
      )

    ; more processing of top level cell
    (dbReplaceProp CV "CastCell" "boolean" t) ; top-level only
    (LabelsToPins ?CV CV) ; create pins by finding labels on "pin" purpose rectangles
    (DeleteLabels ?CV CV ?keepLPPs (list NWellLPP))
    (LabelPins ?CV CV)
    (dbSave CV)

    ; return top-level CV
    CV
    )
  )

(defun ImportOASLib (oasFileName libName)
  (let (PDK_DIR cmd status CV layerMapFile propMapFile objMapFile)
    PDK_DIR = (ConfigFileGetValue TheCDSConfigTable "FULCRUM_PDK_ROOT")
    (unless layerMapFile (sprintf layerMapFile "%s/share/Fulcrum/stream/strmout.layermap" PDK_DIR))
    (unless propMapFile (sprintf propMapFile "%s/share/Fulcrum/stream/oasis_prop.map" PDK_DIR))
    (unless objMapFile (sprintf objMapFile "%s/share/Fulcrum/stream/objectmap" PDK_DIR))

    cmd = (strcat "oasisin "
                 "-library \"" libName "\" "
                  "-oasisFile \"" oasFileName "\" "
                  "-logFile \"oasisIn.log\" "
                  "-attachTechFileOfLib \"" TechLibName "\" "
                  "-layerMap \"" layerMapFile "\" "
                  "-propMap \"" propMapFile "\" "
                  "-objectMap \"" objMapFile "\" "
		  "-textHeight \"0.01\" "
		  "-enableColoring"
		  )

    status = (shell cmd)
    (unless status (error "strmin failed\n"))
    (ddUpdateLibList)
    )
  )

