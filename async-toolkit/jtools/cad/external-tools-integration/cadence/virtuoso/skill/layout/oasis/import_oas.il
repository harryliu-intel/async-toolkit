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
		       (enableColoring t))
  (let (PDK_DIR cmd status CV)
    ; stream in OAS into import_oas_temp library
    PDK_DIR = (ConfigFileGetValue TheCDSConfigTable "FULCRUM_PDK_ROOT")
    (unless layerMapFile (sprintf layerMapFile "%s/share/Fulcrum/stream/strmout.layermap" PDK_DIR))

    cmd = (strcat "oasisin "
                  "-library \"" libName "\" "
		  "-topCell \"" cellName "\" "
                  "-view \"" viewName "\" "
                  "-oasisFile \"" oasFileName "\" "
                  "-logFile \"oasisIn.log\" "
                  "-attachTechFileOfLib \"" TechLibName "\" "
                  "-layerMap \"" layerMapFile "\" "
                  "-objectMap \"" PDK_DIR "/share/Fulcrum/stream/objectmap\" "
		  "-textHeight \"0.01\" "
	          (if enableColoring " -enableColoring" "")
		  )
    status = (shell cmd)
    (unless status (error "strmin failed\n"))
    (ddUpdateLibList)

    ; open imported top CV
    CV = (dbOpenCellViewByType libName cellName viewName)
    (unless CV (error "unable to open import_oas_temp %s layout\n" cellName))

    ; return top-level CV
    CV
    )
  )

(defun ImportOASLib (oasFileName libName)
  (let (PDK_DIR cmd status CV)
    PDK_DIR = (ConfigFileGetValue TheCDSConfigTable "FULCRUM_PDK_ROOT")
    cmd = (strcat "oasisin "
                  "-library \"" libName "\" "
                  "-oasisFile \"" oasFileName "\" "
                  "-logFile \"oasisIn.log\" "
                  "-attachTechFileOfLib \"" TechLibName "\" "
                  "-layerMap \"" PDK_DIR "/share/Fulcrum/stream/strmin.layermap\" "
                  "-objectMap \"" PDK_DIR "/share/Fulcrum/stream/objectmap\" "
		  "-textHeight \"0.01\" "
		  "-enableColoring"
		  )
    status = (shell cmd)
    (unless status (error "strmin failed\n"))
    (ddUpdateLibList)
    )
  )

