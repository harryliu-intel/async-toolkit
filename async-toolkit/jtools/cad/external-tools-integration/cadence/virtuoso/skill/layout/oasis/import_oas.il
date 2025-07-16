; Simple flow to import OAS to DFII.  Streams in OSA to a temporary
; library.  Does not include CopyToNewLib like ImportGDS does.  This is usually
; used to import IP collateral which requires extra skill processing steps.

; Import OAS file to a scratch libName/cellName/viewName
(defun ImportOAS (oasFileName ; source *.oas file
                  libName cellName viewName ; destination lib,cell,view
       		  @key (layerMapFile nil) ; use a different layer map from PDK
                       (propMapFile nil)  ; use a different prop map from PDK
                       (objMapFile nil) ; us a different object map from PDK
                       (setCopyExact nil) ; lamda on CV to decide when to set CopyExact property
                  )
  (let (PDK_DIR cmd status CV)
    ; stream in OAS into import_oas_temp library
    PDK_DIR = (ConfigFileGetValue TheCDSConfigTable "FULCRUM_PDK_ROOT")
    (unless layerMapFile (sprintf layerMapFile "%s/share/Fulcrum/stream/strmout.layermap" PDK_DIR))
    (unless propMapFile (sprintf propMapFile "%s/share/Fulcrum/stream/oasis_prop.map" PDK_DIR))
    (unless objMapFile (sprintf objMapFile "%s/share/Fulcrum/stream/objectmap" PDK_DIR))

    cmd = (strcat "oasisin "
                  "-library \"" libName "\" "
		  "-topCell \"" cellName "\" "
                  "-view \"" viewName "\" "
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

; Import all cells in OAS to scratch libName
(defun ImportOASLib (oasFileName libName)
  (let (PDK_DIR cmd status CV layerMapFile propMapFile objMapFile)
    PDK_DIR = (ConfigFileGetValue TheCDSConfigTable "FULCRUM_PDK_ROOT")
    (sprintf layerMapFile "%s/share/Fulcrum/stream/strmout.layermap" PDK_DIR)
    (sprintf propMapFile "%s/share/Fulcrum/stream/oasis_prop.map" PDK_DIR)
    (sprintf objMapFile "%s/share/Fulcrum/stream/objectmap" PDK_DIR)

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
