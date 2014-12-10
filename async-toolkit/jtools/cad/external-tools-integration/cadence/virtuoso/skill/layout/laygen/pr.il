; Export, place, route, import leaf cell with Laygen
; Copyright Intel 2013
; Andrew Lines

; laygen configuration
LaygenExec = "$LAYGENWARD/bin/sgrlfg"
LaygenRpg  = "$LAYGENWARD/collateral/d04/d04_rpg.txt"
LaygenArch = "$LAYGENWARD/collateral/d04/d04.laygen"
LaygenTcl  = "$LAYGENWARD/collateral/d04/d04_container.tcl"
(sprintf LaygenCmd (strcat
                    "%s \\\n"
                    "-rpg_file %s \\\n"
                    "-arch_file %s \\\n"
                    "-tcl_container_file %s \\\n"
                    "-fabric d04 \\\n"
                    "-upperlayer m2 \\\n"
                    "-placement_effort 3 \\\n"
                    "-quality_effort 2 \\\n"
                    )
         LaygenExec LaygenRpg LaygenArch LaygenTcl)

LaygenPlaceCmd = (strcat LaygenCmd
                         "-route no \\\n"
                         "-generate_snp yes \\\n"
                         )

; place, route, import
(defun LaygenPR (cdlFile libName cellName
                         @key (num_place 10)
                         (shrink 0) (horz_routes 4) (h nil) (w nil)
                         (doCdl t) (doPlace t) (doRoute t) (doImport t))
  (let (CV dir bbox h2 w2 cmd obj file viewName out)
    ; get target bbox from floorplan view
    CV = (dbOpenCellViewByType libName cellName "floorplan" "maskLayout" "r")
    (unless CV (printf "NOTE: no floorplan view\n" cellName))
    (when CV bbox = CV->prBoundary->bBox)
    (when !h && CV h = (cadr (cadr bbox)) - (cadr (car bbox)))
    (when !w && CV w w = (car  (cadr bbox)) - (car  (car bbox)))
    (when !h || !w (error "Must provide floorplan view or ?h and ?w options\n"))
    h2 = (round h/TrackPitch)
    w2 = (round w/GridPolyPitch)
    w2 = w2-shrink
    (when w2<4 doCDL=nil doPlace=nil doRoute=nil doImport=nil)
    w  = w2*GridPolyPitch

    ; make working dir
    (sprintf dir "temp/laygen/%s/%s%d" cellName (if shrink<0 "n" "") (abs shrink))
    (csh (sprintf nil "mkdir -p %s" dir))
    
    ; generate SNP
    (when doCdl
      (printf "Generate SNP for %s\n" cellName)
      (sprintf cmd "cdl2snp %s %s %s/%s.snp" cellName cdlFile dir cellName)
      (csh cmd)
      )

    ; place
    (when doPlace
      ; run LayGen in placement mode
      (printf "Place %s height=%d width=%d placements=%d\n" cellName h2 w2 num_place)
      (sprintf cmd (strcat 
                    LaygenPlaceCmd
                    "-netlist %s.snp \\\n"
                    "-outdir place \\\n"
                    "-target_height %d \\\n"
                    "-target_area %d \\\n"
                    "-placement_max_hor_routes %d \\\n"
                    "-placement_result_idx %d \\\n"
                    ">& %s.log")
               cellName h2 w2 horz_routes*h2 num_place-1 cellName)
      out = (outfile (sprintf nil "%s/%s.csh" dir cellName))
      (fprintf out "#!/bin/csh\n%s\n" cmd)
      (close out)
      (csh (sprintf nil "chmod +x %s/%s.csh" dir cellName))
      (csh (sprintf nil "(cd %s; %s)" dir cmd))
      )

    ; sweep over num_place placements
    (for n 0 num_place-1
         ; delete old laygen views
         (sprintf viewName "laygen_%s%d_%d" (if shrink<0 "n" "") (abs shrink) n)
         obj = (ddGetObj libName cellName viewName)
         (when obj (ddDeleteObj obj))

         ; route
         (sprintf file "%s/tmp_%d.snp" dir n)
         (when doRoute && (fileTimeModified file)
           (printf "Route %s placement=%d\n" cellName n)
           (sprintf cmd (strcat 
                         LaygenCmd
                         "-netlist tmp_%d.snp \\\n"
                         "-outdir %d \\\n"
                         ">& %s.%d.log")
                    n n cellName n)
           out = (outfile (sprintf nil "%s/%s.%d.csh" dir cellName n))
           (fprintf out "#!/bin/csh\n%s\n" cmd)
           (close out)
           (csh (sprintf nil "chmod +x %s/%s.%d.csh" dir cellName n))
           (csh (sprintf nil "(cd %s; %s)" dir cmd))
           )

         ; import
         (sprintf file "%s/%d/%s/u0.gen" dir n cellName)
         (when doImport &&
               (fileTimeModified file)
               (printf "Import %s placement=%d\n" cellName n)
               (LaygenImport file libName cellName ?viewName viewName)
               )
         )
    )
  t
  )

; just delete laygen views
(defun LaygenClean (libName cellName @key (num_place 10))
  (LaygenPR "" libName cellName ?doCdl nil ?doPlace nil ?doRoute nil ?doImport nil)
  )
