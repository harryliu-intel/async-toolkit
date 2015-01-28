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
                    "-placement_effort 2 \\\n"
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
                         (shrink 0) (horz_routes 5) (h nil) (w nil) (upperLayer 1)
                         (doCdl t) (doPlace t) (doRoute t) (doImport t))
  (let (CV dir bbox h2 w2 cmd obj file viewName out)
    ; get target bbox from floorplan view
    CV = (dbOpenCellViewByType libName cellName "floorplan" "maskLayout" "r")
    (unless CV (printf "NOTE: no floorplan view\n" cellName))
    (when CV bbox = CV->prBoundary->bBox)
    (when !h && CV h = (cadr (cadr bbox)) - (cadr (car bbox)))
    (when !w && CV w = (car  (cadr bbox)) - (car  (car bbox)))
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

    ; create place script
    (sprintf cmd (strcat 
                  LaygenPlaceCmd
                  "-netlist %s.snp \\\n"
                  "-outdir place \\\n"
                  "-target_height %d \\\n"
                  "-target_area %d \\\n"
                  "-placement_max_hor_routes %d \\\n"
                  "-placement_result_idx %d \\\n"
                  ">& %s.place.log")
             cellName h2 w2-1 horz_routes*h2 num_place-1 cellName)
    file = (sprintf nil "%s/%s.place.csh" dir cellName)
    out = (outfile file)
    (fprintf out "#!/bin/csh\n%s\n" cmd)
    (close out)
    (csh (sprintf nil "chmod +x %s" file))

    ; create route script
    (sprintf cmd (strcat 
                  LaygenCmd
                  "-upperlayer m%d \\\n"
                  "-netlist tmp_$1.snp \\\n"
                  "-outdir $1 \\\n"
                  ">& %s.route.$1.log")
             upperLayer cellName)
    file = (sprintf nil "%s/%s.route.csh" dir cellName)
    out = (outfile file)
    (fprintf out "#!/bin/csh\n%s\n" cmd)
    (close out)
    (csh (sprintf nil "chmod +x %s" file))

    ; place
    (when doPlace
      (printf "Place  %s height=%d width=%d placements=%d\n" cellName h2 w2 num_place)
      (csh (sprintf nil "(cd %s; %s.place.csh)" dir cellName))
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
           (printf "Route  %s placement=%d\n" cellName n)
           (csh (sprintf nil "(cd %s; %s.route.csh %d)" dir cellName n))
           )

         ; import
         (sprintf file "%s/%d/%s/u0.gen" dir n cellName)
         (when doImport && (fileTimeModified file)
               (printf "Import %s placement=%d\n" cellName n)
               (LaygenImport file libName cellName ?viewName viewName)
               )
         )
    )
  t
  )
