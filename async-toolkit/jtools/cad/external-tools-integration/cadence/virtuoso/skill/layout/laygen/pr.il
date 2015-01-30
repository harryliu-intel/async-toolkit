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
                    )
         LaygenExec LaygenRpg LaygenArch LaygenTcl)

LaygenPlaceCmd = (strcat LaygenCmd
                         "-route no \\\n"
                         "-generate_snp yes \\\n"
                         )

; place, route, import
(defun LaygenPR (cdlFile libName cellName @key
                         (num_place 10) ; number of placements to try
                         (shrink "min") ; reduce width by this many poly pitches; or minimum width
                         (h nil)        ; target height in microns; defaults to floorplan prBoundary
                         (w nil)        ; target width  in microns; defaults to floorplan prBoundary
                         (n_max 2)      ; max NMOS width in diffusion grids (2-3)
                         (p_max 2)      ; max PMOS width in diffusion grids (2-3)
                         (hor_routes 4) ; number of horizontal routes to allow per row (3-5)
                         (upperlayer 1) ; top metal layer to use (1-2)
                         (placement_effort 2) ; 0-3
                         (quality_effort   1) ; 0-2
                         (n_offset 0) ; move NMOS away from NWELL edge by this many dg (0-2)
                         (p_offset 0) ; move PMOS away from NWELL edge by this many dg (0-2)
                         (batch "nbq --block") ; remote launch command; use nil to run locally
                         (jobs 32) ; maximum number of batch routing jobs
                         (doSnp t) (doPlace t) (doRoute t) (doImport t))
  (let (CV dir bbox h2 w2 cmd obj file viewName out device_row_offset trials)

    ; get target bbox from floorplan view
    CV = (dbOpenCellViewByType libName cellName "floorplan" "maskLayout" "r")
    (unless CV (printf "NOTE: no floorplan view\n" cellName))
    (when CV bbox = CV->prBoundary->bBox)
    (when !h && CV h = (cadr (cadr bbox)) - (cadr (car bbox)))
    (when !w && CV w = (car  (cadr bbox)) - (car  (car bbox)))
    (when !h || !w (error "Must provide floorplan view or ?h and ?w options\n"))
    h2 = (round h/TrackPitch)
    w2 = (round w/GridPolyPitch)
    (when (type shrink)==`fixnum
          w2 = w2-shrink
          (sprintf shrink "%s%d" (if shrink<0 "n" "") (abs shrink))
          )
    (when w2<4 doCDL=nil doPlace=nil doRoute=nil doImport=nil)

    ; make working dir
    (sprintf dir "temp/laygen/%s/%s" cellName shrink)
    (csh (sprintf nil "mkdir -p %s" dir))
    
    ; generate SNP
    (when doSnp
      (printf "Generate SNP for %s\n" cellName)
      (sprintf cmd "cdl2snp --gridW %g --maxNmosW %g --maxPmosW %g %s %s %s/%s.snp"
               GridDiffPitch*1e-6 GridDiffPitch*n_max*1e-6 GridDiffPitch*p_max*1e-6
               cellName cdlFile dir cellName)
      (csh cmd)
      )

    ; create place script
    (sprintf device_row_offset "%d,%d,%d,%d"
             n_offset p_offset p_offset n_offset)
    (sprintf cmd (strcat 
                  LaygenPlaceCmd
                  "-netlist %s.snp \\\n"
                  "-outdir place \\\n"
                  "-target_height %d \\\n"
                  "-target_area %s \\\n"
                  "-placement_result_idx %d \\\n"
                  "-placement_max_hor_routes %d \\\n"
                  "-placement_effort %d \\\n"
                  "-quality_effort %d \\\n"
                  "-device_row_offset %s \\\n"
                  ">& %s.place.log")
             cellName h2 (if shrink=="min" "min" (sprintf nil "%d" w2-1))
             num_place-1 hor_routes*h2
             placement_effort quality_effort
             device_row_offset cellName)
    file = (sprintf nil "%s/%s.place.csh" dir cellName)
    out = (outfile file)
    (fprintf out "#!/bin/csh\n%s\n" cmd)
    (close out)
    (csh (sprintf nil "chmod +x %s" file))

    ; create route script
    (sprintf cmd (strcat 
                  LaygenCmd
                  "-netlist tmp_$1.snp \\\n"
                  "-outdir $1 \\\n"
                  "-upperlayer m%d \\\n"
                  "-quality_effort %d \\\n"
                  ">& %s.route.$1.log")
             upperlayer quality_effort cellName)
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

    ; delete old laygen views
    (for n 0 num_place-1
         (sprintf viewName "laygen_%s_%d" shrink n)
         obj = (ddGetObj libName cellName viewName)
         (when obj (ddDeleteObj obj))
         )

    ; route all placements, optionally using batch servers
    trials = ""
    (for n 0 num_place-1
         (sprintf file "%s/tmp_%d.snp" dir n)
         (when (fileTimeModified file) (sprintf trials "%s%d " trials n))
         )
    (when doRoute
      (printf "Route  %s placements %s\n" cellName trials)
      (csh (sprintf nil "(cd %s; echo %s | xargs --max-procs=%d --max-args=1 %s %s.route.csh)"
                    dir trials (if batch jobs 1) (if batch batch "") cellName))
      )

    ; import
    (for n 0 num_place-1
         (sprintf viewName "laygen_%s_%d" shrink n)
         (sprintf file "%s/%d/%s/u0.gen" dir n cellName)
         (when doImport && (fileTimeModified file)
               (printf "Import %s placement=%d\n" cellName n)
               (LaygenImport file libName cellName ?viewName viewName)
               )
         )
    )
  t
  )
