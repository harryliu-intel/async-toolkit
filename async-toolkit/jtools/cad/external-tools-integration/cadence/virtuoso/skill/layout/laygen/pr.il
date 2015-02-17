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
(defun LaygenPR (libName cellName @key
                         (cdlFile nil)  ; specify a CDL file; otherwise use cast2cdl
                         (num_place 10) ; number of placements to try
                         (shrink "min") ; reduce width by this many poly pitches; or minimum width
                         (h nil)        ; target height in rows; defaults to floorplan prBoundary
                         (w nil)        ; target width  in pg;   defaults to floorplan prBoundary
                         (n_max 2)      ; max NMOS width in diffusion grids (2-3)
                         (p_max 2)      ; max PMOS width in diffusion grids (2-3)
                         (hor_routes 4) ; number of horizontal routes to allow per row (3-5)
                         (upperlayer 1) ; top metal layer to use (1-2)
                         (placement_effort 2) ; 0-3
                         (quality_effort   1) ; 0-2
                         (n_offset 1) ; move NMOS away from NWELL edge by this many dg (0-2)
                         (p_offset 0) ; move PMOS away from NWELL edge by this many dg (0-2)
                         (batch "nbq --block") ; remote launch command; use nil to run locally
                         (jobs 32) ; maximum number of batch routing jobs
                         (doCdl t) (doSnp t) (doPlace t) (doRoute t) (doImport t))
  (let (CV dir bbox cmd obj file viewName name out device_row_offset trials success)

    ; get height and width from parameters or floorplan view
    (when !h || !w
      CV = (dbOpenCellViewByType libName cellName "floorplan" "maskLayout" "r")
      (unless CV (error "Must provide floorplan view or both ?h and ?w options\n"))
      bbox = CV->prBoundary->bBox
      (unless h h = (round ((cadr (cadr bbox)) - (cadr (car bbox)))/TrackPitch))
      (unless w w = (round ((car  (cadr bbox)) - (car  (car bbox)))/GridPolyPitch))
      )
    (when (type shrink)==`fixnum
          w = w-shrink
          (sprintf shrink "%s%d" (if shrink<0 "n" "") (abs shrink))
          )
    (when w<3 doCdl=nil doSnp=nil doPlace=nil doRoute=nil doImport=nil)

    ; make working dir 
    name = (sprintf nil "s%s_m%d_hr%d_nm%d_pm%d_no%d_po%d"
                    shrink upperlayer hor_routes n_max p_max n_offset p_offset)
    (sprintf dir "temp/laygen/%s/%s" cellName name)
    (csh (sprintf nil "mkdir -p %s" dir))

    ; generate CDL
    (when doCdl && !cdlFile
      (printf "Generate CDL for %s\n" cellName)
      (sprintf cdlFile "%s/%s.cdl" dir cellName)
      (sprintf cmd (strcat "cast2cdl --cast-path=$CAST_PATH --process-dependent-name --cdl-mos-parameters=w,l " 
                           "--cell=%s --output=%s/hier.cdl") cellName dir)
      (csh cmd)
      (sprintf cmd "inliner --cdl=%s/hier.cdl --library=%s/hier.cdl > %s"
               dir dir cdlFile)
      (csh cmd)
      )

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
             cellName h (if shrink=="min" "min" (sprintf nil "%d" w-1))
             num_place-1 hor_routes*h
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
      (printf "Place  %s height=%d width=%d placements=%d\n" cellName h w num_place)
      (csh (sprintf nil "(cd %s; %s.place.csh)" dir cellName))
      )

    ; delete old laygen views
    (for n 0 num_place-1
         (sprintf viewName "laygen_%s_%d" name n)
         obj = (ddGetObj libName cellName viewName)
         (when obj (ddDeleteObj obj))
         )

    ; route all placements, optionally using batch servers
    trials = ""
    (for n 0 num_place-1
         (sprintf file "%s/tmp_%d.snp" dir n)
         (when (fileTimeModified file) (sprintf trials "%s%d " trials n))
         )
    (when doRoute && trials!=""
      (printf "Route  %s placements %s\n" cellName trials)
      (csh (sprintf nil "(cd %s; echo %s | xargs --max-procs=%d --max-args=1 %s %s.route.csh)"
                    dir trials (if batch jobs 1) (if batch batch "") cellName))
      )

    ; import
    (for n 0 num_place-1
         (sprintf viewName "laygen_%s_%d" name n)
         (sprintf file "%s/%d/%s/u0.gen" dir n cellName)
         (when doImport && (fileTimeModified file)
               (printf "Import %s placement=%d\n" cellName n)
               CV = (LaygenImport file libName cellName ?viewName viewName)
               (when (setof shape CV->shapes shape->layerName=="vcn")!=nil success=t)
               )
         )
    success
    )
  )
