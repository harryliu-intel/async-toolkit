; Delete any old TOP_HALO
(defun DeleteTopHalo (@key (CV (geGetEditCellView)))
  (let (pcre)
    pcre = (pcreCompile "^globals\\.TOP_HALO")
    (foreach inst CV->instances
             (when (pcreExecute pcre inst->cellName)
               (dbDeleteObject inst)
               )
             )
    )
  t
  )

; Add TOP_HALO given a prBoundary.  May need to invert to get
; interior/exterior correct, but usually seems to work by default.
(defun AddTopHalo (@key (CV (geGetEditCellView)) (invert nil) (fill_metal nil))
  (let (vCV nCV ncCV scCV niCV siCV points num idx point last next c name x y xa ya w h)
    ; delete old halo
    (DeleteTopHalo ?CV CV)

    ; find TOP_HALO cellViews
    vCV  = (dbOpenCellViewByType "globals" "globals.TOP_HALO.v_tap"  "layout")
    nCV  = (dbOpenCellViewByType "globals" "globals.TOP_HALO.n"      "layout")
    sCV  = (dbOpenCellViewByType "globals" "globals.TOP_HALO.s"      "layout")
    ncCV = (dbOpenCellViewByType "globals" "globals.TOP_HALO.nc"     "layout")
    scCV = (dbOpenCellViewByType "globals" "globals.TOP_HALO.sc"     "layout")
    niCV = (dbOpenCellViewByType "globals" "globals.TOP_HALO.ni_tap" "layout")
    siCV = (dbOpenCellViewByType "globals" "globals.TOP_HALO.si_tap" "layout")

    vCVm  = (dbOpenCellViewByType "globals" "globals.TOP_HALO.v_m2345678"  "layout")
    nCVm  = (dbOpenCellViewByType "globals" "globals.TOP_HALO.n_m2345678"      "layout")
    sCVm  = (dbOpenCellViewByType "globals" "globals.TOP_HALO.s_m2345678"      "layout")
    ncCVm = (dbOpenCellViewByType "globals" "globals.TOP_HALO.nc_m2345678"     "layout")
    scCVm = (dbOpenCellViewByType "globals" "globals.TOP_HALO.sc_m2345678"     "layout")
    niCVm = (dbOpenCellViewByType "globals" "globals.TOP_HALO.ni_m2345678" "layout")
    siCVm = (dbOpenCellViewByType "globals" "globals.TOP_HALO.si_m2345678" "layout")

    ; other setup
    xa = (car gridAlignment["lego"])
    ya = (cadr gridAlignment["lego"])
    points = CV->prBoundary->points
    points = (if invert (reverse points) points)
    num = (length points)
    idx = 0

    ; walk all points of prBoundary
    (for i 0 num-1
         point = (nth i points)
         last = (nth (mod i+num-1 num) points)
         next = (nth (mod i+1 num) points)
         c = (ClassifyPolygonCorner last point next)

         ; instantiate a corner
         name = (sprintf nil "halo_%d" idx++)
         namem = (sprintf nil "halom_%d" idx++)         
	 (cond 
               (c=="se" 
	       		(dbCreateSimpleMosaic CV scCV name point "R0" 1 1 ya xa)
			(when fill_metal (dbCreateSimpleMosaic CV scCVm namem point "R0" 1 1 ya xa))
	       		)
               (c=="en" 
	       		(dbCreateSimpleMosaic CV scCV name point "MY" 1 1 ya xa)
			(when fill_metal (dbCreateSimpleMosaic CV scCVm namem point "MY" 1 1 ya xa))
	       		)
               (c=="nw" 
	       		(dbCreateSimpleMosaic CV ncCV name point "MY" 1 1 ya xa)
			(when fill_metal (dbCreateSimpleMosaic CV ncCVm namem point "MY" 1 1 ya xa))
	       		)
               (c=="ws" 
	       		(dbCreateSimpleMosaic CV ncCV name point "R0" 1 1 ya xa)
			(when fill_metal (dbCreateSimpleMosaic CV ncCVm namem point "R0" 1 1 ya xa))
	       		)
               (c=="wn" 
	       		(dbCreateSimpleMosaic CV niCV name point "MY" 1 1 ya xa)
                        (dbCreateSimpleMosaic CV nCV  (sprintf nil "halo_%d" idx++) point "R0" 1 1 ya xa)
			(when fill_metal 
	 	       		(dbCreateSimpleMosaic CV niCVm namem point "MY" 1 1 ya xa)
        	                (dbCreateSimpleMosaic CV nCVm (sprintf nil "halom_%d" idx++) point "R0" 1 1 ya xa))
                        )
               (c=="sw" 
	       		(dbCreateSimpleMosaic CV niCV name point "R0" 1 1 ya xa)
                        (dbCreateSimpleMosaic CV nCV  (sprintf nil "halo_%d" idx++) point "MY" 1 1 ya xa)
 			(when fill_metal 
			      (dbCreateSimpleMosaic CV niCVm namem point "R0" 1 1 ya xa)
                              (dbCreateSimpleMosaic CV nCVm  (sprintf nil "halom_%d" idx++) point "MY" 1 1 ya xa))
                       )
               (c=="es" 
	       		(dbCreateSimpleMosaic CV siCV name point "R0" 1 1 ya xa)
                        (dbCreateSimpleMosaic CV sCV  (sprintf nil "halo_%d" idx++) point "MY" 1 1 ya xa)
 			(when fill_metal 
	       		      (dbCreateSimpleMosaic CV siCVm namem point "R0" 1 1 ya xa)
                              (dbCreateSimpleMosaic CV sCVm  (sprintf nil "halom_%d" idx++) point "MY" 1 1 ya xa))
                       )
               (c=="ne" 
	       		(dbCreateSimpleMosaic CV siCV name point "MY" 1 1 ya xa)
                        (dbCreateSimpleMosaic CV sCV  (sprintf nil "halo_%d" idx++) point "R0" 1 1 ya xa)
 			(when fill_metal 
	       		      (dbCreateSimpleMosaic CV siCVm namem point "MY" 1 1 ya xa)
                              (dbCreateSimpleMosaic CV sCVm  (sprintf nil "halom_%d" idx++) point "R0" 1 1 ya xa))
                       )
               )

         ; instantiate an edge
         name = (sprintf nil "halo_%d" idx++)
         namem = (sprintf nil "halo_%d" idx++)
         h = (abs (round ((cadr next)-(cadr point))/ya))
         w = (abs (round ((car next)-(car point))/xa))
         x = (car point)
         y = (cadr point)
         (cond 
               (c=="nw" 
	       		(dbCreateSimpleMosaic CV nCV name x+xa*(1-w):y "R0" 1 w-2 ya xa)
			(when fill_metal (dbCreateSimpleMosaic CV nCVm namem x+xa*(1-w):y "R0" 1 w-2 ya xa))
			)
               (c=="sw" 
	       		(dbCreateSimpleMosaic CV nCV name x+xa*(1-w):y "R0" 1 w-2 ya xa)
			(when fill_metal (dbCreateSimpleMosaic CV nCVm namem x+xa*(1-w):y "R0" 1 w-2 ya xa))
			)
               (c=="ne" 
	       		(dbCreateSimpleMosaic CV sCV name x+xa:y       "R0" 1 w-2 ya xa)
			(when fill_metal (dbCreateSimpleMosaic CV sCVm namem x+xa:y       "R0" 1 w-2 ya xa))
			)
               (c=="se" 
			(dbCreateSimpleMosaic CV sCV name x+xa:y       "R0" 1 w-2 ya xa)
			(when fill_metal (dbCreateSimpleMosaic CV sCVm namem x+xa:y       "R0" 1 w-2 ya xa))
			)
               (c=="wn" 
	       		(dbCreateSimpleMosaic CV vCV name x:y+ya       "MY" h-1 1 ya xa)
			(when fill_metal (dbCreateSimpleMosaic CV vCVm namem x:y+ya       "MY" h-1 1 ya xa))
			)
               (c=="ws" 
			(dbCreateSimpleMosaic CV vCV name x:y+ya*(1-h) "R0" h-1 1 ya xa)
			(when fill_metal (dbCreateSimpleMosaic CV vCVm namem x:y+ya*(1-h) "R0" h-1 1 ya xa))
			)
               (c=="en" 
	       		(dbCreateSimpleMosaic CV vCV name x:y+ya       "MY" h-1 1 ya xa)
			(when fill_metal (dbCreateSimpleMosaic CV vCVm namem x:y+ya       "MY" h-1 1 ya xa))
			)
               (c=="es" 
	       		(dbCreateSimpleMosaic CV vCV name x:y+ya*(1-h) "R0" h-1 1 ya xa)
			(when fill_metal (dbCreateSimpleMosaic CV vCVm namem x:y+ya*(1-h) "R0" h-1 1 ya xa))
			)
               )
         )
    CV
    )
  )

; Figure out which way a polygon is turning given 3 consecutive points
(defun ClassifyPolygonCorner (last point next)
  (cond ((car point) <(car last)  (if (cadr next)<(cadr point) "ws" "wn"))
        ((car point) >(car last)  (if (cadr next)<(cadr point) "es" "en"))
        ((cadr point)<(cadr last) (if (car next) <(car point)  "sw" "se"))
        ((cadr point)>(cadr last) (if (car next) <(car point)  "nw" "ne"))
        )
  )


; make filled dummy block using prbound
(defun CreateBlockDummy (@key (CV (geGetEditCellView)) (invert nil))
  (let (prb prb_shrink)
     (AddTopHalo ?CV CV ?invert invert ?fill_metal t)

     ; Never got around to finishing this. After top_halo, we just need to place the mosaics of filler.

     )
  )

