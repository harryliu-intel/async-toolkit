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

; Does this cell or its subcells have TOP_HALO?
(defun HasTopHalo (@key (CV (geGetEditCellView)))
  (let (pcre)
    pcre = (pcreCompile "^globals\\.TOP_HALO")
    (FindSubcells ?CV CV ?filter (lambda (CV) (pcreExecute pcre CV->cellName)))!=nil
    )
  )

; Detect if a polygon is clockwise.  Works by adding up the sign of all cross products at the corners.
(defun IsPolygonClockwise (points)
  (let (l n tot)
    tot = 0
    l = (length points)
    (for x 0 l-1
         p0 = (nth x points)
         p1 = (nth (mod x+1 l) points)
         p2 = (nth (mod x+2 l) points)
         v01 = (car p1)-(car p0):(cadr p1)-(cadr p0)
         v12 = (car p2)-(car p1):(cadr p2)-(cadr p1)
         cross = (car v01)*(cadr v12) - (cadr v01)*(car v12) ; Z element of 3D vector cross product
         tot = tot + (if cross>0 1 -1)
         )
    tot>0
    )
  )

; Add halo cells inside or outside a polygon
(defun AddHalo (points vCV nCV sCV ncCV scCV niCV siCV @key (CV (geGetEditCellView)) (outside nil) (idx 0))
  (let (num point last next next2 c c1 name x y xa ya cw ch w h cw0 cw1 ch0 ch1)
    ; setup
    xa = (RectGetWidth  (GetPrbound nCV)->bBox)
    ya = (RectGetHeight (GetPrbound vCV)->bBox)
    cw = (round (RectGetWidth  (GetPrbound ncCV)->bBox)/xa) ; corner width
    ch = (round (RectGetHeight (GetPrbound ncCV)->bBox)/ya) ; corner height
    points = (if (IsPolygonClockwise points) points (reverse points))
    points = (if outside (reverse points) points)
    num = (length points)

    ; walk all points of prBoundary
    (for i 0 num-1
         point = (nth i points)
         last = (nth (mod i+num-1 num) points)
         next = (nth (mod i+1 num) points)
         next2 = (nth (mod i+2 num) points)
         c  = (ClassifyPolygonCorner last point next)
         c1 = (ClassifyPolygonCorner point next next2)

         ; instantiate a corner
         name = (sprintf nil "halo_%s_%d" c idx++)
 	 (cond
               (c=="se" (dbCreateSimpleMosaic CV scCV name point "R0" 1 1 ya xa))
               (c=="en" (dbCreateSimpleMosaic CV scCV name point "MY" 1 1 ya xa))
               (c=="nw" (dbCreateSimpleMosaic CV ncCV name point "MY" 1 1 ya xa))
               (c=="ws" (dbCreateSimpleMosaic CV ncCV name point "R0" 1 1 ya xa))
               (c=="wn" (dbCreateSimpleMosaic CV niCV name point "MY" 1 1 ya xa))
               (c=="sw" (dbCreateSimpleMosaic CV niCV name point "R0" 1 1 ya xa))
               (c=="es" (dbCreateSimpleMosaic CV siCV name point "R0" 1 1 ya xa))
               (c=="ne" (dbCreateSimpleMosaic CV siCV name point "MY" 1 1 ya xa))
               )

         ; instantiate an edge
         name = (sprintf nil "halo_%d" idx++)
         h = (abs (round ((cadr next)-(cadr point))/ya))
         w = (abs (round ((car next)-(car point))/xa))
         x = (car point)
         y = (cadr point)

         ; correct for interior corners
         ch0=0
         ch1=0
         cw0=0
         cw1=0
         (cond (c=="nw" cw0=cw)
               (c=="se" cw0=cw)
               (c=="ws" ch0=ch)
               (c=="en" ch0=ch)
               )
         (cond (c1=="nw" ch1=ch)
               (c1=="se" ch1=ch)
               (c1=="ws" cw1=cw)
               (c1=="en" cw1=cw)
               )

         ; draw edges
         (cond
          (c=="nw" && w>=cw0+cw1 (dbCreateSimpleMosaic CV nCV name x-xa*(w-cw1):y "R0" 1 w-cw0-cw1 ya xa))
          (c=="sw" && w>=cw0+cw1 (dbCreateSimpleMosaic CV nCV name x-xa*(w-cw1):y "R0" 1 w-cw0-cw1 ya xa))
          (c=="ne" && w>=cw0+cw1 (dbCreateSimpleMosaic CV sCV name x+xa*cw0:y     "R0" 1 w-cw0-cw1 ya xa))
          (c=="se" && w>=cw0+cw1 (dbCreateSimpleMosaic CV sCV name x+xa*cw0:y     "R0" 1 w-cw0-cw1 ya xa))
          (c=="wn" && h>=ch0+ch1 (dbCreateSimpleMosaic CV vCV name x:y+ya*ch0     "MY" h-ch0-ch1 1 ya xa))
          (c=="ws" && h>=ch0+ch1 (dbCreateSimpleMosaic CV vCV name x:y-ya*(h-ch1) "R0" h-ch0-ch1 1 ya xa))
          (c=="en" && h>=ch0+ch1 (dbCreateSimpleMosaic CV vCV name x:y+ya*ch0     "MY" h-ch0-ch1 1 ya xa))
          (c=="es" && h>=ch0+ch1 (dbCreateSimpleMosaic CV vCV name x:y-ya*(h-ch1) "R0" h-ch0-ch1 1 ya xa))
          )
         )
    idx
    )
  )

; Add TOP_HALO given a prBoundary or around instances with topHalo=nil directive
(defun AddTopHalo (@key (CV (geGetEditCellView)) (fill_metal nil) (flatten t) (outside t) (around_instances nil))
  (let (vCV nCV ncCV scCV niCV siCV cnt idx prop)
    ; delete old halo
    (DeleteTopHalo ?CV CV)

    ; choose halo cells
    (cond (fill_metal
           vCV  = (dbOpenCellViewByType "globals" "globals.TOP_HALO.v_tap_m2345678"  CV->viewName)
           nCV  = (dbOpenCellViewByType "globals" "globals.TOP_HALO.n_m2345678"      CV->viewName)
           sCV  = (dbOpenCellViewByType "globals" "globals.TOP_HALO.s_m2345678"      CV->viewName)
           ncCV = (dbOpenCellViewByType "globals" "globals.TOP_HALO.nc_m2345678"     CV->viewName)
           scCV = (dbOpenCellViewByType "globals" "globals.TOP_HALO.sc_m2345678"     CV->viewName)
           niCV = (dbOpenCellViewByType "globals" "globals.TOP_HALO.ni_tap_m2345678" CV->viewName)
           siCV = (dbOpenCellViewByType "globals" "globals.TOP_HALO.si_tap_m2345678" CV->viewName)
           )
          (t
           vCV  = (dbOpenCellViewByType "globals" "globals.TOP_HALO.v_m0_10x2"  CV->viewName)
           nCV  = (dbOpenCellViewByType "globals" "globals.TOP_HALO.n_m0_10x2"  CV->viewName)
           sCV  = (dbOpenCellViewByType "globals" "globals.TOP_HALO.s_m0_10x2"  CV->viewName)
           ncCV = (dbOpenCellViewByType "globals" "globals.TOP_HALO.ni_m0_10x2"  CV->viewName)
           scCV = (dbOpenCellViewByType "globals" "globals.TOP_HALO.si_m0_10x2"  CV->viewName)
           niCV = (dbOpenCellViewByType "globals" "globals.TOP_HALO.nc_m0_10x2"  CV->viewName)
           siCV = (dbOpenCellViewByType "globals" "globals.TOP_HALO.sc_m0_10x2"  CV->viewName)
           )
          )

    ; add the halo
    idx=0
    prop=(dbGetPropByName CV "topHalo")
    (cond (around_instances && prop && prop->value=="TRUE"
           (foreach polygon (NoHaloSubcellBoundaries ?CV CV)
                    idx=(AddHalo polygon vCV nCV sCV ncCV scCV niCV siCV ?CV CV ?outside outside ?idx idx)
                    )
           )
          (t
           (AddHalo CV->prBoundary->points vCV nCV sCV ncCV scCV niCV siCV ?CV CV ?outside outside)
           )
          )

    ; flatten, flip, and rename for DEF
    (when flatten
      (foreach i CV->mosaics
        (when (rexMatchp "TOP_HALO" i->cellName)
          (dbFlattenInst i 1)
	  )
	)
      )

    cnt = 0
    (foreach inst CV->instances
      (when (strncmp inst->cellName "globals.TOP_HALO." (strlen "globals.TOP_HALO."))==0
        ; check m6 flip
        (unless (CheckInstanceAlignment inst ?check_x nil ?check_parent_grid nil)
          x = (car  inst->xy)
          y = (cadr inst->xy)
          (cond (inst->orient=="R0"   inst->orient="MX"   inst->xy=x:y+2*TrackPitch)
                (inst->orient=="MX"   inst->orient="R0"   inst->xy=x:y-2*TrackPitch)
                (inst->orient=="MY"   inst->orient="R180" inst->xy=x:y+2*TrackPitch)
                (inst->orient=="R180" inst->orient="MY"   inst->xy=x:y-2*TrackPitch)
                )
          )
        inst->status = "locked"
	inst->name = (sprintf nil "halo_%d" cnt)
	cnt++
        )
      )
    (dbSave CV)
    cnt
    )
  )

; Add CHIP_HALO given a prBoundary.
(defun AddChipHalo (@key (CV (geGetEditCellView)))
  (let (vCV nCV ncCV scCV niCV siCV)
    ; n/s and i/c confusingly swapped because this goes OUTSIDE the prBoundary
    vCV  = (dbOpenCellViewByType "globals" "globals.CHIP_HALO.v"  "layout")
    nCV  = (dbOpenCellViewByType "globals" "globals.CHIP_HALO.s"  "layout")
    sCV  = (dbOpenCellViewByType "globals" "globals.CHIP_HALO.n"  "layout")
    ncCV = (dbOpenCellViewByType "globals" "globals.CHIP_HALO.si" "layout")
    scCV = (dbOpenCellViewByType "globals" "globals.CHIP_HALO.ni" "layout")
    niCV = (dbOpenCellViewByType "globals" "globals.CHIP_HALO.sc" "layout")
    siCV = (dbOpenCellViewByType "globals" "globals.CHIP_HALO.nc" "layout")
    (AddHalo CV->prBoundary->points vCV nCV sCV ncCV scCV niCV siCV ?CV CV ?outside t)
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

; Make filled dummy block given prBoundary
(defun FillDummyView (@key (CV (geGetEditCellView)))
  (AddTopHalo ?CV CV ?fill_metal t)
  (MakeFillGrid ?CV CV)
  (MakeColorGrid ?CV CV)
  (MakePowerGrid ?CV CV)
  (MakePowerGridAbstract ?CV CV)
  (CreatePowerPins ?CV CV)
  (dbFlattenInst (dbFindAnyInstByName CV "fill") 1)
  (dbFlattenInst (dbFindAnyInstByName CV "color") 1)
  (dbFlattenInst (dbFindAnyInstByName CV "pg") 1)
  (dbSave CV)
  CV
  )

; Create versions of TOP_HALO that include metal fill up to M8
(defun MakeFilledTopHaloCells ()
  (let (oldCV newCV)
    (foreach sub (list "v" "n" "s" "nc" "sc" "ni" "si" "ni_tap" "si_tap" "v_tap")
             oldCV = (ReadCV "globals" (strcat "globals.TOP_HALO." sub) "layout")
             newCV = (ReadCV "globals" (strcat "globals.TOP_HALO." sub "_m2345678") "layout")
             (when newCV (cdsp4edit ?CV newCV))
             newCV = (dbOpenCellViewByType "globals"
                                           (strcat "globals.TOP_HALO." sub "_m2345678") "layout"
                                           "maskLayout" "w")
             (dbCreatePRBoundary newCV oldCV->prBoundary->points)
             (dbCreateInst newCV oldCV "halo" 0:0 "R0")
             (TemplateFill ?CV newCV ?botMetal 2 ?topMetal 8)
             (dbCreatePolygon newCV (list "m2" "block") newCV->prBoundary->points)
             (dbSave newCV)
             (cdsp4add ?CV newCV)
             )
    )
  t
  )

; return a list of polygons for all prBoundary of no-halo subcells
(defun NoHaloSubcellBoundaries (@key (CV (geGetEditCellView)))
  (let (polygons prop lpp shapes)
    lpp=(list "y1" "drawing")
    (foreach inst CV->instances
             prop=(dbGetPropByName inst->master "topHalo")
             (when prop && prop->value=="FALSE"
                   (dbCreatePolygon CV lpp (dbTransformPointList inst->master->prBoundary->points inst->transform))
                   )
             )
    shapes=(leMergeShapes (setof s CV->shapes s->lpp==lpp))
    (foreach s shapes
             polygons = (cons s->points polygons)
             (dbDeleteObject s)
             )
    polygons
    )
  )
