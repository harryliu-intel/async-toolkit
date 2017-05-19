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

; Add halo cells inside or outside prBoundary
(defun AddHalo (vCV nCV sCV ncCV scCV niCV siCV @key (CV (geGetEditCellView)) (outside nil))
  (let (points num idx point last next c name x y xa ya w h)
    ; setup
    xa = (car gridAlignment["lego"])
    ya = (cadr gridAlignment["lego"])
    points = CV->prBoundary->points
    points = (if (IsPolygonClockwise points) points (reverse points))
    points = (if outside (reverse points) points)
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
	 (cond
               (c=="se" (dbCreateSimpleMosaic CV scCV name point "R0" 1 1 ya xa))
               (c=="en" (dbCreateSimpleMosaic CV scCV name point "MY" 1 1 ya xa))
               (c=="nw" (dbCreateSimpleMosaic CV ncCV name point "MY" 1 1 ya xa))
               (c=="ws" (dbCreateSimpleMosaic CV ncCV name point "R0" 1 1 ya xa))
               (c=="wn" (dbCreateSimpleMosaic CV niCV name point "MY" 1 1 ya xa)
                        (dbCreateSimpleMosaic CV nCV  (sprintf nil "halo_%d" idx++) point "R0" 1 1 ya xa)
                        )
               (c=="sw" (dbCreateSimpleMosaic CV niCV name point "R0" 1 1 ya xa)
                        (dbCreateSimpleMosaic CV nCV  (sprintf nil "halo_%d" idx++) point "MY" 1 1 ya xa)
                        )
               (c=="es" (dbCreateSimpleMosaic CV siCV name point "R0" 1 1 ya xa)
                        (dbCreateSimpleMosaic CV sCV  (sprintf nil "halo_%d" idx++) point "MY" 1 1 ya xa)
                        )
               (c=="ne" (dbCreateSimpleMosaic CV siCV name point "MY" 1 1 ya xa)
                        (dbCreateSimpleMosaic CV sCV  (sprintf nil "halo_%d" idx++) point "R0" 1 1 ya xa)
                        )
               )

         ; instantiate an edge
         name = (sprintf nil "halo_%d" idx++)
         h = (abs (round ((cadr next)-(cadr point))/ya))
         w = (abs (round ((car next)-(car point))/xa))
         x = (car point)
         y = (cadr point)
         (cond
               (c=="nw" (dbCreateSimpleMosaic CV nCV name x+xa*(1-w):y "R0" 1 w-2 ya xa))
               (c=="sw" (dbCreateSimpleMosaic CV nCV name x+xa*(1-w):y "R0" 1 w-2 ya xa))
               (c=="ne" (dbCreateSimpleMosaic CV sCV name x+xa:y       "R0" 1 w-2 ya xa))
               (c=="se" (dbCreateSimpleMosaic CV sCV name x+xa:y       "R0" 1 w-2 ya xa))
               (c=="wn" (dbCreateSimpleMosaic CV vCV name x:y+ya       "MY" h-1 1 ya xa))
               (c=="ws" (dbCreateSimpleMosaic CV vCV name x:y+ya*(1-h) "R0" h-1 1 ya xa))
               (c=="en" (dbCreateSimpleMosaic CV vCV name x:y+ya       "MY" h-1 1 ya xa))
               (c=="es" (dbCreateSimpleMosaic CV vCV name x:y+ya*(1-h) "R0" h-1 1 ya xa))
               )
         )
    CV
    )
  )

; Add TOP_HALO given a prBoundary.
(defun AddTopHalo (@key (CV (geGetEditCellView)) (fill_metal nil))
  (let (vCV nCV ncCV scCV niCV siCV)
    ; delete old halo
    (DeleteTopHalo ?CV CV)

    ; choose halo cells
    (cond (fill_metal
           vCV  = (dbOpenCellViewByType "globals" "globals.TOP_HALO.v_tap_m2345678"  "layout")
           nCV  = (dbOpenCellViewByType "globals" "globals.TOP_HALO.n_m2345678"      "layout")
           sCV  = (dbOpenCellViewByType "globals" "globals.TOP_HALO.s_m2345678"      "layout")
           ncCV = (dbOpenCellViewByType "globals" "globals.TOP_HALO.nc_m2345678"     "layout")
           scCV = (dbOpenCellViewByType "globals" "globals.TOP_HALO.sc_m2345678"     "layout")
           niCV = (dbOpenCellViewByType "globals" "globals.TOP_HALO.ni_tap_m2345678" "layout")
           siCV = (dbOpenCellViewByType "globals" "globals.TOP_HALO.si_tap_m2345678" "layout")
           )
          (t
           vCV  = (dbOpenCellViewByType "globals" "globals.TOP_HALO.v_tap"  "layout")
           nCV  = (dbOpenCellViewByType "globals" "globals.TOP_HALO.n"      "layout")
           sCV  = (dbOpenCellViewByType "globals" "globals.TOP_HALO.s"      "layout")
           ncCV = (dbOpenCellViewByType "globals" "globals.TOP_HALO.nc"     "layout")
           scCV = (dbOpenCellViewByType "globals" "globals.TOP_HALO.sc"     "layout")
           niCV = (dbOpenCellViewByType "globals" "globals.TOP_HALO.ni_tap" "layout")
           siCV = (dbOpenCellViewByType "globals" "globals.TOP_HALO.si_tap" "layout")
           )
          )

    ; add the halo
    (AddHalo vCV nCV sCV ncCV scCV niCV siCV ?CV CV)
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
    (AddHalo vCV nCV sCV ncCV scCV niCV siCV ?CV CV ?outside t)
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
