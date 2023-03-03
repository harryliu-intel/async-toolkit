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
(defun AddHalo (points vCV hCV cCV icCV @key (CV (geGetEditCellView)) (outside nil) (idx 0))
  (let (num point last next next2 c c1 name x y xa ya cw ch w h cw0 cw1 ch0 ch1)
    ; setup
    xa = (RectGetWidth  (GetPrbound hCV)->bBox)
    ya = (RectGetHeight (GetPrbound vCV)->bBox)
    cw = (round (RectGetWidth  (GetPrbound icCV)->bBox)/xa) ; interior corner width
    ch = (round (RectGetHeight (GetPrbound icCV)->bBox)/ya) ; interior corner height
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
         name = (sprintf nil "boundarycell_%s_%d" c idx++)
 	 (cond
               (c=="se" (dbCreateInst CV icCV name point "MX"))
               (c=="en" (dbCreateInst CV icCV name point "R180"))
               (c=="nw" (dbCreateInst CV icCV name point "MY"))
               (c=="ws" (dbCreateInst CV icCV name point "R0"))
               (c=="wn" (dbCreateInst CV cCV  name point "MY"))
               (c=="sw" (dbCreateInst CV cCV  name point "R0"))
               (c=="es" (dbCreateInst CV cCV  name point "MX"))
               (c=="ne" (dbCreateInst CV cCV  name point "R180"))
               )

         ; instantiate an edge
         name = (sprintf nil "boundarycell_%d" idx++)
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
          (c=="nw" && w>=cw0+cw1 (dbCreateSimpleMosaic CV hCV name x-xa*(w-cw1):y "R0" 1 w-cw0-cw1 ya xa))
          (c=="sw" && w>=cw0+cw1 (dbCreateSimpleMosaic CV hCV name x-xa*(w-cw1):y "R0" 1 w-cw0-cw1 ya xa))
          (c=="ne" && w>=cw0+cw1 (dbCreateSimpleMosaic CV hCV name x+xa*cw0:y     "MX" 1 w-cw0-cw1 ya xa))
          (c=="se" && w>=cw0+cw1 (dbCreateSimpleMosaic CV hCV name x+xa*cw0:y     "MX" 1 w-cw0-cw1 ya xa))
          (c=="wn" && h>=ch0+ch1 (dbCreateSimpleMosaic CV vCV name x:y+ya*ch0     "MY" h-ch0-ch1 1 ya xa))
          (c=="ws" && h>=ch0+ch1 (dbCreateSimpleMosaic CV vCV name x:y-ya*(h-ch1) "R0" h-ch0-ch1 1 ya xa))
          (c=="en" && h>=ch0+ch1 (dbCreateSimpleMosaic CV vCV name x:y+ya*ch0     "MY" h-ch0-ch1 1 ya xa))
          (c=="es" && h>=ch0+ch1 (dbCreateSimpleMosaic CV vCV name x:y-ya*(h-ch1) "R0" h-ch0-ch1 1 ya xa))
          )
         )
    idx
    )
  )

; Add reminder cells above/below TOP_HALO cells
(defun AddHaloRemainder (@key (CV (geGetEditCellView)) (small nil) (viewName nil))
  (let (hCV cCV remCV cellName xy rem bbox n y inst)
    ; find CV of remainder cells
    hCV=(makeTable "hCV" nil)
    cCV=(makeTable "cCV" nil)
    (unless viewName viewName=CV->viewName)
    (foreach rem (list 60 120 180)
             cellName=(sprintf nil "globals.TOP_HALO.h_%srem%d" (if small "_small" "") rem)
             hCV[rem]=(ReadCV "globals" cellName viewName)
             cellName=(sprintf nil "globals.TOP_HALO.c_%srem%d" (if small "_small" "") rem)
             cCV[rem]=(ReadCV "globals" cellName viewName)
             )

    ; instantiate remainder cells
    n=0
    (foreach inst CV->instances
      (when inst->libName=="globals"
            remCV=nil
            (cond ((strncmp inst->cellName "globals.TOP_HALO.h" (length "globals.TOP_HALO.h"))==0 remCV=hCV)
                  ((strncmp inst->cellName "globals.TOP_HALO.c" (length "globals.TOP_HALO.c"))==0 remCV=cCV)
                  )
            (when remCV
              bbox=inst->master->prBoundary->bBox
              xy=(car (car bbox)):(cadr (car bbox))
              xy=(dbTransformPoint xy inst->transform)
              y=(cadr xy)
              y=(round y/0.06)
              y=(if inst->orient=="R180" || inst->orient=="MX" -y y)
              rem=(posmod y 4)*60
              (when rem!=0 && remCV[rem]
                remCV=remCV[rem]
                inst=(dbCreateInst CV remCV (sprintf nil "boundarycell_rem_%d" n inst->orient) xy inst->orient)
                inst->status="locked"
                n++
                )
              )
            )
      )
    n
    )
  )

; Add TOP_HALO given a prBoundary or around instances with topHalo=nil directive
(defun AddTopHalo (@key (CV (geGetEditCellView)) (small nil) (outside t)
                        (around_instances nil) (remainder t) (viewName nil))
  (let (vCV hCV cCV icCV cnt idx prop)
    ; delete old halo
    (DeleteTopHalo ?CV CV)

    ; choose halo cells
    (unless viewName viewName=CV->viewName)
    (cond (small
           vCV  = (dbOpenCellViewByType "globals" "globals.TOP_HALO.v_small"  viewName)
           hCV  = (dbOpenCellViewByType "globals" "globals.TOP_HALO.h_small"  viewName)
           cCV  = (dbOpenCellViewByType "globals" "globals.TOP_HALO.c_small"  viewName)
           icCV = (dbOpenCellViewByType "globals" "globals.TOP_HALO.ic_small" viewName)
           )
          (t
           vCV  = (dbOpenCellViewByType "globals" "globals.TOP_HALO.v"  viewName)
           hCV  = (dbOpenCellViewByType "globals" "globals.TOP_HALO.h"  viewName)
           cCV  = (dbOpenCellViewByType "globals" "globals.TOP_HALO.c"  viewName)
           icCV = (dbOpenCellViewByType "globals" "globals.TOP_HALO.ic" viewName)
           )
          )

    ; add the halo
    idx=0
    (cond (around_instances
           prop=(dbGetPropByName CV "topHalo")
           (when prop && prop->value=="TRUE"
                 (foreach polygon (NoHaloSubcellBoundaries ?CV CV)
                          idx=(AddHalo polygon vCV hCV cCV icCV ?CV CV ?outside outside ?idx idx)
                          )
                 )
           )
          (t
           (AddHalo CV->prBoundary->points vCV hCV cCV icCV ?CV CV ?outside outside)
           )
          )

    ; flatten mosaics and legalize halo variants
    cnt=(LegalizeTopHalo ?CV CV)

    ; add remainder cells next to halo cells where necessary
    (when remainder cnt+=(AddHaloRemainder ?CV CV ?small small ?viewName viewName))
    (dbSave CV)
    cnt
    )
  )

; flatten mosaics and legalize variants
(defun LegalizeTopHalo (@key (CV (geGetEditCellView)))
  (let (cnt x y suffix newCV net term)
    ; flatten TOP_HALO mosaics
    (foreach inst CV->mosaics
             (when (rexMatchp "TOP_HALO" inst->cellName)
               (dbFlattenInst inst 1)
               )
             )

    cnt=0
    (foreach inst CV->instances
      (when (strncmp inst->cellName "globals.TOP_HALO." (strlen "globals.TOP_HALO."))==0
            (unless (strncmp inst->name "halo" 4)==0
                    inst->name = (sprintf nil "boundarycell_%d" cnt)
                    cnt++
                    )
            (unless (CheckInstanceAlignment inst ?check_x nil ?check_parent_grid nil)
              newCV=(ReadCV "globals" (strcat inst->cellName "_oddrow") inst->viewName)
              (unless newCV (error "unable to read oddrow version of %s\n" inst->name))
              inst->master=newCV
              )
            inst->status = "locked"
            net  = (dbFindNetByName CV "GND")
            term = (dbFindTermByName inst->master "GND")
            (when net && term (dbCreateInstTerm net inst term))
            )
      )
    cnt
    )
  )

; Figure out which way a clockwise polygon is turning given 3 consecutive points
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

; quick utility to query the prop
(defun GetTopHalo (@key (CV (geGetEditCellView)))
  (let (halo prop)
    (when prop=(dbGetPropByName CV->master "topHalo")
      halo = prop->value
      )
    )
  )
