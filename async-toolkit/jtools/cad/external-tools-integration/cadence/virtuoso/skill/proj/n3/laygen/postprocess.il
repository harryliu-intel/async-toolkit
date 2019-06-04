; Stretch a poly-pitch 54nm E08 cell to poly-pitch 60nm and shift the
; M1 for variant 1..4.

; adjust X coordinate
(defun StretchPoint (layer side m1xw xy)
  (let (x y shrink xoff n xw w)
    x=(car xy)
    y=(cadr xy)
    (cond (layer=="v0" || layer=="v1" || layer=="m1" ; special mapping of v0/m1/v1 layers
           n=(round x/0.036)
           xw=(nth (mod n 6) m1xw)
           w=(cadr xw)
           (when layer=="v1" w=0.02) ; v1 stayes width=0.02
           x=n*0.040 - (if side==0 w/2 0) + (if side==1 w/2 0) + (car xw)
           )
          (t ; default stretch
           shrink = layer=="vt" || layer=="tcn" ; shrink vt/tcn width
           xoff = (if shrink 0.003 0)
           n=(round x/0.054)
           x=x + n*0.006 + (if side==0 xoff 0) - (if side==1 xoff 0)
           )
          )
    x:y
    )
  )

; Convert a nominal pp54 CellView into a pp60 cell with selected M1 variant
(defun MakeVariant (variant @key (CV (geGetEditCellView)))
  (let (points m1xw x0 y0 x1 y1 prbx0 prbx1)
    ; m1 patterns with xoffset and width
    (cond (variant==1 m1xw=(list 0.000:0.032 0.002:0.020 0.001:0.026 0.00:0.020 -0.001:0.026 -0.002:0.020))
          (variant==2 m1xw=(list 0.021:0.026 0.02:0.020 0.019:0.026 0.018:0.020 0.02:0.032 0.022:0.020))
          (variant==3 m1xw=(list 0.000:0.020 -0.001:0.026 -0.002:0.020 0.00:0.032 0.002:0.020 0.001:0.026))
          (variant==4 m1xw=(list 0.018:0.020 0.02:0.032 0.022:0.020 0.021:0.026 0.02:0.020 0.019:0.026))
          (t (error "variant %d not supported\n" variant))
          )
    
    ; stretch poly pitch and shift m1
    (foreach xy CV->prBoundary->points points=(append points (list (StretchPoint nil 0 m1xw xy))))
    (dbDeleteObject CV->prBoundary)
    (dbCreatePRBoundary CV points)
    (foreach obj CV->shapes
             (cond (obj->objType=="rect"
                    obj->bBox=(list (StretchPoint obj->layerName 0 m1xw (car  obj->bBox))
                                    (StretchPoint obj->layerName 1 m1xw (cadr obj->bBox))))
                   (obj->objType=="label"  obj->xy=(StretchPoint obj->layerName 2 m1xw obj->xy))
                   )
             )

    ; delete out-of-bounds v0/m1/v1
    prbx0 = (car (car  CV->prBoundary->bBox))
    prbx1 = (car (cadr CV->prBoundary->bBox))
    (foreach obj (setof x CV->shapes x->layerName=="m1" || x->layerName=="v0" || x->layerName=="v1")
             (cond (obj->objType=="rect"  x0=(car (car obj->bBox)) x1=(car (cadr obj->bBox)))
                   (obj->objType=="label" x0=(car obj->xy) x1=x0)
                   (t x0=nil x1=nil)
                   )
             (when x0!=nil && x1!=nil && (x0>prbx1 || x1<prbx0) (dbDeleteObject obj))
             )

    ; trim m2,nwell overhang
    (foreach obj (setof x CV->shapes x->objType=="rect" && (x->layerName=="m2" || x->layerName=="nwell"))
             x0=(car  (car  obj->bBox))
             y0=(cadr (car  obj->bBox))
             x1=(car  (cadr obj->bBox))
             y1=(cadr (cadr obj->bBox))
             overhang=(if obj->layerName=="m2" 0.026 0.09)
             x0=(max x0 prbx0-overhang)
             x1=(min x1 prbx1+overhang)
             obj->bBox=(list x0:y0 x1:y1)
             )
    )

  ; save
  (dbSave CV)
  CV
  )

; Make all 4 M1 variants given the nominal pp54 CellView
(defun MakeVariants (libName cellName @key (CV (geGetEditCellView)) (pre_action nil) (post_action nil))
  (let (nCV varName)
    (for n 1 4
         varName = (sprintf nil "%sv%d" cellName n)
         nCV = (when (ddGetObj libName varName "layout")
                 (dbOpenCellViewByType libName varName "layout"))
         (when nCV && pre_action (apply pre_action (list nCV)))
         (dbPurge nCV)
         nCV = (dbCopyCellView CV libName varName "layout" nil nil t)
         (dbReopen nCV "a")
         nCV = (MakeVariant n ?CV nCV)
         (when post_action (apply post_action (list nCV)))
         (dbPurge nCV)
         )
    )
  t
  )
