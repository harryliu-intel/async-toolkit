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
  (let (points m1v)
    ; m1 patterns with xoffset and width
    (cond (variant==1 m1xw=(list 0.000:0.032 0.002:0.020 0.001:0.026 0.00:0.020 -0.001:0.026 -0.002:0.020))
          (variant==2 m1xw=(list 0.021:0.026 0.02:0.020 0.019:0.026 0.018:0.020 0.02:0.032 0.022:0.020))
          (variant==3 m1xw=(list 0.000:0.020 -0.001:0.026 -0.002:0.020 0.00:0.032 0.002:0.020 0.001:0.026))
          (variant==4 m1xw=(list 0.018:0.020 0.02:0.032 0.022:0.020 0.021:0.026 0.02:0.020 0.019:0.026))
          (t (error "variant %d not supported\n" variant))
          )
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
    )
  (dbSave CV)
  CV
  )

; Make all 4 M1 variants given the nominal pp54 CellView
(defun MakeVariants (libName cellName @key (CV (geGetEditCellView)))
  (let (nCV)
    (for n 1 4
         nCV = (dbCopyCellView CV libName (sprintf nil "%sv%d" cellName n) "layout" nil nil t)
         (MakeVariant n ?CV nCV)
         )
    )
  t
  )
