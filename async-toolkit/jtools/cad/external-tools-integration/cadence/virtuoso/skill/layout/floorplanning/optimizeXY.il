; Functions to move selected cells to minimize the vertical and/or
; horizontal wirespan to their fanin or fanout.  Designed to help
; placement of complex structures like
; lib.6T.ecc.HAMMING_PARITY_TREE's or CTREE's.
;
; Author: Andrew Lines

; Make X coordinate of selected cells half-way between minimum and
; maximum X coordinate of fanins and fanouts.  Snap to Xsnap grid.
(defun OptimizeX (Xsnap)
  (for i 0 20 (OptimizeXY t nil Xsnap 1.0) nil)
)

; Make Y coordinate of selected cells half-way between minimum and
; maximum Y coordinate of fanins and fanouts.  Snap to Ysnap grid.
(defun OptimizeY (Ysnap)
  (for i 1 20 (OptimizeXY nil t 1.0 Ysnap) nil)
)

; Core function (does only one iteration)
(defun OptimizeXY (doX doY Xsnap Ysnap)
  (let (moved minX maxX minY maxY x0 y0 x1 y1 ox oy cx cy nx ny x y)
    moved = nil
    (foreach obj (geGetSelSet)
      (cond (obj->objType=="inst"
             minX= 10000 minY= 10000
             maxX=-10000 maxY=-10000
             (foreach conn obj->conns
                net = conn->net
                (cond ((and net->name!="GND" net->name!="Vdd" net->name!="_RESET")
                       bbox = (NetBoundingBox net obj)
                       x0 = (car  (car  bbox))
                       y0 = (cadr (car  bbox))
                       x1 = (car  (cadr bbox))
                       y1 = (cadr (cadr bbox))
;                      (printf "  %s %f %f %f %f\n" net->name x0*1.0 y0*1.0 x1*1.0 y1*1.0)
                       minX = (min minX x0)
                       maxX = (max maxX x1)
                       minY = (min minY y0)
                       maxY = (max maxY y1)
                       )
                      )
                )
             ; adjust origin of this instance
             ox = (car  obj->xy)
             oy = (cadr obj->xy)
             cx = ((car  (car obj->bBox)) + (car  (cadr obj->bBox)))/2
             cy = ((cadr (car obj->bBox)) + (cadr (cadr obj->bBox)))/2
             nx = (minX+maxX)/2
             ny = (minY+maxY)/2
             x = ox+nx-cx
             y = oy+ny-cy
             x = (round x/Xsnap)*Xsnap
             y = (round y/Ysnap)*Ysnap
             x = (if doX x ox)
             y = (if doY y oy)
             moved = (or (car obj->xy)!=x (cadr obj->xy)!=y)
             obj->xy = (list x y)
             )
            )
      )
    moved
    )
  )

; Return the bounding box of pins and instTerms of a net excluding the
; terminals of excludeInst.
(defun NetBoundingBox (net excludeInst)
  (let (bbox x0 y0 x1 y1 minX minY maxX maxY)
    minX= 10000 minY= 10000
    maxX=-10000 maxY=-10000
    (foreach pin net->pins
       bbox = pin~>fig~>bBox
       x0 = (car  (car  bbox))
       y0 = (cadr (car  bbox))
       x1 = (car  (cadr bbox))
       y1 = (cadr (cadr bbox))
       minX = (min minX x0)
       maxX = (max maxX x1)
       minY = (min minY y0)
       maxY = (max maxY y1)
       )
    (foreach term net->instTerms
      (cond (term~>inst!=excludeInst
             bbox = term~>inst~>bBox
             x0 = (car  (car  bbox))
             y0 = (cadr (car  bbox))
             x1 = (car  (cadr bbox))
             y1 = (cadr (cadr bbox))
             minX = (min minX x0)
             maxX = (max maxX x1)
             minY = (min minY y0)
             maxY = (max maxY y1)
             )
            )
      )
    (list minX:minY maxX:maxY)
    )
  )
