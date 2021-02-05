; Functions to move selected cells to minimize the vertical and/or
; horizontal wirespan to their fanin or fanout.  Designed to help
; placement of complex structures like
; lib.6T.ecc.HAMMING_PARITY_TREE's or CTREE's.
;
; Author: Andrew Lines

(defun OptimizeX () (OptimizeXY ?doY nil))
(defun OptimizeY () (OptimizeXY ?doX nil))

; Move selected instances to average XY of their connections
(defun OptimizeXY (@key (doX t) (doY t) (Xsnap GridPolyPitch*3) (Ysnap GridPitch) (iter 20))
  (let (moved x0 y0 x1 y1 ox oy cx cy nx ny x y n l)
    moved = nil
    (for i 0 iter-1
      (foreach obj (geGetSelSet)
        (cond (obj->objType=="inst"
               n=0
               nx=0
               ny=0
               (foreach conn obj->conns
                        net = conn->net
                        l  = (NetAverageXY net obj)
                        n  = n  + (car l)
                        nx = nx + (cadr l)
                        ny = ny + (caddr l)
                        )
               nx=nx/n
               ny=ny/n

               ; adjust origin of this instance
               ox = (car  obj->xy)
               oy = (cadr obj->xy)
               cx = ((car  (car obj->bBox)) + (car  (cadr obj->bBox)))/2
               cy = ((cadr (car obj->bBox)) + (cadr (cadr obj->bBox)))/2
               nx = nx
               ny = ny
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
      )
    moved
    )
  )

; Return the sum of x:y of all fanout of a net and the number of
; fanouts.  Ignore connections to excludeInst.
(defun NetAverageXY (net excludeInst)
  (let (n nx ny bbox x0 y0 x1 y1)
    n=0
    nx=0
    ny=0
    (foreach pin net->pins
       bbox = pin->fig->bBox
       x0 = (car  (car  bbox))
       y0 = (cadr (car  bbox))
       x1 = (car  (cadr bbox))
       y1 = (cadr (cadr bbox))
       n  = n+1
       nx = nx+(x0+x1)/2
       ny = ny+(y0+y1)/2
       )
    (foreach term net->instTerms
      (cond (term->inst!=excludeInst
             bbox = term->inst->bBox
             x0 = (car  (car  bbox))
             y0 = (cadr (car  bbox))
             x1 = (car  (cadr bbox))
             y1 = (cadr (cadr bbox))
             n  = n+1
             nx = nx+(x0+x1)/2
             ny = ny+(y0+y1)/2
             )
            )
      )
    (list n nx ny)
    )
  )
