(defvar PowerGateCellName "lib.util.power.POWER_GATE.0")
(defvar PowerGateCV (dbOpenCellViewByType "lib.util.power" "lib.util.power.POWER_GATE.0" "floorplan"))


; shotgun power gate cells into floorplan
; doesn't avoid FILL mosaics yet
(defun PlacePwrGateCells (@key (CV (geGetEditCellView)))
  (let (gatenum gatename prb xpitch ypitch startx starty endx endy newx newy newinst)
    xpitch = PowerGridXPitch*32
    ypitch = PowerGridYPitch*16

    prb = (GetPrbound CV)
    startx = (caar prb->bBox)+xpitch/2
    starty = (cadar prb->bBox)+ypitch/2
    endx = (caadr prb->bBox)-xpitch/2
    endy = (cadadr prb->bBox)-ypitch/2
    newx = startx
    newy = starty
    gatenum = 0

    (while newx<endx
      (while newy<endy
        (when (IsPointInPolygon (list newx newy) prb->points)
	  gatename = (sprintf nil "pwrgate%d" gatenum)
          gatenum++
  	  newinst = (dbCreateInst CV PowerGateCV gatename (list newx newy) "R0")
          ;(printf "Placing pillar %s at %f %f\n" gatename newx newy)
	  newinst->status = "locked"
	  )
        newy = newy + ypitch
        )
      newy = starty
      newx = newx + xpitch
      )

    (printf "Placed %d power gate pillars\n" gatenum)
    (dbSave CV)
    t
    )
  )

(defun DeletePwrGateCells (@key (CV (geGetEditCellView)))
   (foreach inst CV->instances
     (when inst->cellName==PowerGateCellName
       (dbDeleteObject inst)
       )
     )
  )
