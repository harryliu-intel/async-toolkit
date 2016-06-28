; repair all vias in current CV
(defun RepairViasAndPaths (@key (CV (geGetEditCellView)))
  (let (n)
    n=0
    (foreach via CV->vias
             n = n+(RepairVia via ?CV CV)
             )
    (foreach path (setof a CV->shapes a->objType=="pathSeg" || a->objType=="path")
             n = n+(RepairPath path ?CV CV)
             )
    n
    )
  )

; repair a path
(defun RepairPath (path @key (CV (geGetEditCellView)))
  (let (n m w xy)
    n=0

    ; figure out metal number
    m = MetalNum[path->layerName]

    ; choose width
    (when m
      xy = (if path->objType=="path" (car path->points) path->beginPt)
      w = (GetTrackWidth m xy)
      (when w && path->width!=w
            (printf "Repair %s path from %g width to %g width at %g:%g\n" path->layerName path->width w
                    (car xy) (cadr xy)
                    )
            path->width=w
            (geSelectObject path)
            n++
            )
      )
    n
    )
  )

; repair one via
(defun RepairVia (via @key (CV (geGetEditCellView)))
  (let (n v wb wt viaDef params tech)
    n=0
    tech = (techGetTechFile CV)

    ; figure out via number
    v = ViaNum[(car via->viaHeader->viaDef->params)]
    (unless v (error "Can't figure out via number for %s\n" via->viaHeader->viaDefName))

    ; figure out x track width
    wb = (GetTrackWidth v   via->xy)
    wt = (GetTrackWidth v+1 via->xy)
    (when wb wb = (round wb/MfgGrid))
    (when wt wt = (round wt/MfgGrid))

    ; choose appropriate via
    viaName = viaTable[(list v wb wt)]
    (when viaName && viaName!=via->viaHeader->viaDefName
          (cond ((atom viaName)
                 viaDef = (techFindViaDefByName tech viaName)
                 params = nil
                 )
                (t
                 viaDef = (techFindViaDefByName tech (car viaName))
                 params = (cdr viaName)
                 viaName = (car viaName)
                 )
                ) 
          (printf "Repair %s to %s at %g:%g\n" via->viaHeader->viaDefName 
                  viaName (car via->xy) (cadr via->xy))
          newVia = (dbCreateVia CV viaDef via->xy via->orient params)
          newVia->net = via->net
          (dbDeleteObject via)
          (geSelectObject newVia)
          n=1
          )
    n
    )
  )
