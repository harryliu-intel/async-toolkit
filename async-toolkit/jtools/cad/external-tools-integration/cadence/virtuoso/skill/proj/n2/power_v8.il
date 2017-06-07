; Drop in a crapload of v8 to connect our m8 to m9 power grids.
; Slighty irregular due to SRAM's and such so it has to detect which
; power supply is on the m8 track.  Uses coordinates for the m9
; tracks.  Puts the vias in an overlay cell.  Vias avoid m9:block
; which is used in the PADS region.

; this is edited from proj/n1 directory because we changed the m9 pattern

(defun PowerV8 (@key (CV (geGetEditCellView))
                     (xp 1.68)  ; horizontal pitch for potential v8
                     (yp 0.798) ; vertical   pitch for potential v8 (NOTE: half the 1.596 pitch)
                     (xo 0.84)  ; horizontal offset
                     (yo 0.0)   ; vertical   offset
                     (checkerboard nil) ; for filler, only connect vias in a checkerboard
                     (supplies (list "GND" "Vdd" "GND" "VDDM")) ; repeats endlessly at constant wire pitch
                     )
  (let (num viaCV inst overhang via8 bbox x0 x1 y0 y1 s sx sy name net done filter rect via)
    num = (length supplies)
    inst = (dbFindAnyInstByName CV "vias")
    (when inst (dbDeleteObject inst))
    overhang = 0.52 ; minimum m8 overhang from center of v8
    viaCV = (dbOpenCellViewByType CV->libName (strcat CV->cellName "_vias") "layout" "maskLayout" "w")
    (dbCreatePRBoundary viaCV CV->prBoundary->points)
    (dbCreatePolygon viaCV (list "m9" "block") CV->prBoundary->points)
    via8 = (techFindViaDefByName (techGetTechFile CV) "via8")
    bbox = CV->prBoundary->bBox
    x0 = (car  (car  bbox))
    x1 = (car  (cadr bbox))
    y0 = (cadr (car  bbox))
    y1 = (cadr (cadr bbox))
    x0 = (floor   x0/xp)
    x1 = (ceiling x1/xp)
    y0 = (floor   y0/yp)
    y1 = (ceiling y1/yp)
    (for x x0 x1-1
         (for y y0 y1-1
              ; s    = (mod x-x0 num) ; left edge aligned
              s    = (mod (mod x num)+num num) ; origin aligned
              sx   = x*xp+xo
              sy   = y*yp+yo
              name = (nth s supplies)
              net  = (if name (dbMakeNet viaCV name) nil)
              done = nil
              filter = (lambda (shape) name==nil || shape->net->name==name)
              (when (!checkerboard || (mod (mod x 2)+2 2)==(mod (mod y 2)+2 2)) &&
                    (dbGetOverlaps CV (list sx:sy sx:sy) (list "m9" "block") 32)==nil
                (foreach purpose (list "pin" "vdd" "gnd" "drawing") ; precedence order
                  (unless done
                    (foreach overlap (dbGetOverlaps CV (list sx:sy sx:sy) (list "m8" purpose) 32)
                      rect = (TransformOverlapBBox overlap ?filter filter)
                      (when !done && rect &&
                         (leftEdge rect)<=sx-overhang && (rightEdge rect)>=sx+overhang
                         (printf "%s v8 at %g:%g to m8:%s\n" (if name name "nil") sx sy purpose)
                         via = (MyDrawVia viaCV via8 sx:sy 0.88 0.38 net)
                         done = t
                         )
                      )
                    )
                  )
                )
              )
         )
    (dbSave viaCV)
    (dbCreateInst CV viaCV "vias" 0:0 "R0")
    (dbSave CV)
    viaCV
    )
  )

; transform overlap shapes up to a top level bbox
(defun TransformOverlapBBox (a @key (filter nil))
  (let (b xy mosaic)
    (cond ((atom a) && (a->objType=="rect" || a->objType=="polygon" ||
                        a->objType=="path" || a->objType=="pathSeg")
           (if !filter || (apply filter (list a)) a->bBox nil)
           )
          (!(atom a) && ((car a)->objType=="inst" || (car a)->objType=="stdVia")
            b = (TransformOverlapBBox (cadr a) ?filter filter)
            (if b (dbTransformBBox b (dbGetInstTransform (car a))) nil)
            )
          (!(atom a) && (car a)->objType=="mosaic"
            ; (printf "WARNING: ignoring shapes in mosaic %s\n" (car a)->name)
            nil
            )
          (t nil)
          )
    )
  )

; Draw a via
(defun MyDrawVia (CV viaDef xy w h net)
  (let (via)
    via = (dbCreateVia CV viaDef xy "R0" (list (list "cutWidth" w) (list "cutHeight" h)))
    (dbReplaceProp via "autogen" "boolean" t)
    via->net=net
    via
    )
  )
