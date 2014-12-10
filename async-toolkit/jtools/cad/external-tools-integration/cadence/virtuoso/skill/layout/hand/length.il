; Wire length checking functions.  Use net connectivity as a hint, but
; use overlaps to find wiring without connectivity as well.
; Recursively accumulates lengths in subcells.
;
; NOTE: Needs cleanup, refactoring, optimization.  There are at least
; 3 other codebases that check for long wires!

; Report total wirelengths of nets.  Optionally provide the net name,
; or a regular expression.  Optionally length cutoff, select top-level
; shapes, enable recursion.
(defun ReportNetLengths
  (@key (CV (geGetEditCellView)) ; cell view
        (minLength 0) ; minimum length to report
        (rex  nil)    ; regular expression to pick nets
        (name nil)    ; a single net name to evaluate
        (select nil)  ; select top-level nets for debugging
        (recurse t)   ; recurse into subcells
        )
  (let (nets Vdd GND file lines line cachelength cacheleaf done_polygons)
    (cond (rex
           (rexCompile rex)
           nets = (setof net CV->nets (rexExecute net->name))
           )
          (name
           nets = (list (dbFindNetByName CV name))
           )
          (t
           nets = CV->nets
           )
          )
    GND = (dbFindNetByName CV "GND")
    Vdd = (dbFindNetByName CV "Vdd")
    (when (length nets)>100 (printf "# Evaluating %d nets\n" (length nets)))
    cachelength = (makeTable "cachelength" nil)
    cacheleaf   = (makeTable "cacheleaf"   nil)
    done_polygons = 0
    (foreach net (sort nets 'CompareNetNames)
      (unless net==Vdd || net==GND
        length = (NetLength net ?select select ?recurse recurse)
        (when length>minLength
          line = (list net->name length)
          (printf "%s %g\n" (car line) (cadr line))
          lines = (cons line lines)
          )
        )
      )
    file = (outfile (sprintf nil "%s.lengths" CV->cellName))
    lines = (sort lines 'CompareNetLengths)
    (foreach line lines (fprintf file "%s %g\n" (car line) (cadr line)))
    (close file)
    (when done_polygons>0
      (printf "WARNING: found %d polygons, results may be inaccurate!\n"
              done_polygons))
    )
  t
  )

; compare (name length) pairs in decreasing length
(defun CompareNetLengths (a b)
  (cadr a) >= (cadr b)
  )

; compare net names in increasing order
(defun CompareNetNames (a b)
  (strcmp a->name b->name)<=0
  )

; total length of all path/rect/polygon in a list of shapes
(defun TotalLengthOfObjects (shapes)
  (let (length points p0 p1 x0 y0 x1 y1)
    length = 0.0
    (foreach shape shapes
      (when (isMetalDrawing shape->layerName shape->purpose)
        (cond (shape->objType=="path"
               ; length of segments
               points = shape->points
               (for i 0 (length points)-2
                    p0 = (nth i   points)
                    p1 = (nth i+1 points)
                    x0 = (car  p0)
                    y0 = (cadr p0)
                    x1 = (car  p1)
                    y1 = (cadr p1)
                    length = length+(abs x1-x0)+(abs y1-y0)
                    )
               )
              (shape->objType=="rect"
               ; maximum width or height
               x0 = (leftEdge   shape->bBox)
               y0 = (bottomEdge shape->bBox)
               x1 = (rightEdge  shape->bBox)
               y1 = (topEdge    shape->bBox)
               length = length+(max x1-x0 y1-y0)
               )
              (shape->objType=="polygon"
               ; half the perimeter of a polygon
               points = shape->points
               (for i 0 (length points)-1
                    p0 = (nth i points)
                    p1 = (nth (mod (i+1) (length points)) points)
                    x0 = (car  p0)
                    y0 = (cadr p0)
                    x1 = (car  p1)
                    y1 = (cadr p1)
                    length = length+((abs x1-x0)+(abs y1-y0))/2
                    )
               )
              )
        )
      )
    length
    )
  )

; return list of wires/contacts by net
(defun GetNetWiring (net @key (pins t) (figs t) (contacts t))
  (let (found)
    found = nil
    (when pins (foreach pin net->pins
                        found = (cons pin->fig found)
                        ))
    (when figs (foreach fig net->figs
                        found = (cons fig found)
                        ))
    (when contacts (foreach term net->instTerms
                            (when term->inst->libName==TechLibName
                              found = (cons term->inst found)
                              )
                            ))
    found
    )
  )

; identify connected wiring (using "done" table to mark)
(defun GetConnectedWiring (obj)
  (let (connected bboxes lpps transform lpp bbox overlaps o)
    done[obj] = t
    connected = nil
    lpps = nil
    bboxes = nil
    (cond (obj->objType=="rect"
           lpps   = (list obj->lpp)
           bboxes = (list obj->bBox)
           )
          (obj->objType=="path"
           (foreach rect (nrConvertPathToRectList obj)
              lpps   = (cons obj->lpp lpps)
              bboxes = (cons rect bboxes)
              )
           )
          (obj->objType=="polygon"
           ; BUG: should convert polygons into rectangles
           done_polygons = done_polygons+1
           )
          (obj->objType=="inst"
           transform = (geGetInstTransform obj)
           (foreach shape obj->master->shapes
             lpps   = (cons shape->lpp lpps)
             bboxes = (cons (geTransformUserBBox shape->bBox transform) bboxes)
             )
           )
          )
    (for i 0 (length lpps)-1
      lpp  = (nth i lpps)
      bbox = (nth i bboxes)
      lpp  = (list (car lpp) "drawing")
      overlaps = (dbGetTrueOverlaps obj->cellView bbox lpp 1)
      (foreach overlap overlaps
        o = (if (atom overlap) overlap (car overlap))
        (when !done[o] && o->objType!="label" &&
              (o->objType!="inst" || o->libName==TechLibName)
          connected = (cons o connected)
          connected = (append (GetConnectedWiring o) connected)
          )
        )
      )
    connected
    )
  )

; Report total length of a net
(defun NetLength (net @key (select nil) (recurse t))
  (let (CV length explicit connected done leaf inst transform lpp bbox overlaps o)
    CV = net->cellView

    ; detect leaf cells (only trust their pin connectivity!)
    leaf = cacheleaf[CV]
    (unless leaf
      leaf = (length (setof x CV->instances x->libName=="gate" || x->libName=="stack"))
      cacheleaf[CV] = leaf
      )

    ; get top level explicitly connected shapes
    explicit = (GetNetWiring net ?contacts nil ?figs nil)
    done = (makeTable "done" nil)
    (foreach obj explicit done[obj]=t)

    ; add wires/contacts that overlap subcell pins
    (when leaf==0
      (foreach instTerm net->instTerms
        inst = instTerm->inst
        (when inst->libName!=TechLibName
          transform = (geGetInstTransform inst)
          (foreach pin instTerm->term->pins
            (unless pin->fig->objType=="rect"
                    (error "non-rectangular pin on %s = %s/%s\n"
                           net->name inst->name pin->net->name))
            bbox = (geTransformUserBBox pin->fig->bBox transform)
            lpp = (list (car pin->fig->lpp) "drawing")
            overlaps = (dbGetTrueOverlaps CV bbox lpp 1)
            (foreach overlap overlaps
              o = (if (atom overlap) overlap (car overlap))
              (when !done[o] && o->objType!="label" &&
                    (o->objType!="inst" || o->libName==TechLibName)
                done[o] = t
                explicit = (cons o explicit)
                )
              )
            )
          )
        )
      )
    connected = explicit

    ; expand connected list to include other overlapping shapes
    (foreach obj explicit
             connected = (append (GetConnectedWiring obj) connected)
             )

    ; select top level objects
    (when select
      (foreach obj connected (geSelectObject obj))
      )

    ; compute top-level wirelength
    length = (TotalLengthOfObjects connected)

    ; recursely accumulate connected subcells
    (when leaf==0 && recurse
      (foreach instTerm net->instTerms
        (when instTerm->inst->libName!=TechLibName
          length = length +
            (if cachelength[instTerm->term->net] cachelength[instTerm->term->net]
                cachelength[instTerm->term->net]=(NetLength instTerm->term->net))
          )
        )
      )

    ; return total length
    length
    )
  )
