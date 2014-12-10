; Copyright 2006 Fulcrum Microsy\stems.  All rights reserved.
; $Id: //depot/sw/main/cad/external-tools-integration/cadence/virtuoso/skill/layout/bus/bus.il#4 $
; $DateTime: 2006/09/08 20:01:38 $
; $Author: lines $

; Flatten prBoundary shapes to overlap view, then select any overlaps
(defun FL_Overlap (@key (CV (geGetEditCellView)))
  (let (ovCV window instances pts
            pr_list count BB x0 y0 x1 y1 overlap_list)

    ; create overlap CV
    window = (geOpen ?lib CV->libName ?cell CV->cellName
                     ?view "overlap" ?viewType "maskLayout" ?mode "w")
    ovCV = (geGetWindowCellView window)

    ; load the full hierarchy
    (dbOpenHier CV 32)

    ; create shapes from PRBoundary boundary objects
    instPaths=(dbBoundaryQuery CV CV->bBox)
    (foreach item instPaths
      pts=TransformGetBBoxFromPath(item)
      (dbCreateRect ovCV BoundaryLPP pts)
    )

    ; make a list of prBoundaries shapes
    pr_list = (setof scell ovCV->shapes scell->lpp=BoundaryLPP )

    ; for each prBoundary, look for overlaps
    count = 0
    (geDeselectAllFig ovCV)
    (foreach obj1 pr_list

      ; shrink BBox slightly
      BB = obj1->bBox
      x0 = (car  (car  BB)) + 0.01
      y0 = (cadr (car  BB)) + 0.01
      x1 = (car  (cadr BB)) - 0.01
      y1 = (cadr (cadr BB)) - 0.01
      BB = (list x0:y0 x1:y1)

      ; find any overlapping prBoundary
      overlap_list = (dbGetOverlaps ovCV BB (list "prBoundary"))

      ; select overlapping shapes
      (cond ((length overlap_list)>1
             (foreach obj2 overlap_list (geSelectFig obj2))))
      )

    ; return number of overlaps
    (length (geGetSelSet))
    )
  )
