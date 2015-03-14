; Map from BusPattern string name to pattern structure.  All native
; patterns of the PDK should be included here.  Wide and sparse array
; patterns are parsed separately and can apply to all of these base
; patterns.  Try to put common patterns near the top for performance
; reasons.  Also supports user-defined busPatterns, a list of lists.
; The inner list contains the name and the pattern.  This busPatterns
; should be set in wires.il
(defun MapBusPattern (name @key (defaults busUseDefaultPatterns) (patterns busPatterns))
  (let (pattern)
    (when defaults pattern = (MapBusPatternPDK name))
    (unless pattern pattern = (cadr (car (setof p patterns name==(car p)))))
    pattern
    )
  )

; Should a pattern use double vias?
(defun RecommendDoubleVias (name)
  (cond (name=="e1of1" t)
        (name=="e1of2" t)
        (name=="e1of3" t)
        (name=="e1of4" t)
        (name=="1of1" t)
        (name=="1of2" t)
        (name=="1of3" t)
        (name=="1of4" t)
        (name=="e1of1d" t)
        (name=="e1of2d" t)
        (name=="e1of3d" t)
        (name=="e1of4d" t)
        (name=="ChanDft2" t)
        (t nil)
        )
  )

; draw bus wires using guide paths (NOTE: upto is obsolete)
(defun DrawTracks (@key (guideInst nil) (guideInstName nil) (CV (geGetEditCellView)))
  (DrawChannels      nil nil nil nil
                     ?guideInst guideInst ?guideInstName guideInstName ?CV CV)
  (DrawChannelArrays nil nil nil nil nil nil
                     ?guideInst guideInst ?guideInstName guideInstName ?CV CV)
  t
  )
