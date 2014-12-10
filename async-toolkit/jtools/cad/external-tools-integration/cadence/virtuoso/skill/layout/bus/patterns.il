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
(defun DrawTracks (@key (upto 64) (guideInst nil) (guideInstName nil))
  (DrawChannels      nil nil nil nil
                     ?guideInst guideInst ?guideInstName guideInstName)
  (DrawChannelArrays nil nil nil nil nil nil
                     ?guideInst guideInst ?guideInstName guideInstName)
  t
  )

; obsolete
(defun DrawExtraTracks (@key (upto 64) (guideInstName nil) (guideInst nil))
  (printf "WARNING: DrawExtraTracks is obsolete -- DrawTracks is sufficient.\n")
  nil
  )

; obsolete
(defun DrawPartialArrays (@key (guideInstName nil))
  (printf "WARNING: DrawPartialArrays is obsolete -- DrawTracks is sufficient.\n")
  nil
  )

; draw double_default_e1of2/double_default_e1of4 arrays
(defun DrawDefaultTracks (@key (upto 64) (guideInstName nil) (guideInst nil) (pack_e1of2 nil))
 (let (chan str)
   (for hi 1 upto
        chan=(CreateDoubleDefaulte1of4 hi)
        str=(sprintf nil "double_default_e1of4[%d]" hi)
        (DrawChannels nil chan nil str ?guideInst guideInst ?guideInstName guideInstName)
        )
   (for hi 1 upto
        chan=(CreateDoubleDefaulte1of3 hi)
        str=(sprintf nil "double_default_e1of3[%d]" hi)
        (DrawChannels nil chan nil str ?guideInst guideInst ?guideInstName guideInstName)
        )
   (for hi 1 upto
        chan=(CreateDoubleDefaulte1of2 hi ?pack pack_e1of2)
        str=(sprintf nil "double_default_e1of2[%d]" hi)
        (DrawChannels nil chan nil str ?guideInst guideInst ?guideInstName guideInstName)
        )
   )
 t
 )

(defun CreateDoubleDefaulte1of4 (array)
  (let (channel defchan str)
    channel=(list "")
    (for i 0 array-1
         str=(sprintf nil "[%d]" i)
         (if (mod i 2)==0 then
             channel=(BuildDefChanList channel str 1
                                       inlv_e1of4_A i/2*TrackPitch nil ?use_dots nil)
             else
             channel=(BuildDefChanList channel str 1
                                       inlv_e1of4_B i/2*TrackPitch nil ?use_dots nil)
             )
         )
    defchan=(DefChan (cdr channel))
    )
  )

(defun CreateDoubleDefaulte1of3 (array)
  (let (channel defchan str)
    channel=(list "")
    (for i 0 array-1
         str=(sprintf nil "[%d]" i)
         (if (mod i 2)==0 then
             channel=(BuildDefChanList channel str 1
                                       inlv_e1of3_A i/2*TrackPitch nil ?use_dots nil)
             else
             channel=(BuildDefChanList channel str 1
                                       inlv_e1of3_B i/2*TrackPitch nil ?use_dots nil)
             )
         )
    defchan=(DefChan (cdr channel))
    )
  )

(defun CreateDoubleDefaulte1of2 (array @key (pack nil))
  (let (channel defchan str typeA typeB)
    (if pack then
      typeA=inlv_e1of2_A
      typeB=inlv_e1of2_B
    else
      typeA=inlv_e1of2_A2
      typeB=inlv_e1of2_B2
    )

    channel=(list "")
    (for i 0 array-1
         str=(sprintf nil "[%d]" i)
         (if (mod i 2)==0 then
             channel=(BuildDefChanList channel str 1
                                       typeA i/2*TrackPitch nil ?use_dots nil)
             else
             channel=(BuildDefChanList channel str 1
                                       typeB i/2*TrackPitch nil ?use_dots nil)
             )
         )
    defchan=(DefChan (cdr channel))
    )
  )
