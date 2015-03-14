; Copyright 2003 Fulcrum Microsy\stems.  All rights reserved.
; $Id: //depot/sw/main/cad/external-tools-integration/cadence/virtuoso/skill/layout/bus/obs.il#1 $
; $DateTime: 2009/08/07 15:34:02 $
; $Author: stevemc $


(defun isBusObs (obj)
  (when (car obj->lpp)==(car BusObs) && (cadr obj->lpp)==(cadr BusObs) t)
)

(defun CreateObsProps (@key (shapes nil))
  (let (obs_m1 obs_m2 obs_m3 obs_m4 obs_m5 obs_m6 obs_m7 blk_pr)
    shapes = (if shapes==nil (geGetSelSet) shapes)
    (foreach obj shapes
             (cond ((isBusObs obj)
                                  blk_pr = (GetProp obj "ChopPRB" nil)
                                  obs_m1 = (GetProp obj "M1" nil)
                                  obs_m2 = (GetProp obj "M2" nil)
                                  obs_m3 = (GetProp obj "M3" nil)
                                  obs_m4 = (GetProp obj "M4" nil)
                                  obs_m5 = (GetProp obj "M5" nil)
                                  obs_m6 = (GetProp obj "M6" nil)
                                  obs_m7 = (GetProp obj "M7" nil)
                                  
                                  ; delete properties
                                  (dbDeletePropByName obj "ChopPRB")
                                  (dbDeletePropByName obj "M1")
                                  (dbDeletePropByName obj "M2")
                                  (dbDeletePropByName obj "M3")
                                  (dbDeletePropByName obj "M4")
                                  (dbDeletePropByName obj "M5")
                                  (dbDeletePropByName obj "M6")
                                  (dbDeletePropByName obj "M7")

                                  ; set new properties
                                  (dbReplaceProp obj "ChopPRB" "boolean" blk_pr)
                                  (dbReplaceProp obj "M1" "boolean" obs_m1)
                                  (dbReplaceProp obj "M2" "boolean" obs_m2)
                                  (dbReplaceProp obj "M3" "boolean" obs_m3)
                                  (dbReplaceProp obj "M4" "boolean" obs_m4)
                                  (dbReplaceProp obj "M5" "boolean" obs_m5)
                                  (dbReplaceProp obj "M6" "boolean" obs_m6)
                                  (dbReplaceProp obj "M7" "boolean" obs_m7)
                                  ))
             )
    )
  t
  )


(defun DrawObsLayers (view obj @key (CV (geGetEditCellView)))
  (let (points obs_m1 obs_m2 obs_m3 obs_m4 obs_m5 obs_m6 obs_m7 blk_pr)
  ; get props
  blk_pr = (GetProp obj "ChopPRB" nil)
  obs_m1 = (GetProp obj "M1" nil)
  obs_m2 = (GetProp obj "M2" nil)
  obs_m3 = (GetProp obj "M3" nil)
  obs_m4 = (GetProp obj "M4" nil)
  obs_m5 = (GetProp obj "M5" nil)
  obs_m6 = (GetProp obj "M6" nil)
  obs_m7 = (GetProp obj "M7" nil)
  ; get points
  (when obj->objType=="rect"
    points = obj->bBox
    (when blk_pr (SetBusProp (dbCreateRect CV (list "prBoundary" "block") points)) )
    (when obs_m1 (SetBusProp (dbCreateRect CV Metal1byLPP points)) )
    (when obs_m2 (SetBusProp (dbCreateRect CV Metal2byLPP points)) )
    (when obs_m3 (SetBusProp (dbCreateRect CV Metal3byLPP points)) )
    (when obs_m4 (SetBusProp (dbCreateRect CV Metal4byLPP points)) )
    (when obs_m5 (SetBusProp (dbCreateRect CV Metal5byLPP points)) )
    (when obs_m6 (SetBusProp (dbCreateRect CV Metal6byLPP points)) )
    (when obs_m7 (SetBusProp (dbCreateRect CV Metal7byLPP points)) )
  )
  (when obj->objType=="polygon"
    points = obj->points
    (when blk_pr (SetBusProp (dbCreatePolygon CV (list "prBoundary" "block") points)) )
    (when obs_m1 (SetBusProp (dbCreatePolygon CV Metal1byLPP points)) )
    (when obs_m2 (SetBusProp (dbCreatePolygon CV Metal2byLPP points)) )
    (when obs_m3 (SetBusProp (dbCreatePolygon CV Metal3byLPP points)) )
    (when obs_m4 (SetBusProp (dbCreatePolygon CV Metal4byLPP points)) )
    (when obs_m5 (SetBusProp (dbCreatePolygon CV Metal5byLPP points)) )
    (when obs_m6 (SetBusProp (dbCreatePolygon CV Metal6byLPP points)) )
    (when obs_m7 (SetBusProp (dbCreatePolygon CV Metal7byLPP points)) )
   )
  )
)

; draw obstructions (NOTE: upto is obsolete)
(defun DrawObstructions (@key (upto 128) (guideInstName nil) (guideInst nil)
                              (CV (geGetEditCellView)))
  (let (view obstype top pitch pattern flip track
             busKeepout busPatterns busUseDefaultPatterns)

    ; set guide instance
    guideInst = (GetGuideInst guideInst guideInstName ?CV CV)
    view = guideInst->master
    (when !view view = CV)

    ; draw OBS shapes
    (foreach shape view->shapes
             (when (isBusObs shape) (DrawObsLayers view shape) )
             )

    ; draw bus paths with BusPattern "obstruction"
    busKeepout = t
    busUseDefaultPatterns = nil
    busPatterns = (list (list "obstruction" node_TW))
    (DrawChannels      nil nil nil nil ?guideInst guideInst ?CV CV)
    (DrawChannelArrays nil nil nil nil nil nil ?guideInst guideInst ?CV CV)

    ; draw obstructM*
    (for i 2 7
         obstype=(sprintf nil "obstructM%d" i)
         (foreach shape view->shapes
                  pattern=(GetProp shape "BusPattern" nil)
                  (when shape->objType=="path" && pattern==obstype
                        pitch=(GetProp shape "BusArrayPitch" nil)
                        (if (mod i 2)==0 then
                            flip=(GetProp shape "BusFlipX" nil)
                            track=(caar shape->points)-TrackPitch/2
                            (if !flip then
                              top=(caadr CV->bBox)-track
                              else
                              top=track-(caar CV->bBox)
                              )
                            else
                            flip=(GetProp shape "BusFlipY" nil)
                            track=(cadar shape->points)-TrackPitch/2
                            (if !flip then
                              top=(cadadr CV->bBox)-track
                              else
                              top=track-(cadar CV->bBox)
                              )
                            )
                        )
                  )
         (when top && pitch
               (DrawChannelArrays nil node_TW "" 0:(floor top/pitch) nil obstype )
               )
         )
    )
  )
