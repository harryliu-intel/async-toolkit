; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

(defun PathGetPolygonPoints ( Path )
  (let (
        ( Width ( getq Path width ) )
        ( BeginExt ( getq Path beginExt ) )
        ( EndExt ( getq Path endExt ) )
        ( Points ( ListUniq ( getq Path points ) ) ) )
    ( PathGetPolygonPointsImpl
      Points
      BeginExt
      EndExt
      Width ) ) )


(defun PathGetPolygonPointsImpl ( Points
                                  BeginExt
                                  EndExt
                                  Width )
  (let (        
        ( Width ( times 0.5 Width ) )
        ( RevPoints ( reverse ( copy Points ) ) ) )
    ( append 
      ( PathGetLeftRightHandPointsForLine
        ( list ( car Points ) ( cadr Points ) )
        BeginExt
        Width )
      ( append 
        ( append
          ( PathGetRightHandedPoints
            Points
            Width )
          ( PathGetLeftRightHandPointsForLine
            ( list ( car RevPoints ) ( cadr RevPoints ) )
            EndExt
            Width ) )
        ( PathGetRightHandedPoints
          RevPoints
          Width ) )
      ) ) )

(defun PathGetRightHandedPoints ( Points                                 
                                  Width )
  ( mapcar 
    (lambda ( Corner )
      ( PathGetRightHandedPointForCorner
        Corner
        Width ) )
    ( ListRemoveLastNElements
      ( maplist
        (lambda ( List )
          ( list ( car List ) ( cadr List ) ( caddr List ) ) )
        Points )
      2 ) ) )
        
(defun PathGetRightHandedPointForCorner ( Corner 
                                          Width )
 (let (
       ( Line ( CornerGetEntryLine Corner ) )
       ( Point ( CornerGetCornerPoint Corner ) ) )
    (cond (
           ( LineIsSegmentHorizontal Line )
           (cond (
                  ( LineIsSegmentIncreasing Line )
                  (cond (
                         ( CornerTurnsRelativeLeft Corner )
                         ( PointAdd Point ( list -Width -Width ) ) )
                        ( 
                         ( PointAdd Point ( list Width -Width ) ) ) ) )
                 (
                  t
                  (cond (
                         ( CornerTurnsRelativeLeft Corner )
                         ( PointAdd Point ( list Width Width ) ) )
                        ( 
                         ( PointAdd Point ( list -Width Width ) ) ) ) ) ) )
          (
           t
           (cond (
                  ( LineIsSegmentIncreasing Line )
                  (cond (
                         ( CornerTurnsRelativeLeft Corner )
                         ( PointAdd Point ( list Width Width ) ) )
                        ( 
                         ( PointAdd Point ( list Width -Width ) ) ) ) )
                 (
                  t
                  (cond (
                         ( CornerTurnsRelativeLeft Corner )
                         ( PointAdd Point ( list -Width -Width ) ) )
                        ( 
                         ( PointAdd Point ( list -Width Width ) ) ) ) ) ) ) ) ) )
                  
  
    
(defun PathGetLeftRightHandPointsForLine ( Line
                                           Extension
                                           Width )
  (let (
        ( Point ( car Line ) )
        ( HLOffset ( list Extension -Width ) )
        ( HROffset ( list Extension Width ) ) )
    (cond (
           ( LineIsSegmentHorizontal Line )
           (cond (
                  ( LineIsSegmentIncreasing Line )
                  ( list 
                    ( PointDifference Point HLOffset )
                    ( PointDifference Point HROffset ) ) )
                 ( 
                  ( list 
                    ( PointAdd Point HLOffset )
                    ( PointAdd Point HROffset ) ) ) ) )
          (
           t
           (cond (
                  ( LineIsSegmentIncreasing Line )
                  ( list 
                    ( PointDifference Point ( reverse HLOffset ) )
                    ( PointDifference Point ( reverse HROffset ) ) ) )
                 ( 
                  ( list 
                    ( PointAdd Point ( reverse HLOffset ) )
                    ( PointAdd Point ( reverse HROffset ) ) ) ) ) ) ) ) )


(defun PathRoundPointsToGrid ( points @key (grid MfgGrid) )
 (let (x y newpoints)
  newpoints = nil
  (foreach point points
    x = (round 1/grid*(car point))*grid
    y = (round 1/grid*(cadr point))*grid
    newpoints = (cons (list x y) newpoints)
  )
  (reverse newpoints)
 )
)

(defun PathGetDistance ( Path )
  (letseq (
           ( Sum 0 )
           ( Points ( getq Path points ) )
           ( LastPoint ( car Points ) )
          )
    ( foreach
      Point
      ( cdr Points )
      ( setq Sum ( plus Sum ( PointGetDistance LastPoint Point ) ) )
      ( setq LastPoint Point ) )
    Sum ) )
