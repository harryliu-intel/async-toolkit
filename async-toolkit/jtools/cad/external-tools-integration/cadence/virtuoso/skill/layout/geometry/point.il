; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

(defun PointDifference ( Point1 Point2 )
  ( list ( difference ( car Point1 ) ( car Point2 ) )
         ( difference ( cadr Point1 ) ( cadr Point2 ) ) ) )

(defun PointAdd ( Point1 Point2 )
  ( list ( plus ( car Point1 ) ( car Point2 ) )
         ( plus ( cadr Point1 ) ( cadr Point2 ) ) ) )

(defun PointGetDistance ( Point1 Point2 ) 
  (let (
        ( XDist ( difference ( car Point1 ) ( car Point2 ) ) )
        ( YDist ( difference ( cadr Point1 ) ( cadr Point2 ) ) ) )
    ( sqrt ( plus ( times XDist XDist )
                  ( times YDist YDist ) ) ) ) )

(defun PointManhattanDistance ( Point1 Point2 )
  (let (
        ( XDist ( difference ( car Point1 ) ( car Point2 ) ) )
        ( YDist ( difference ( cadr Point1 ) ( cadr Point2 ) ) ) )
    ( plus ( abs XDist ) ( abs YDist ) ) ) )

(defun PointScale ( Point S )
  ( list
    ( times ( car Point ) S )
    ( times ( cadr Point ) S ) ) )


(defun PointOnLayer ( cv point lpp )
 (let (on bbox overlaps shapes)
   on = nil;
   bbox = (list (car point)-MfgGrid:(cadr point)-MfgGrid (car point)+MfgGrid:(cadr point)+MfgGrid)
   overlaps = (dbGetTrueOverlaps cv bbox lpp 32)
   shapes = (GetShapesFromOverlaps overlaps)
   (foreach obj shapes
     (when obj->lpp==lpp
        on = t
     )
   )
   on
))

; apply a transform to a list of points
(defun TransformPoints (points transform)
  (when points
    (cons (geTransformUserPoint (car points) transform)
          (TransformPoints (cdr points) transform))
    )
  )
