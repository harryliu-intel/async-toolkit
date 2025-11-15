; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

(defun BBoxCanonicalize ( BBox )  
  ( list ( list ( min ( caar BBox ) ( caadr BBox ) )
                ( min ( cadar BBox ) ( cadadr BBox ) ) )
         ( list ( max ( caar BBox ) ( caadr BBox ) )
                ( max ( cadar BBox ) ( cadadr BBox ) ) ) ) )

(defun BBoxExpandHorizontal ( BBox ExpandWidth )
  ( BBoxExpandHorizontalAsymmetric BBox ExpandWidth ExpandWidth ) )

(defun BBoxExpandHorizontalAsymmetric ( BBox StartExpandWidth EndExpandWidth )
  ( BBoxCanonicalize
    (cond (
           ( lessp ( caar BBox ) ( caadr BBox ) )
           ( list ( list ( difference ( caar BBox ) StartExpandWidth )
                         ( cadar BBox )
                         )
                  ( list ( plus ( caadr BBox ) EndExpandWidth ) 
                         ( cadadr BBox ) ) ) )
          (
           ( list ( list ( plus ( caar BBox ) StartExpandWidth )
                         ( cadar BBox )
                         )
                  ( list ( difference ( caadr BBox ) EndExpandWidth ) 
                         ( cadadr BBox ) ) ) ) ) ) )
  

(defun BBoxExpandVertical ( BBox ExpandWidth )
  ( BBoxExpandVerticalAsymmetric BBox ExpandWidth ExpandWidth ) )

(defun BBoxExpandVerticalAsymmetric ( BBox StartExpandWidth EndExpandWidth )
  ( BBoxCanonicalize
    (cond (
           ( lessp ( cadar BBox ) ( cadadr BBox ) )
           ( list ( list ( caar BBox )
                         ( difference ( cadar BBox ) StartExpandWidth )
                         )
                  ( list ( caadr BBox )
                         ( plus ( cadadr BBox ) EndExpandWidth ) ) ) )
          (
           ( list ( list ( caar BBox )
                         ( plus ( cadar BBox ) StartExpandWidth )
                         )
                  ( list ( caadr BBox )
                         ( difference ( cadadr BBox ) EndExpandWidth ) ) ) ) ) ) )

(defun BBoxGetHeight ( BBox )
  ( difference ( cadadr BBox ) ( cadar BBox ) ) )

(defun BBoxGetWidth ( BBox )
  ( difference ( caadr BBox ) ( caar BBox ) ) )

(defun BBoxGetCenter ( BBox )
  (list ((car  (car BBox)) + (car  (cadr BBox)))/2
        ((cadr (car BBox)) + (cadr (cadr BBox)))/2)
  )

;round up to nearest grid spacing
(defun BBoxSnapToHorizontalGrid ( BBox GridSpacing )
  (let (
	( BoundWidth ( times ( ceiling ( quotient ( BBoxGetWidth BBox ) GridSpacing ) )
			     GridSpacing ) ) )          
    ( list ( car BBox )
	   ( list
	     ( plus ( caar BBox ) BoundWidth )
	     ( cadadr BBox ) ) ) ) )	     

(defun BBoxIsPoint ( BBox )
  ( and ( eqv ( caar BBox ) ( caadr BBox ) )
        ( eqv ( cadar BBox ) ( cadadr BBox ) ) ) )


(defun BBoxClose ( B1 B2 Eps )
  ( and ( lessp ( abs ( difference ( caar B1 ) ( caar B2 ) ) ) Eps )
        ( lessp ( abs ( difference ( caadr B1 ) ( caadr B2 ) ) ) Eps )
        ( lessp ( abs ( difference ( cadar B1 ) ( cadar B2 ) ) ) Eps )
        ( lessp ( abs ( difference ( cadadr B1 ) ( cadadr B2 ) ) ) Eps ) ) )


(defun BBoxGetPolygonEdges ( BBox ) 
  ( let (
	 ( LowerLeft  ( car BBox ) )
	 ( UpperRight ( cadr BBox ) )
        )  
    ( PolygonGetEdges ( list 
			LowerLeft 
			( list ( car UpperRight ) ( cadr LowerLeft  ) )
			UpperRight 
			( list ( car LowerLeft  ) ( cadr UpperRight ) )		      
		      )
    )
  )
)


(defun BBoxSnapOutwardToGrid ( BBox GridSpacing )
 (let (newgrid x1 y1 x2 y2)
  x1 = (caar BBox)
  y1 = (cadar BBox)
  x2 = (caadr BBox)
  y2 = (cadadr BBox)  
  newgrid = (list
              (list (floor (round 1000*x1)/(round 1000*GridSpacing))*GridSpacing 
                (floor (round 1000*y1)/(round 1000*GridSpacing))*GridSpacing )
              (list (ceiling (round 1000*x2)/(round 1000*GridSpacing))*GridSpacing 
                (ceiling (round 1000*y2)/(round 1000*GridSpacing))*GridSpacing )
            )
  newgrid
 )
)


(defun IsBBoxOnGrid ( BBox GridSpacing )
 (let ((ongrid t))
  (when (modulo (round 1000*(caar BBox)) (round 1000*GridSpacing)) != 0
    ongrid = nil
  )
  (when (modulo (round 1000*(cadar BBox)) (round 1000*GridSpacing)) != 0
    ongrid = nil
  )
  (when (modulo (round 1000*(caadr BBox)) (round 1000*GridSpacing)) != 0
    ongrid = nil
  )
  (when (modulo (round 1000*(cadadr BBox)) (round 1000*GridSpacing)) != 0
    ongrid = nil
  )
  ongrid
 ) 
)

(defun BBoxCombine ( @rest BBoxes )
  (let (
        ( Result nil ) )
    ( foreach BBox BBoxes
      (if BBox
          (if Result
              ( setq
                Result
                ( list
                  ( list ( min ( leftEdge Result ) ( leftEdge BBox ) )
                         ( min ( bottomEdge Result ) ( bottomEdge BBox ) ) )
                  ( list ( max ( rightEdge Result ) ( rightEdge BBox ) )
                         ( max ( topEdge Result ) ( topEdge BBox ) ) ) ) )
            ( setq Result BBox ) ) ) )
    Result ) )

; Geometric And of two bboxes, or nil if no overlap
(defun BBoxAnd (a b)
  (let (x0 y0 x1 y1)
    x0 = (max (car  (car  a))  (car  (car  b)) )
    y0 = (max (cadr (car  a))  (cadr (car  b)) )
    x1 = (min (car  (cadr a))  (car  (cadr b)) )
    y1 = (min (cadr (cadr a))  (cadr (cadr b)) )
    (if x1>=x0 && y1>=y0 (list x0:y0 x1:y1) nil)
    )
  )

; Find BBox of a list of points
(defun BBoxOfPoints (points)
  (let (x0 y0 x1 y1 x y)
    x0=(car  (car  points))
    y0=(cadr (car  points))
    x1=(car  (cadr points))
    y1=(cadr (cadr points))
    (foreach xy (cdr points)
             x=(car xy)
             y=(cadr xy)
             x0=(min x0 x)
             y0=(min y0 y)
             x1=(max x1 x)
             y1=(max y1 y)
             )
    (list x0:y0 x1:y1)
    )
  )

; Convert bBox to polygon
(defun BBoxToPolygon (bbox)
  (let (x0 y0 x1 y1)
    x0=(car  (car  bbox))
    y0=(cadr (car  bbox))
    x1=(car  (cadr bbox))
    y1=(cadr (cadr bbox))
    (list x0:y0 x1:y0 x1:y1 x0:y1)
    )
  )

; Convert to integer manufacturing grid for comparison purposes
(defun BBoxToInteger (bbox @key (grid MfgGrid))
  (list (round (car (car  bbox))/grid):(round (cadr (car  bbox))/grid)
        (round (car (cadr bbox))/grid):(round (cadr (cadr bbox))/grid)
        )
  )
