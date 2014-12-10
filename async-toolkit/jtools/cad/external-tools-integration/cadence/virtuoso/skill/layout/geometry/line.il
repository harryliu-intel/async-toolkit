; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$


(defun LineIsManhattan ( Line )
  ( or ( LineIsSegmentVertical Line )
       ( LineIsSegmentHorizontal Line ) ) )

(defun LineIsSegmentVertical ( SegmentPoints )
  (let (
	( Point0 ( car SegmentPoints ) )
	( Point1 ( cadr SegmentPoints ) ) )
    (let (
	  ( X0 ( car Point0 ) )
	  ( X1 ( car Point1 ) ) )
      ( equal X0 X1 ) ) ) )

(defun LineIsSegmentHorizontal ( SegmentPoints )
  (let (
	( Point0 ( car SegmentPoints ) )
	( Point1 ( cadr SegmentPoints ) ) )
    (let (
	  ( Y0 ( cadr Point0 ) )
	  ( Y1 ( cadr Point1 ) ) )
      ( equal Y0 Y1 ) ) ) )

(defun LineGetSegmentMaxX ( SegmentPoints )
  (let (
	( Point0 ( car SegmentPoints ) )
	( Point1 ( cadr SegmentPoints ) ) )
    (let (
	  ( X0 ( car Point0 ) )
	  ( X1 ( car Point1 ) ) )
      (when ( and X0 X1 )
        ( max X0 X1 ) ) ) ) )

(defun LineGetSegmentMaxY ( SegmentPoints )
  (let (
        ( Point0 ( car SegmentPoints ) )
	( Point1 ( cadr SegmentPoints ) ) )
    (let (
	  ( Y0 ( cadr Point0 ) )
	  ( Y1 ( cadr Point1 ) ) )
      (when ( and Y0 Y1 )
        ( max Y0 Y1 ) ) ) ) )

(defun LineGetSegmentMinX ( SegmentPoints )
  (let (
	( Point0 ( car SegmentPoints ) )
	( Point1 ( cadr SegmentPoints ) ) )
    (let (
	  ( X0 ( car Point0 ) )
	  ( X1 ( car Point1 ) ) )
      (when ( and X0 X1 )
        ( min X0 X1 ) ) ) ) )

(defun LineGetSegmentMinY ( SegmentPoints )
  (let (
        ( Point0 ( car SegmentPoints ) )
	( Point1 ( cadr SegmentPoints ) ) )
    (let (
	  ( Y0 ( cadr Point0 ) )
	  ( Y1 ( cadr Point1 ) ) )
      (when ( and Y0 Y1 )
        ( min Y0 Y1 ) ) ) ) )


(defun LineMakeLineDataFromPoints ( Point1 Point2 )
  (let (
        ( X0 ( car Point1 ) )
        ( Y0 ( cadr Point1 ) )
        ( X1 ( car Point2 ) )
        ( Y1 ( cadr Point2 ) ) )
    (if ( equal X0 X1 )
        ( list nil X0 )
      (let (
            ( Slope ( quotient
                      ( float ( difference Y1 Y0 ) )
                      ( float ( difference X1 X0 ) ) ) ) )
        (let (
              ( Intercept ( float ( difference Y0 ( times Slope ( float X0 ) ) ) ) ) )
          ( list Slope Intercept ) ) ) ) ) )

(defun LineMakeLineDataFromPointList ( PointList )
  ( LineMakeLineDataFromPoints ( car PointList ) ( cadr PointList ) ) )

(defun LineSolveForX ( LineData Y )
  (unless ( equal ( float ( car LineData ) ) ( float 0 ) )
    (if ( car LineData )
        ( quotient
          ( float
            ( difference Y ( cadr LineData ) ) )
          ( car LineData ) ) 
      ( cadr LineData ) ) ) )
      
(defun LineSolveForY ( LineData X )
  (when ( car LineData )
    ( plus
      ( times 
        ( float ( car LineData ) )
        ( float X ) )
      ( float ( cadr LineData ) ) ) ) )

(defun LineGetClosestPointOnLineToPoint ( Point LineData ) 
  (let (
        ( PointX ( float ( car Point ) ) )
        ( PointY ( float ( cadr Point ) ) )
        ( Slope ( car LineData  ) )
        ( Intercept ( cadr LineData ) ) )  
    (if Slope
	(let (
	      ( Slope ( float Slope ) )
	      ( Intercept ( float Intercept ) )
	     )
        (let (
              ( RetX ( quotient
                       ( difference
                         ( plus
                           PointX
                           ( times PointY Slope) )
                         ( times Intercept Slope ) )
                       ( plus
                         ( times Slope Slope )
                         1 ) ) ) )
          ( list 
            RetX
            ( plus
              ( times RetX Slope )
              Intercept ) ) ) ) 
      ( list ( cadr LineData ) PointY ) ) ) )
    
(defun LineGetSegmentLength ( LineSegmentPoints )
  (let (
        ( Point0 ( car LineSegmentPoints ) )
        ( Point1 ( cadr LineSegmentPoints ) ) )
    (let (
          ( X0 ( float ( car Point0 ) ) )
          ( Y0 ( float ( cadr Point0 ) ) )
          ( X1 ( float ( car Point1 ) ) )
          ( Y1 ( float ( cadr Point1 ) ) ) )
      (let (
            ( XDiff ( difference X1 X0 ) )
            ( YDiff ( difference Y1 Y0 ) ) )
        ( sqrt
          ( plus 
            ( times XDiff XDiff )
            ( times YDiff YDiff ) ) ) ) ) ) )
 
(defun LinePointIsOnSegment ( Point LineSegmentPoints )
  (let (
        ( Point0 ( car LineSegmentPoints ) )
        ( Point1 ( cadr LineSegmentPoints ) ) )
    (let (
          ( X0 ( float ( car Point0 ) ) )
          ( Y0 ( float ( cadr Point0 ) ) )
          ( X1 ( float ( car Point1 ) ) )
          ( Y1 ( float ( cadr Point1 ) ) ) 
          ( X2 ( float ( car Point ) ) )
          ( Y2 ( float ( cadr Point ) ) ) )
      (let (
            ( SegmentMaxX ( max X0 X1 ) )
            ( SegmentMinX ( min X0 X1 ) )
            ( SegmentMaxY ( max Y0 Y1 ) )
            ( SegmentMinY ( min Y0 Y1 ) ) )
        ( and
          ( leqp X2 SegmentMaxX )
          ( geqp X2 SegmentMinX )
          ( leqp Y2 SegmentMaxY )
          ( geqp Y2 SegmentMinY )
          (if ( equal X0 X1 )
              ( equal X2 X0 )
            ( equal
              Y2
              ( LineSolveForY
                ( LineMakeLineDataFromPoints Point0 Point1 ) 
                X2 ) ) ) ) ) ) ) )
                
      
         
(defun LineGetClosestPointOnSegmentToPoint ( Point LineSegmentPoints )
  (let (
        ( SegmentLine ( LineMakeLineDataFromPoints
                        ( car LineSegmentPoints )
                        ( cadr LineSegmentPoints ) ) ) ) 
    (let (
          ( ClosePointOnSegmentLine ( LineGetClosestPointOnLineToPoint
                                      Point
                                      SegmentLine ) ) )    
          
      (if ( LinePointIsOnSegment ClosePointOnSegmentLine LineSegmentPoints )
          ClosePointOnSegmentLine
        (let (
              ( Point0 ( car LineSegmentPoints ) )
              ( Point1 ( cadr LineSegmentPoints ) ) )
          (let (
                ( PointPoint0Distance ( LineGetSegmentLength ( list Point0 Point ) ) )
                ( PointPoint1Distance ( LineGetSegmentLength ( list Point1 Point ) ) )
                )
            (if ( lessp PointPoint0Distance PointPoint1Distance )
                Point0
              Point1 ) ) ) ) ) ) )


(defun LineGetXRange ( Line )
  (when Line
    ( RangeCanonicalize 
      ( list ( caar Line ) ( caadr Line ) ) ) ) )

(defun LineGetYRange ( Line )
  (when Line
    ( RangeCanonicalize 
      ( list ( cadar Line ) ( cadadr Line ) ) ) ) )

         
(defun LineIsPoint ( Line ) 
  ( and ( eq ( caar Line ) ( caadr Line ) ) 
	( eq ( cadar Line ) ( cadadr Line ) )
	)      
)

(defun LineIsNoStart ( Line )  
  ( null ( car Line ) ) 
)

(defun LineIsNoEnd ( Line )
 ( null ( cadr Line ) )
)

(defun LineIsSegmentIncreasing ( Line )
  ( and ( leqp ( caar Line ) ( caadr Line ) ) ( leqp ( cadar Line ) ( cadadr Line ) ) )
)

;returns nil if no intersect
(defun LineGetIntersectX ( LineData1 LineData2 )
  (if ( null ( car LineData1 ) )
      (if ( null ( car LineData2 ) )
          ;both vertical
          (if ( equal ( cadr LineData1 ) ( cadr LineData2 ) )
              ( cadr LineData1 )
            nil )
        ;first is vertical
        ( cadr LineData1 ) )
    (if ( null ( car LineData2 ) )
        ;second is vertical
        ( cadr LineData2 )
      ;neither vertical              
      (if ( equal ( car LineData1 ) ( car LineData2 ) )
          ;parallel
          nil
          ;normal
        ( quotient ( difference ( cadr LineData2 ) ( cadr LineData1 ) )
                   ( difference ( car LineData1 ) ( car LineData2 ) ) ) ) ) ) )

(defun LineIntersect ( Line1 Line2 )
  (let (
        ( LineData1 ( LineMakeLineDataFromPointList Line1 ) )
        ( LineData2 ( LineMakeLineDataFromPointList Line2 ) ) )   
    ;parallel
    (if ( LineParallel Line1 Line2 )
        ( and 
          ( RangeIntersection ( LineGetXRange Line1 )
                              ( LineGetXRange Line2 ) )
          ( RangeIntersection ( LineGetYRange Line1 )
                              ( LineGetYRange Line2 ) ) )
      (let (
            ( IntersectX ( LineGetIntersectX LineData1 LineData2 ) ) )      
      ;check to see if intersection is in segment
        (if IntersectX
            (let (
                  ( IntersectY 
                    (if ( car LineData1 )
                        ( LineSolveForY LineData1 IntersectX )
                      ( LineSolveForY LineData2 IntersectX ) ) ) )               
              (if IntersectY
                  ( and ( RangeIsNumberInRangeClose IntersectX ( LineGetXRange Line1 ) 1e-9  )
                        ( RangeIsNumberInRangeClose IntersectX ( LineGetXRange Line2 ) 1e-9 )
                        ( RangeIsNumberInRangeClose IntersectY ( LineGetYRange Line1 ) 1e-9 )
                        ( RangeIsNumberInRangeClose IntersectY ( LineGetYRange Line2 ) 1e-9 ) ) ) ) ) ) ) ) )

(defun LineIntersectNoEndPointsAllowed ( Line1 Line2 ) 
  ( and ( LineIntersect Line1 Line2 )     
        ( not ( LineIntersectAtEndPoint Line1 Line2 ) ) ) )

(defun LineContainsLine ( BigLine LittleLine )
  ( and
    ( LineParallel BigLine LittleLine )
    ( RangeIsRangeInRangeClose ( LineGetXRange LittleLine )
                               ( LineGetXRange BigLine 1e-9 ) )
    ( RangeIsRangeInRangeClose ( LineGetYRange LittleLine )
                               ( LineGetYRange BigLine 1e-9 ) ) ) )

(defun LineParallel ( Line1 Line2 )
 (let (
        ( LineData1 ( LineMakeLineDataFromPointList Line1 ) )
        ( LineData2 ( LineMakeLineDataFromPointList Line2 ) ) )        
     ( equal (if ( car LineData1 ) ( abs ( car LineData1 ) ) nil )
             (if ( car LineData2 ) ( abs ( car LineData2 ) ) nil ) ) ) )
                                                
(defun LineIntersectAtEndPoint ( Line1 Line2 )
  ( or 
    ( LinePointIsOnSegment ( car Line1 ) Line2 )
    ( LinePointIsOnSegment ( cadr Line1 ) Line2 )
    ( LinePointIsOnSegment ( car Line2 ) Line1 )
    ( LinePointIsOnSegment ( cadr Line2 ) Line1 ) ) )

(defun LineOverlap ( Line1 Line2 )
 ( and
    ( LineParallel Line1 Line2 )
    ( LineIntersect Line1 Line2 ) ) )
