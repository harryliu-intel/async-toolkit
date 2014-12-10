; Copyright 2003 Fulcrum Microsy\stems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$



(defun RectApplyToCoords ( Rect Func )
  ( mapcar 
    (lambda ( Pt )
      ( mapcar
        Func
        Pt ) )
    Rect ) )

(defun RectGetWidth ( Rect )
  ( difference
    ( RectGetRight Rect )
    ( RectGetLeft Rect )
    )
  )

(defun RectGetHeight ( Rect )
  ( difference
    ( RectGetTop Rect )
    ( RectGetBottom Rect )
    )
  )

(defun RectGetLeft ( Rect )
  ( car
    ( car
      Rect
      )
    )
  )

(defun RectGetRight ( Rect )
  ( car
    ( car
      ( cdr
	Rect
	)
      )
    )
  )

(defun RectGetBottom ( Rect )
  ( car
    ( cdr
      ( car
	Rect
	)
      )
    )
  )

(defun RectGetTop ( Rect )
  ( car
    ( cdr
      ( car
	( cdr
	  Rect
	  )
	)
      )
    )
  )

(defun RectGetCenter ( Rect )
  
 ( list
   ( quotient
     ( plus
       ( RectGetLeft
	 Rect
	 )
       ( RectGetRight
	 Rect
	 )
       )
     2.0
     )
   ( quotient
     ( plus
       ( RectGetTop
	 Rect
	 )
       ( RectGetBottom
	 Rect
	 )
       )
     2.0
     )
   )
 )

(defun RectGetPoints ( Rect )
  ( list
    ( list
      ( RectGetLeft
	Rect
	)
      ( RectGetBottom
	Rect
	)
      )
    ( list
      ( RectGetLeft
	Rect
	)
      ( RectGetTop
	Rect
	)
      )
    ( list
      ( RectGetRight
	Rect
	)
      ( RectGetTop
	Rect
	)
      )
    ( list
      ( RectGetRight
	Rect
	)
      ( RectGetBottom
	Rect
	)
      )
    )
  )

(defun RectMakeRect ( LeftBound BottomBound Width Height )
  ( list
    ( list
      LeftBound
      BottomBound
      )
    ( list
      ( plus
	LeftBound
	Width
	)
      ( plus
	BottomBound
	Height
	)
      )
    )
  )

(defun RectMakeFromCenter ( CenterPos Width Height )
  (let ( 
	 ( halfwidth ( quotient
		       Width
		       2.0
		       )
		     )
	 ( halfheight ( quotient
			Height
			2.0
			)
		      )
	 ( CenterX ( car CenterPos ) )
	 ( CenterY ( car ( cdr CenterPos )  ) )
	 )
    (let (
          ( Left ( difference
                   CenterX
                   halfwidth
                   )
                 )
          ( Right ( plus
                    CenterX
                    halfwidth
                    )
                  )
          ( Bottom ( difference
                     CenterY
                     halfheight
                     )
                   )
          ( Top ( plus
                  CenterY
                  halfheight
                  )
                )
          )
      ( list
	( list
	  Left
	  Bottom
	  )
	( list
	  Right
	  Top
	  )
	)
      )
    )
  )


(defun RectMakeFromLeftBottom ( Left Bottom Width Height )
  ( list
    ( list
      Left
      Bottom
      )
    ( list
      ( plus
	Left
	Width
	)
      ( plus
	Bottom
	Height
	)
      )
    )
  )

(defun RectMakeFromRightTop ( Right Top Width Height )
  ( list
    ( list
      ( difference
	Right
	Width
	)
      ( difference
	Top
	Height
	)
      )
    ( list
      Right
      Top
      )
    )
  )

(defun RectExpandRight ( Rect HowMuch )
  ( list
    ( car Rect )
    ( list ( plus ( RectGetRight Rect ) HowMuch )
           ( RectGetTop Rect ) ) ) )

(defun RectShrinkInY ( Rect HowMuch )
  ( list
    ( list
      ( RectGetLeft
	Rect
	)
      ( plus
	( RectGetBottom
	  Rect
	  )
	HowMuch
	)
      )
    ( list
      ( RectGetRight
	Rect
	)
      ( difference
	( RectGetTop
	  Rect
	  )
	HowMuch
	)
      )
    )
  )

(defun RectShrinkInX ( Rect HowMuch )
  ( list
    ( list
      ( plus
        ( RectGetLeft
          Rect )
        HowMuch )
      ( RectGetBottom
        Rect ) )
    ( list
      ( difference
        ( RectGetRight
          Rect )
        HowMuch )
      ( RectGetTop
        Rect  ) ) ) )

(defun RectGetBoundRect ( Rect0 Rect1 )
  ( cond
    (
     ( null Rect0 )
     Rect1
     )
    (
     ( null Rect1 )
     Rect0
     )
    ( 
     t
     ( list
       ( list
	 ( min
	   ( RectGetLeft
	     Rect0
	     )
	   ( RectGetLeft
	     Rect1
	     )
	   )
	 ( min
	   ( RectGetBottom
	     Rect0
	     )
	   ( RectGetBottom
	     Rect1
	     )
	   )
	 )
       ( list
	 ( max
	   ( RectGetRight
	     Rect0
	     )
	   ( RectGetRight
	     Rect1
	     )
	   )
	 ( max
	   ( RectGetTop
	     Rect0
	     )
	   ( RectGetTop
	     Rect1
	     )
	   )
	 )
       )
     )
    )
  )

(defun RectIsRectInRect ( ContainedRect ContainingRect )
  ( RectRectsAreEqual 
    ( RectGetBoundRect ContainedRect ContainingRect ) 
    ContainingRect 
    )
  )

(defun RectRectsAreEqual ( Rect0 Rect1 )
  ( and
    ( and
      ( equal
	( RectGetLeft
	  Rect0
	  )
	( RectGetLeft
	  Rect1
	  )
	)
      ( equal
	( RectGetBottom
	  Rect0
	  )
	( RectGetBottom
	  Rect1
	  )
	)
      )
    ( and
      ( equal
	( RectGetRight
	  Rect0
	  )
	( RectGetRight
	  Rect1
	  )
	)
      ( equal
	( RectGetTop
	  Rect0
	  )
	( RectGetTop
	  Rect1
	  )
	)
      )
    )
  )

(defun RectGetDistanceFromRectToSurroundingRect ( InnerRect OuterRect )
  ( ListFindMinimum
    ( list
      ( difference ( RectGetLeft InnerRect )
                   ( RectGetLeft OuterRect ) )
      ( difference ( RectGetRight OuterRect )
                   ( RectGetRight InnerRect ) )
      ( difference ( RectGetBottom InnerRect )
                   ( RectGetBottom OuterRect ) )
      ( difference ( RectGetTop OuterRect )
                   ( RectGetTop InnerRect ) ) )
    nil ) )

(defun RectGetDistanceFromRectToRect ( Rect1 Rect2 )
  (let (
        ( YIntersect ( RangeDoIntersect
                       ( RectGetYRange Rect1 ) 
                       ( RectGetYRange Rect2 )
                       1e-9 ) )
        ( XIntersect ( RangeDoIntersect
                       ( RectGetXRange Rect1 ) 
                       ( RectGetXRange Rect2 )
                       1e-9 ) ) )
    (if YIntersect
        (if XIntersect
            0.0
          ( min
            ( abs ( difference ( RectGetLeft Rect2 ) ( RectGetRight Rect1 ) ) )
            ( abs ( difference ( RectGetLeft Rect1 ) ( RectGetRight Rect2 ) ) ) ) )
      (if XIntersect
          ( min ( abs ( difference ( RectGetBottom Rect2 ) ( RectGetTop Rect1 ) ) )
                ( abs ( difference ( RectGetBottom Rect1 ) ( RectGetTop Rect2 ) ) ) )
        ( max
          ( min
            ( abs ( difference ( RectGetBottom Rect2 ) ( RectGetTop Rect1 ) ) )
            ( abs ( difference ( RectGetBottom Rect1 ) ( RectGetTop Rect2 ) ) ) )
          ( min
            ( abs ( difference ( RectGetLeft Rect2 ) ( RectGetRight Rect1 ) ) )
            ( abs ( difference ( RectGetLeft Rect1 ) ( RectGetRight Rect2 ) ) ) ) )
 ) ) ) )

(defun RectIsRectInRectClose ( ContainedRect ContainingRect Eps )
  ( RectAreRectsClose
    ( RectGetBoundRect ContainedRect ContainingRect ) 
    ContainingRect 
    Eps ) )

(defun RectAreRectsClose ( B1 B2 Eps )
  ( and ( lessp ( abs ( difference ( caar B1 ) ( caar B2 ) ) ) Eps )
        ( lessp ( abs ( difference ( caadr B1 ) ( caadr B2 ) ) ) Eps )
        ( lessp ( abs ( difference ( cadar B1 ) ( cadar B2 ) ) ) Eps )
        ( lessp ( abs ( difference ( cadadr B1 ) ( cadadr B2 ) ) ) Eps ) ) )

(defun RectMoveRectInsideRect ( InnerRect OuterRect )
  (let (
        ( InnerLeft ( RectGetLeft InnerRect ) )
        ( InnerRight ( RectGetRight InnerRect ) )
        ( InnerBottom ( RectGetBottom InnerRect ) )
        ( InnerTop ( RectGetTop InnerRect ) )
        ( OuterLeft ( RectGetLeft OuterRect ) )
        ( OuterRight ( RectGetRight OuterRect ) )
        ( OuterBottom ( RectGetBottom OuterRect ) )
        ( OuterTop ( RectGetTop OuterRect ) ) 
        )
    ( list
      ( plus
	( if ( lessp
	       InnerLeft
	       OuterLeft
	       )
	    ( difference
	      OuterLeft
	      InnerLeft
	      )
	  0.0
	  )
	( if ( lessp
	       OuterRight
	       InnerRight
	       )
	    ( difference
	      OuterRight
	      InnerRight
	      )
	  0.0
	  )
	)
      ( plus
	( if ( lessp
	       InnerBottom
	       OuterBottom
	       )
	    ( difference
	      OuterBottom
	      InnerBottom
	      )
	  0.0
	  )
	( if ( lessp
	       OuterTop
	       InnerTop
	       )
	    ( difference
	      OuterTop
	      InnerTop
	      )
	  0.0
	  )
	)
      )
    )
  )

(defun RectIsPointInRect ( Rect Point IsClosedRect )

  (let (
	( Left ( RectGetLeft Rect ) )
	( Right ( RectGetRight Rect ) )
	( Bottom ( RectGetBottom Rect ) )
	( Top ( RectGetTop Rect ) )
	( PointX ( car Point ) )
	( PointY ( car ( cdr Point ) ) ) )
    ( and
      ( or
	( lessp Left PointX )
	( and
	  IsClosedRect
	  ( equal Left PointX ) ) )
      ( or
	( greaterp Right PointX )
	( and
	  IsClosedRect
	  ( equal Right PointX ) ) )
      ( or
	( lessp Bottom PointY )
	( and
	  IsClosedRect
	  ( equal Bottom PointY ) ) )
      ( or
	( greaterp Top PointY )
	( and
	  IsClosedRect
	  ( equal Top PointY ) ) ) ) ) )

(defun RectGetEdgesForPoint ( Rect Point )
  ( setof Edge ( BBoxGetPolygonEdges Rect ) 
          ( LinePointIsOnSegment Point Edge ) ) ) 

(defun RectIsLineSegmentEverInRect ( Rect Line IsClosedRect )
  ;if closed, a point must be in closed rect
  (if IsClosedRect
      ( or ( RectIsPointInRect Rect ( car Line ) t )
           ( RectIsPointInRect Rect ( cadr Line ) t ) )
  ;if open, a point must be in open rect, or
  ; line must go through an edge interseting edges for endpoints must be nonempty and disjoint
  ; 
    ( or ( RectIsPointInRect Rect ( car Line ) nil )
         ( RectIsPointInRect Rect ( cadr Line ) nil )              
         (if ( exists Edge ( BBoxGetPolygonEdges Rect )
                  ( LineIntersectNoEndPointsAllowed Line Edge ) )
             t )
         (let ( 
               ( Edges1 ( RectGetEdgesForPoint Rect ( car Line ) ) )
               ( Edges2 ( RectGetEdgesForPoint Rect ( cadr Line ) ) ) )
           ( and 
             Edges1
             Edges2
             ( null ( ListIntersect Edges1 Edges2 ) ) ) ) ) ) )
                  

(defun RectGetOverlapArea ( Rect0 Rect1 )
  ( times
    ( RangeGetOverlap 
      ( RectGetXRange Rect0 )
      ( RectGetXRange Rect1 ) )
    ( RangeGetOverlap 
      ( RectGetYRange Rect0 )
      ( RectGetYRange Rect1 ) ) ) )

(defun RectGetOverlap ( Rect0 Rect1 )
  (if ( RectDoRectsOverlapOpenRects Rect0 Rect1 )
      ( min
        ( RangeGetOverlap 
          ( RectGetXRange Rect0 )
          ( RectGetXRange Rect1 ) )
        ( RangeGetOverlap 
          ( RectGetYRange Rect0 )
          ( RectGetYRange Rect1 ) ) )
    0.0 ) )
       
(defun RectDoRectsOverlapOpenRects ( Rect0 Rect1 )
  ( and ( RectDoRectsOverlap Rect0 Rect1 )
        ( not ( RectDoRectsAbutClose Rect0 Rect1 1e-9 ) ) ) )

;corner's count
(defun RectDoRectsAbutClose ( Rect0 Rect1 Eps )
  ( or ( leqp ( abs ( difference ( RectGetLeft Rect0 ) ( RectGetRight Rect1 ) ) ) Eps )
       ( leqp ( abs ( difference ( RectGetRight Rect0 ) ( RectGetLeft Rect1 ) ) ) Eps )
       ( leqp ( abs ( difference ( RectGetBottom Rect0 ) ( RectGetTop Rect1 ) ) ) Eps )
       ( leqp ( abs ( difference ( RectGetTop Rect0 ) ( RectGetBottom Rect1 ) ) ) Eps ) ) )
       
(defun RectDoRectsOverlap ( Rect0 Rect1 )
  ( or
    ( not
      ( forall
	Point
	( RectGetPoints
	  Rect0 )
	( not
	  ( RectIsPointInRect
	    Rect1
	    Point
	    t ) ) ) )
    ( not
      ( forall
	Point
	( RectGetPoints
	  Rect1 )
	( not
	  ( RectIsPointInRect
	    Rect0
	    Point
	    t ) ) ) ) ) )
    

(defun RectIsLeftOnLambdaGrid ( Rect )
  ( equal
    ( round
      ( RectGetLeft
	Rect
	)
      )
    ( RectGetLeft
      Rect
      )
    )
  )

(defun RectIsRightOnLambdaGrid ( Rect )
  ( equal
    ( round
      ( RectGetRight
	Rect
	)
      )
    ( RectGetRight
      Rect
      )
    )
  )

(defun RectIsBottomOnLambdaGrid ( Rect )
  ( equal
    ( round
      ( RectGetBottom
	Rect
	)
      )
    ( RectGetBottom
      Rect
      )
    )
  )
(defun RectIsTopOnLambdaGrid ( Rect )
  ( equal
    ( round
      ( RectGetTop
	Rect
	)
      )
    ( RectGetTop
      Rect
      )
    )
  )

(defun RectGetArea ( Rect )
  ( times
    ( RectGetWidth
      Rect
      )
    ( RectGetHeight
      Rect
      )
    )
  )

(defun RectCompareRects ( Rect0 Rect1 )
  (if ( equal
        ( RectGetBottom
          Rect0
          )
        ( RectGetBottom
          Rect1
          )
        )
      ( lessp
        ( RectGetLeft
          Rect0
          )
        ( RectGetLeft
          Rect1
          )
        )
    ( lessp
      ( RectGetBottom
        Rect0
        )
      ( RectGetBottom
        Rect1
        )
      )
    )
  )


(defun RectGetXRange ( Rect )
  ( LineGetXRange
    Rect ) )

(defun RectGetYRange ( Rect )
  ( LineGetYRange
    Rect ) )

(defun RectMakeFromRanges ( XRange YRange )
  (when ( and XRange YRange )
    ( list ( list ( car XRange ) ( car YRange ) )
           ( list ( cadr XRange ) ( cadr YRange ) ) ) ) )
         
(defun RectGetIntersection ( Rect1 Rect2 )
  ( RectMakeFromRanges
    ( RangeIntersection ( RectGetXRange Rect1 ) ( RectGetXRange Rect2 ) )
    ( RangeIntersection ( RectGetYRange Rect1 ) ( RectGetYRange Rect2 ) ) ) )



(defun RectGetSortedXYRangePairs ( Rects )
  ( sort
    ( ListNonDestructiveMapCan
      (lambda ( Rect )
        ( list 
          ( list 
            ( caar Rect )
            ( RectGetYRange Rect )
            t
            )
          ( list 
            ( caadr Rect )
            ( RectGetYRange Rect )
            nil
            ) ) )
      Rects )
    (lambda ( X Y ) ( lessp ( car X ) ( car Y ) ) ) ) )

(defun RectGetAreaOfUnion ( Rects )
  (let (
        ( SortedXYRangePairs
          ( RectGetSortedXYRangePairs 
            Rects ) ) )
    (let (
          ( Area 0.0 )
          ( STree ( SegmentTreeCreate
                          ( ListNonDestructiveMapCan
                            `cadr
                            SortedXYRangePairs ) ) ) )

      (while SortedXYRangePairs
        (let (
              ( CurrXYRangePair ( car SortedXYRangePairs ) ) 
              ( NextXYRangePair ( cadr SortedXYRangePairs ) ) )
          (let (
                ( X ( car CurrXYRangePair ) )
                ( YRange ( cadr CurrXYRangePair ) )
                ( Insert ( caddr CurrXYRangePair ) )
                ( NextX ( car NextXYRangePair ) ) )
            (if Insert
                ( SegmentTreeInsert STree YRange ) 
              ( SegmentTreeDelete STree YRange ) )
            (when NextXYRangePair
              ( setq Area ( plus
                            Area
                            ( times
                              ( difference NextX X )
                              ( SegmentTreeGetCover STree ) ) ) ) )
            ( setq SortedXYRangePairs ( cdr SortedXYRangePairs ) ) ) ) )
Area
) ) )


(defun RectGetPolygonPoints ( Rect )
  ( list ( car Rect )
         ( list ( car ( cadr Rect ) ) ( cadr ( car Rect ) ) )
         ( cadr Rect )
         ( list ( car ( car Rect ) ) ( cadr ( cadr Rect ) ) )
  )
)
