; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

(defun PolygonComparePointX ( Point0 Point1 )
  ( difference
    ( car Point0 )
    ( car Point1 ) ) )

(defun PolygonComparePointY ( Point0 Point1 )
  ( difference
    ( car ( cdr Point0 ) )
    ( car ( cdr Point1 ) ) ) )

(defun PolygonComparePointXY ( Point0 Point1 XMult YMult )
  (let (
        ( XResult ( PolygonComparePointX
                    Point0
                    Point1 ) ) )
    (if ( equal XResult 0 )
	( times
	  ( PolygonComparePointY
	    Point0
	    Point1 )
	  YMult )
      ( times XResult XMult ) ) ) )


(defun PolygonComparePointYX ( Point0 Point1 YMult XMult )
  (let (
        ( YResult ( PolygonComparePointY
                    Point0
                    Point1 ) ) )
    (if ( equal YResult 0 )
	( times
	  ( PolygonComparePointX
	    Point0
	    Point1 )
	  XMult )
      ( times YResult YMult ) ) ) )

(defun PolygonGetBottomLeftPoint ( PointList )
  (let (
        ( CurrRet ( car PointList ) )
        ( RemainingPoints ( cdr PointList ) ) )
    (while RemainingPoints
      (let (
            ( CurrPoint ( car RemainingPoints ) )
            )
        ( setq RemainingPoints ( cdr RemainingPoints ) )
        (when ( lessp ( PolygonComparePointYX
                        CurrPoint
                        CurrRet
                        1
                        1 ) 
                      0 )
          ( setq CurrRet CurrPoint ) ) ) ) 
    CurrRet ) )

(defun PolygonGetLeftBottomPoint ( PointList )
  (let (
        ( CurrRet ( car PointList ) )
        ( RemainingPoints ( cdr PointList ) ) )
    (while RemainingPoints
      (let (
            ( CurrPoint ( car RemainingPoints ) )
            )
        ( setq RemainingPoints ( cdr RemainingPoints ) )
        (when ( lessp ( PolygonComparePointXY
                        CurrPoint
                        CurrRet
                        1
                        1 ) 
                      0 )
          ( setq CurrRet CurrPoint ) ) ) ) 
    CurrRet ) )
          


(defun PolygonGetLeftTopPoint ( PointList )
  (let (
        ( CurrRet ( car PointList ) )
        ( RemainingPoints ( cdr PointList ) ) )
    (while RemainingPoints
      (let (
            ( CurrPoint ( car RemainingPoints ) )
            )
        ( setq RemainingPoints ( cdr RemainingPoints ) )
        (when ( lessp ( PolygonComparePointXY
                        CurrPoint
                        CurrRet
                        -1
                        -1 ) 
                      0 )
          ( setq CurrRet CurrPoint ) ) ) ) 
    CurrRet ) )


(defun PolygonGetTopLeftPoint ( PointList )
  (let (
        ( CurrRet ( car PointList ) )
        ( RemainingPoints ( cdr PointList ) ) )
    (while RemainingPoints
      (let (
            ( CurrPoint ( car RemainingPoints ) )
            )
        ( setq RemainingPoints ( cdr RemainingPoints ) )
        (when ( lessp ( PolygonComparePointYX
                        CurrPoint
                        CurrRet
                        -1
                        -1 ) 
                      0 )
          ( setq CurrRet CurrPoint ) ) ) ) 
    CurrRet ) )


(defun PolygonGetTopRightPoint ( PointList )
  (let (
        ( CurrRet ( car PointList ) )
        ( RemainingPoints ( cdr PointList ) ) )
    (while RemainingPoints
      (let (
            ( CurrPoint ( car RemainingPoints ) )
            )
        ( setq RemainingPoints ( cdr RemainingPoints ) )
        (when ( lessp ( PolygonComparePointYX
                        CurrRet
                        CurrPoint
                        -1
                        -1 ) 
                      0 )
          ( setq CurrRet CurrPoint ) ) ) ) 
    CurrRet ) )


(defun PolygonGetRightTopPoint ( PointList )
  (let (
        ( CurrRet ( car PointList ) )
        ( RemainingPoints ( cdr PointList ) ) )
    (while RemainingPoints
      (let (
            ( CurrPoint ( car RemainingPoints ) )
            )
        ( setq RemainingPoints ( cdr RemainingPoints ) )
        (when ( lessp ( PolygonComparePointXY
                        CurrRet
                        CurrPoint
                        -1
                        -1 ) 
                      0 )
          ( setq CurrRet CurrPoint ) ) ) ) 
    CurrRet ) )

(defun PolygonGetRightBottomPoint ( PointList )
  (let (
        ( CurrRet ( car PointList ) )
        ( RemainingPoints ( cdr PointList ) ) )
    (while RemainingPoints
      (let (
            ( CurrPoint ( car RemainingPoints ) )
            )
        ( setq RemainingPoints ( cdr RemainingPoints ) )
        (when ( lessp ( PolygonComparePointXY
                        CurrRet
                        CurrPoint
                        -1
                        1 ) 
                      0 )
          ( setq CurrRet CurrPoint ) ) ) ) 
    CurrRet ) )

(defun PolygonGetBottomRightPoint ( PointList )
  (let (
        ( CurrRet ( car PointList ) )
        ( RemainingPoints ( cdr PointList ) ) )
    (while RemainingPoints
      (let (
            ( CurrPoint ( car RemainingPoints ) )
            )
        ( setq RemainingPoints ( cdr RemainingPoints ) )
        (when ( lessp ( PolygonComparePointYX
                        CurrRet
                        CurrPoint
                        1
                        -1 ) 
                      0 )
          ( setq CurrRet CurrPoint ) ) ) ) 
    CurrRet ) )





(defun PolygonGetEdges ( PolygonPointList )
  (let (
        ( FirstPoint ( car PolygonPointList ) )
        ( PrevPoint ( car PolygonPointList ) )
        ( RemainingPoints ( cdr PolygonPointList ) )
        ( Ret nil ) )
    (while RemainingPoints
      (let (
            ( CurrPoint ( car RemainingPoints ) ) )
        ( setq RemainingPoints ( cdr RemainingPoints ) )
        ( setq Ret ( tconc Ret ( list PrevPoint CurrPoint ) ) )
        ( setq PrevPoint CurrPoint ) ) )
    (when ( car ( car PolygonPointList ) )
      ( setq Ret ( tconc Ret ( list PrevPoint FirstPoint ) ) ) )
    ( car Ret ) ) )


(defun PolygonEdgeHasY ( Edge YPos )
  (let ( 
        ( Y0 ( cadr ( car Edge ) ) )
        ( Y1 ( cadr ( cadr Edge ) ) ) )
        Y0=round(Y0/MfgGrid)*MfgGrid
        Y1=round(Y1/MfgGrid)*MfgGrid
        YPos=round(YPos/MfgGrid)*MfgGrid
    ( or
      ( and
	( lessp Y0 Y1 )
	( and
	  ( not ( greaterp Y0 YPos ) )
	  ( not ( greaterp YPos Y1 ) ) ) )
      ( and
	( greaterp Y0 Y1 )
	( and 
          ( not ( greaterp YPos Y0 ) )
	  ( not ( greaterp Y1 YPos ) ) ) ) ) ) )
	 

(defun PolygonEdgeHasX ( Edge XPos )
  (let (
        ( X0 ( car ( car Edge ) ) )
        ( X1 ( car ( cadr Edge ) ) ) )
        X0=round(X0/MfgGrid)*MfgGrid
        X1=round(X1/MfgGrid)*MfgGrid
        XPos=round(XPos/MfgGrid)*MfgGrid
    ( or
      ( and
	( lessp X0 X1 )
	( and
          ( not ( greaterp X0 XPos ) )
	  ( not ( greaterp XPos X1 ) ) ) )
      ( and
	( greaterp X0 X1 )
	( and
	  ( not ( greaterp XPos X0 ) )
	  ( not ( greaterp X1 XPos ) ) ) ) ) ) )

(defun PolygonGetEdgesThatHaveY ( PolygonEdges YPos )
  ( ListApplyFuncToListAndAccumulateNonNilResults
    PolygonEdges
    ( lambda
      ( Edge )
      when( PolygonEdgeHasY( Edge YPos)
	Edge ) )
    nil ) )
  

(defun PolygonGetEdgesThatHaveX ( PolygonEdges XPos )
  ( ListApplyFuncToListAndAccumulateNonNilResults
    PolygonEdges
    ( lambda
      ( Edge )
      ( when ( PolygonEdgeHasX Edge XPos)
	Edge ) )
    nil ) )


(defun PolygonGetEdgeAverageX ( PolygonEdge )
  ( quotient
    ( plus
      ( car ( car PolygonEdge ) )
      ( car ( car ( cdr PolygonEdge ) ) ) )
    2 ) )

(defun PolygonGetEdgeAverageY ( PolygonEdge )
  ( quotient
    ( plus
      ( car ( cdr ( car PolygonEdge ) ) )
      ( car ( cdr ( car ( cdr PolygonEdge ) ) ) ) )
    2 ) )

(defun PolygonGetEdgesAverageX ( Edges )
  (when Edges
    (let (
	  ( Sum 0 ) )
      ( foreach
	Edge
	Edges
	( setq
	  Sum
	  ( plus
	    Sum
	    ( PolygonGetEdgeAverageX
	      Edge ) ) ) )
      ( quotient
	Sum
	( length Edges ) ) ) ) )

(defun PolygonGetEdgesAverageY ( Edges )
  (when Edges
    (let (
	  ( Sum 0 ) )
      ( foreach
	Edge
	Edges
	( setq
	  Sum
	  ( plus
	    Sum
	    ( PolygonGetEdgeAverageY
	      Edge ) ) ) )
      ( quotient
	Sum
	( length
	  Edges ) ) ) ) )

(defun PolygonGetCenterXAtY ( PolygonEdges YPos )
  (let (
	( EdgesAtY ( PolygonGetEdgesThatHaveY
		     PolygonEdges
		     YPos ) ) )
    ( PolygonGetEdgesAverageX
      EdgesAtY ) ) )

(defun PolygonGetCenterYAtX ( PolygonEdges XPos )
  (let (
	( EdgesAtX ( PolygonGetEdgesThatHaveX
		     PolygonEdges
		     XPos ) ) )
    ( PolygonGetEdgesAverageY
      EdgesAtX ) ) )

(defun PolygonGetClosestEdgeAndIndexOfClosestEdge ( PolygonEdges Point )
  (let (
        ( CurrClosest ( car PolygonEdges ) )
        ( CurrClosestIndex 0 )
        ( CurrIndex 0 )
        ( RemainingEdges ( cdr PolygonEdges ) ) )
    (let (
          ( CurrClosestDist ( LineGetClosestPointOnSegmentToPoint
                              Point
                              CurrClosest ) ) )
      (while RemainingEdges
        (let (
              ( CurrEdge ( car RemainingEdges ) ) )
          ( setq RemainingEdges ( cdr RemainingEdges ) )
          (let (
                ( CurrDist ( LineGetClosestPointOnSegmentToPoint
                              Point
                              CurrEdge ) ) )
            (when ( lessp CurrDist CurrClosestDist )
              ( setq CurrClosest CurrEdge )
              ( setq CurrClosestDist CurrDist ) 
              ( setq CurrClosestIndex CurrIndex ) ) )
          ( setq CurrIndex ( plus CurrIndex 1 ) ) ) ) )
    ( list CurrClosest CurrClosestIndex ) ) )


(defun PolygonGetClosestEdge ( PolygonEdges Point )
  ( car ( PolygonGetClosestEdgeAndIndexOfClosestEdge PolygonEdges Point ) ) )

(defun PolygonGetIndexOfClosestEdge ( PolygonEdges Point )
  ( cadr ( PolygonGetClosestEdgeAndIndexOfClosestEdge PolygonEdges Point ) ) )


(defun PolygonGetRightMostEdge ( PolygonEdges )
  (let (
        ( CurrResultEdge ( car PolygonEdges ) )
        ( CurrResultEdgeMaxX ( LineGetSegmentMaxX ( car PolygonEdges ) ) )
        ( RemainingEdges ( cdr PolygonEdges ) ) )
    (while RemainingEdges
      (let (
            ( CurrEdge ( car RemainingEdges ) ) )
        ( setq RemainingEdges ( cdr RemainingEdges ) )
        (let (
              ( CurrEdgeMaxX ( LineGetSegmentMaxX CurrEdge ) )
              )
          (when ( or
                  ( greaterp CurrEdgeMaxX CurrResultEdgeMaxX )
                  ( greaterp
                    ( LineGetSegmentMinX CurrEdge )
                    ( LineGetSegmentMinX CurrResultEdge ) ) )
            ( setq CurrResultEdge CurrEdge )
            ( setq CurrResultEdgeMaxX CurrEdgeMaxX ) ) ) ) )
    CurrResultEdge ) )

(defun PolygonGetTopMostEdge ( PolygonEdges )
  (let (
        ( CurrResultEdge ( car PolygonEdges ) )
        ( CurrResultEdgeMaxY ( LineGetSegmentMaxY ( car PolygonEdges ) ) )
        ( RemainingEdges ( cdr PolygonEdges ) ) )
    (while RemainingEdges
      (let (
            ( CurrEdge ( car RemainingEdges ) ) )
        ( setq RemainingEdges ( cdr RemainingEdges ) )
        (let (
              ( CurrEdgeMaxY ( LineGetSegmentMaxY CurrEdge ) )
              )
          (when ( or
                  ( greaterp CurrEdgeMaxY CurrResultEdgeMaxY )
                  ( greaterp
                    ( LineGetSegmentMinY CurrEdge )
                    ( LineGetSegmentMinY CurrResultEdge ) ) )
            ( setq CurrResultEdge CurrEdge )
            ( setq CurrResultEdgeMaxY CurrEdgeMaxY ) ) ) ) )
    CurrResultEdge ) )

(defun PolygonGetLeftMostEdge ( PolygonEdges )
  (let (
        ( CurrResultEdge ( car PolygonEdges ) )
        ( CurrResultEdgeMinX ( LineGetSegmentMinX ( car PolygonEdges ) ) )
        ( RemainingEdges ( cdr PolygonEdges ) ) )
    (while RemainingEdges
      (let (
            ( CurrEdge ( car RemainingEdges ) ) )
        ( setq RemainingEdges ( cdr RemainingEdges ) )
        (let (
              ( CurrEdgeMinX ( LineGetSegmentMinX CurrEdge ) )
              )
          (when ( or
                  ( lessp CurrEdgeMinX CurrResultEdgeMinX )
                  ( lessp
                    ( LineGetSegmentMaxX CurrEdge )
                    ( LineGetSegmentMaxX CurrResultEdge ) ) )
            ( setq CurrResultEdge CurrEdge )
            ( setq CurrResultEdgeMinX CurrEdgeMinX ) ) ) ) )
    CurrResultEdge ) )


(defun PolygonGetHeight ( PolygonEdges )
  (let (
        ( BottomEdge
          ( PolygonGetBottomMostEdge PolygonEdges ) )
        ( TopEdge
          ( PolygonGetTopMostEdge PolygonEdges ) ) )
    ( difference 
      ( max ( cadar TopEdge )
            ( cadadr TopEdge ) )
      ( min ( cadar BottomEdge )
            ( cadadr BottomEdge ) ) ) ) )

(defun PolygonGetWidth ( PolygonEdges )
  (let (
        ( LeftEdge
          ( PolygonGetLeftMostEdge PolygonEdges ) )
        ( RightEdge
          ( PolygonGetRightMostEdge PolygonEdges ) ) )
    ( difference 
      ( max ( caar RightEdge )
            ( caadr RightEdge ) )
      ( min ( caar LeftEdge )
            ( caadr LeftEdge ) ) ) ) )


(defun PolygonGetBottomMostEdge ( PolygonEdges )
  (let (
        ( CurrResultEdge ( car PolygonEdges ) )
        ( CurrResultEdgeMinY ( LineGetSegmentMinY ( car PolygonEdges ) ) )
        ( RemainingEdges ( cdr PolygonEdges ) ) )
    (while RemainingEdges
      (let (
            ( CurrEdge ( car RemainingEdges ) ) )
        ( setq RemainingEdges ( cdr RemainingEdges ) )
        (let (
              ( CurrEdgeMinY ( LineGetSegmentMinY CurrEdge ) )
              )
          (when ( or
                  ( lessp CurrEdgeMinY CurrResultEdgeMinY )
                  ( lessp
                    ( LineGetSegmentMaxY CurrEdge )
                    ( LineGetSegmentMaxY CurrResultEdge ) ) )
            ( setq CurrResultEdge CurrEdge )
            ( setq CurrResultEdgeMinY CurrEdgeMinY ) ) ) ) )
    CurrResultEdge ) )

(defun PolygonGetRightMostEdgeAtY ( PolygonEdges YPos )
  ( PolygonGetRightMostEdge ( PolygonGetEdgesThatHaveY PolygonEdges YPos ) ) )

(defun PolygonGetTopMostEdgeAtX ( PolygonEdges XPos )
  ( PolygonGetTopMostEdge ( PolygonGetEdgesThatHaveX PolygonEdges XPos ) ) )
		   
(defun PolygonGetLeftMostEdgeAtY ( PolygonEdges YPos )
  ( PolygonGetLeftMostEdge ( PolygonGetEdgesThatHaveY PolygonEdges YPos ) ) )

(defun PolygonGetBottomMostEdgeAtX ( PolygonEdges XPos )
  ( PolygonGetBottomMostEdge ( PolygonGetEdgesThatHaveX PolygonEdges XPos ) ) )

                         
(defun PolygonGetHorizontalLeftAndRightBoundaryFromBottomAndTop ( BottomAndTop 
                                                                  BoundaryPolygonEdges )
  (let (
        ( Bottom ( car BottomAndTop ) )
        ( Top ( cadr BottomAndTop ) ) )
    (let (
          ( BoundaryLeft 
            ( max 
              ( LineGetSegmentMaxX 
                ( PolygonGetLeftMostEdgeAtY BoundaryPolygonEdges Bottom ) )
              ( LineGetSegmentMaxX 
                ( PolygonGetLeftMostEdgeAtY BoundaryPolygonEdges Top ) ) ) )
          ( BoundaryRight 
            ( min
              ( LineGetSegmentMinX 
                ( PolygonGetRightMostEdgeAtY BoundaryPolygonEdges Bottom ) )
              ( LineGetSegmentMinX 
                ( PolygonGetRightMostEdgeAtY BoundaryPolygonEdges Top ) ) ) ) )
      ( list BoundaryLeft BoundaryRight ) ) ) )

(defun PolygonGetVerticalTopAndBottomBoundaryFromLeftAndRight ( LeftAndRight
                                                                BoundaryPolygonEdges )
 (let (
       ( Left ( car LeftAndRight ) )
       ( Right ( cadr LeftAndRight ) ) )
   (let (
         ( BoundaryBottom
           ( max 
             ( LineGetSegmentMaxY 
               ( PolygonGetBottomMostEdgeAtX BoundaryPolygonEdges Left ) )
             ( LineGetSegmentMaxY 
               ( PolygonGetBottomMostEdgeAtX BoundaryPolygonEdges Right ) ) ) )
         ( BoundaryTop
           ( min
             ( LineGetSegmentMinY  
               ( PolygonGetTopMostEdgeAtX BoundaryPolygonEdges Left ) )
             ( LineGetSegmentMinY  
               ( PolygonGetTopMostEdgeAtX BoundaryPolygonEdges Right ) ) ) ) )
     ( list BoundaryBottom BoundaryTop ) ) ) )


(defun PolygonGetPointOnPolygonRightEdgeForY ( PolygonPoints YPos )
  ( list 
    ( LineSolveForX
      ( LineMakeLineDataFromPointList
        ( PolygonGetRightMostEdgeAtY
          ( PolygonGetEdges
            PolygonPoints )
          YPos ) )
      YPos )
    YPos ) )

(defun PolygonGetPointOnPolygonLeftEdgeForY ( PolygonPoints YPos )
  ( list 
    ( LineSolveForX
      ( LineMakeLineDataFromPointList
        ( PolygonGetLeftMostEdgeAtY
          ( PolygonGetEdges
            PolygonPoints )
          YPos ) )
      YPos )
    YPos ) )



(defun PolygonGetCenterOfGravity ( PolygonPoints )
  (when PolygonPoints
    (let (
          ( RemainingPoints PolygonPoints )
          ( CurrXSum 0 )
          ( CurrYSum 0 )
          ( Count 0 ) )
      (while RemainingPoints
        (let (
              ( CurrPoint ( car RemainingPoints ) ) )
          ( setq RemainingPoints ( cdr RemainingPoints ) )
          ( setq Count ( plus Count 1 ) )
          ( setq CurrXSum ( plus CurrXSum ( car CurrPoint ) ) )
          ( setq CurrYSum ( plus CurrYSum ( cadr CurrPoint ) ) ) ) )
      ( list
        ( quotient
          CurrXSum
          Count )
        ( quotient
          CurrYSum
          Count ) ) ) ) )


(defun PolygonIsLeftEdge ( Edge PolygonEdges )
  (unless ( LineIsSegmentHorizontal Edge )
    (let (
	  ( EdgeY ( PolygonGetEdgeAverageY Edge ) )
	  ( EdgeX ( PolygonGetEdgeAverageX Edge ) )
	  )
      (let (
	    ( CenterAtY ( PolygonGetCenterXAtY
			  PolygonEdges
			  EdgeY ) ) )
	( lessp
	  EdgeX
	  CenterAtY ) ) ) ) )

(defun PolygonIsRightEdge ( Edge PolygonEdges )
  (unless ( LineIsSegmentHorizontal Edge )
    ( not ( PolygonIsLeftEdge Edge PolygonEdges ) ) ) )

(defun PolygonIsBottomEdge ( Edge PolygonEdges )
  (unless ( LineIsSegmentVertical Edge )
    (let (
	  ( EdgeY ( PolygonGetEdgeAverageY Edge ) )
	  ( EdgeX ( PolygonGetEdgeAverageX Edge ) ) )
      (let (
	    ( CenterAtX ( PolygonGetCenterYAtX
			  PolygonEdges
			  EdgeX ) ) )
	( lessp
	  EdgeY
	  CenterAtX ) ) ) ) )

(defun PolygonIsTopEdge ( Edge PolygonEdges )
  (unless ( LineIsSegmentVertical Edge )
    ( not ( PolygonIsBottomEdge Edge PolygonEdges ) ) ) )

(defun PolygonMoveEdgeInX ( PolygonPoints Edge Delta )
  (when PolygonPoints
    ( ListApplyFuncToListAndAccumulateResults
      PolygonPoints
      ( lambda
	( Point Edge Delta )
	(if ( or
	      ( lessp
		( LineGetSegmentLength
                  ( list ( car Edge ) Point ) )
		0.0001 )
	      ( lessp
		( LineGetSegmentLength
                  ( list ( cadr Edge ) Point ) )
		0.0001 ) )
	    ( list
	      ( plus ( car Point ) Delta )
	      ( cadr Point ) )
	  Point ) )
      ( list Edge Delta ) ) ) )

(defun PolygonMoveEdgeInY ( PolygonPoints Edge Delta )
  (when PolygonPoints
    ( ListApplyFuncToListAndAccumulateResults
      PolygonPoints
      ( lambda
	( Point Edge Delta )
	(if ( or
	      ( lessp
		( LineGetSegmentLength
                  ( list ( car Edge ) Point ) )
                0.0001 )
	      ( lessp
		( LineGetSegmentLength
                  ( list ( cadr Edge ) Point ) )
                0.0001 ) )
	    ( list
	      ( car Point )
	      ( plus ( cadr Point ) Delta ) )
	  Point ) )
      ( list Edge Delta ) ) ) )

;gets smallest lwoer left block
(defun PolygonGetRectInsidePolygon ( PolygonPoints )  
  (let (
        ( BottomLeftPoint (  PolygonGetBottomLeftPoint PolygonPoints ) ) )
    ( list BottomLeftPoint ( PolygonGetNextPointToRightAndUp BottomLeftPoint PolygonPoints ) ) ) )
   

(defun PolygonGetNextPointToRightAndUp ( Point PolygonPoints )
 (let (
        ( PolygonEdges ( PolygonGetEdges PolygonPoints ) ) )
   ( car ( exists PolygonPoint PolygonPoints                 
                  ( and ( greaterp ( car PolygonPoint ) ( car Point ) )
                        ( greaterp ( cadr PolygonPoint ) ( cadr Point ) )
                        (let (
                              ( Rect ( list Point PolygonPoint ) ) )
                          (let (
                                ( BBoxEdges ( BBoxGetPolygonEdges Rect ) ) )
                            ( not ( exists BBoxEdge BBoxEdges
                                           ( exists Edge PolygonEdges                                             
                                                    ( and ( LineIntersect Edge BBoxEdge )
                                                          ( RectIsLineSegmentEverInRect Rect Edge nil ) 
                                                                   ) ) ) ) ) ) ) ) ) ) )

;corners count
(defun PolygonEdgesAbut ( PolygonEdges1 PolygonEdges2 )
  (if ( exists PolygonEdge1 PolygonEdges1
               ( exists PolygonEdge2 PolygonEdges2
                        (let ()
                          ( or 
                          ( LineIntersectAtEndPoint PolygonEdge1 PolygonEdge2 )
                          ( LineOverlap PolygonEdge1 PolygonEdge2 ) ) ) ) )
      t ) )


(defun PolygonGetShortestEdge ( PolygonPoints )
  ( ListFindMinimum
    ( PolygonGetEdges PolygonPoints )
    (lambda ( Edge )
      ( PointGetDistance 
        ( car Edge )
        ( cadr Edge ) ) ) ) )
  
(defun PolygonIsAngled ( Points )
  ( not ( forall Line ( PolygonGetEdges ( getq Shape points ) )
                 ( LineIsManhattan Line ) ) ) )

(defun PathCanonicalizePoints ( Points
                                @key 
                                ( Snap 0.0 )
                                )
  (let (
        ( Points
          ( ListUniq
            ( mapcar 
              (lambda ( Point )
                ( mapcar
                  (lambda ( Coord ) 
                    (cond ( ( equal 0.0 Snap ) Coord )
                          ( ( MathRoundToNearest Coord Snap ) ) ) )
                  Point ) )
              Points ) ) ) )
    (when ( equal ( car Points ) ( car ( last Points ) ) )
      ( setq Points ( cdr Points ) ) )
    Points
    ) )

(defun PolygonCanonicalizePoints ( Points
                                   @key 
                                   ( Snap 0.0 )
                                   )
  (let (
        ( Points
          ( PathCanonicalizePoints Points ?Snap Snap ) ) )

    (when ( equal ( car Points ) ( car ( last Points ) ) )
      ( setq Points ( cdr Points ) ) )
    Points
    ) )

(defun PolygonPerimeter ( PolygonPoints )
  ( ListFindSum
    ( PolygonGetEdges PolygonPoints )
    (lambda ( Edge )
      ( PointGetDistance 
        ( car Edge )
        ( cadr Edge ) ) ) ) )




(defun IsPointInPolygon (point points)
  (let (inside ptj 
	       (testX (car point)) 
	       (testY (cadr point)))
    (setq ptj (car (last points)))
    (foreach pti points
	     (when
	       (and
		 (nequal
		   (greaterp (yCoord pti) testY)
		   (greaterp (yCoord ptj) testY))
		 (lessp
		   testX
		   (plus
		     (quotient
		       (times
			 (difference (xCoord ptj) (xCoord pti))
			 (difference testY (yCoord pti))
			 )
		       (difference (yCoord ptj) (yCoord pti))
		       )
		     (xCoord pti)
		     )
		   )
		 )
	       (setq inside (null inside))
	       )
	     (setq ptj pti)
	     )
    inside
    )
  )


procedure(FracturePolygon(p)
   let( (bb xll xur xy xx yll yur yy pieces pp pnts)
      (if (p~>objType == "polygon") then        ; only fracture polygons
         (if (p~>nPoints > 3) then                ; not a triangle
            bb=p~>bBox
            ; printf("bBox=%L\n" bb)
            xll=xCoord(lowerLeft(bb))
            yll=yCoord(lowerLeft(bb))
            xur=xCoord(upperRight(bb))
            yur=yCoord(upperRight(bb))
            pieces=nil
            ; first look for places to cut vertically
            pnts=p~>points
            (while pnts
               xy=car(pnts)
               ; printf("xy=%L\n" xy)
               pnts=cdr(pnts)
               xx=xCoord(xy)
               (if (xx > xll && xx < xur) then
                  ; printf("vchop %L\n" list(xx:yll xx:yur) )
                  pieces=leChopShape(p
                     list(xx:yll xx:yur xll:yur xll:yll) t nil)
                  pnts=nil
               )
            )
            (if (pieces==nil) then
               ; didn't find vertical cut, look for horizontal cut
               pnts=p~>points
               (while pnts
                  xy=car(pnts)
                  pnts=cdr(pnts)
                  yy=yCoord(xy)
                  (if (yy > yll && yy < yur) then
                     ; printf("hchop %L\n" list(xll:yy xur:yy) )
                     pieces=leChopShape(p
                        list(xll:yy xur:yy xur:yll xll:yll) t nil)
                     pnts=nil
                  )
               )
            )
            (foreach pp pieces
               fracture(pp)
            )
         )
      )
   )
)

procedure( FractureAllPolygons(lib name view)
   prog( (c l s)
      c=dbOpenCellView(lib name view nil "an")
      (foreach s c~>shapes
         FracturePolygon(s)
      )
      dbSave(c)
      dbClose(c)
   )
)

