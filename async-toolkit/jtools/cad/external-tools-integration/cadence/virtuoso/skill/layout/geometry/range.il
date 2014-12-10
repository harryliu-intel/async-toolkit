; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

(defun RangeMakeRange ( Left Size )
  ( list
    Left
    ( plus Left Size ) ) )

(defun RangeMakeRangeFromRight ( Right Size )
  ( list
    ( difference Right Size )
    Right ) )

(defun RangeMakeRangeFromCenter ( Center Radius )
  ( list ( difference Center Radius ) 
         ( plus Center Radius ) ) )

(defun RangeGetDistance ( Range1 Range2 )
  (cond (
         ( RangeDoIntersect Range1 Range2 1e-9 )
        0.0 )
        (
         ( min
           ( abs ( difference
                   ( cadr Range1 )
                   ( car Range2 ) ) )
           ( abs ( difference
                   ( car Range1 )
                   ( cadr Range2 ) ) ) ) ) ) )

(defun RangeExpandRange ( Range Radius )
  ( list ( difference ( car Range ) Radius ) 
         ( plus ( cadr Range ) Radius ) ) )

(defun RangeCanonicalize ( Range )
  ( list ( min ( car Range ) ( cadr Range ) )
         ( max ( car Range ) ( cadr Range ) ) ) )

(defun RangeShareBound ( Range1 Range2 )
  ( or ( equal ( car Range1 ) ( cadr Range2 ) )
       ( equal ( cadr Range1 ) ( car Range2 ) ) ) )

(defun RangeAddOffset ( Range Offset )
  ( list ( plus ( car Range ) Offset )
         ( plus ( cadr Range ) Offset ) ) )

(defun RangeGetSize ( Range )
  ( difference ( cadr Range ) ( car Range ) ) )

(defun RangeGetMidpoint ( Range )
  ( plus ( car Range )
         ( times 0.5 ( RangeGetSize Range ) ) ) )

(defun RangeUnion ( Range1 Range2 )
  (cond (
         ( null Range1 )
         Range2 )
        (
         ( null Range2 )
         Range1 )
        (
         t
         ( list ( min ( car Range1 ) ( car Range2 ) )
                ( max ( cadr Range1 ) ( cadr Range2 ) ) ) ) ) )

(defun RangeUnionMultiple ( Ranges )
  (let (
        ( Union ( car Ranges ) )
        ( TheRanges ( cdr Ranges ) )
        )
    ( foreach Range TheRanges
              ( setq Union ( RangeUnion Union Range ) ) )
    Union ) )

(defun RangeIntersection ( Range1 Range2 )
  (let (
        ( Ret
          ( list ( max ( car Range1 ) ( car Range2 ) )
                 ( min ( cadr Range1 ) ( cadr Range2 ) ) ) ) )
    (if ( lessp ( car Ret ) ( cadr Ret ) )
        Ret ) ) )

(defun RangeDoIntersect ( Range1 Range2 Eps )
  ( or ( RangeIsNumberInRangeClose ( car Range1 ) Range2  Eps )
       ( RangeIsNumberInRangeClose ( cadr Range1 ) Range2 Eps )
       ( RangeIsNumberInRangeClose ( car Range2 ) Range1 Eps ) ) )

(defun RangeGetOverlap ( Range1 Range2 )
  (let (
        ( Int ( RangeIntersection Range1 Range2 ) ) )
    (if Int ( RangeGetSize Int ) 0.0 ) ) )

(defun RangeIsNumberInRangeClose ( Value Range Eps )
  ( and ( leqp ( car Range ) ( plus Value Eps )  )         
        ( geqp ( plus ( cadr Range ) Eps ) Value ) ) )
 

(defun RangeIsRangeInRangeClose ( LittleRange BigRange Eps )
  ( and BigRange 
        ( and ( RangeIsNumberInRangeClose ( car LittleRange ) BigRange Eps )
              ( RangeIsNumberInRangeClose ( cadr LittleRange ) BigRange Eps ) ) ) )

(defun RangeIsRangeInRangeProperClose ( LittleRange BigRange Eps )
  ( and ( geqp ( difference ( car LittleRange ) ( car BigRange ) Eps ) )
        ( geqp ( difference ( cadr BigRange ) ( cadr LittleRange ) Eps ) ) ) )
        
(defun RangeIsRangeContainedInUnionOfRangesClose ( Range Ranges Eps )
   ( or 
      ( leqp ( cadr Range ) ( car Range ) )
      (let (
            ( IntersectingRange 
              ( car ( exists OtherRange Ranges 
                             ( or
                               ( and ( leqp ( car OtherRange ) ( car Range ) )
                                     ( greaterp ( cadr OtherRange ) ( car Range ) ) )
                               ( and ( lessp ( car OtherRange ) ( cadr Range ) )
                                     ( geqp ( cadr OtherRange ) ( cadr Range ) ) ) ) ) ) ) )
        (if IntersectingRange
            ( RangeIsRangeContainedInUnionOfRangesClose
              (cond (
                     ( lessp ( car Range ) ( car IntersectingRange ) )
                     ( list ( car Range ) ( car IntersectingRange ) ) )
                    (
                     ( list ( cadr IntersectingRange ) ( cadr Range ) ) ) )
              Ranges
              Eps ) ) ) ) )


(defun RangeGetAllRangesNotCovered ( RangeToCover Ranges )
  ( RangeGetAllRangesNotCoveredInner
    RangeToCover
    ( sort ( copy Ranges )
           (lambda ( X Y ) ( lessp ( car X ) ( car Y ) ) ) ) ) )

(defun RangeGetAllRangesNotCoveredInner ( RangeToCover
                                          SortedRanges )
  (let (
        ( Missed nil ) )
    ( foreach Range SortedRanges
              (when ( lessp ( car RangeToCover ) ( car Range ) )
                ( setq Missed ( cons ( list ( car RangeToCover )
                                            ( car Range ) )
                                     Missed ) ) )
              ( setq RangeToCover ( list ( max ( car RangeToCover )
                                               ( cadr Range ) )
                                         ( cadr RangeToCover ) ) ) )
    (when ( greaterp ( cadr RangeToCover ) ( car RangeToCover ) )
      ( setq Missed ( cons RangeToCover Missed ) ) )
    Missed ) )

(defun RangeGetCover ( Ranges )
  (let (
        ( SortedRanges
          ( sort ( copy Ranges )
                 (lambda ( X Y ) ( lessp ( car X ) ( car Y ) ) ) ) ) )
    (let (
          ( MaxRange
            ( list
              ( car ( car SortedRanges ) )
              ( cadr ( car ( last SortedRanges ) ) )
              ) ) )
    ( difference 
      ( RangeGetSize MaxRange )
      ( ListFindSum
        ( RangeGetAllRangesNotCoveredInner 
          MaxRange
          SortedRanges )
        (lambda ( Range )
          ( RangeGetSize Range ) ) ) ) ) ) )

      
