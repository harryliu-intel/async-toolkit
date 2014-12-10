; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

(defstruct SegmentTree segment left right cover segments shared )

(defun SegmentTreeCreate ( EndPoints  )
  (let (
        ( IntMap ( makeTable `foo nil ) )
        ( RealMap ( makeTable `foo nil ) )
        ( SegmentMap ( makeTable `foo nil ) )
        ( K 0 ) )
    ( foreach EndPoint
              ( ListUniq
                ( sort ( copy EndPoints )
                       (lambda ( X Y ) ( lessp X Y ) ) ) )
              ( setarray RealMap K EndPoint )
              ( setarray IntMap EndPoint K )
              ( setq K ( plus K 1 ) ) )
    ( SegmentTreeInit
      0
      ( difference K 1 )
      ( list IntMap RealMap SegmentMap ) )
    ) )

(defun SegmentTreeInit ( Left Right Data )
    (let (
          ( Mid 
            ( quotient ( plus Left Right ) 2 ) ) )
    ( make_SegmentTree 
      ?segment ( list Left Right )
      ?left 
      (when ( greaterp Right 
                       ( plus Left 1 ) )
        ( SegmentTreeInit 
          Left
          Mid 
          Data ) )
      ?right
      (when ( greaterp Right 
                       ( plus Left 1 ) )
        ( SegmentTreeInit
          Mid
          Right 
          Data ) )
      ?cover 0.0
      ?segments ( makeTable `foo 0 )
      ?shared Data ) ) )
  
(defun SegmentTreeGetMidPoint ( STree )
  ( quotient
    ( plus
      ( car STree->segment )
      ( cadr STree->segment ) ) ) )

(defun SegmentTreeGetIntMap ( STree )
  ( car STree->shared  ) )

(defun SegmentTreeGetRealMap ( STree )
  ( cadr STree->shared ) )

(defun SegmentTreeGetInternalSegment ( STree Segment )
  (let (
        ( Map ( SegmentTreeGetIntMap STree ) ) )
    ( mapcar 
      (lambda ( Point )
        ( arrayref Map Point ) )
      Segment
      ) ) )

(defun SegmentTreeGetSegmentSize ( STree )
  (let (
        ( TreeSegment STree->segment )
        ( Map ( SegmentTreeGetRealMap STree ) ) )
  ( difference
    ( arrayref Map ( cadr TreeSegment ) )
    ( arrayref Map ( car TreeSegment ) ) ) ) )

(defun SegmentTreeGetCover ( STree )
  STree->cover )

(defun SegmentTreeInsert ( STree Segment )
  (let (
        ( IntegerSegment ( SegmentTreeGetInternalSegment
                           STree
                           Segment ) ) )
    ( SegmentTreeImpl STree IntegerSegment nil ) ) )

(defun SegmentTreeDelete ( STree Segment )
  (let (
        ( IntegerSegment ( SegmentTreeGetInternalSegment
                           STree
                           Segment ) ) )
    ( SegmentTreeImpl STree IntegerSegment t ) ) )


(defun SegmentTreeImpl ( STree Segment BDelete )
  (let (
        ( TreeSegment
          STree->segment ) )
;    ( println ( list TreeSegment Segment  ) )
  (if ( and
        ( leqp ( car Segment ) ( car TreeSegment ) )
        ( geqp ( cadr Segment ) ( cadr TreeSegment ) ) )
      (if BDelete
          ( SegmentTreeRemoveSegment STree Segment )
        ( SegmentTreeAddSegment STree Segment ) )
    (let (
          ( MidPoint 
            ( quotient ( plus ( car TreeSegment )
                              ( cadr TreeSegment ) )
                       2 ) ) )
      (if ( lessp ( car Segment ) MidPoint )
          ( SegmentTreeImpl 
            STree->left
            Segment
            BDelete ) )
      (if ( lessp MidPoint ( cadr Segment ) )
          ( SegmentTreeImpl
            STree->right
            Segment
            BDelete ) ) ) )
  (cond (
         ( not ( SegmentTreeIsSegments STree ) )
         STree->cover = ( plus (cond (
                                      STree->left
                                      STree->left->cover )
                                     (
                                      0.0 ) )
                               (cond (
                                      STree->right
                                      STree->right->cover )
                                     (
                                      0.0 ) ) ) )
        (
         t
         STree->cover = ( SegmentTreeGetSegmentSize
                          STree ) ) ) ) )

;segments:
;this is not a set, but a list
(defun SegmentTreeIsSegments ( STree )
  ( exists
    Segment 
    STree->segments
    ( geqp ( arrayref STree->segments Segment ) 1 ) ) )

(defun SegmentTreeAddSegment ( STree Segment )
  ( setarray
    STree->segments
    Segment
    ( plus ( arrayref STree->segments Segment ) 1 ) ) )

(defun SegmentTreeRemoveSegment ( STree Segment )
  (let (
        ( Count
          ( arrayref STree->segments Segment ) ) )
    (cond (
           ( geqp Count 2 )
           ( setarray STree->segments Segment 
                      ( difference Count 1 ) ) )
          (
           t
           ( remove Segment STree->segments ) ) ) ) )
