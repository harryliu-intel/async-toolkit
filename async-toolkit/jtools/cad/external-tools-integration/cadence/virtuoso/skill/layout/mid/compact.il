; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

/*
This module contains functions for horizontally compacting floorplan views.<br>

The primary function is <skill MidLevelCompactInstances>.<br>

Compaction just means the instances are moved horizontally so that there is no more room to squeeze around in the x direction.<br>

This is accomplished by moving each floorplan instance to the left until its' boundary box runs into boundary boxes of other cells.<br>

Boundary boxes are the bounding box of the biggest BoundaryLPP (prBoundary/drawing) shape in the cell.<br>

This could be done hierarchically from the bottom up for best results.<br>
This is used by <tool gen_mid_cells>.<br>
*/

(defun MidLevelCompactInstances ( CellView
                                  Instances
                                  BoundaryLPP
                                  XGrid )
  "For each instance, from left to right, move it left until it's BoundaryLPP bbox collides with another.  Snap to XGrid."
  (let (
        ( SortedInstances
          ( sort
            Instances
            (lambda ( X Y )
              ( lessp ( RectGetLeft ( getq X bBox ) )
                      ( RectGetLeft ( getq Y bBox ) ) ) ) ) ) )
    (let (
          ( Groups
            ( ListSplitOnPredicate
              SortedInstances
              (lambda ( X Y )
                ( or
                  ( not ( equal ( getq X cellName )
                                ( getq Y cellName ) ) )
                  ( not ( equal ( RectGetLeft ( getq X bBox ) )
                                ( RectGetLeft ( getq Y bBox ) ) ) ) ) ) ) ) )
      ( foreach Group Groups
                (let (
                      ( OffsetX
                        ( MidLevelGetOffsetX
                          CellView
                          Group
                          BoundaryLPP ) ) )
                  ( foreach Instance Group
                            ;move
                            ( dbMoveFig
                              Instance
                              nil
                              ( list -OffsetX:0 "R0" ) )

                            ;snap to grid
                            (when ( greaterp XGrid 0.0 )
                              ( dbSetq
                                Instance
                                ( list ( times
                                         XGrid
                                         ( ceiling 
                                           ( quotient
                                             ( car ( getq Instance xy ) )
                                             XGrid ) ) )
                                       ( cadr ( getq Instance xy ) ) )
                              xy )
                              )
                            

                            ) ) ) ) ) )

(defun MidLeveGetOffendingBBoxes ( BBox Instances Paths BoundaryLPP )
  ( setof
    OverlapBBox
    ( mapcar
      (lambda ( Path )
        (let (
              ( OverlapBBox
                ( TransformGetBBoxFromPath
                  Path ) ) )
          (when ( and 
                  ( not 
                    ( exists Instance Instances
                             ( equal 
                               ( car Path )
                               Instance ) ) )
                  ( leqp
                    ( RectGetLeft ( getq ( car Path ) bBox ) )
                    ( RectGetLeft BBox )
                    )
                  ( exists 
                    Instance
                    Instances
                    ( dbProduceOverlap 
                      ( getq Instance master )
                      ( dbTransformBBox
                        ( RectShrinkInX
                          ( RectShrinkInY
                            OverlapBBox
                            .005 )
                          .005 )
                        ( TransformGetInverseTransform 
                          ( getq Instance transform ) ) )
                      ( list 0 32 )
                      BoundaryLPP )
                    ) )
            OverlapBBox ) ) )
      Paths )
    OverlapBBox
 ) )


(defun MidLevelGetLPPs ( CellView
                        )
  ( ListRemoveDuplicatesNoTableElements
    ( append
      ( mapcar
        (lambda ( LPP )
          ( list ( getq LPP layerName )
                 ( getq LPP purpose ) ) )
        CellView->lpps )
      ( ListNonDestructiveMapCan
        (lambda ( SubInstance )
          ( MidLevelGetLPPs
            SubInstance->master
            ) )
        ( getq CellView instances ) ) ) ) )


(defun MidLevelGetBBoxes ( Instance
                           TheLPP )
  (let ( 
        ( LPP
          ( car ( exists LPP 
                         ( getq ( getq Instance master ) lpps )
                         ( and
                           ( equal LPP->layerName ( car TheLPP ) )
                           ( equal LPP->purpose ( cadr TheLPP ) ) ) ) ) ) )
    ( mapcar
      (lambda ( BBox )
        ( dbTransformBBox
          BBox
          ( getq Instance transform ) ) )
      ( append
        LPP->shapes~>bBox
        ( ListNonDestructiveMapCan
          (lambda ( SubInstance )
            ( MidLevelGetBBoxes
              SubInstance
              TheLPP ) )
          ( getq ( getq Instance master ) instances ) ) ) ) ) )

(defun MidLevelGetOffsetX ( CellView
                            Instances
                            BoundaryLPP )
  
  (cond (
         ( ListFindMinimum
           ( MidLevelGetLeftBBoxesForInstances
             CellView
             Instances
             BoundaryLPP
             )
           (lambda ( BBox )
             ( MidLevelGetOffsetXForBBox 
               CellView
               BBox
               Instances
               BoundaryLPP ) ) ) )
        (
         0.0 ) ) )

(defun MidLevelGetLeftBBoxesForInstances ( CellView
                                           Instances
                                           BoundaryLPP )
  ( setof
    BBox
    ( ListNonDestructiveMapCan
      (lambda ( Instance )
        ( MidLevelGetBBoxes
          Instance
          BoundaryLPP ) )
      Instances )
    (let (
          ( OffendingInstance
              ( caadr
                ( dbGetNeighbor 
                  CellView
                  ( RectShrinkInY
                    BBox
                    .005 )
                  "left"
                  BoundaryLPP
                  32 ) ) ) )
      ( println ( list "gingivitis" OffendingInstance ) )
      ( not 
        ( exists
          Instance
          Instances
          ( equal
            OffendingInstance
            Instance
            ) ) ) )
      ) )

(defun MidLevelGetLeftBBoxesForInstances ( CellView
                                           Instances
                                           BoundaryLPP )
  ( list
    ( ListApplyFuncToListAndAccumulateResult
      Instances
      (lambda ( Instance BBox ) 
        ( RectGetBoundRect 
          ( getq Instance bBox )
          BBox ) )
      nil
      nil ) ) )

(defun MidLevelGetOffsetXForBBox ( CellView
                                   BBox
                                   Instances
                                   BoundaryLPP )
    ( min
      (let (
            ( OverlapRet 
             ( dbProduceOverlap 
                CellView 
                ( RectShrinkInX
                  ( RectShrinkInY
                    BBox
                    .005 )
                  .005 )
                ( list 0 32 )
                BoundaryLPP ) ) )
        (let (
              ( OverlapBBoxes
                ( MidLeveGetOffendingBBoxes 
                  BBox
                  Instances
                  OverlapRet
                  BoundaryLPP
                  ) ) )
          ( difference
            ( RectGetLeft BBox )
            (cond (
                   ( ListFindMaximum 
                     OverlapBBoxes
                     `RectGetRight ) )
                  (
                   ( difference 0 1e99 )
                   ) )
            ) ) )
      (let (
            ( NeighborRet
              ( dbGetNeighbor 
                CellView
                ( RectShrinkInY
                  BBox
                  .005 )
                "left"
                BoundaryLPP
                32 ) ) )
        (cond (
               ( car NeighborRet ) )
              (
               0.0 ) ) ) ) )
