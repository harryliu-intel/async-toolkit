; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$


;aligns reference point of Fig to Point
(defun FigMove ( Fig 
                 Point
                 @key
                 ( Orient nil )
                 ( Align "ll" )
                 )
  ;first orient
  (when Orient
    ( dbSetq Fig Orient orient ) )

  ( dbMoveFig
    Fig
    nil
    ( list
      ( PointDifference
        Point
        (cond (
               ( equal Align "ll" )
               ( car ( getq Fig bBox ) ) )
              (
               ( equal Align "lr" )
               ( list ( RectGetRight ( getq Fig bBox ) )
                      ( RectGetBottom ( getq Fig bBox ) ) ) ) ) )
      "R0" ) ) )

(defun FigStackClumps ( Clumps Y )
  ( foreach Clump Clumps
            (when Clump
              (let (
                    ( Bottom
                      ( ListFindMinimum
                        Clump
                        (lambda ( Fig )
                          ( RectGetBottom ( getq Fig bBox ) ) ) ) ) )
                ( foreach Fig Clump
                          ( dbMoveFig 
                            Fig
                            nil
                            ( list ( list 0 ( difference Y Bottom ) )
                                   "R0" ) ) )
                ( setq 
                  Y       
                  ( ListFindMaximum
                    Clump
                    (lambda ( Fig )
                      ( RectGetTop ( getq Fig bBox ) ) ) ) )
                ) ) ) )


(defun FigStack ( Stack Y )
  ( foreach Fig Stack
            ( FigMove 
              Fig
              ( list
                ( RectGetLeft ( getq Fig bBox ) )
                Y ) )
            ( setq Y 
                   ( RectGetTop ( getq Fig bBox ) ) ) ) )

(defun FigStackHorizontal ( Stack X  )
  ( foreach Fig Stack
            ( FigMove 
              Fig
              ( list
                X
                ( RectGetBottom ( getq Fig bBox ) ) ) )
            ( setq X
                   ( RectGetRight ( getq Fig bBox ) ) )
 ) )

;Move all figs together so that left-most point is at X
(defun FigStackHorizontalMaintainSep ( Stack X )
  (let (
        ( Offset ( difference 
                   X
                   ( RectGetLeft ( getq ( car Stack ) bBox ) )
                   ) ) )
    ( foreach Fig Stack
              ( dbMoveFig
                Fig
                nil
                ( list Offset:0 "R0" ) ) ) ) )



(defun FigGetClumpTable ( Instances
                         @key
                         ( ClumpPredicate
                           `RectDoRectsOverlapOpenRects )
                         ( Clumpify
                           (lambda ( C ) ( getq C bBox ) ) )
                         ( ClumpUnion
                           `RectGetBoundRect )
                         ( InstanceTable
                            ( makeTable `bar nil ) )
                         )
  ( foreach Instance Instances
            (let (
                  ( Clump
                    ( apply Clumpify ( list Instance ) ) ) )
              (let (
                    ( Union Clump )
                    ( IntersectingClumps
                      ( setof OtherClump InstanceTable
                              ( apply ClumpPredicate ( list Clump OtherClump ) ) ) ) )
                (let (
                      ( Instances
                        ( ListNonDestructiveMapCan
                          (lambda ( IntersectingClump )
                            (let (
                                  ( Instances ( arrayref 
                                                InstanceTable
                                                IntersectingClump ) ) )
                              ( remove IntersectingClump InstanceTable )
                              ( setq Union ( apply 
                                             ClumpUnion
                                             ( list IntersectingClump
                                                    Union ) ) )
                              Instances  ) )
                          IntersectingClumps ) ) )
                  ( setarray InstanceTable Union
                             ( cons Instance Instances ) ) ) ) ) )
  InstanceTable )
