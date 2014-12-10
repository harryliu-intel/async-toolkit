; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$


(defun RectOnPitchFindNearestPitch ( MetalSpacing MetalWidth BoundPosition Position )
  (let (
        ( Pitch ( plus MetalSpacing MetalWidth ) )
        ( HalfSpacing ( quotient MetalSpacing 2.0 ) )
        ( HalfWidth ( quotient MetalWidth 2.0 ) ) )
    ( plus
      BoundPosition
      ( plus
        HalfSpacing
        ( plus
          HalfWidth
          ( times
            Pitch
            ( round
              ( quotient
                ( difference
                  ( difference
                    ( difference
                      Position
                      BoundPosition )
                    HalfWidth )
                  HalfSpacing )
                Pitch ) ) ) ) ) ) ) )

(defun RectOnPitchMakeRectNearestPitchOnHorizontalLayer ( MfgGrid 
                                                          MetalSpacing
                                                          MetalWidth
                                                          MinArea
                                                          BoundaryPoints
                                                          Point )
  (let (
        ( BottomOfBound ( cadr ( PolygonGetBottomLeftPoint BoundaryPoints ) ) ) )
    (let (
          ( CenterY ( RectOnPitchFindNearestPitch
                      MetalSpacing
                      MetalWidth
                      BottomOfBound
                      ( cadr Point ) ) )
          ( CenterX ( car Point ) ) )
      ( RectMakeFromCenter
        ( list
          CenterX
          CenterY )
        ( times
          MfgGrid
          ( ceiling
            ( quotient
              ( quotient
                MinArea
                MetalWidth )
              MfgGrid ) ) )
        MetalWidth ) ) ) )

(defun RectOnPitchMakeRectNearestPitchOnVerticleLayer ( MfgGrid 
                                                        MetalSpacing
                                                        MetalWidth
                                                        MinArea
                                                        BoundaryPoints
                                                        Point )
  (let (
        ( LeftOfBound ( car ( PolygonGetLeftBottomPoint BoundaryPoints ) ) ) )
    (let (
          ( CenterX ( RectOnPitchFindNearestPitch
                      MetalSpacing
                      MetalWidth
                      LeftOfBound
                      ( car Point ) ) )
          ( CenterY ( cadr Point ) ) )
      ( RectMakeFromCenter
        ( list
          CenterX
          CenterY )
        MetalWidth
        ( times
          MfgGrid
          ( ceiling
            ( quotient
              ( quotient
                MinArea
                MetalWidth )
              MfgGrid ) ) ) ) ) ) )

; ( RectOnPitchDrawRectOnPitch 0.05 0.24 0.24 0.2 ( list "METAL2" "drawing" ) nil ( list "prBoundary" "drawing" ) ( geGetWindowCellView ) ( hiGetPoint ( hiGetCurrentWindow ) ) )
(defun RectOnPitchDrawRectOnPitch ( MfgGrid
                                    MetalSpacing
                                    MetalWidth
                                    MinArea
                                    LayerPP
                                    IsHorizontal
                                    BoundaryLPP
                                    CellView
                                    Point )
  (let (
        ( BoundaryShape ( car
                          ( setof
                            Shape
                            ( getq CellView shapes )
                            (let (
                                  ( ShapeLPP ( getq Shape lpp ) ) )
                              ( and
                                ( equal ( car BoundaryLPP ) ( car ShapeLPP ) )
                                ( equal ( cadr BoundaryLPP ) ( cadr ShapeLPP ) ) ) ) ) ) ) )
    (let (
          ( BoundaryPoints (if ( equal ( getq BoundaryShape objType ) "rect" )
                               ( RectGetPoints ( getq BoundaryShape bBox ) )
                             ( getq BoundaryShape points ) ) ) )
      (let (
            ( TheRect (if IsHorizontal
                          ( RectOnPitchMakeRectNearestPitchOnHorizontalLayer
                            MfgGrid
                            MetalSpacing
                            MetalWidth
                            MinArea
                            BoundaryPoints
                            Point )
                        ( RectOnPitchMakeRectNearestPitchOnVerticleLayer
                          MfgGrid
                          MetalSpacing
                          MetalWidth
                          MinArea
                          BoundaryPoints
                          Point ) ) ) )
        ( dbCreateRect
          CellView
          LayerPP
          TheRect ) ) ) ) )
