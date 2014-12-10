; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

(defun ConductorPropogateLabelAndTerminals ( RootCellView
                                             LabelFuncFactory
                                             MaxDepth
                                             LibCellsToIgnore )

  (let (
        ( TopLabels
          ( setof
            Shape
            ( getq RootCellView shapes )
            ( equal ( getq Shape objType ) "label" ) ) ) )

    ( ConductorRecursivelyCopyLabels 
      RootCellView
      RootCellView
      ( list 0:0 "R0" )
      LabelFuncFactory
      ( apply
        LabelFuncFactory
        ( list nil
               nil ) )
      0 
      MaxDepth 
      LibCellsToIgnore )


    ( foreach
      Shape
      TopLabels
      ( dbDeleteObject Shape ) )
    ) )

;returns a list of pairs
;terminal name, position
(defun ConductorGetTerminalNamePositionLPPs ( CellView )
  ( ListNonDestructiveMapCan
    (lambda ( Terminal )
      ( mapcar
        (lambda ( Pin )
          ( list ( getq ( getq Pin net ) name )
                 ( list 
                   ( RectGetCenter 
                     ( getq ( getq Pin fig ) bBox ) )
                   ( getq ( getq Pin fig ) lpp )
                   ) ) )
        ( getq Terminal pins ) ) )
    ( getq CellView terminals ) ) )

(defun ConductorGetLabelNamePositionLPPs ( CellView )
  ( mapcar
    (lambda ( Label )
      ( list ( getq Label theLabel )
             ( list 
               ( getq Label xy )
               ( getq Label lpp )
             ) ) )
    ( setof
      Shape
      ( getq CellView shapes )
      ( equal ( getq Shape objType ) "label" )
      ) ) )

(defun ConductorGetTerminalAndLabelNamePositionLPPs ( CellView )
  (let (
        ( PortTable ( makeTable `foo nil ) ) )

    ( foreach 
      NamePositionLPP
      ( append
        ( ConductorGetTerminalNamePositionLPPs CellView )
        ( ConductorGetLabelNamePositionLPPs CellView ) )
      ( setarray PortTable
                 ( car NamePositionLPP )
                 ( cadr NamePositionLPP ) ) )

    PortTable
    ) )


;if a cells has borkificatified nets, we will label it's ports
;This means subcells will never 
(defun ConductorRecursivelyCopyLabels ( TopLevelCellView
                                        CellView
                                        Transform
                                        LabelFuncFactory
                                        LabelFunc
                                        Depth
                                        MaxDepth 
                                        LibCellsToIgnore
                                        )
  (when ( leqp Depth MaxDepth )

    ( foreach 
      NamePositionLPP
      ( tableToList 
        ( ConductorGetTerminalAndLabelNamePositionLPPs
          CellView ) )
      (let (
            ( NetName
              ( car NamePositionLPP ) )
            ( Position
              ( dbTransformPoint
                ( car ( cadr NamePositionLPP ) )
                Transform )
              )
            ( LPP
              ( cadr ( cadr NamePositionLPP ) ) ) )

        ( apply
          LabelFunc
          ( list NetName Position LPP ) ) ) )


    ( foreach
      Instance
      ;ignore mosaics
      ( car
        ( NameFilterInstances
          ( setof 
            Instance
            ( getq CellView instances )
            ( equal ( getq Instance objType ) "inst" ) )
          LibCellsToIgnore ) )

      ( ConductorRecursivelyCopyLabels 
        TopLevelCellView
        ( getq Instance master )
        ( dbConcatTransform
          ( getq Instance transform )
          Transform )
        LabelFuncFactory
        ( apply
          LabelFuncFactory
          ( list ( getq CellView cellName )
                 ( getq Instance name )
                 ) )
        ( plus 1 Depth )
        MaxDepth
        LibCellsToIgnore ) )

 ) )





(defun ConductorDraw ( CellView
                       Pin
                       Instance
                       Net
                       Force
                       )
  (let (
        ( SubCellToTopLevelTransform
          ( getq Instance transform ) ) )
    (let (
          ( PinBBoxAtTopLevel
            ( dbTransformBBox
              ( getq ( getq Pin fig ) bBox )  
              SubCellToTopLevelTransform ) ) )
      (let (
            ( LPP ( getq ( getq Pin fig ) lpp ) )
            ( SubCellView
              ( getq Instance master ) ) )
        (let (
              ( Overlaps
                ( dbGetTrueOverlaps
                  CellView
                  PinBBoxAtTopLevel
                  ( list ( car LPP )
                         "drawing" )
                  ( list 1 32 )
                  ) ) )
          ( foreach
            Overlap
            Overlaps
            (let (
                  ( FigStack
                    ( reverse
                      ( TransformCanonicalizeInstancePath
                        Overlap ) ) ) )
              (let (
                    ( FigInSomeCell ( car FigStack ) ) )
                
                (when ( and
                        ( not ( listp FigInSomeCell ) )
                        ( or
                          ( equal 
                            FigInSomeCell->objType 
                            "rect" )
                          ( equal 
                            FigInSomeCell->objType 
                            "polygon" )
                          ( equal 
                            FigInSomeCell->objType 
                            "path" ) ) )
                  (let (
                        ( SomeCellToTopLevelTransform
                          ( TransformGetTransformFromInstanceStack
                            ( cdr FigStack ) ) ) )
                    (let (
                          ( Fig
                            ( dbCopyFig
                              FigInSomeCell
                              CellView
                              SomeCellToTopLevelTransform ) )
                          )
                      (cond (
                             ( and 
                               ( not Force )
                               ( RectIsRectInRectClose
                                 ( getq Fig bBox )
                                 PinBBoxAtTopLevel
                                 1e-6
                                 ) )
                             ( dbDeleteObject Fig )
                             nil )
                            (
                             t
                             Fig->net = Net
                             t
                             ) ) ) ) ) ) ) ) ) ) ) ) )




(defun Conductor ( CellView
                   FoldableCellLibCellPairRegExs
                   GNDNetName
                   VddNetName
                 )
  (let (
        ( LayoutInstanceMap
          ( NameMakeInstanceMapFromCellView
            CellView
            FoldableCellLibCellPairRegExs ) ) )
    ( foreach Net ( getq CellView nets )
              (when ( and 
                      ( or Local ( getq Net pins ) )
                      ( not ( equal Net->name GNDNetName ) )
                      ( not ( equal Net->name VddNetName ) ) )
                (let (
                  ( PinInstancePairs
                    ( PinUtilGetConnectedPinInstancePairs_2 
                      CellView 
                      CellView 
                      LayoutInstanceMap
                      ( getq Net name )
                      ) )
                  ( Done nil )
                  )
              ( foreach
                PinInstancePair
                PinInstancePairs
                (let (
                      ( Pin
                        ( car PinInstancePair ) )
                      ( Instance
                        ( cadr PinInstancePair ) )
                      )

                  (when ( ConductorDraw
                          CellView
                          Pin
                          Instance
                          Net
                          ( not Done )
                          )
                    ( setq Done t )

                    ) ) ) ) ) ) ) )
