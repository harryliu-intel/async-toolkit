; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

(defun LVSNodesDrawFromDirectives ( CellView
                                    DirectivesTable
                                    LPP
                                    LibCellPairRegExsToIgnore
                                    )
  ( rexCompile "\\#" )
  (let (
        ( LVSNodeNames
          ( mapcar
            (lambda ( Name )
              ( rexReplace Name "-H" 0 )
              )
            ( CellInfoLookup
              DirectivesTable
              "lvs_labels" ) ) ) )
    ( LVSNodesDraw 
      CellView
      LVSNodeNames
      LPP
      LibCellPairRegExsToIgnore
      ) ) )

(defun LVSNodesDraw ( CellView
                      LVSNodeNames
                      LPP
                      LibCellPairRegExsToIgnore
                                    )
  (let (
        ( Instances
          ( car ( NameFilterInstances
                  ( getq CellView instances )
                  LibCellPairRegExsToIgnore ) ) ) )

    ( foreach
      LVSNodeName
      LVSNodeNames
      ( println LVSNodeName )
      (let (
            ( LabelRect 
              ( car 
                ( PinUtilGetPinRectsForInstances
                  Instances
                  (lambda ( NetName ) ( equal NetName LVSNodeName ) )
                  (lambda ( SomeLPP ) ( equal ( car SomeLPP ) ( car LPP ) ) ) ) ) ) )
        ( dbCreateLabel
          CellView
          LPP
          ( RectGetCenter LabelRect )
          LVSNodeName
          "centerCenter"
          "R0"
          "stick"
          0.1
           )
        ) )
 ) )
                       
                                   
                          
