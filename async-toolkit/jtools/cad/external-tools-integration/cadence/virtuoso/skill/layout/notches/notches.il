; Copyright 2003 Fulcrum Microsy\stems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$


(defun NotchesFillNotchesOnCellView ( CellView
                                      AssuraLayerMappings
                                      AssuraSets
                                      RuleFile
                                      TempDir
                                      @key
                                      ( Area nil )
                                      ( NoPCells nil )
                                      )
  
  (let (
        ( TechLibName ( techGetTechFileName CellView ) )
        ( TempLibName ( LibCreateTempLibraryFromCellView 
                        CellView 
                        ( sprintf nil "NOTCHES_%s" ( getq CellView cellName ) )
                        TempDir ) )
        ( ErrorStr nil ) )
    ( setq
      ErrorStr
      ( AssuraRunAssuraLayerProcessor
        CellView
        TempLibName
        ( getq CellView cellName )
        ( getq CellView viewName )
        RuleFile
        TempDir
        AssuraLayerMappings
        nil
        ?LeaveMess nil
        ?AssuraSets AssuraSets
        ?Area Area
        ?NoPCells NoPCells
        ) )
               
    ( println ErrorStr )

    (unless ErrorStr
      (let (
            ( OutputCellDDObj ( ddGetObj TempLibName ( getq CellView cellName ) ( getq CellView viewName ) ) ) )
        (if ( and OutputCellDDObj ( getq OutputCellDDObj files ) )
            (let (
                  ( CellWithNotches ( dbOpenCellViewByType
                                      TempLibName
                                      ( getq CellView cellName )
                                      ( getq CellView viewName )
                                      nil
                                      "a" ) ) )
              (if CellWithNotches
                  (let ()
                    ( AssuraCopyShapesFromCellView 
                      CellWithNotches
                      CellView
                      nil )
                    ;( NotchesRemoveShortingShapes
                    ;  CellView )
                    )
                ( setq 
                  ErrorStr
                  ( sprintf 
                    nil
                    "Unable to open cell view %L %L %L, even though DDObj exists."
                    TempLibName
                    ( getq CellView cellName )
                    ( getq CellView viewName ) ) ) ) )
          ( setq 
            ErrorStr
            ( sprintf
              nil
              "%L %L %L does not exist."
              TempLibName
              ( getq CellView cellName )
              ( getq CellView viewName ) ) ) ) ) )
   ( ddDeleteObj ( ddGetObj TempLibName ) )
   ( techBindTechFile CellView TechLibName )
    ErrorStr ) )

                                  
(defun NotchesRemoveShortingShapes ( CellView )
  (let (
        ( PolygonTable ( makeTable `polygon nil ) )
        ( AllShapes ( getq CellView shapes ) ) )

    ( foreach Shape AllShapes
              ( setarray PolygonTable Shape 
                         ( FigGetPolygonEdgesForFig Shape ) ) )
    ( mapcar 
      (lambda ( Shape )
        ( dbDeleteObject Shape ) )
      ( setof Shape ( getq CellView shapes )
              (let (
                    ( AbuttingShapes
                      ( NotchesGetAbuttingFigsOnSameLPP
                        PolygonTable
                        Shape
                        AllShapes ) ) )
                (let (
                      ( AbuttingNet ( getq 
                                      ( car ( exists AbutShape AbuttingShapes 
                                                     ( getq Shape net ) ) )
                                      net ) ) )
                  ( not ( forall AbuttingShape AbuttingShapes
                                 ( or ( null ( getq AbuttingShape net ) )
                                      ( equal ( getq AbuttingShape net ) AbuttingNet ) ) ) ) ) ) ) ) ) )
  

(defun NotchesGetAbuttingFigsOnSameLPP ( PolygonTable Shape Shapes )
  ( setof OtherShape Shapes
          ( and ( equal ( getq Shape lpp ) ( getq OtherShape lpp ) )
                ( PolygonEdgesAbut 
                  ( arrayref PolygonTable Shape )
                  ( arrayref PolygonTable OtherShape ) ) ) ) )
