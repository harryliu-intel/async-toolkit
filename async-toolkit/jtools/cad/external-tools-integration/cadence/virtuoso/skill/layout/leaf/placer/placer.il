; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

(defun PlacerRunPlacer ( CellView
                         DestViewName
                         ComponentClasses
                         Columns
                         WorkingDir
                         RuleFile
                         @key 
                         ( ConductorDepth 1 )
                         ( KeepoutDepth 32 )
                         ( Import t )
                         ( Quit t )
                         ( Graphics nil )
                         ( Space nil )
                         )
 (let (
       ( SrcLibName ( getq CellView libName ) )
       ( SrcCellName ( getq CellView cellName ) )
       ( SrcViewName ( getq CellView viewName ) )
       ( DestLibName ( getq CellView libName ) )
       ( DestCellName ( getq CellView cellName ) ) )
   (let (
         ( TempDirForThisRun 
           ( makeTempFileName 
             ( sprintf 
               nil
               "%s/PLACER.XXXXXX" 
               WorkingDir ) ) ) )
    (let (
          ( DestCellViewObj ( ddGetObj 
                              DestLibName
                              DestCellName
                              DestViewName ) )
          ( PlacerRunLog ( sprintf nil "%s/placer.log" TempDirForThisRun ) )
          ( PlacerDebugLog ( sprintf nil "%s/placer.debug" TempDirForThisRun ) )
          ( PlacerCommand "$runplacer$" ) )
      (when DestCellViewObj
        ( printf "WARNING...overwriting dest cell view\n" )
        ( ddDeleteObj DestCellViewObj ) )
      ( createDir TempDirForThisRun )
      (let (
            ( SpecFileName ( sprintf nil "%s/spec" TempDirForThisRun ) )
            ( CurrColumnNum 1 ) )
        (let (
              ( SpecFileOut 
                ( outfile SpecFileName ) ) )

      ( setq PlacerCommand 
             ( sprintf nil "%s --src-lib=%s --src-cell=%s --src-view=%s " PlacerCommand SrcLibName SrcCellName SrcViewName ) )
      ( setq PlacerCommand 
             ( sprintf nil "%s --dest-lib=%s --dest-cell=%s --dest-view=%s " PlacerCommand DestLibName DestCellName DestViewName ) )
      ( setq PlacerCommand 
             ( sprintf nil "%s --conductor-depth=%d --keepout-depth=%d " PlacerCommand ConductorDepth KeepoutDepth ) )
      ( setq PlacerCommand 
             ( sprintf nil "%s --rule-file=%s --spec-file=%s --working-dir=%s " PlacerCommand RuleFile SpecFileName TempDirForThisRun ) )     
      ( setq PlacerCommand 
             ( sprintf nil "%s --log-file=%s" PlacerCommand PlacerRunLog ) )

    ( setq PlacerCommand 
           ( sprintf nil "%s --debug-log-file=%s" PlacerCommand PlacerDebugLog ) )

      (when ( not Graphics )
        ( setq PlacerCommand 
               ( sprintf nil "%s --nog" PlacerCommand ) ) )
      (when Quit
        ( setq PlacerCommand 
               ( sprintf nil "%s --quit " PlacerCommand ) ) )
      (when Import
        ( setq PlacerCommand 
               ( sprintf nil "%s --import " PlacerCommand ) ) )

      (when Space
        ( setq PlacerCommand 
               ( sprintf nil "%s --space " PlacerCommand ) ) )

      ( fprintf SpecFileOut "#%s\n" ( PlacerGetBBoxString 
                                      ( PlacementRegionsGetCellRegionDataBBox CellRegionData) ) )
      ;class = ( classname libcellviews )
      ( foreach Class ComponentClasses
                ( fprintf SpecFileOut ( car Class ) )
                ( foreach LibCellView ( cadr Class )
                          ( fprintf SpecFileOut " %s:%s:%s" 
                                    ( nth 0 LibCellView )
                                    ( nth 1 LibCellView )
                                    ( nth 2 LibCellView ) ) )
                ( fprintf SpecFileOut "\n" ) )

      ;column = ( bbox alignment classes orients )
      ( foreach Column Columns
                (let (
                      ( ClassString "" )
                      ( OrientString "" ) )
                  ( foreach Class ( nth 2 Column )
                            ( setq ClassString ( sprintf nil "%s %s" ClassString Class ) ) )
                  ( foreach Orient ( nth 3 Column )
                            ( setq OrientString ( sprintf nil "%s %s" OrientString ( PlacerGetOrientString Orient ) ) ) )
                  ( fprintf SpecFileOut "define (column row%d (area %s ) (alignment %s) (allow_type %s) (allow_orient %s))\n"
                            CurrColumnNum
                            ( PlacerGetBBoxString ( nth 0 Column ) )
                            ( nth 1 Column )
                            ClassString
                            OrientString )
                  ( setq CurrColumnNum ( plus CurrColumnNum 1  ) ) ) )
      ( close SpecFileOut )
      ( printf PlacerCommand )
      ( shell PlacerCommand )
      ;( shell ( sprintf nil "rm -rf %s &>/dev/null" TempDirForThisRun ) )
      
      ) ) ) ) ) )


(defun PlacerGetColumnsFromCellRegionData ( CellRegionData
                                            GateClasses
                                            NClasses
                                            PClasses
                                            NRegionOffsetFromColumnCenter
                                            PRegionOffsetFromColumnCenter
                                            EvilGatePRegionOffset )
  ( ListNonDestructiveMapCan
    (lambda ( ColumnData )
      (let (
            ( NRegionRect ( PlacementRegionGetColumnNRegionRect
                            ColumnData ) )
            ( PRegionRect ( PlacementRegionGetColumnPRegionRect
                            ColumnData ) )
            ( NPRegionParity ( PlacementRegionGetNPRegionParity
                               ColumnData ) )
            ( TopGateRegionRect ( PlacementRegionGetColumnTopGateRegionRect
                                  ColumnData ) )
            ( BottomGateRegionRect ( PlacementRegionGetColumnBottomGateRegionRect  ColumnData ) )
            ( NPRegionParity 
              ( PlacementRegionGetNPRegionParity
                ColumnData ) ) )
        ( list 
          ( list
            ( PlacementRegionsGateRegionToPlacementRowRect
              ( RectGetBoundRect ( PlacementRegionGetColumnBottomGateRegionRect ColumnData )
                                 ( PlacementRegionGetColumnTopGateRegionRect ColumnData ) )
              ( plus 
                (if NPRegionParity EvilGatePRegionOffset -EvilGatePRegionOffset )
                ( RectGetWidth
                  (if NPRegionParity PRegionRect NRegionRect ) ) ) )
            
            "origin"
            GateClasses
            (if NPRegionParity ( list "MY" "R180" ) ( list "R0" "MX" ) ) )
          ( list
            (if NPRegionParity
                ( PlacementRegionsRightRegionToPlacementRowRect
                  NRegionRect
                  NRegionOffsetFromColumnCenter )
              ( PlacementRegionsLeftRegionToPlacementRowRect
                NRegionRect
                NRegionOffsetFromColumnCenter ) )
            (if NPRegionParity "left" "right" )
            NClasses
            (if NPRegionParity ( list "MY" "R180" ) ( list "R0" "MX" ) ) )
          ( list
            (if NPRegionParity
                ( PlacementRegionsLeftRegionToPlacementRowRect
                  PRegionRect
                  PRegionOffsetFromColumnCenter )
              ( PlacementRegionsRightRegionToPlacementRowRect
                PRegionRect
                PRegionOffsetFromColumnCenter ) )
            (if NPRegionParity "right" "left" )
            PClasses
            (if NPRegionParity ( list "MY" "R180" ) ( list "R0" "MX" ) ) ) ) ) )
    ( PlacementRegionsGetCellRegionDataColumnList
      CellRegionData ) ) )

(defun PlacerGetBBoxString ( BBox )    
  ( sprintf nil "%f %f %f %f"
            ( caar BBox )
            ( cadar BBox )
            ( caadr BBox )
            ( cadadr BBox ) ) )
  
(defun PlacerGetOrientString ( Orient )
  (cond (
         ( equal ( substring Orient 1 1  ) "R" )
         ( substring 
           Orient
           2 
           ( difference ( strlen Orient ) 1 )
           ) )
        (
         ( equal Orient "MX" )
         "X0" )
        (
         ( equal Orient "MY" )
         "Y0" )
        (
         ( equal Orient "MXR90" )
         "X90" )
        (
         ( equal Orient "MYR90" )
         "Y90" ) ) )
