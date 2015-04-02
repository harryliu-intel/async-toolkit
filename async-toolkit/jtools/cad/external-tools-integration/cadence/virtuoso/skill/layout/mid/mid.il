                                        ; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
                                        ; $Id$
                                        ; $DateTime$
                                        ; $Author$



(defun MidLevelGetRects ( CellView 
                          LPP 
                          LeafCellPredicate )
  ( append
    (when ( apply LeafCellPredicate ( list CellView ) )
      ( mapcar
        (lambda ( Shape )
          ( getq Shape bBox ) )
        ( PinUtilGetAllShapesOnLPP CellView LPP ) ) )
    ( ListNonDestructiveMapCan
      (lambda ( Instance )
        ( mapcar
          (lambda ( InstanceShape )
            ( dbTransformBBox
              InstanceShape
               (if ( equal ( getq Instance objType ) "mosaicInst" )
                   ( list ( car Instance->bBox ) 
                          ( car Instance->mosaic->tileArray ) )
                   Instance->transform
              ) ) )
          ( MidLevelGetRects
            ( getq Instance master )
            LPP
            LeafCellPredicate )
          ) )
      ( getq CellView instances )
      ) ) )


(defun MidLevelGetTopMetalLPP ( CellView 
                                MetalLayerFormat )
  ( ListFindMaximumElement  
    ( setof 
      LPP
      ( MidLevelGetLPPs
        CellView )
      ( equal ( cadr LPP ) "drawing" ) )
    (lambda ( LPP )
      (let ( 
            ( Num 0 ) )
        ( sscanf ( car LPP ) MetalLayerFormat Num )
        Num
        ) ) ) )


(defun MidLevelConvertFloorplanToLayout ( Floorplan
                                          TargetLibName
                                          DirectiveTable
                                          PinsFile
                                          LayoutViewName
                                          CompactedViewName
                                          RoutedViewNamePrefix
                                          BoundaryLPP
                                          PreDoFiles
                                          PostDoFiles
                                          LPPsToUse
                                          LPPsToSearch
                                          KeepOutDepth
                                          RuleFile
                                          WorkingDir
                                          GNDNetName
                                          VddNetName
                                          ViaLibraryName
                                          ViaNameFormat
                                          MetalLayerRegEx
                                          MetalLayerFormat
                                          HorizontalMetalLPPs
                                          VerticalMetalLPPs
                                          UserUnitsPerMeter
                                          DirectiveUnitsPerMeter
                                          DRCRuleFile
                                          DRCAssuraSets
                                          LVSRuleFile
                                          LVSBindFile
                                          LVSCompareFile
                                          LVSIncludeFile
                                          SchematicFile
                                          TechLibName
                                          NotchDRCRuleFile
                                          BoundaryPinHorizontalSpacing
                                          BoundaryPinVerticalSpacing
                                          ViaLayerRegEx
                                          PinLength
                                          PinLPP
                                          HorizontalLPP
                                          FoldableCellLibCellPairRegExs
                                          SkillNetlistDir
                                          DirectivesDir
                                          Graphics
                                          )
  (let (
        ( ConductorDepths 
          ( ListFillList
            (lambda ( X ) ( difference X 1 ) )
            ( plus 1 ( HierarchyGetMaxDepth Floorplan ) ) ) )
        ( CellName ( getq Floorplan cellName ) ) )
    (let (
          ( Compacted
            ( betterCopyCellView
              Floorplan
              TargetLibName
              CellName
              CompactedViewName
              nil
              nil
              t ) ) )
                                        ;convert to layout views
      ( SwapInstanceMasterViews
        Compacted
        LayoutViewName
        "maskLayout" )
                                        ;die if failed
      (let (
            ( LayoutDontExist
              ( setof 
                Instance
                ( getq Compacted instances )
                ( not ( equal 
                        ( getq ( getq Instance master ) viewName )
                        LayoutViewName ) ) ) ) )
        (when LayoutDontExist
          (error 
           ( sprintf 
             nil
             "Can't find layout view for these cells: %s\n"
             ( StringUtilConvertStringListToSpaceSeperatedString
               ( ListRemoveDuplicatesNoTableElements
                 LayoutDontExist~>cellName ) ) ) ) ) )
   
      ( println
        ( MidLevelGetTopMetalLPP 
          Compacted
          MetalLayerFormat ) )

      (let (
            ( BoundingBox nil )
            ( XGrid
              ( PDKGetInstanceXGridForMetalLPP
                ( MidLevelGetTopMetalLPP 
                  Compacted
                  MetalLayerFormat ) ) ) )

        ( MidLevelCompactInstances
          Compacted
          ( getq Compacted instances )
          BoundaryLPP
          XGrid )

                                        ;sync to netlist

        ( UpdateNetlistGenFromSkillNetlistUsingPDKInfo
          Compacted
          SkillNetlistDir
          DirectivesDir )

                                        ;draw pins
        ( PinPlaceDraw
          Compacted
          UserUnitsPerMeter
          BoundaryLPP
          PinLength
          PinLPP
          HorizontalLPP
          FoldableCellLibCellPairRegExs
          BoundaryPinHorizontalSpacing
          BoundaryPinVerticalSpacing
          MetalLayerRegEx
          ViaLayerRegEx
          ?PinsDirectory "."
          ?ConnectivityViewName CompactedViewName
          ?PinsFile PinsFile
          ?DeleteOld nil
          ?CreateLog nil
          ?PowerGrid `inplace
          ?InPlace t
          ?DeleteExistingPins t
          )


        ( foreach 
          Box
          ( ListNonDestructiveMapCan
            (lambda ( Instance )
              ( MidLevelGetBBoxes
                Instance
                BoundaryLPP ) )
            ( getq Compacted instances ) )
          ( setq BoundingBox ( RectGetBoundRect 
                               BoundingBox
                               Box ) ) )

                                        ;                ( geSelectObject 
                                        ;( dbCreateRect
                                        ;  Compacted
                                        ;  BoundaryLPP
                                        ;  Box )
                                        ;)
                                        ;         )
                                        ;      ( leHiMerge )
                                        ;     ( geDeselectAll )

        ( dbCreateRect
          Compacted
          BoundaryLPP
          BoundingBox )
        
        ( dbSave Compacted )
        
        (let (
              ( CurrConductorDepths 
                ConductorDepths )
              ( Done nil )
              ( SuccessView nil )
              )
          (while ( not Done )
            (let (
                  ( ConductorDepth 
                    ( car CurrConductorDepths ) ) )
              (let (
                    ( RoutedViewName
                      ( sprintf nil
                                "%s_%s_%d"
                                RoutedViewNamePrefix
                                ( ListApplyFuncToListAndAccumulateResult
                                  ( mapcar
                                    (lambda ( LPP )
                                      ( rexCompile "METAL" )
                                      ( rexReplace ( car LPP ) "" 0 ) )
                                    LPPsToUse )
                                  `strcat
                                  nil
                                  "" )
                                ConductorDepth ) )
                    )
                (let (
                      ( WorkingDir 
                        ( sprintf nil
                                  "%s/%s"
                                  WorkingDir
                                  RoutedViewName ) )
                      ( PreRoute
                        ( betterCopyCellView
                          Compacted
                          TargetLibName
                          CellName
                          "scratch"
                          nil
                          nil
                          t
                          ) )
                      ( PowerGridMaster
                        ( dbOpenCellViewByType
                          PowerGridLibName
                          ( PDKGetPowerGridGetCellNameForMetalLPP
                            ( car ( last LPPsToUse ) ) )
                          PowerGridViewName
                          "maskLayout"
                          "r" ) )
                      )


                                        ;instantiate power grid
                  (when PowerGridMaster
                    ( dbCreateSimpleMosaic
                      PreRoute
                      PowerGridMaster
                      nil
                      ( list 0 0 )
                      "R0"
                      ( ceiling 
                        ( quotient 
                          ( RectGetHeight BoundingBox )
                          ( RectGetHeight ( getq PowerGridMaster bBox ) ) ) )
                      ( ceiling 
                        ( quotient 
                          ( RectGetWidth BoundingBox )
                          ( RectGetWidth ( getq PowerGridMaster bBox ) ) ) )
                      ( RectGetHeight ( getq PowerGridMaster bBox ) )
                      ( RectGetWidth ( getq PowerGridMaster bBox ) )
                      ) )
                      
                  ( dbSave PreRoute )
                  ( dbPurge PreRoute )
                  (unless ( or
                            ( isDir WorkingDir )
                            ( createDir WorkingDir ) )
                    (error 
                     ( sprintf 
                       nil
                       "Can't create %s"
                       WorkingDir ) ) )


                  (let (
                        ( Ret
                          ( MidLevelRouteWithLPPsAndConductorDepth
                            TargetLibName
                            CellName
                            "scratch"
                            RoutedViewName
                            LPPsToUse
                            ConductorDepth
                            KeepOutDepth
                            RuleFile
                            PreDoFiles
                            PostDoFiles
                            WorkingDir
                            GNDNetName
                            VddNetName
                            ViaLibraryName
                            ViaNameFormat
                            Graphics ) )
                        ( MetalLPPs
                          ( setof LPP LPPsToUse ( rexMatchp 
                                                  MetalLayerRegEx
                                                  ( car LPP ) ) ) )
                        )


                    ( printf "%s\n" RoutedViewName )
                    ( printf "Router: Crossing Violations: %d\n" ( nth 0 Ret ) )
                    ( printf "Router: Clearance Violations: %d\n" ( nth 1 Ret ) )
                    ( printf "Router: Unroutes: %d\n" ( nth 2 Ret ) )
                    ( printf "Router: Completion: %f\n" ( nth 3 Ret ) )

                    (let (
                          ( Routed
                            ( dbOpenCellViewByType
                              TargetLibName
                              CellName
                              RoutedViewName
                              "maskLayout"
                              "a"
                              ) ) )
              
                      (let (
                            ( RoutingGood
                              ( and
                                ( lessp ( nth 0 Ret ) 100 )
                                ( equal ( nth 2 Ret ) 0 )
                                (if (let (
                                          ( LVSStatus
                                            ( AssuraRunLVS
                                              Routed
                                              LVSRuleFile
                                              nil
                                              LVSBindFile
                                              LVSCompareFile
                                              LVSIncludeFile
                                              SchematicFile
                                              WorkingDir
                                              ( sprintf nil "%s/lvs"
                                                        WorkingDir ) ) )
                                          )
                                      ( printf "LVS Status: %s\n"
                                               (if LVSStatus "Passed" "Failed" ) )
                                      LVSStatus )
                                    t
                                  ) ) ) )

                        (when RoutingGood 
                          ( setq SuccessView Routed )
                          ( KeepOutDrawKeepOut
                            Routed
                            DirectiveTable
                            ( mapcar
                              (lambda ( LPP )
                                ( KeepOutCreateRoutingLayerStruct LPP ?Purpose "dummy" ) )
                              ( ListIntersect HorizontalMetalLPPs MetalLPPs ) )
                            ( mapcar
                              (lambda ( LPP )
                                ( KeepOutCreateRoutingLayerStruct LPP ?Purpose "dummy" ) )
                              ( ListIntersect VerticalMetalLPPs MetalLPPs ) )
                            BoundaryLPP
                            UserUnitsPerMeter
                            DirectiveUnitsPerMeter
                            ( list GNDNetName VddNetName )
                            ( list nil )
                            t
                            nil
                            0 )

                          ( NotchesFillNotchesOnCellView
                            Routed
                            ( append
                              ( list ( list "polyfix" PolyLPP )
                                     ( list "notchpoly" PolyLPP ) )
                              ( append
                                ( mapcar
                                  (lambda ( MetalLPP )
                                    ( rexCompile MetalLayerRegEx )
                                    ( rexExecute ( car MetalLPP ) )
                                    ( list ( rexSubstitute "area\\1" )
                                           MetalLPP ) )
                                  MetalLPPs )
                                ( append
                                  ( mapcar
                                    (lambda ( MetalLPP )
                                      ( rexCompile MetalLayerRegEx )
                                      ( rexExecute ( car MetalLPP ) )
                                      ( list ( rexSubstitute "notch\\1" )
                                             MetalLPP ) )
                                    MetalLPPs )
                                  ( mapcar
                                    (lambda ( MetalLPP )
                                      ( rexCompile MetalLayerRegEx )
                                      ( rexExecute ( car MetalLPP ) )
                                      ( list ( rexSubstitute "extension\\1" )
                                             MetalLPP ) )
                                    MetalLPPs ) ) ) )
                            ( append
                              ( mapcar
                                (lambda ( MetalLPP )
                                  ( car MetalLPP ) )
                                MetalLPPs )
                              ( append 
                                ( list "NOTCH" "AREA" "EXTENSION" "POLY" )
                                ( mapcar
                                  (lambda ( MetalLPP )
                                    ( rexCompile MetalLayerRegEx )
                                    ( rexExecute ( car MetalLPP ) )
                                    ( rexSubstitute "M\\1_AREA_KEEPIN" ) )
                                  MetalLPPs ) ) )

                            NotchDRCRuleFile
                            WorkingDir )

                          ( println MetalLPPs )
                          ( foreach 
                            LPP
                            ( mapcar
                              (lambda ( LPP )
                                ( list ( car LPP ) "dummy" ) )
                              MetalLPPs )
                            ( foreach Shape 
                                      ( PinUtilGetAllShapesOnLPP Routed LPP )
                                      ( dbDeleteObject Shape ) ) )

                          ( techBindTechFile
                            Routed
                            TechLibName
                            "techfile.cds"
                            t
                            )
                          )

                        (when RoutingGood
                          ( setq Done t )
                          )

                        ( dbSave Routed )
                        (unless RoutingGood 
                          ( dbPurge Routed ) )

                                        ;cycle conductor depth, LPPs
                        ( setq CurrConductorDepths
                               (cond (
                                      ( cdr CurrConductorDepths ) )
                                     (
                                      t
                                      ( setq LPPsToUse 
                                             ( append 
                                               LPPsToUse
                                               ( list ( car LPPsToSearch ) ) ) )
                                      ( setq LPPsToSearch 
                                             (cond (
                                                    ( cdr LPPsToSearch )
                                                    )
                                                   (
                                                    t
                                                    ( setq Done t )
                                                    nil ) ) )
                                      ConductorDepths
                                      ) ) ) ) ) ) ) ) ) )
          ;end while
          
          (when SuccessView
            (let (
                  ( DRCStatus
                    ( AssuraRunDRC
                      SuccessView
                      DRCRuleFile
                      DRCAssuraSets
                      WorkingDir
                      ( sprintf nil "%s/drc" 
                                WorkingDir
                                )
                      ) ) )
             ( printf "DRC Status: %s\n" (if DRCStatus "Passed" "Failed" ) )
             (let (
                   ( FinalView
                     ( betterCopyCellView
                       SuccessView
                       TargetLibName
                       CellName
                       (cond ( 
                              DRCStatus
                              "drc_lvs_clean" )
                             (
                              "lvs_clean" ) )
                       nil
                       nil
                       t ) ) )
               ( dbSave FinalView )
               ( dbPurge FinalView ) ) ) )
          
          ( dbPurge Compacted )
         

          ) ) ) ) )
  
(defun MidLevelRouteWithLPPsAndConductorDepth ( TargetLibName
                                                CellName
                                                SrcViewName
                                                RoutedViewName
                                                LPPs
                                                ConductorDepth
                                                KeepOutDepth
                                                RuleFile
                                                PreDoFiles
                                                PostDoFiles
                                                WorkingDir
                                                GNDNetName
                                                VddNetName
                                                ViaLibraryName
                                                ViaNameFormat
                                                Graphics )

  (let (
        ( DoFileName 
          ( sprintf
            nil
            "%s/cell.do"
            WorkingDir
            ) ) )
    (let (
          ( DoFilePort
            ( outfile DoFileName ) ) )
      (unless ( portp DoFilePort )
        (error 
         ( sprintf 
           nil
           "Can't write to %s"
           DoFileName ) ) )
          
      ( ConstraintsDumpLayers_ICC 
        LPPs
        ( ConstraintsConvertLPPListToViaNameList 
          LPPs
          ViaLibraryName
          ViaNameFormat )
        DoFilePort )
      ( close DoFilePort )

      ( RouterRunRouter
        TargetLibName
        CellName
        SrcViewName
        TargetLibName
        CellName
        RoutedViewName
        ConductorDepth
        KeepOutDepth
        RuleFile
        ( append PreDoFiles 
                 ( cons DoFileName PostDoFiles ) )
        WorkingDir
        ( list GNDNetName VddNetName )
        ?Quit t
        ?Import t
        ?Blocking t
        ?Graphics Graphics
        )
      
      ) ) )
          


(defun MidLevelAutoUsingPDKInfo ( CellName
                                  TargetLibName
                                  FulcrumPDKRoot
                                  SchematicFile
                                  WorkingDir
                                  LPPsToUse
                                  LPPsToSearch
                                  @key 
                                  ( Graphics t )
                                  ( MinVia nil )
                                  )

  (let (
        ( DirectivesDir
          ( sprintf
            nil
            "%s/ildirectives"
            WorkingDir ) )
        ( SkillNetlistDir
          ( sprintf
            nil
            "%s/ilnets"
            WorkingDir ) )
        ( FloorplanViewName "floorplan" )
        ( SrcLibName
          ( car ( NameParseCellName CellName ) ) ) )
    (let (
          ( Floorplan
            ( dbOpenCellViewByType
              SrcLibName
              CellName
              FloorplanViewName
              "maskLayout"
              "r"
              ) ) )
      (unless Floorplan 
        (error 
         ( sprintf 
           nil
           "Can't open floorplan view %s %s %s"
           SrcLibName
           CellName
           FloorplanViewName
           ) ) )

      (let (
            ( KeepOutDepth 32 )
            ( DirectiveTable 
              ( CellInfoGetTableForCellName
                CellName
                DirectivesDir
                ) )
            ( PinsFile
              ( sprintf
                nil
                "%s/autopins/%s.pins.il"
                WorkingDir
                CellName
                ) )
            ( AllDoFile 
              ( sprintf
                nil
                "%s/share/Fulcrum/cell_automation/router/all.do"
                FulcrumPDKRoot ) )
            ( DoFile 
              ( sprintf
                nil
                "%s/share/Fulcrum/cell_automation/router/mid.do"
                FulcrumPDKRoot ) )
            ( RuleFile 
              ( sprintf
                nil
                "%s/share/Fulcrum/cell_automation/router/all.rul"
                FulcrumPDKRoot ) )
            ( DRCRuleFile
              ( sprintf
                nil
                "%s/share/Fulcrum/assura/drc.rul"
                FulcrumPDKRoot ) )
            ( DRCAssuraSets
              ( list "LATCH_UP" ) )
            ( NotchDRCRuleFile
              ( sprintf
                nil
                "%s/share/Fulcrum/notch/fill_notches.assura.rules.all"
                FulcrumPDKRoot ) )
            ( LVSRuleFile
              ( sprintf
                nil
                "%s/share/Fulcrum/assura/extract.rul"
                FulcrumPDKRoot ) )
            ( LVSBindFile
              ( sprintf
                nil
                "%s/share/Fulcrum/assura/bind.rul"
                FulcrumPDKRoot ) )
            ( LVSCompareFile
              ( sprintf
                nil
                "%s/share/Fulcrum/assura/compare.rul"
                FulcrumPDKRoot ) )
            ( LVSIncludeFile
              ( sprintf
                nil
                "%s/share/Fulcrum/assura/LVSinclude.rsf"
                FulcrumPDKRoot ) )
            )

        ( MidLevelConvertFloorplanToLayout 
          Floorplan
          TargetLibName
          DirectiveTable
          PinsFile
          "layout"
          "compacted"
          "routed"
          BoundaryLPP
          nil; ( list AllDoFile )
          ( list DoFile )
          LPPsToUse
          LPPsToSearch
          KeepOutDepth
          RuleFile
          WorkingDir
          GNDNetName
          VddNetName
          ContactLibrary
          (if MinVia ContactMinCellNameFormat ContactCellNameFormat )
          MetalLayerRegEx
          MetalLayerFormat
          HorizontalMetalLPPs
          VerticalMetalLPPs
          UserUnitsPerMeter
          DirectiveUnitsPerMeter
          DRCRuleFile
          DRCAssuraSets
          LVSRuleFile
          LVSBindFile
          LVSCompareFile
          LVSIncludeFile
          SchematicFile
          TechLibName
          NotchDRCRuleFile
          BoundaryPinHorizontalSpacing
          BoundaryPinVerticalSpacing
          ViaLayerRegEx
          PinLength
          PinLPP
          ( list ( car PinLPP ) "drawing" )
          FoldableCellLibCellPairRegExs
          SkillNetlistDir
          DirectivesDir
          Graphics
          )
        ) ) ) )
