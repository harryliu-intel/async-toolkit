;; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
;; $Id$
;; $DateTime$
;; $Author$

; Function: CompactSuperstacks
; Parameter: CellView (CVId)
; Return: CellView (CVId)
;
; Compact the superstacks
;
;
;

(defun CompactSuperstacks ( CellView )

  (let (
        ( RoundToNearest ManufacturingGrid )
        ( InstanceMap
          ( NameMakeInstanceMapFromCellView
            CellView
            FoldableCellLibCellPairRegExs ) )
        ( AllStacks
          ( cadr
            ( NameFilterInstances
              ( getq CellView instances )
              SuperStackLibCellPairRegExs ) ) ) )
    ( foreach 
      Column
      ( PlacementRegionsGetCellRegionDataColumnList
        CellRegionData )
      (let (
            ( NPRegionParity ( PlacementRegionGetNPRegionParity Column ) )
            ( ColumnBBox ( PlacementRegionGetColumnBBox Column ) ) )
        ;sort the stacks in this column by y location
        (let (
              ( Stacks
                ( sort
                  ( setof Stack AllStacks 
                          ( and ( getq Stack bBox )
                                ( RangeIsNumberInRangeClose
                                  ( car ( getq Stack xy ) )
                                  ( RectGetXRange ColumnBBox )
                                  1e-9  ) ) )
                  (lambda ( X Y )
                    ( lessp ( cadar ( getq X bBox ) )
                            ( cadar ( getq Y bBox ) ) ) ) ) ) )

          ;remove this column's stacks from the list of stacks
          ( setq AllStacks ( ListDifference
                             AllStacks
                             Stacks
                             `equal 
                             nil ) )
          ;Break up the stacks according to the type
          ;i.e. split up NMOS PMOS GATE superstacks
          ;split if the run length exceeds 5, to aid in spacing
          ;things out
          (let (
                ( StackClumps 
                  ( ListSplitOnPredicate
                    Stacks
                    (lambda ( C1 C2  )
                      (let (
                            ( Split
                              ( and 
                                ( exists 
                                  C
                                  ( list C1 C2 )
                                  ( equal 
                                    ( getq C cellName )
                                    GateSuperStackCellName ) )
                                ( exists
                                  C 
                                  ( list C1 C2 )
                                  ( not ( equal 
                                          ( getq C cellName )
                                          GateSuperStackCellName ) ) ) ) ) )
                        Split ) ) ) ) ) 
            ;Inline the stacks into components
            (let (
                  ( Clumps
                    ( mapcar
                      (lambda ( StackClump )
                        ( ListNonDestructiveMapCan
                          (lambda ( Stack )
                            ( InlineSuperStack
                              CellView
                              CellView
                              InstanceMap
                              InstanceMap
                              Stack
                              "netlist"
                              nil
                              FoldableCellLibCellPairRegExs ) )
                          StackClump
                          ) )
                      StackClumps ) ) )
              ;This is all the components in the column
              (let (
                    ( Components
                      ( ListNonDestructiveMapCan
                        (lambda ( Clump ) Clump )
                        Clumps ) ) )
                ;Derive width data of all the components in this column only
                (let (
                      ( NWidthData
                        ( NPSearchGetWidthData 
                          ( cadr ( NameFilterInstances 
                                   Components
                                   GateLibCellPairRegExs ) )
                          ( cadr ( NameFilterInstances 
                                   Components
                                   NChainLibCellPairRegExs ) )
                          ( times UserUnitsPerMeter EvilGatePRegionOffset )
                          ( times UserUnitsPerMeter TransistorMinimumContactableWidth )
                          ( times UserUnitsPerMeter TransistorDiffusionToBBoxDistance )
                          MaximumComponentWidthRatio
                          ( times UserUnitsPerMeter NRegionOffsetFromColumnCenter )
                          ( times UserUnitsPerMeter 
                                  ( CellInfoGetLayerWirePitchInMeters
                                    DirectiveTable
                                    Metal3LPP
                                    DirectiveUnitsPerMeter ) )
                          MaximumNColumnWidth
                          UserUnitsPerMeter
                          t
                          PreferEvenFolds
                          ( times UserUnitsPerMeter ExtraGateWidth )
                          ?MaxTransistorFolds 1
                          ) )
                      ( PWidthData
                        ( NPSearchGetWidthData 
                          ( cadr ( NameFilterInstances 
                                   Components
                                   GateLibCellPairRegExs ) )
                          ( cadr ( NameFilterInstances 
                                   Components
                                   PChainLibCellPairRegExs ) )
                          ( times UserUnitsPerMeter EvilGatePRegionOffset )
                          ( times UserUnitsPerMeter TransistorMinimumContactableWidth )
                          ( times UserUnitsPerMeter TransistorDiffusionToBBoxDistance )
                          MaximumComponentWidthRatio
                          ( times UserUnitsPerMeter PRegionOffsetFromColumnCenter )
                          ( times UserUnitsPerMeter 
                                  ( CellInfoGetLayerWirePitchInMeters
                                    DirectiveTable
                                    Metal3LPP
                                    DirectiveUnitsPerMeter ) )
                          MaximumPColumnWidth
                          UserUnitsPerMeter
                          nil
                          PreferEvenFolds
                          ( times UserUnitsPerMeter ExtraGateWidth )
                          ?MaxTransistorFolds 1
                          ) ) )
                  ;binary search over the possible columns widths so as to maximally compact the resultant superstacks
                  (let (
                        ( MinimumNIndex
                          ( BinarySearchFindSmallest
                            0
                            ( difference 
                              ( length 
                                ( NPSearchGetWidthVectorFromWidthData 
                                  NWidthData ) ) 1 )
                            (lambda ( Trial )
                              ( geqp
                                ( NPSearchGetWidthFromIndex
                                  NWidthData
                                  Trial )
                                ( NPSearchGetMinFitWidthFromWidthData
                                  NWidthData ) ) )
                            nil ) )
                        ( MinimumPIndex
                          ( BinarySearchFindSmallest
                            0
                            ( difference 
                              ( length
                                ( NPSearchGetWidthVectorFromWidthData 
                                  PWidthData ) ) 1 )
                            (lambda ( Trial )
                              ( geqp
                                ( NPSearchGetWidthFromIndex
                                  PWidthData
                                  Trial )
                                ( NPSearchGetMinFitWidthFromWidthData
                                  PWidthData ) ) )
                            nil ) )
                        ( MaximumNIndex
                          ( BinarySearchFindGreatest
                            0
                            ( difference 
                              ( length 
                                ( NPSearchGetWidthVectorFromWidthData 
                                  NWidthData ) ) 1 )
                            (lambda ( Trial )
                              ( leqp
                                ( NPSearchGetWidthFromIndex
                                  NWidthData
                                  Trial )
                                NWidth ) )
                            nil ) )
                        ( MaximumPIndex
                          ( BinarySearchFindGreatest
                            0
                            ( difference 
                              ( length 
                                ( NPSearchGetWidthVectorFromWidthData 
                                  PWidthData ) ) 1 )
                            (lambda ( Trial )
                              ( leqp
                                ( NPSearchGetWidthFromIndex
                                  PWidthData
                                  Trial )
                                PWidth ) )
                            nil ) ) )
                    ( setq MinimumNIndex ( min MinimumNIndex
                                               MaximumNIndex ) )
                    ( setq MinimumPIndex ( min MinimumPIndex
                                               MaximumPIndex ) )
                    (let (
                          ( TrialPair
                            ( NPSearchGetSmallestFittingTrialPairForColumn
                              CellView
                              ( setof Clump Clumps
                                      ( equal ( getq ( car Clump ) libName )
                                              GateLibraryName ) )
                              ( setof Clump Clumps
                                      ( equal ( getq ( car Clump ) libName )
                                              StackLibraryName ) )
                              NChainLibCellPairRegExs
                              PChainLibCellPairRegExs
                              NWidthData
                              PWidthData
                              MinimumNIndex
                              MinimumPIndex
                              MaximumNIndex
                              MaximumPIndex
                              ( times UserUnitsPerMeter
                                      NRegionOffsetFromColumnCenter )
                              ( times UserUnitsPerMeter
                                      PRegionOffsetFromColumnCenter )
                              NColumnWidthParamName
                              PColumnWidthParamName
                              ( difference
                                ( RectGetHeight ColumnBBox )
                                1.15 )
                              SuperStackLibraryName
                              GateSuperStackCellName
                              NSuperStackCellName
                              PSuperStackCellName
                              SuperStackViewName
                              UserUnitsPerMeter
                              RoundToNearest
                              ( list MaxNonPowerMergeHeight
                                     MaxPowerMergeHeight
                                     MaxMergeHeight )
                              GNDNetName
                              VddNetName
                              PreferEvenFolds
                              nil ) ) )
                      (let (
                            ( NColumnWidth
                              ( NPSearchGetWidthFromIndex
                                NWidthData
                                ( car TrialPair ) ) )
                            ( PColumnWidth
                              ( NPSearchGetWidthFromIndex
                                PWidthData
                                ( cadr TrialPair ) ) ) )

                        ;re-instantiate the superstack groups
                        ( FigStackClumps
                          ( mapcar
                            `SuperStackCompactStackGroup
                            ;fold the components first
                            ( mapcar
                              (lambda ( Clump )
                                (cond (
                                       ;gate folding
                                       ( equal ( getq ( car Clump ) libName )
                                               GateLibraryName )
                                       ( NPSearchFoldGates
                                         Clump
                                         NColumnWidthParamName
                                         PColumnWidthParamName
                                         NColumnWidth
                                         PColumnWidth
                                         UserUnitsPerMeter )
                                       ( SuperStackCreateStackGroupsFromClumpUsingPDKInfo 
                                         CellView
                                         Clump ) )
                                      (
                                       ;chains
                                       (let (
                                             ( NChains ( cadr ( NameFilterInstances
                                                                Clump
                                                                NChainLibCellPairRegExs ) ) )
                                             ( PChains ( cadr ( NameFilterInstances
                                                                Clump 
                                                                PChainLibCellPairRegExs ) ) ) )
                                           
                                         ( list
                                           ( NPSearchRefoldAndChainChains
                                             CellView
                                             NChains
                                             SuperStackLibraryName
                                             NSuperStackCellName
                                             SuperStackViewName
                                             NChainPrefix
                                             NColumnWidth
                                             ( times 
                                               UserUnitsPerMeter
                                               NRegionOffsetFromColumnCenter )
                                             UserUnitsPerMeter
                                             RoundToNearest
                                             ( list MaxNonPowerMergeHeight
                                                    MaxPowerMergeHeight
                                                    MaxMergeHeight )
                                             GNDNetName
                                             VddNetName
                                             PreferEvenFolds
                                             ?Align 
                                             (cond (
                                                    NPRegionParity
                                                    "ll" )
                                                   (
                                                    "lr" ) )
                                             )
                                           ( NPSearchRefoldAndChainChains
                                             CellView
                                             PChains
                                             SuperStackLibraryName
                                             PSuperStackCellName
                                             SuperStackViewName
                                             PChainPrefix
                                             PColumnWidth
                                             ( times 
                                               UserUnitsPerMeter
                                               PRegionOffsetFromColumnCenter )
                                             UserUnitsPerMeter
                                             RoundToNearest
                                             ( list MaxNonPowerMergeHeight
                                                    MaxPowerMergeHeight
                                                    MaxMergeHeight )
                                             GNDNetName
                                             VddNetName
                                             PreferEvenFolds
                                             ?Align 
                                             (cond (
                                                    NPRegionParity
                                                    "lr" )
                                                   (
                                                    "ll" ) )
                                             ) ) ) ) ) )
                              Clumps ) )
                          ( RectGetBottom ColumnBBox )
                          )
                        ) ) ) ) ) ) ) ) ) )
    
    ;space out the superstacks
    (when t
      ( DeCompactVerticalKeepOrderFromCellRegionData
        CellView
        CellRegionData
        `DeCompactVerticalKeepOrderDefault
        ( list
          ( times UserUnitsPerMeter ManufacturingGrid )
          t ) )
      )
      
    ;( dbSave CellView )
    ) 
CellView
)





