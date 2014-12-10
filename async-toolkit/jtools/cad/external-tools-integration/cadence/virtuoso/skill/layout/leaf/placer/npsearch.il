; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$


(defun NPSearchSnapToGrid ( GridWidth value )
  ceiling( (value - UserUnitsPerMeter*M2StrutOffset) /GridWidth)*GridWidth + UserUnitsPerMeter*M2StrutOffset
)
;snap to n+1/2 grid so that it lines up with Metal2 power strip.

(defun NPSearchMakeWidthData ( ComponentInfoListTable
                               GateComponentInfoListTable
                               WidthVector
                               MinFitWidth )
  ( list ComponentInfoListTable
         GateComponentInfoListTable
         WidthVector
         MinFitWidth ) )

;npwidth data
(defun NPSearchGetComponentInfoListTableFromWidthData  ( WidthData )
  ( nth 0 WidthData ) )

(defun NPSearchGetGateComponentInfoListTableFromWidthData  ( WidthData )
  ( nth 1 WidthData ) )

(defun NPSearchGetWidthVectorFromWidthData  ( WidthData )
  ( nth 2 WidthData ) )

(defun NPSearchGetMinFitWidthFromWidthData  ( WidthData )
  ( nth 3 WidthData ) )

;component info
(defun NPSearchMakeComponentInfo ( Instance Width Height )
  ( list ( getq Instance name ) Width Height ) )

(defun NPSearchGetInstance ( ComponentInfo )
  ( nth 0 ComponentInfo ) )

(defun NPSearchGetWidth ( ComponentInfo )
  ( nth 1 ComponentInfo ) )

(defun NPSearchGetHeight ( ComponentInfo )
  ( nth 2 ComponentInfo ) )

;componetn info lists are in order of increasing width
(defun NPSearchGetMinHeightComponentInfoFromColumnWidth ( ComponentInfoList ColumnWidth )  
  ( while ( and ( cdr ComponentInfoList )
                ( leqp ( NPSearchGetWidth ( cadr ComponentInfoList ) ) ColumnWidth ) )
    (let () 
      ( setq ComponentInfoList ( cdr ComponentInfoList ) ) )
    )
  ( car ComponentInfoList ) )

(defun NPSearchGetMinHeightFromColumnWidth ( ComponentInfoList ColumnWidth )  
  ( NPSearchGetHeight ( NPSearchGetMinHeightComponentInfoFromColumnWidth
                        ComponentInfoList 
                        ColumnWidth ) ) )

(defun NPSearchGetMaxWidthFromColumnWidth ( ComponentInfoList ColumnWidth )  
  ( NPSearchGetWidth ( NPSearchGetMinHeightComponentInfoFromColumnWidth
                       ComponentInfoList 
                       ColumnWidth ) ) )










;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Component Info Lists
;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;
;; GATE
;;;;;;;;;

(defun NPSearchGateGetNRange ( Gate EvilGatePRegionOffset )
  ( RectGetXRange
    ( NPSearchGateGetNRect
      Gate
      EvilGatePRegionOffset ) ) )

(defun NPSearchGateGetPRange ( Gate EvilGatePRegionOffset )
  ( RectGetXRange
    ( NPSearchGateGetPRect
      Gate
      EvilGatePRegionOffset ) ) )

(defun NPSearchGateGetNRect ( Gate EvilGatePRegionOffset )
  (let (
        ( Box ( getq ( getq Gate master ) bBox ) ) )
    ( dbTransformBBox
      ( list
        ( car Box )
        ( list
          EvilGatePRegionOffset
          ( RectGetTop Box ) ) )
      ( getq Gate transform ) ) ) )

(defun NPSearchGateGetPRect ( Gate EvilGatePRegionOffset )
  (let (
        ( Box ( getq ( getq Gate master ) bBox ) ) )
    ( dbTransformBBox
      ( list
        ( list
          EvilGatePRegionOffset
          ( RectGetBottom Box ) )
        ( cadr Box )
        )
      ( getq Gate transform ) ) ) )

(defun NPSearchGateGetNWidth ( Gate EvilGatePRegionOffset )
  ( RangeGetSize 
    ( NPSearchGateGetNRange 
      Gate
      EvilGatePRegionOffset ) ) )

(defun NPSearchGateGetPWidth ( Gate EvilGatePRegionOffset )
  ( RangeGetSize 
    ( NPSearchGateGetPRange 
      Gate
      EvilGatePRegionOffset ) ) )

(defun NPSearchGateGetComponentInfo ( Gate Folds EvilGatePRegionOffset     ExtraGateWidth ReturnFunc ) 
 ( cond (
	 ( NPSearchGateCanOverrideFolds Gate )
	 ( NPSearchGateSetFolds Gate Folds ) ) )
 (let (
       ( NWidth ( plus ExtraGateWidth 
                       ( NPSearchGateGetNWidth Gate EvilGatePRegionOffset ) ) )
       ( PWidth ( plus ExtraGateWidth 
                       ( NPSearchGateGetPWidth Gate EvilGatePRegionOffset ) ) )
       ( Height ( RectGetHeight ( getq Gate bBox ) ) ) 
       )
   ( apply ReturnFunc ( list Gate NWidth PWidth Height ) ) ) )

;gets a list of infos in order of decreasing width
;includes with/without well plugs
(defun NPSearchGateGetComponentInfoList ( Gate MinFolds MaxFolds EvilGatePRegionOffset     ExtraGateWidth ReturnFunc )
  (let (
	( GateComponentInfoList nil )
       )
    ( cond ( 
	    ( NPSearchGateCanOverrideFolds Gate )	  
	    ( NPSearchGateSetOverrideFolds Gate "TRUE" )  	  
	    ( for Folds MinFolds MaxFolds
		  ( setq GateComponentInfoList 
                         ( cons 
                           ( NPSearchGateGetComponentInfo 
                             Gate 
                             Folds 
                             EvilGatePRegionOffset 
                             ExtraGateWidth
                             ReturnFunc )
                           GateComponentInfoList ) )
                  )
	    ( NPSearchGateSetOverrideFolds Gate nil )
            )
	   (
            t
	    ( setq GateComponentInfoList 
                   ( cons 
                     ( NPSearchGateGetComponentInfo 
                       Gate 
                       0 
                       EvilGatePRegionOffset 
                       ExtraGateWidth
                       ReturnFunc ) 
                     GateComponentInfoList ) )
            ) )
   ;sort in decreasing width
    ( ListUniq
      ( sort ( copy GateComponentInfoList ) 
             (lambda ( ComponentInfo1 ComponentInfo2 ) 
               ( or ( lessp ( NPSearchGetWidth ComponentInfo1 )
                            ( NPSearchGetWidth ComponentInfo2 ) )
                    ( greaterp ( NPSearchGetHeight ComponentInfo1 )
                               ( NPSearchGetHeight ComponentInfo2 ) )
                    ) ) ) ) ) )

(defun NPSearchGateGetNComponentInfoList ( Gate MinFolds MaxFolds EvilGatePRegionOffset ExtraGateWidth )
  ( NPSearchGateGetComponentInfoList 
    Gate
    MinFolds
    MaxFolds
    EvilGatePRegionOffset
    ExtraGateWidth
    (lambda ( Instance NWidth PWidth Height ) ( NPSearchMakeComponentInfo Instance NWidth Height ) ) ) )

(defun NPSearchGateGetPComponentInfoList ( Gate MinFolds MaxFolds EvilGatePRegionOffset ExtraGateWidth )
  ( NPSearchGateGetComponentInfoList 
    Gate
    MinFolds
    MaxFolds
    EvilGatePRegionOffset
    ExtraGateWidth
    (lambda ( Instance NWidth PWidth Height ) ( NPSearchMakeComponentInfo Instance PWidth Height ) ) ) )

(defun NPSearchGateSetFolds ( Gate Folds )
  ( dbReplaceProp Gate "Folds" "int" Folds ) )

(defun NPSearchGateGetFolds ( Gate )
  ( PropGetPropValueForPropName ( getq ( getq Gate master ) prop ) "folds" ) )

(defun NPSearchGateSetOverrideFolds ( Gate Value )  
  ( dbReplaceProp Gate "override_folds" "boolean" Value ) )

(defun NPSearchGateCanOverrideFolds ( Gate )
  ( exists Value ( getq ( NPSearchGateGetParameterProp Gate ) value ) ( equal ( getq Value name ) "override_folds" ) ) )

(defun NPSearchGateGetParameterProp ( Gate )
 ( car ( exists Property ( getq ( getq ( getq Gate master ) superMaster ) prop ) ( equal ( getq Property name ) "parameters" ) ) ) )




;;;;;;;;;;;;;;;;;;;;;;
;; N and P components
;;;;;;;;;;;;;;;;;;;;;;;

(defun NPSearchChainGetComponentInfo ( Component
                                       Folds
                                       ComponentRegionOffsetFromColumnCenter 
                                       TransistorDiffusionToBBoxDistance
                                       ExtraGateWidth )
  (let (
	( Strength ( difference ( RectGetWidth ( getq Component bBox ) ) 
                                ( times 2.0 TransistorDiffusionToBBoxDistance ) ) )
        )
    (let (
          ( Width ( plus ComponentRegionOffsetFromColumnCenter
                         ( plus ( quotient Strength ( float ( plus Folds 1 ) ) )
                                ( times 2.0 TransistorDiffusionToBBoxDistance )
                                ExtraGateWidth
                                ) ) )
          ( Height ( times ( plus Folds 1 ) ( RectGetHeight ( getq Component bBox ) ) ) )
          )
      
      ( NPSearchMakeComponentInfo Component Width Height ) ) ) )

(defun NPSearchChainGetComponentInfoList ( Component
                                           MinFolds 
                                           MaxFolds
                                           ComponentRegionOffsetFromColumnCenter
                                           TransistorDiffusionToBBoxDistance
                                           PreferEvenFolds
                                           ExtraGateWidth )
  (let (
	( ComponentInfoList nil ) )
    ( for Folds MinFolds MaxFolds
          (if ( or ( not PreferEvenFolds )
                   ( equal Folds 0 )
                   ( equal ( mod Folds 2 ) 1 ) )
              ( setq ComponentInfoList 
                     ( cons ( NPSearchChainGetComponentInfo
                              Component
                              Folds
                              ComponentRegionOffsetFromColumnCenter
                              TransistorDiffusionToBBoxDistance
                              ExtraGateWidth )
                            ComponentInfoList ) ) ) )
    ComponentInfoList ) )

;;;;;;;;;;;;
;; API
;;;;;;;;;;;;

(defun NPSearchGetMaxWidthMeanWidthRatioFromWidthData ( WidthData PossibleColumnWidth )
  ( NPSearchGetMaxWidthMeanWidthRatio
    ( append 
      ( mapcar `cadr ( tableToList 
                       ( NPSearchGetComponentInfoListTableFromWidthData WidthData ) ) )
      ( mapcar `cadr ( tableToList 
                       ( NPSearchGetGateComponentInfoListTableFromWidthData WidthData ) ) ) )
    PossibleColumnWidth ) )
      

(defun NPSearchGetMaxWidthMeanWidthRatio ( AllComponentInfoLists PossibleColumnWidth )
 ;max/mean<ratio
  (let (
        ( MaximumWidthComponentInfoList
          ( ListFindMaximumElement
            AllComponentInfoLists
            (lambda ( OtherComponentInfoList )
              ( NPSearchGetMaxWidthFromColumnWidth 
                OtherComponentInfoList
                PossibleColumnWidth ) ) ) ) )
    ( quotient ( NPSearchGetMaxWidthFromColumnWidth 
                 MaximumWidthComponentInfoList
                 PossibleColumnWidth )
               ( ListFindMean
                 AllComponentInfoLists
                 (lambda ( OtherComponentInfoList )
                   ( NPSearchGetMaxWidthFromColumnWidth 
                     OtherComponentInfoList
                     PossibleColumnWidth ) ) ) ) ) )



(defun NPSearchGetWidthData ( Gates
                              Components
                              EvilGatePRegionOffset
                              TransistorMinimumContactableWidth
                              TransistorDiffusionToBBoxDistance
                              MaximumComponentWidthRatio
                              ComponentRegionOffsetFromColumnCenter
                              SnapToGridWidth
                              MaximumColumnWidth
                              UserUnitsPerMeter
                              IsNRegion
                              PreferEvenFolds
                              ExtraGateWidth
                              @key 
                              ( MaxTransistorFolds 6 )
                              )
  (let (
        ( GateComponentInfoListTable ( makeTable `GateComponentInfoTable ) )
        ( ComponentInfoListTable ( makeTable `ComponentInfoListTable ) )
        ( WidthTable ( makeTable `WidthTable nil ) )
        )
      ; get the info list
      ( foreach Gate Gates 	
 	( setarray 
	  GateComponentInfoListTable 
	  ( getq Gate name )
          (if IsNRegion
              ( NPSearchGateGetNComponentInfoList 
                Gate 
                0 
                ( max 0
                      ( difference
                        ( floor
                          ( quotient
                            ( times
                              UserUnitsPerMeter
                              ( ComponentInfoGetNStrengthFromComponent
                                Gate ) )
                            TransistorMinimumContactableWidth ) )
                        1 ) )
                EvilGatePRegionOffset
                ExtraGateWidth )
            ( NPSearchGateGetPComponentInfoList 
              Gate 
              0 
              ( max 0
                    ( difference
                      ( floor
                        ( quotient
                          ( times 
                              UserUnitsPerMeter
                              ( ComponentInfoGetPStrengthFromComponent
                                Gate ) )
                          TransistorMinimumContactableWidth ) )
                      1 ) )
              EvilGatePRegionOffset
              ExtraGateWidth ) ) ) )
	
    
      ;;
      ;; Assumes superstacks only!!!
      ;;
      ( foreach Component Components
                ( setarray ComponentInfoListTable 
                           ( getq Component name )
                           ( NPSearchChainGetComponentInfoList 
                             Component
                             0
                ( min
                  MaxTransistorFolds
                  ( max 0
                        ( difference 
                          ( floor
                            ( quotient
                              ( times 
                                UserUnitsPerMeter
                                (if IsNRegion
                                    ( ComponentInfoGetNStrengthFromComponent
                                      Component )
                                  ( ComponentInfoGetPStrengthFromComponent
                                               Component ) ) )
                              TransistorMinimumContactableWidth ) )
                          1 ) ) )
                ComponentRegionOffsetFromColumnCenter
                TransistorDiffusionToBBoxDistance
                PreferEvenFolds
                ExtraGateWidth
                ) ) )
      
      (let (
            ( GateComponentInfoLists ( mapcar `cadr ( tableToList GateComponentInfoListTable ) ) )
            ( ComponentInfoLists ( mapcar `cadr ( tableToList ComponentInfoListTable ) ) ) )

                                        ;get the set of n,p widths - O(n)     
        (let (
              ( SmallestColumnWidth MaximumColumnWidth )
              ( AllComponentInfoLists ( append GateComponentInfoLists ComponentInfoLists ) ) )
                     
;          ( printf "gates: %L\n"  GateComponentInfoLists )
;          ( printf "stacks: %L\n" ComponentInfoLists )

          ( foreach ComponentInfoList AllComponentInfoLists
                    ( foreach ComponentInfo ComponentInfoList
                              (let ( 
                                    ( PossibleColumnWidth 
                                          NPSearchSnapToGrid( SnapToGridWidth NPSearchGetWidth( ComponentInfo ))))
                  ; snap to (2n+1)*0.24
                  ;              ( printf "possible width: %f\n" PossibleColumnWidth )
                  ;              ( printf "max/mean=%f\n" ( NPSearchGetMaxWidthMeanWidthRatio

                                (when ( and
                                        ( not ( arrayref WidthTable PossibleColumnWidth ) )
                                        ( leqp PossibleColumnWidth MaximumColumnWidth )
                                        )
                                  ( setarray WidthTable PossibleColumnWidth ComponentInfo )
                                  (when ( and
                                          ( lessp PossibleColumnWidth SmallestColumnWidth )
                                          ( forall OtherComponentInfoList AllComponentInfoLists
                                                   ( leqp 
                                                     ( NPSearchGetMaxWidthFromColumnWidth
                                                       OtherComponentInfoList
                                                       PossibleColumnWidth )
                                                     PossibleColumnWidth )
                                                   ) )
                                    ( setq SmallestColumnWidth PossibleColumnWidth )
                                    ) ) ) ) )
          ( setq WidthList ( mapcar `car ( tableToList WidthTable ) ) )
          ( setq WidthVector ( sort WidthList `lessp ) )
          ( NPSearchMakeWidthData 
            ComponentInfoListTable
            GateComponentInfoListTable 
            WidthVector
            SmallestColumnWidth
            ) ) ) ) )
  



(defun NPSearchGetWidthFromIndex ( WidthData WidthIndex )
  ( nth WidthIndex ( NPSearchGetWidthVectorFromWidthData WidthData ) ) )


(defun NPSearchGetChainSuperStackHeightAreaPairFromColumnWidth ( CellView
                                                                 SuperStackLibName
                                                                 SuperStackCellName
                                                                 SuperStackViewName
                                                                 ChainLibCellPairRegExs
                                                                 ColumnWidth
                                                                 UserUnitsPerMeter
                                                                 RoundToNearest
                                                                 MergeParams
                                                                 GNDNetName
                                                                 VddNetName
                                                                 PreferEvenFolds
                                                                 BVerbose )
  (let (
        ( FilterResult 
          ( NameFilterInstances 
            ( getq CellView instances )
            ChainLibCellPairRegExs ) ) )
    (let (
          ( Chains ( cadr FilterResult ) )
          ( Others ( car FilterResult ) ) )
      (let (
            ( ChainInfos 
              ( ListNonDestructiveMapCan 
                (lambda ( Chain )
                  ( ChainFoldToChainInfos 
                    CellView
                    Chain
                    ColumnWidth 
                    UserUnitsPerMeter
                    RoundToNearest 
                    PreferEvenFolds ) )
                Chains ) ) )
        (let ( 
              ( HeightAreaPair 
                ( SuperStackCalculateTotalHeightTotalAreaPairOfStackedComponentInfos
                  CellView
                  SuperStackLibName
                  SuperStackCellName
                  SuperStackViewName
                  ChainInfos
                  Others
                  `SuperStackGetDefaultMergeFuncInfoLists_Chain
                  MergeParams
                  GNDNetName
                  VddNetName
                  t
                  BVerbose ) ) )
          HeightAreaPair ) ) ) ) )


(defun NPSearchGetGateComponentInfoForColumnWidth ( Gate
                                                    ColumnWidthParameterName 
                                                    ColumnWidth
                                                    OtherColumnWidthParameterName
                                                    OtherColumnWidth
                                                    UserUnitsPerMeter )
  (let (
        ( OriginalColumnWidth ( PCellGetParameterValue 
                                Gate
                                ColumnWidthParameterName ) )
        ( OriginalOtherColumnWidth ( PCellGetParameterValue 
                                     Gate
                                     OtherColumnWidthParameterName ) )
        )
    (when OriginalColumnWidth 
      ( dbReplaceProp Gate ColumnWidthParameterName "float"
                      ( quotient ColumnWidth UserUnitsPerMeter ) ) )
    (when OriginalOtherColumnWidth 
      ( dbReplaceProp Gate OtherColumnWidthParameterName "float"
                      ( quotient OtherColumnWidth UserUnitsPerMeter ) ) )
    (let (
          ( Ret ( ComponentInfoCreateComponentInfoFromComponent Gate ) ) )
      (when OriginalColumnWidth 
        ( dbReplaceProp Gate ColumnWidthParameterName "float" OriginalColumnWidth ) )
      (when OriginalOtherColumnWidth 
        ( dbReplaceProp Gate OtherColumnWidthParameterName "float" OriginalOtherColumnWidth ) )
      Ret ) ) )


(defun NPSearchGetGateSuperStackHeightAreaPairFromColumnWidth ( CellView
                                                                SuperStackLibName
                                                                SuperStackCellName
                                                                SuperStackViewName
                                                                GateLibCellPairRegExs
                                                                ColumnWidthParameterName
                                                                OtherColumnWidthParameterName
                                                                ColumnWidth
                                                                UserUnitsPerMeter
                                                                RoundToNearest
                                                                MergeParams
                                                                GNDNetName
                                                                VddNetName
                                                                PreferEvenFolds
                                                                BVerbose )
  (let (
        ( FilterResult 
          ( NameFilterInstances 
            ( getq CellView instances )
            GateLibCellPairRegExs ) ) )
    (let (
          ( Gates ( cadr FilterResult ) )
          ( Others ( car FilterResult ) ) )
      (let (
            ( ComponentInfos
              ( mapcar
                (lambda ( Gate )
                  ( NPSearchGetGateComponentInfoForColumnWidth
                    Gate
                    ColumnWidthParameterName 
                    ColumnWidth
                    OtherColumnWidthParameterName 
                    1e-3
                    UserUnitsPerMeter) )
                Gates ) ) )
        (let ( 
              ( HeightAreaPair 
                ( SuperStackCalculateTotalHeightTotalAreaPairOfStackedComponentInfos
                  CellView
                  SuperStackLibName
                  SuperStackCellName
                  SuperStackViewName
                  ComponentInfos
                  Others
                  `SuperStackGetDefaultMergeFuncInfoLists_Gate
                  ( append MergeParams
                           ( list GNDNetName VddNetName ) )
                  GNDNetName
                  VddNetName
                  nil
                  BVerbose ) ) )
          HeightAreaPair ) ) ) ) )

(defun NPSearchGetMinimumTrialMaximumTriple ( RegionHeightAvailable
                                              WidthData
                                              ColumnWidthParameterName
                                              OtherColumnWidthParameterName
                                              CellView
                                              SuperStackLibName
                                              SuperStackCellName
                                              GateSuperStackCellName
                                              SuperStackViewName
                                              ChainLibCellPairRegExs
                                              GateLibCellPairRegExs
                                              UserUnitsPerMeter
                                              RoundToNearest
                                              MergeParams
                                              GNDNetName
                                              VddNetName
                                              PreferEvenFolds
                                              Verbose )
 
  (let ( 
        ( Min ( NPSearchGetSmallestFittingTrial
                WidthData
                ColumnWidthParameterName
                OtherColumnWidthParameterName
                RegionHeightAvailable
                CellView
                SuperStackLibName
                SuperStackCellName
                GateSuperStackCellName
                SuperStackViewName
                ChainLibCellPairRegExs
                GateLibCellPairRegExs
                UserUnitsPerMeter
                RoundToNearest
                MergeParams
                GNDNetName
                VddNetName
                PreferEvenFolds
                Verbose ) )
        ( Max ( NPSearchGetSearchMaximum
                WidthData ) ) )
    ( list 0 Min Max ) ) )

(defun NPSearchGetSearchMaximum ( WidthData )
  (let (   
        ( WidthVectorLength ( length ( NPSearchGetWidthVectorFromWidthData WidthData ) ) ) )
    ( difference WidthVectorLength 1 ) ) )
    

(defun NPSearchGetSmallestFittingTrial ( WidthData
                                         ColumnWidthParameterName
                                         OtherColumnWidthParameterName
                                         RegionHeightAvailable
                                         CellView
                                         SuperStackLibName
                                         SuperStackCellName
                                         GateSuperStackCellName
                                         SuperStackViewName
                                         ChainLibCellPairRegExs
                                         GateLibCellPairRegExs
                                         UserUnitsPerMeter
                                         RoundToNearest
                                         MergeParams
                                         GNDNetName
                                         VddNetName
                                         PreferEvenFolds
                                         BVerbose )
                                   
  (let (
        ( WidthVectorLength 
          ( length ( NPSearchGetWidthVectorFromWidthData WidthData ) ) ) )
        
    ;binary search over width indices to find first one that fits in the height
    (let (
          ( Min 0 )
          ( Max ( difference WidthVectorLength 1 ) ) )
      ( BinarySearchFindSmallest
        Min
        Max 
        (lambda ( Trial RegionHeightAvailable WidthData )
          (let (
                ( ColumnWidth ( NPSearchGetWidthFromIndex WidthData Trial ) ) )
            (let ( 
                  ( ChainHeight
                    ( car ( NPSearchGetChainSuperStackHeightAreaPairFromColumnWidth
                            CellView
                            SuperStackLibName
                            SuperStackCellName
                            SuperStackViewName
                            ChainLibCellPairRegExs
                            ColumnWidth
                            UserUnitsPerMeter
                            RoundToNearest
                            MergeParams
                            GNDNetName
                            VddNetName
                            PreferEvenFolds
                            BVerbose ) ) ) 
                  ( GateHeight 
                    ( car ( NPSearchGetGateSuperStackHeightAreaPairFromColumnWidth 
                              CellView
                              SuperStackLibName
                              GateSuperStackCellName
                              SuperStackViewName
                              GateLibCellPairRegExs
                              ColumnWidthParameterName
                              OtherColumnWidthParameterName
                              ColumnWidth
                              UserUnitsPerMeter
                              RoundToNearest
                              MergeParams
                              GNDNetName
                              VddNetName
                              PreferEvenFolds
                              BVerbose ) ) ) )
              (let (
                    ( RegionHeightUsed 
                      ( plus ChainHeight GateHeight ) ) )
                ( lessp ( times 0.8 RegionHeightUsed ) RegionHeightAvailable ) ) ) ) )
        ( list RegionHeightAvailable WidthData  ) ) ) ) )

(defun NPSearchGetSmallestFittingTrialPairForColumn ( CellView
                                                      GateClumps
                                                      ChainClumps
                                                      NChainLibCellPairRegExs
                                                      PChainLibCellPairRegExs
                                                      NWidthData
                                                      PWidthData
                                                      MinimumNIndex
                                                      MinimumPIndex
                                                      MaximumNIndex
                                                      MaximumPIndex
                                                      NRegionOffsetFromColumnCenter
                                                      PRegionOffsetFromColumnCenter 
                                                      NColumnWidthParameterName
                                                      PColumnWidthParameterName
                                                      RegionHeightAvailable
                                                      SuperStackLibName
                                                      GateSuperStackCellName
                                                      NSuperStackCellName
                                                      PSuperStackCellName
                                                      SuperStackViewName
                                                      UserUnitsPerMeter
                                                      RoundToNearest
                                                      MergeParams
                                                      GNDNetName
                                                      VddNetName
                                                      PreferEvenFolds
                                                      BVerbose )
  (let (
        ( CurrentSearchTable nil )
        ( PlacementGood nil )
        ( NWidthSearchTable ( makeTable `N nil ) )
        ( PWidthSearchTable ( makeTable `P nil ) )
        )

    ( BinarySearchInitTable
      NWidthSearchTable
      MinimumNIndex
      MaximumNIndex
      MinimumNIndex )
    ( BinarySearchInitTable
      PWidthSearchTable
      MinimumPIndex
      MaximumPIndex
      MinimumPIndex )

    (while ( not 
             ( forall 
               SearchTable
               ( list NWidthSearchTable PWidthSearchTable )
               ( arrayref SearchTable "done" ) ) )
      ( setq CurrentSearchTable
             (cond (
                    ( and CurrentSearchTable
                          ( and
                            ( not ( arrayref CurrentSearchTable "done" ) )
                            ( not PlacementGood ) ) )
                    CurrentSearchTable )
                   (
                    ( ListFindMaximumElement
                      ( setof SearchTable
                              ( list NWidthSearchTable PWidthSearchTable )
                              ( not ( arrayref SearchTable "done" ) ) )
                      (lambda ( SearchTable )
                        ( NPSearchGetMaxWidthMeanWidthRatioFromWidthData
                          (if ( equal SearchTable NWidthSearchTable ) 
                              NWidthData PWidthData )
                          (if ( equal SearchTable NWidthSearchTable ) 
                              ( NPSearchGetWidthFromIndex
                                NWidthData
                                ( arrayref NWidthSearchTable "max" ) )
                              ( NPSearchGetWidthFromIndex
                                NWidthData
                                ( arrayref NWidthSearchTable "max" ) )
                          ) ) ) ) ) ) )
      ( println CurrentSearchTable )


      (let (
            ( NWidth
              ( NPSearchGetWidthFromIndex
                NWidthData
                (if ( equal CurrentSearchTable NWidthSearchTable )
                    ( arrayref NWidthSearchTable "trial" )
                  ( arrayref NWidthSearchTable "max" ) ) ) )
            ( PWidth
              ( NPSearchGetWidthFromIndex
                PWidthData
                (if ( equal CurrentSearchTable PWidthSearchTable )
                    ( arrayref PWidthSearchTable "trial" )
                  ( arrayref PWidthSearchTable "max" ) ) ) )
            )
          (let (
                ( GateHeight
                  ( ListFindSum
                    GateClumps
                    (lambda ( GateClump )
                      ( car
                        ( SuperStackCalculateTotalHeightTotalAreaPairOfSuperStackSet
                          ( SuperStackCreateSuperStackSetFromComponentInfos_PreserveOrder 
                            ( mapcar
                              (lambda ( Gate )
                                ( NPSearchGetGateComponentInfoForColumnWidth
                                  Gate
                                  NColumnWidthParameterName 
                                  NWidth
                                  PColumnWidthParameterName 
                                  PWidth
                                  UserUnitsPerMeter ) )
                              GateClump ) )
                          ) ) ) ) )
                ( ChainHeight
                  ( ListFindSum
                    ChainClumps
                    (lambda ( ChainClump )
                    (let (
                          ( NChains
                            ( cadr ( NameFilterInstances
                                     ChainClump
                                     NChainLibCellPairRegExs ) ) )
                          ( PChains
                            ( cadr ( NameFilterInstances
                                     ChainClump
                                     PChainLibCellPairRegExs ) ) ) )
                      ( max
                        ( car 
                          ( SuperStackCalculateTotalHeightTotalAreaPairOfSuperStackSet
                            ( SuperStackRefoldAndChainChainsToSuperStackSet 
                              CellView
                              NChains
                              NWidth
                              NRegionOffsetFromColumnCenter
                              UserUnitsPerMeter
                              RoundToNearest
                              MergeParams
                              GNDNetName
                              VddNetName
                              PreferEvenFolds
                              )
                             ) )
                        ( car 
                          ( SuperStackCalculateTotalHeightTotalAreaPairOfSuperStackSet
                            ( SuperStackRefoldAndChainChainsToSuperStackSet 
                              CellView
                              PChains
                              PWidth
                              PRegionOffsetFromColumnCenter
                              UserUnitsPerMeter
                              RoundToNearest
                              MergeParams
                              GNDNetName
                              VddNetName
                              PreferEvenFolds
                              )
                             ) ) ) ) ) ) ) )
            (let (
                  ( RegionHeightUsed 
                    ( plus
                      GateHeight
                      ChainHeight ) ) )
              ( setq PlacementGood
                     ( leqp RegionHeightUsed RegionHeightAvailable ) )
              
              ( println ( list NWidth PWidth 
                               GateClumps ChainClumps
                               GateHeight ChainHeight PlacementGood ) )
              ( SearchFindSmallestStep
                CurrentSearchTable
                (lambda ( Trial ) PlacementGood )
                nil )
              ) ) ) )
    ( list 
      ( arrayref NWidthSearchTable "max" )
      ( arrayref PWidthSearchTable "max" ) )
    ) )
  

(defun NPSearchFoldGates ( Gates
                           NColumnWidthParameterName
                           PColumnWidthParameterName
                           NWidth
                           PWidth
                           UserUnitsPerMeter )
  ( foreach
    Gate
    Gates
    (when ( PCellGetParameterValue Gate "override_folds" )
      ( dbReplaceProp Gate "override_folds" "boolean" "FALSE" ) )
    ( dbReplaceProp
      Gate
      NColumnWidthParameterName
      "float"
      ( quotient
        NWidth
        UserUnitsPerMeter ) )
    ( dbReplaceProp
        Gate
        PColumnWidthParameterName
        "float"
        ( quotient
          PWidth
          UserUnitsPerMeter ) ) ) )

(defun NPSearchRefoldAndChainChains  ( CellView
                                       Chains
                                       SuperStackLibName
                                       SuperStackCellName
                                       SuperStackViewName
                                       ChainPrefix
                                       ColumnWidth
                                       RegionOffsetFromColumnCenter
                                       UserUnitsPerMeter
                                       RoundToNearest
                                       MergeParams
                                       GNDNetName
                                       VddNetName
                                       PreferEvenFolds
                                       @key 
                                       ( Align "ll" )
                                       )
  (let (
        ( SuperStackSet
          ( SuperStackRefoldAndChainChainsToSuperStackSet 
            CellView
            Chains
            ColumnWidth
            RegionOffsetFromColumnCenter
            UserUnitsPerMeter
            RoundToNearest
            MergeParams
            GNDNetName
            VddNetName
            PreferEvenFolds
             ) ) )

    ( foreach Chain Chains
              ( dbDeleteObject Chain ) )

    ( SuperStackCreateFromSuperStackSet 
      CellView
      SuperStackSet
      SuperStackLibName
      SuperStackCellName
      SuperStackViewName
      ChainPrefix
      nil
      ?Align Align
      ) ) )
