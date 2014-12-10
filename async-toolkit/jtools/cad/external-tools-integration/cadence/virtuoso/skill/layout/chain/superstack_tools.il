; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$


(defun SuperStackCalculateTotalHeightTotalAreaPairOfStackedComponentInfos 
  ( CellView
    SuperStackLibName
    SuperStackCellName
    SuperStackViewName
    ComponentInfos
    OtherInstances
    MergeFuncInfoListsFunc
    MergeFuncInfoListsFuncArgs
    GNDNetName
    VddNetName
    BCutRings
    BVerbose )
  (let (
        ( SuperStackSet 
          ( SuperStackCreateSuperStackSetFromComponentInfos_Default
            CellView
            ComponentInfos
            OtherInstances
            MergeFuncInfoListsFunc
            MergeFuncInfoListsFuncArgs
            GNDNetName
            VddNetName
            BCutRings
            BVerbose ) ) )
    ( SuperStackCalculateTotalHeightTotalAreaPairOfSuperStackSet 
      SuperStackSet
      ) ) )


(defun SuperStackCalculateTotalHeightTotalAreaPairOfSuperStackSet
  ( SuperStackSet )
  (let (
        ( GetOverlapFunc `SuperStackGetOverlap )
        ( TotalHeight 0.0 )
        ( TotalArea 0.0 ) )
    ( foreach 
      Key
      SuperStackSet 
      (let (
            ( SuperStackInfo ( arrayref SuperStackSet Key ) ) )
        (let (
              ( Height ( SuperStackCalculateHeight
                         SuperStackInfo
                         GetOverlapFunc ) )
              ( Width ( SuperStackGetMaxWidth SuperStackInfo ) ) )
          
          ( setq TotalHeight ( plus TotalHeight Height ) )
          ( setq TotalArea ( plus TotalArea ( times Height Width ) ) )
          ;( printf "height: %L width: %L\n" Height Width )
          ) ) )
    ( list TotalHeight TotalArea ) ) )

                                        
(defun SuperStackCreateFromCellView ( CellView
                                      SuperStackLibName
                                      SuperStackCellName
                                      SuperStackViewName
                                      ComponentPrefix
                                      MergeFuncInfoListsFunc
                                      MergeFuncInfoListsFuncArgs
                                      GNDNetName
                                      VddNetName
                                      ComponentLibCellPairRegExs
                                      BCutRings
                                      BVerbose )
  (let ( 
        ( FilterResult 
          ( NameFilterInstances 
            ( getq CellView instances ) 
            ComponentLibCellPairRegExs ) ) )
    (let (
          ( Components 
            ( cadr FilterResult ) )
          ( Others 
            ( car FilterResult ) ) )
      (let ( 
            ( ComponentInfos 
              ( mapcar 
                (lambda ( Component )
                  ( ComponentInfoCreateComponentInfoFromComponent Component ) )
                Components ) ) )
        
        ( SuperStackCreateFromComponentInfos
           CellView
           ComponentInfos
           Others
           SuperStackLibName
           SuperStackCellName
           SuperStackViewName
           ComponentPrefix
           MergeFuncInfoListsFunc
           MergeFuncInfoListsFuncArgs
           GNDNetName
           VddNetName   
           BCutRings
           BVerbose )
         
         ( foreach Component Components 
                   ( dbDeleteObject Component ) )

 ) ) ) )



(defun SuperStackCreateSuperStackSetFromComponentInfos_Default 
  ( CellView
    ComponentInfos
    OtherInstances
    MergeFuncInfoListsFunc
    MergeFuncInfoListsFuncArgs
    GNDNetName
    VddNetName
    BCutRings
    BVerbose )
  (let (
        ( GetOverlapFunc `SuperStackGetOverlap )
        ( MergeFuncInfoLists
          ( apply 
            MergeFuncInfoListsFunc
            MergeFuncInfoListsFuncArgs ) ) )
    (let (
          ( NetTable
            ( SuperStackCreateNetTable
              CellView
              ComponentInfos
              OtherInstances ) ) )
       ( SuperStackCreateSuperStackSetFromComponentInfos             
         NetTable
         ComponentInfos
         ( car MergeFuncInfoLists )
         ( cadr MergeFuncInfoLists )
         `SuperStackSymbolMatch
         GetOverlapFunc
         GNDNetName
         VddNetName
         BCutRings
         BVerbose ) ) ) )

(defun SuperStackCreateFromComponentInfos ( CellView
                                            ComponentInfos
                                            OtherInstances
                                            SuperStackLibName
                                            SuperStackCellName
                                            SuperStackViewName
                                            ComponentPrefix
                                            MergeFuncInfoListsFunc
                                            MergeFuncInfoListsFuncArgs
                                            GNDNetName
                                            VddNetName
                                            BCutRings
                                            BVerbose )
  ( SuperStackCreateFromSuperStackSet 
    CellView
    ( SuperStackCreateSuperStackSetFromComponentInfos_Default 
      CellView
      ComponentInfos
      OtherInstances
      MergeFuncInfoListsFunc
      MergeFuncInfoListsFuncArgs
      GNDNetName
      VddNetName
      BCutRings
      BVerbose )
    SuperStackLibName
    SuperStackCellName
    SuperStackViewName
    ComponentPrefix
    BVerbose ) )

(defun SuperStackCreateStackGroupsFromClumpUsingPDKInfo
  ( CellView
    Clump )
  (let (
        ( N                     
          ( cadr 
            ( NameFilterInstances
              Clump
              NChainLibCellPairRegExs  ) ) )
        ( P
          ( cadr 
            ( NameFilterInstances
              Clump
              PChainLibCellPairRegExs  ) ) )
        ( G
          ( cadr 
            ( NameFilterInstances
              Clump
              GateLibCellPairRegExs  ) ) ) )
    ( mapcar
      (lambda ( Group )
        ( SuperStackCreateFromGroupUsingPDKInfo
          CellView
          Group ) )
      ( list N P G ) ) ) )


(defun SuperStackCompactStackGroup ( StackGroups )
  (let (
        ( Y ( ListFindMinimum 
              ( ListNonDestructiveMapCan
                (lambda ( StackGroup )
                  StackGroup )
                StackGroups )
              (lambda ( Stack )
                ( RectGetBottom ( getq Stack bBox ) ) ) ) )
        ( H ( ListFindMaximum
              StackGroups
              (lambda ( StackGroup )
                ( ListFindSum
                  StackGroup
                  (lambda ( Stack )
                    ( RectGetHeight ( getq Stack bBox ) ) ) ) ) ) )
        )
    (when ( exists StackGroup StackGroups StackGroup )
      ( ListNonDestructiveMapCan
        (lambda ( StackGroup )
          (when StackGroup
            ( DeCompactVerticalKeepOrderDefault 
              StackGroup
              ( list Y ( plus Y H ) )
              .05
              t ) )
          StackGroup
          )
        StackGroups ) ) ) )


(defun SuperStackFlipComponentInfos ( ComponentInfos )
  "Tries to greedily optimize the order of a stack according to this simple rule: flip the current component if the last one doesn't match the current, and if the current matches the next when flipped."
  (let (
        ( Prev nil )
        ( Curr nil )
        ( Next nil )
        )

    (while ( or ComponentInfos Curr )
      ( setq Prev Curr )
      ( setq Curr Next )
      ( setq Next ( car ComponentInfos ) )
        ;when
        (when ( and 
                Curr
                ( or
                  ( and
                    ;curr doesn't match previous
                    ( or ( null Prev )
                         ( not ( SuperStackSymbolMatch
                                 ( DominoGetTopSymbol Prev )
                                 ( DominoGetBottomSymbol Curr ) ) )
                         ( not ( SuperStackIsSharable
                                 ( ComponentInfoGetTopCSInfo Prev )
                                 ( ComponentInfoGetBottomCSInfo Curr ) ) )
                         )
                    ;but it  matches next if flipped!
                    ( and Next
                          ( SuperStackSymbolMatch
                            ( DominoGetBottomSymbol Curr )
                            ( DominoGetBottomSymbol Next ) )
                          ( SuperStackIsSharable
                            ( ComponentInfoGetBottomCSInfo Curr )
                            ( ComponentInfoGetBottomCSInfo Next ) )
                          ) )
                  ;or
                  ( and
                    ;curr doesn't match next 
                    ( or ( null Next )
                         ( not ( SuperStackSymbolMatch
                                 ( DominoGetTopSymbol Curr )
                                 ( DominoGetBottomSymbol Next ) ) )
                         ( not ( SuperStackIsSharable
                                 ( ComponentInfoGetTopCSInfo Curr )
                                 ( ComponentInfoGetBottomCSInfo Next ) ) )
                         )
                    ;but it matches prev if flipped!
                    ( and Prev
                         ( SuperStackSymbolMatch
                           ( DominoGetTopSymbol Curr )
                           ( DominoGetTopSymbol Prev ) )
                         ( SuperStackIsSharable
                           ( ComponentInfoGetTopCSInfo Curr )
                           ( ComponentInfoGetTopCSInfo Prev ) ) )
                    ) ) )
          ;then flip curr!
          ( ComponentInfoSetParity
            Curr
            ( null ( ComponentInfoGetParity Curr ) ) ) )
        ( setq ComponentInfos ( cdr ComponentInfos ) )
        ) ) )             

(defun SuperStackCreateFromGroupUsingPDKInfo ( CellView
                                               Group )
  (let (
        ( Info
          (cond (
                 ( cadr 
                   ( NameFilterInstances
                     Group
                     NChainLibCellPairRegExs  ) )
                 ( list NSuperStackCellName NChainPrefix ) )
                (
                 ( cadr 
                   ( NameFilterInstances
                     Group
                     PChainLibCellPairRegExs  ) )
                 ( list PSuperStackCellName PChainPrefix ) )
                (
                 ( cadr 
                   ( NameFilterInstances
                     Group
                     GateLibCellPairRegExs  ) )
                 ( list GateSuperStackCellName GateChainPrefix ) ) ) )
        ( ComponentInfos
          ( mapcar
            (lambda ( Chain )
              ( ComponentInfoCreateComponentInfoFromComponent
                Chain ) )
            Group ) )
        )

    (let (
          ( Ret 
            ( SuperStackCreateFromComponentInfos_PreserveOrder
              CellView
              ComponentInfos
              SuperStackLibraryName
              ( car Info )
              SuperStackViewName
              ( cadr Info )
              nil
              ?Flip nil ) ) )
      ( foreach Component Group ( dbDeleteObject Component ) )
      Ret
 ) ) )


(defun SuperStackCreateSuperStackSetFromComponentInfos_PreserveOrder 
  ( ComponentInfos 
    @key
    ( Flip t )
    )
  ( SuperStackCreateSuperStackSetFromComponentInfoClumps_PreserveOrder 
    ( list ComponentInfos )
    ?Flip Flip )
 )
(defun SuperStackCreateSuperStackSetFromComponentInfoClumps_PreserveOrder 
  ( Clumps
    @key
    ( Flip t ) )
  (let (
        ( SuperStackSet ( makeTable `foo nil ) ) )
    (let ( 
          ( SuperStackInfos
            ( mapcar
              (lambda ( Clump )
                (let (
                      ( SortedComponentInfos
                        ( sort ( copy Clump )
                               (lambda ( C1 C2 )
                                 ( lessp ( RectGetBottom ( ComponentInfoGetBBox C1 ) )
                                         ( RectGetBottom ( ComponentInfoGetBBox C2 ) )
                                         ) ) ) ) )
                  (when Flip
                    ( SuperStackFlipComponentInfos
                      SortedComponentInfos ) )
                  ( SuperStackCreateSuperStackInfoFromComponentInfoList
                    SortedComponentInfos
                    nil
                    nil
                    ) ) )
              Clumps ) ) )
      
      ( foreach SuperStackInfo SuperStackInfos
                (when ( DominoStackGetDominoList SuperStackInfo )
                  ( DominoStackAdd
                    SuperStackInfo
                    SuperStackSet ) ) )
      SuperStackSet
      ) ) )

(defun SuperStackStackSuperStackSet ( SuperStackSet )
  "merges superstacks in a superstack set into a single superstack, which is just the original superstacks ordered by their original y location"
  (let (
        ( NewSuperStackSet ( makeTable `foo nil ) ) )
    (let (
          ( SortedComponentInfos
            ( mapcan
              (lambda ( SuperStackInfo )
                ( copy ( SuperStackGetComponentInfoList
                         SuperStackInfo ) ) )
              ;sorted superstackinfos
              ( sort ( mapcar `cadr ( tableToList SuperStackSet ) )
                     (lambda ( CIL1 CIL2 )
                       ( lessp 
                         ( RectGetBottom 
                           ( ComponentInfoGetBBox ( DominoStackGetBottomDomino CIL1 ) ) )
                         ( RectGetBottom
                           ( ComponentInfoGetBBox ( DominoStackGetBottomDomino CIL2 ) ) ) ) ) ) ) ) )
      (when SortedComponentInfos
        ( SuperStackFlipComponentInfos
          SortedComponentInfos )
        ( DominoStackAdd
          ( SuperStackCreateSuperStackInfoFromComponentInfoList
            SortedComponentInfos
            nil
            nil
            )
          NewSuperStackSet ) )
      NewSuperStackSet
      ) ) )

(defun SuperStackCreateFromComponentInfos_PreserveOrder ( CellView
                                                          ComponentInfos
                                                          SuperStackLibName
                                                          SuperStackCellName
                                                          SuperStackViewName
                                                          ComponentPrefix
                                                          BVerbose
                                                          @key
                                                          ( Flip t )
                                                        )
  ( SuperStackCreateFromSuperStackSet 
    CellView
    ( SuperStackCreateSuperStackSetFromComponentInfos_PreserveOrder 
      ComponentInfos
      ?Flip Flip )
    SuperStackLibName
    SuperStackCellName
    SuperStackViewName
    ComponentPrefix
    BVerbose
    ) )

  
(defun SuperStackRefoldAndChainChainsToSuperStackSet  ( CellView
                                                        Chains
                                                        ColumnWidth
                                                        RegionOffsetFromColumnCenter
                                                        UserUnitsPerMeter
                                                        RoundToNearest
                                                        MergeParams
                                                        GNDNetName
                                                        VddNetName
                                                        PreferEvenFolds
                                                        )
  "Fold chains and put them in a single stack so that they fit in ColumnWidth"
  (let (
        ( ChainInfos
          ( ListNonDestructiveMapCan 
            (lambda ( Chain )
              ( ChainFoldToChainInfos 
                CellView
                Chain
                ( difference 
                  ColumnWidth 
                  RegionOffsetFromColumnCenter )
                UserUnitsPerMeter
                RoundToNearest 
                PreferEvenFolds ) )
            Chains
            ) ) )
    ;if no folding, leave it be
    (cond (
           ( equal ( length ChainInfos )
                   ( length Chains ) )
           t ;don't fold for now
           ( SuperStackCreateSuperStackSetFromComponentInfos_PreserveOrder 
             ( mapcar 
               (lambda ( Chain )
                 ( ComponentInfoCreateComponentInfoFromComponent Chain ) )
               Chains ) ) )
          (
           t
           (let (
                 ( Others
                   ( ListDifference
                     ( getq CellView instances )
                     Chains
                     `equal
                     nil
                     ) ) )
             ;put the superstacks together
             ( SuperStackStackSuperStackSet
               ( SuperStackCreateSuperStackSetFromComponentInfos_Default
                 CellView
                 ChainInfos
                 Others
                 `SuperStackGetDefaultMergeFuncInfoLists_Chain
                 MergeParams
                 GNDNetName
                 VddNetName
                 t
                 nil ) ) ) ) ) ) )

 
(defun SuperStackGetDefaultMergeFuncInfoLists_Chain ( MaxNonPowerMergeHeight
                                                      MaxPowerMergeHeight
                                                      MaxMergeHeight )
  (unless ( fboundp `SuperStackIsSharable )
    (error "Must define SuperStackIsSharable function in PDK!" ) )

  (unless ( fboundp `SuperStackGetOverlap )
    (error "Must define SuperStackGetOverlap function in PDK!" ) )
  (let (
        ( IsSharableFunc
          `SuperStackIsSharable )
        ( GetOverlapFunc
          `SuperStackGetOverlap ) )
    (let (
          ( MergeTest_Match 
            ( list `SuperStackMergeTest_Match 
                   ( list IsSharableFunc ) ) )
          ( MergeTest_MaxHeight 
            ( list `SuperStackMergeTest_Height 
                   ( list MaxMergeHeight GetOverlapFunc ) ) )
          ( NonPowerMergeTest_Height 
            ( list `SuperStackMergeTest_Height 
                   ( list MaxNonPowerMergeHeight GetOverlapFunc ) ) )
          ( PowerMergeTest_Height 
            ( list `SuperStackMergeTest_Height 
                   ( list MaxPowerMergeHeight GetOverlapFunc ) ) ) )
    (let (
          ( NonPowerMergeTestFuncInfoList 
            ( list 
              ( list `SuperStackAndFuncs
                     ( list 
                       ( list
                         MergeTest_Match
                         ( list `SuperStackMergeTest_FoldsOfSameStack nil )
                         MergeTest_MaxHeight
                         ) ) )
              ( list `SuperStackAndFuncs
                     ( list 
                       ( list
                         MergeTest_Match
                         ( list `SuperStackMergeTest_SameEnds nil )
                         MergeTest_MaxHeight
                         ) ) )
              ( list `SuperStackAndFuncs
                     ( list 
                       ( list
                         MergeTest_Match
                         ( list `SuperStackMergeTest_ShareNet ( list "[0-9]+-H" ) )
                         MergeTest_MaxHeight
                         ) ) )
              ( list `SuperStackAndFuncs
                     ( list 
                       ( list
                         MergeTest_MaxHeight
                         MergeTest_Match
                         ( list `SuperStackMergeTest_AnyShareNet ( list "[0-9]+-H" ) )
                         ) ) )
              ( list `SuperStackAndFuncs
                     ( list
                       ( list
                         NonPowerMergeTest_Height
                         MergeTest_Match
                         ( list `SuperStackMergeTest_Width ( list 1.0 ) )
                         ) ) )
              ( list `SuperStackAndFuncs
                     ( list
                       ( list
                         NonPowerMergeTest_Height
                         MergeTest_Match
                         ( list `SuperStackMergeTest_Width ( list 1.5 ) )
                         ) ) )
              ( list `SuperStackAndFuncs
                     ( list 
                       ( list
                         ( list `SuperStackMergeTest_Height 
                                ( list ( times 0.5 ( plus MaxMergeHeight
                                                          MaxNonPowerMergeHeight ) )
                                       GetOverlapFunc ) )
                         ( list `SuperStackMergeTest_AnyShareNet ( list "[0-9]+-H" ) )
                         ) ) )
              ) )
            ( PowerMergeTestFuncInfoList 
              ( list 
                ( list `SuperStackAndFuncs
                       ( list
                         ( list
                           MergeTest_Match
                           ( list `SuperStackMergeTest_Width ( list 1.0 ) )
                           PowerMergeTest_Height
                           ) ) )
                ( list `SuperStackAndFuncs
                       ( list
                         ( list
                           MergeTest_Match
                           ( list `SuperStackMergeTest_Width ( list 1.5 ) )
                           PowerMergeTest_Height
                           ) ) ) ) )
            )
      ( list
         NonPowerMergeTestFuncInfoList 
         PowerMergeTestFuncInfoList
        ) ) ) ) )


(defun SuperStackGetDefaultMergeFuncInfoLists_Gate ( MaxNonPowerMergeHeight
                                                     MaxPowerMergeHeight
                                                     MaxMergeHeight
                                                     GNDNetName 
                                                     VddNetName )
  (unless ( fboundp `SuperStackIsSharable )
    (error "Must define SuperStackIsSharable function in PDK!" ) )

  (unless ( fboundp `SuperStackGetOverlap )
    (error "Must define SuperStackGetOverlap function in PDK!" ) )
  (let (
        ( IsSharableFunc
          `SuperStackIsSharable )
        ( GetOverlapFunc
          `SuperStackGetOverlap ) )
    (let (
          ( MergeTest_ShareNonPower
            ( list `SuperStackMergeTest_ShareNonPower
                   ( list ( list GNDNetName 
                                 VddNetName ) ) ) )
          ( MergeTest_AnyShareNonPower
            ( list `SuperStackMergeTest_AnyShareNonPower
                   ( list ( list GNDNetName 
                                 VddNetName ) ) ) )
          ( MergeTest_Match 
            ( list `SuperStackMergeTest_Match 
                   ( list IsSharableFunc ) ) )
          ( NonPowerMergeTest_Height 
            ( list `SuperStackMergeTest_Height 
                   ( list MaxNonPowerMergeHeight GetOverlapFunc ) ) )
          ( PowerMergeTest_Height 
            ( list `SuperStackMergeTest_Height 
                   ( list MaxPowerMergeHeight GetOverlapFunc ) ) ) )
    (let (
          ( NonPowerMergeTestFuncInfoList 
            nil )
          ( PowerMergeTestFuncInfoList 
              ( list 
                ( list `SuperStackAndFuncs
                       ( list
                         ( list
                           ( list `SuperStackMergeTest_2Sided nil )
                           MergeTest_Match
                           MergeTest_ShareNonPower
                           PowerMergeTest_Height
                           ) ) )    
                ( list `SuperStackAndFuncs
                       ( list
                         ( list
                           MergeTest_Match
                           ( list `SuperStackMergeTest_Width ( list 1.0 ) )
                           MergeTest_ShareNonPower
                           PowerMergeTest_Height
                           ) ) )    
                ( list `SuperStackAndFuncs
                       ( list
                         ( list
                           MergeTest_Match
                           MergeTest_ShareNonPower
                           PowerMergeTest_Height
                           ) ) )
                ( list `SuperStackAndFuncs
                       ( list
                         ( list
                           MergeTest_Match
                           MergeTest_AnyShareNonPower
                           PowerMergeTest_Height
                           ) ) ) ) )
          )
      ( list
        NonPowerMergeTestFuncInfoList 
        PowerMergeTestFuncInfoList
        ) ) ) ) )

;;;;;;;;;;;;;;;;;;;;;;
;; MergeTestFuncs 
;;;;;;;;;;;;;;;;;;;;;;

(defun SuperStackAndFuncs ( SuperStackInfoAndNodeIndex1 SuperStackInfoAndNodeIndex2 FuncInfoList )
  ( ListAndFuncs 
    ( mapcar 
      (lambda ( FuncInfo ) 
        ( list ( car FuncInfo )
               ( append ( list SuperStackInfoAndNodeIndex1 SuperStackInfoAndNodeIndex2 )
                        ( cadr FuncInfo ) ) ) )
      FuncInfoList ) ) )

(defun SuperStackMergeTest_Height ( SuperStackInfoAndNodeIndex1 
                                    SuperStackInfoAndNodeIndex2 
                                    MaxHeight 
                                    GetOverlapFunc
                                    )
  ( leqp ( plus ( SuperStackGetHeight 
                  ( car SuperStackInfoAndNodeIndex1 ) )
                ( SuperStackGetHeight 
                  ( car SuperStackInfoAndNodeIndex2 ) )
                )
         MaxHeight ) )



; ------------------------ Heuristics --------------

(defun SuperStackMergeTest_Match ( SuperStackInfoAndNodeIndex1
                                   SuperStackInfoAndNodeIndex2
                                   SuperStackIsSharableFunc )
 ( and 
   ( SuperStackSymbolMatch 
     ( DominoStackGetSymbolFromDominoStackAndNodeIndex 
       SuperStackInfoAndNodeIndex1 )
     ( DominoStackGetSymbolFromDominoStackAndNodeIndex
       SuperStackInfoAndNodeIndex2 ) )
   (let (
         ( ComponentInfoAndNodeIndex1
           ( DominoStackGetDominoAndNodeIndexFromDominoStackAndNodeIndex
             SuperStackInfoAndNodeIndex1 ) )
         ( ComponentInfoAndNodeIndex2
           ( DominoStackGetDominoAndNodeIndexFromDominoStackAndNodeIndex
             SuperStackInfoAndNodeIndex2 ) ) )
     (let (
           ( CSProps1 (if ( cadr ComponentInfoAndNodeIndex1 )
                          ( ComponentInfoGetTopCSInfo
                            ( car ComponentInfoAndNodeIndex1 ) )
                        ( ComponentInfoGetBottomCSInfo
                          ( car ComponentInfoAndNodeIndex1 ) ) ) )
           ( CSProps2 (if ( cadr ComponentInfoAndNodeIndex2 )
                          ( ComponentInfoGetTopCSInfo
                            ( car ComponentInfoAndNodeIndex2 ) )
                        ( ComponentInfoGetBottomCSInfo
                          ( car ComponentInfoAndNodeIndex2 ) ) ) ) )

         ( apply SuperStackIsSharableFunc
                 ( list CSProps1 CSProps2 ) ) ) ) ) )



(defun SuperStackMergeTest_ShareNet ( SuperStackInfoAndNodeIndex1 
                                      SuperStackInfoAndNodeIndex2 
                                      NetRegEx )
  (let (
        ( ComponentInfoAndNodeIndex1
          ( DominoStackGetDominoAndNodeIndexFromDominoStackAndNodeIndex
            SuperStackInfoAndNodeIndex1 ) )
        ( ComponentInfoAndNodeIndex2
          ( DominoStackGetDominoAndNodeIndexFromDominoStackAndNodeIndex
            SuperStackInfoAndNodeIndex2 ) )
        )
    ( rexCompile NetRegEx )
    (let (
          ( ConnTable1 
            ( ComponentInfoGetConnTable ( car ComponentInfoAndNodeIndex1 ) ) )
          ( ConnTable2
            ( ComponentInfoGetConnTable ( car ComponentInfoAndNodeIndex2 ) ) ) )
      ( exists Term1 ConnTable1
               ( exists Term2 ConnTable2
                        ( and ( equal ( arrayref ConnTable1 Term1 ) ( arrayref ConnTable2 Term2 ) )
                              ( rexExecute ( arrayref ConnTable1 Term1 ) ) ) ) ) ) ) )

(defun SuperStackMergeTest_AnyShareNet ( SuperStackInfoAndNodeIndex1 SuperStackInfoAndNodeIndex2 NetRexEx )
  ( exists 
    ComponentInfo1
    ( SuperStackGetComponentInfoList
      ( car SuperStackInfoAndNodeIndex1 ) )
    ( exists 
      ComponentInfo2
      ( SuperStackGetComponentInfoList
        ( car SuperStackInfoAndNodeIndex2 ) )
      (let (
            ( ConnTable1 ( ComponentInfoGetConnTable ComponentInfo1 ) )
            ( ConnTable2 ( ComponentInfoGetConnTable ComponentInfo2 ) ) )
        ( exists Term1 ConnTable1
                 ( exists Term2 ConnTable2
                          ( and ( equal ( arrayref ConnTable1 Term1 ) ( arrayref ConnTable2 Term2 ) )
                                ( rexExecute ( arrayref ConnTable1 Term1 ) ) ) ) ) ) ) ) )


(defun SuperStackMergeTest_ShareNonPower ( SuperStackInfoAndNodeIndex1 SuperStackInfoAndNodeIndex2 PowerNetNames )
  (let (
        ( ComponentInfoAndNodeIndex1
          ( DominoStackGetDominoAndNodeIndexFromDominoStackAndNodeIndex
            SuperStackInfoAndNodeIndex1 ) )
        ( ComponentInfoAndNodeIndex2
          ( DominoStackGetDominoAndNodeIndexFromDominoStackAndNodeIndex
            SuperStackInfoAndNodeIndex2 ) ) )
    (let (
          ( ConnTable1 ( ComponentInfoGetConnTable ( car ComponentInfoAndNodeIndex1 ) ) )
          ( ConnTable2 ( ComponentInfoGetConnTable ( car ComponentInfoAndNodeIndex2 ) ) ) )
      ( exists Term1 ConnTable1
               ( exists Term2 ConnTable2
                        ( and ( equal ( arrayref ConnTable1 Term1 )
                                      ( arrayref ConnTable2 Term2 ) )
                              ( not ( exists PowerNetName PowerNetNames
                                             ( equal ( arrayref ConnTable1 Term1 ) PowerNetName ) ) ) ) ) ) ) ) )


(defun SuperStackMergeTest_AnyShareNonPower ( SuperStackInfoAndNodeIndex1 SuperStackInfoAndNodeIndex2 PowerNetNames )
  "Tests if any non-power net is shared between 2 superstacks."
  ( exists 
    ComponentInfo1
    ( SuperStackGetComponentInfoList
      ( car SuperStackInfoAndNodeIndex1 ) )
    ( exists 
      ComponentInfo2
      ( SuperStackGetComponentInfoList
        ( car SuperStackInfoAndNodeIndex2 ) )
      (let (
            ( ConnTable1 ( ComponentInfoGetConnTable ComponentInfo1 ) )
            ( ConnTable2 ( ComponentInfoGetConnTable ComponentInfo2 ) ) )
        ( exists Term1 ConnTable1
                 ( exists Term2 ConnTable2
                          ( and ( equal 
                                  ( arrayref ConnTable1 Term1 )
                                  ( arrayref ConnTable2 Term2 ) )
                                ( not ( exists
                                        PowerNetName
                                        PowerNetNames
                                        ( equal
                                          ( arrayref ConnTable1 Term1 )
                                          PowerNetName ) ) ) ) ) ) ) ) ) )

(defun SuperStackMergeTest_2Sided ( SuperStackInfoAndNodeIndex1 SuperStackInfoAndNodeIndex2 )
  "Tests to see if one of 2 superstacks is not a ring."
  ( or 
    ( and 
      ( or 
        ( car ( DominoStackGetBottomSymbol ( car SuperStackInfoAndNodeIndex1 ) ) )
        ( cadr ( DominoStackGetBottomSymbol ( car SuperStackInfoAndNodeIndex1 ) ) ) )
      ( or 
        ( car ( DominoStackGetTopSymbol ( car SuperStackInfoAndNodeIndex1 ) ) )
        ( cadr ( DominoStackGetTopSymbol ( car SuperStackInfoAndNodeIndex1 ) ) ) ) )
    ( and 
      ( or 
        ( car ( DominoStackGetBottomSymbol ( car SuperStackInfoAndNodeIndex2 ) ) )
        ( cadr ( DominoStackGetBottomSymbol ( car SuperStackInfoAndNodeIndex2 ) ) ) )
      ( or
        ( car ( DominoStackGetTopSymbol ( car SuperStackInfoAndNodeIndex2 ) ) )
        ( cadr ( DominoStackGetTopSymbol ( car SuperStackInfoAndNodeIndex2 ) ) ) ) ) ) )

(defun SuperStackMergeTest_FoldsOfSameStack ( SuperStackInfoAndNodeIndex1 SuperStackInfoAndNodeIndex2 )
  "Tests if components of 2 superstacks have same canonical name, and are thus from the same chain."
   (let (
         ( Name1 ( NameCanonicalizeFoldableInstanceName 
                   ( ComponentInfoGetName
                     ( DominoStackGetDominoFromDominoStackAndNodeIndex
                       SuperStackInfoAndNodeIndex1 ) ) ) )
         ( Name2 ( NameCanonicalizeFoldableInstanceName 
                   ( ComponentInfoGetName
                     ( DominoStackGetDominoFromDominoStackAndNodeIndex
                       SuperStackInfoAndNodeIndex2 ) ) ) ) )
       ( equal Name1 Name2 ) ) )

(defun SuperStackMergeTest_SameEnds ( SuperStackInfoAndNodeIndex1 SuperStackInfoAndNodeIndex2 )
  "Tests if both sides match on 2 superstacks"
 ( or 
   ( and ( SuperStackSymbolMatch 
           ( DominoStackGetTopSymbol ( car SuperStackInfoAndNodeIndex1 ) )
           ( DominoStackGetTopSymbol ( car SuperStackInfoAndNodeIndex2 ) ) )
         ( SuperStackSymbolMatch
           ( DominoStackGetBottomSymbol ( car SuperStackInfoAndNodeIndex1 ) )
           ( DominoStackGetBottomSymbol ( car SuperStackInfoAndNodeIndex2 ) ) ) )
   
   ( and ( SuperStackSymbolMatch
           ( DominoStackGetBottomSymbol ( car SuperStackInfoAndNodeIndex1 ) )
           ( DominoStackGetTopSymbol ( car SuperStackInfoAndNodeIndex2 ) ) )
         ( SuperStackSymbolMatch
           ( DominoStackGetTopSymbol ( car SuperStackInfoAndNodeIndex1 ) )
           ( DominoStackGetBottomSymbol ( car SuperStackInfoAndNodeIndex2 ) ) ) ) ) )
  
(defun SuperStackMergeTest_Width ( SuperStackInfoAndNodeIndex1 SuperStackInfoAndNodeIndex2 WidthRatioThreshold )
  "Tests if 2 superstacks widths are within a threshold ratio"
  (let (
        ( MinWidth ( min ( SuperStackGetMinWidth ( car SuperStackInfoAndNodeIndex1 ) )
                         ( SuperStackGetMinWidth ( car SuperStackInfoAndNodeIndex2 ) ) ) )
        ( MaxWidth ( max ( SuperStackGetMaxWidth ( car SuperStackInfoAndNodeIndex1 ) )
                         ( SuperStackGetMaxWidth ( car SuperStackInfoAndNodeIndex2 ) ) ) ) )
    ( leqp ( quotient MaxWidth MinWidth ) WidthRatioThreshold ) ) )

