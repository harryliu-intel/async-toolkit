; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

(defun RefoldFilterComponentInfos ( ComponentInfos LibCellPairRegExs )
  ( NameFilterObjects 
    ComponentInfos
    LibCellPairRegExs
    (lambda ( ComponentInfo )
      ( nth 0 ( ComponentInfoGetLibCellView ) ) )
    (lambda ( ComponentInfo )
      ( nth 1 ( ComponentInfoGetLibCellView ) ) ) ) )

(defun RefoldCreateSameFoldChainInfoTable  ( ChainInfos )
  (let (
        ( SameFoldChainInfoTable ( makeTable `foo nil ) ) )
    ( foreach ChainInfo ChainInfos
              (let (
                    ( CanonicalName
                      ( NameCanonicalizeFoldableInstanceName 
                        ( ComponentInfoGetName ChainInfo ) ) ) )
                ( setarray SameFoldChainInfoTable CanonicalName
                           ( cons ChainInfo ( arrayref SameFoldChainInfoTable CanonicalName ) ) ) ) )
    SameFoldChainInfoTable ) )


(defun RefoldCombineFolds ( ChainInfos )
  ( PDKCombineFolds
    ( mapcar
      `ComponentInfoGetStrengthFromChainInfo 
      ChainInfos ) ) )

(defun RefoldCreateOriginalChainInfoFromSameFoldChainInfos ( SameFoldChainInfos
                                                             UserUnitsPerMeter )

  (let (
        ;get chain with name=bla.x with lowest x
        ( FirstChainInfo 
          ( car SameFoldChainInfos ) ) )
    (let (
          ( CanonicalName
            ( NameCanonicalizeFoldableInstanceName 
              ( ComponentInfoGetName FirstChainInfo ) ) ) )
      (let (
            ( NewStrength 
              ( RefoldCombineFolds
                SameFoldChainInfos ) )
            ( NewChainInfo ( ComponentInfoCopy FirstChainInfo ) ) )

        ( setarray NewChainInfo `name CanonicalName )

        ( ComponentInfoSetStrengthForChainInfo
          NewChainInfo
          NewStrength )

        ( ComponentInfoSetWidth
          NewChainInfo
          ( plus
            NewStrength
            ( difference
              ( ComponentInfoGetWidth FirstChainInfo )
              ( times UserUnitsPerMeter
                      ( ComponentInfoGetStrengthFromChainInfo FirstChainInfo ) ) ) ) )

        NewChainInfo ) ) ) )


;1) get all chain infos and other instances
;2) get map from canonical name -> list of chain infos
;3) create original chain infos from existing chain infos of same fold
;4) create a map from canonical name -> original chain info
;5) get all original chain infos
;6) get original chain infos of given chains
;7) get original chain infos to refold: 
;      - AllInDomain: get all chain infos that aare in domains of given chains
;      - Otherwise  : use 6)
;8) return folds of all the original chain infos to refold
                                     
(defun RefoldGetAllOriginalChainInfosForGivenChains ( CellView 
                                                      GivenChains
                                                      ChainLibCellPairRegExs
                                                      UserUnitsPerMeter       
                                                      AllInDomain
                                                      GNDNetName
                                                      VddNetName )
  (let (
        ( FilterResult ( NameFilterInstances
                         ( getq CellView instances )
                         ChainLibCellPairRegExs ) ) )
    (let (
          ( OtherInstances ( car FilterResult ) )
          ( AllChainInfos 
            ( mapcar
              (lambda ( Chain )
                ( ComponentInfoCreateComponentInfoFromComponent
                  Chain ) )
              ( cadr FilterResult ) ) ) )
      (let (
            ( SameFoldChainInfoTable
              ( RefoldCreateSameFoldChainInfoTable 
                AllChainInfos ) ) )

        (let (
              ( OriginalChainInfoTable ( makeTable `original nil ) )
              ( OriginalChainInfoToFoldsTable ( makeTable `fold nil ) ) )
          ( foreach SameFoldChainInfoList ( mapcar `cadr ( tableToList SameFoldChainInfoTable ) )
                    (let (
                          ( OriginalChainInfo
                            ( RefoldCreateOriginalChainInfoFromSameFoldChainInfos 
                              SameFoldChainInfoList
                              UserUnitsPerMeter
                              ) ) )
                      ( setarray OriginalChainInfoTable
                                 ( ComponentInfoGetName OriginalChainInfo )
                                 OriginalChainInfo )
                      ( setarray OriginalChainInfoToFoldsTable
                                 ( ComponentInfoGetName OriginalChainInfo )
                                 SameFoldChainInfoList ) ) )
            (let (
                  ( AllOriginalChainInfos
                    ( mapcar `cadr ( tableToList OriginalChainInfoTable ) ) )
                  ( OriginalChainInfosOfGivenChains
                    ( mapcar 
                      ( lambda ( Chain )
                        ( arrayref OriginalChainInfoTable  ( NameCanonicalizeFoldableInstanceName 
                                                             ( getq Chain name ) ) ) )
                      GivenChains ) ) )
              (let (
                    ( OriginalChainInfosToRefold
                      (if AllInDomain
                        ( RefoldGetAllChainInfosInDomainsOfGivenChainInfos
                          CellView 
                          AllOriginalChainInfos
                          OriginalChainInfosOfGivenChains
                          OtherInstances
                          GNDNetName
                          VddNetName )
                        OriginalChainInfosOfGivenChains ) ) )
                OriginalChainInfosToRefold
            
                ) ) ) ) ) ) )
     
  
(defun RefoldRefoldChainInfosOfSameFold ( CellView
                                          NewChainInfos
                                          InstanceMap )
  (when NewChainInfos
  (let (
        ( CanonicalName ( NameCanonicalizeFoldableInstanceName
                          ( ComponentInfoGetName ( car NewChainInfos ) ) ) ) )
    (let (
          ( ChainTable ( makeTable `chain nil ) )
          ( ChainsOfFold
            ( NameGetInstancesForCanonicalName 
              InstanceMap
              CanonicalName ) ) )   
      (let (
            ( Location ( getq ( car ChainsOfFold ) xy ) )
            ( Orient ( getq ( car ChainsOfFold ) orient ) )
            ( Offset ( RectGetWidth ( getq ( car ChainsOfFold ) bBox  ) ) )
            )
        ( foreach Chain ChainsOfFold
                  ( setarray ChainTable Chain Chain ) )
        ( foreach ChainInfo NewChainInfos
                  (let (
                        ( Chain ( dbFindAnyInstByName 
                                  CellView
                                  ( ComponentInfoGetName ChainInfo ) ) ) )
                    (if Chain
                        (let ()
                          ( dbReplaceProp 
                            Chain 
                            ( ComponentInfoGetWidthParameterNameFromChainInfo 
                              ChainInfo ) 
                            "float"
                            ( ComponentInfoGetStrengthFromChainInfo
                              ChainInfo ) )
                          ( remove Chain ChainTable ) )
                      (let (
                            ( NewChain 
                              ( ComponentInfoCreateComponentFromComponentInfo
                                CellView
                                ChainInfo
                                ( setq Location
                                       ( list 
                                         ( plus ( car Location ) Offset )
                                         ( cadr Location ) ) ) ) ) )
                        ( dbSetq NewChain Orient orient ) ) ) ) )
        ( foreach Chain ( mapcar `cadr ( tableToList ChainTable ) )
                  ( dbDeleteObject Chain ) ) ) ) ) ) )
  

(defun RefoldGetAllChainInfosInDomainsOfGivenChainInfos ( CellView 
                                                          AllChainInfos
                                                          GivenChainInfos
                                                          OtherInstances
                                                          GNDNetName
                                                          VddNetName )
                                                                                                              
  (let (
        ( NetTable ( SuperStackCreateNetTable 
                     CellView
                     AllChainInfos 
                     OtherInstances ) ) )
    (let (
          ( Domains
            ( SuperStackGetDomains
              AllChainInfos
              NetTable
              ( list GNDNetName
                     VddNetName ) ) ) )
      (let (
            ( DomainsOfGivenChains
              ( setof Domain Domains
                      ( exists ChainInfo1 Domain
                               ( exists ChainInfo2 GivenChainInfos
                                        ( equal ChainInfo1 ChainInfo2 ) ) ) ) ) )
        ( ListNonDestructiveMapCan
          (lambda ( Domain ) Domain )
          DomainsOfGivenChains ) ) ) ) )


(defun RefoldGetDefaultFoldFunc ( CellView 
                                  ColumnWidth
                                  UserUnitsPerMeter
                                  RoundToNearest 
                                  PreferEvenFolds )

  ( ExpressionReplaceSymbolsWithValues
    `(lambda ( ChainInfo )
       ( println 
         ( list
           CellView 
           ChainInfo
           ColumnWidth
           UserUnitsPerMeter
           RoundToNearest
           PreferEvenFolds ) )

       ( ChainFoldChainInfoToChainInfos 
         CellView 
         ChainInfo
         ColumnWidth
         UserUnitsPerMeter
         RoundToNearest
         PreferEvenFolds )
       )
    ( list `CellView 
           `ColumnWidth
           `UserUnitsPerMeter
           `RoundToNearest 
           `PreferEvenFolds ) ) )


(defun RefoldGetOverrideFoldsFoldFunc ( CellView 
                                        Folds
                                        RoundToNearest )
         
  ( ExpressionReplaceSymbolsWithValues
    `(lambda ( ChainInfo )
       ( ChainFoldChainInfoToChainInfosOverrideFolds
         CellView 
         ChainInfo
         Folds
         RoundToNearest  ) )
    ( list `CellView 
           `RoundToNearest
           `Folds
           ) ) )


(defun RefoldRefoldChains ( CellView
                            GivenChains
                            ChainLibCellPairRegExs
                            UserUnitsPerMeter
                            AllInDomain
                            FoldFunc
                            GNDNetName
                            VddNetName
                            )
  (let (
        ( InstanceMap ( NameMakeInstanceMapFromCellView 
                        CellView
                        ChainLibCellPairRegExs ) ) )
    (let (
          ( OriginalChainInfos 
            ( RefoldGetAllOriginalChainInfosForGivenChains
              CellView 
              GivenChains
              ChainLibCellPairRegExs
              UserUnitsPerMeter
              AllInDomain
              GNDNetName
              VddNetName ) ) )
      ( foreach OriginalChainInfo OriginalChainInfos 
                (let (
                      ( NewFoldedChainInfos 
                        ( apply
                          FoldFunc
                          ( list OriginalChainInfo ) ) ) )
                  ( RefoldRefoldChainInfosOfSameFold 
                    CellView
                    NewFoldedChainInfos
                    InstanceMap 
                    ) ) ) ) ) )

(defun RefoldReStackChains ( CellView
                             GivenChains
                             SuperStackLibName
                             SuperStackCellName
                             SuperStackViewName
                             ChainPrefix
                             NonPowerMergeFuncInfoList
                             PowerMergeFuncInfoList
                             GNDNetName
                             VddNetName
                             ChainLibCellPairRegExs
                             AllInDomain
                             BCutRings
                             BVerbose
                             PreserveOrder )
  (let (
        ( FilterResult ( NameFilterInstances
                         ( getq CellView instances )
                         ChainLibCellPairRegExs ) ) )
    (let (
          ( OtherInstances ( car FilterResult ) )
          ( AllChainInfos 
            ( mapcar
              (lambda ( Chain )
                ( ComponentInfoCreateComponentInfoFromComponent
                  Chain ) )
              ( cadr FilterResult )
              ) ) )
      ( println ( mapcar 
              (lambda ( ChainInfo )
                ( ComponentInfoGetName ChainInfo ) )
              AllChainInfos ) )
    (let (
          ( ChainInfosOfGivenChains
            ( TableGetCorrespondingList
              AllChainInfos
              GivenChains
              (lambda ( ChainInfo )
                ( ComponentInfoGetName ChainInfo ) )
              (lambda ( Chain )
                ( getq Chain name )
                ) ) ) )
      ( println ChainInfosOfGivenChains )
      ( println GivenChains~>name )
      (let (
            ( ChainInfosToStack
              (if AllInDomain
                  ( RefoldGetAllChainInfosInDomainsOfGivenChainInfos
                    CellView 
                    AllChainInfos
                    ChainInfosOfGivenChains
                    OtherInstances
                    GNDNetName
                    VddNetName )
                ChainInfosOfGivenChains ) ) )
        ( println ChainInfosToStack )
        (let (
              ( ChainsToStack
                ( mapcar 
                  (lambda ( ChainInfo )
                    ( dbFindAnyInstByName CellView  ( ComponentInfoGetName ChainInfo ) ) )
                  ChainInfosToStack ) ) )

          (if PreserveOrder
              ( SuperStackCreateFromComponentInfos_PreserveOrder
                CellView
                (mapcar (lambda ( Chain )
                          ( ComponentInfoCreateComponentInfoFromComponent 
                            Chain ) )
                        ChainsToStack )
                SuperStackLibName
                SuperStackCellName
                SuperStackViewName
                ChainPrefix
                BVerbose )
            
            ( SuperStackCreateFromComponentInfos
              CellView
              (mapcar (lambda ( Chain ) 
                        ( ComponentInfoCreateComponentInfoFromComponent 
                          Chain ) )
                      ChainsToStack )
              ( append OtherInstances  
                       ( ListDifference ( cadr FilterResult ) ChainsToStack (lambda ( x y ) ( equal x y ) ) nil ) )
              SuperStackLibName
              SuperStackCellName
              SuperStackViewName
              ChainPrefix
              NonPowerMergeFuncInfoList
              PowerMergeFuncInfoList
              GNDNetName
              VddNetName
              BCutRings
              BVerbose ) )
         

          
          ( foreach Chain ChainsToStack
                    ( dbDeleteObject Chain ) ) ) ) ) ) ) )
                    
