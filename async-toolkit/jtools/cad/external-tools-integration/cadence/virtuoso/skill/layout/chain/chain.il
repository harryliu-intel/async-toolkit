; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$


(defun ChainCalculateFolds ( ChainInfo 
                             ColumnWidth 
                             UserUnitsPerMeter
                             PreferEvenFolds )
  "Calculate the number of <a href=glossary.html#fold>folds</a> for a <a href=glossary.html#ChainInfo>ChainInfo</a>."
  (unless ( fboundp `PDKFoldChain )
    (error "The PDK pdkinfo.il must define (PDKFoldChain 
                                            ColumnWidth 
                                            OverrideFolds
                                            Folds
                                            MaxFolds)"))
  (let (
        ( Folds
          ( length
            ( PDKFoldChain 
              ( ComponentInfoGetStrengthFromChainInfo ChainInfo )
              ColumnWidth
              nil
              0
              16
              ) ) ) )
    (if ( and PreferEvenFolds 
              ( greaterp Folds 1)
              ( equal ( mod Folds 2 ) 1 ) )
        ( plus Folds 1 )
      Folds ) ) )
            

(defun ChainCreateFoldedChainsFromChainInfos ( CellView 
                                               ChainInfos
                                               Location )
  "Instantiates a list of <a href=glossary.html#ChainInfo>ChainInfos</a> in a row starting at given Location."
  (let ( 
        ( X ( car Location ) ) )
    ( mapcar 
      (lambda ( ChainInfo ) 
        ( ComponentInfoCreateComponentFromComponentInfo
          CellView
          ChainInfo
          ( list 
            ( setq X ( plus X ( ComponentInfoGetWidth ChainInfo  ) ) )
            ( cadr Location ) ) ) )
      ChainInfos ) ) )


(defun ChainFoldChainInfoToChainInfos ( CellView ChainInfo ColumnWidth UserUnitsPerMeter RoundToNearest PreferEvenFolds )
  (let (
        ( ChainFolds ( ChainCalculateFolds ChainInfo ColumnWidth UserUnitsPerMeter PreferEvenFolds ) ) )
    ( ChainFoldChainInfoToChainInfosOverrideFolds
      CellView
      ChainInfo
      ChainFolds
      RoundToNearest ) ) )


(defun ChainFoldChainInfoToChainInfosOverrideFolds ( CellView ChainInfo ChainFolds RoundToNearest )
  "Constructs folded <a href=glossary.html#ChainInfo>ChainInfos</a> from a ChainInfo according to PDKFoldChain."
  (unless ( fboundp `PDKFoldChain )
    (error "The PDK pdkinfo.il must define (PDKFoldChain 
                                            ColumnWidth 
                                            OverrideFolds
                                            Folds
                                            MaxFolds)"))
  
  (let (
        ( OldConnTable ( ComponentInfoGetConnTable ChainInfo ) )
        ( ChainName ( ComponentInfoGetName ChainInfo ) )
        ( ChainParameters ( ComponentInfoGetParameterList ChainInfo ) )
        ( Parity ( ComponentInfoGetParity ChainInfo ) )
        ( WidthParameterName ( ComponentInfoGetWidthParameterNameFromChainInfo ChainInfo ) )
        ( ChainStrength ( ComponentInfoGetStrengthFromChainInfo ChainInfo ) )
        ( ChainHeight ( ComponentInfoGetHeight ChainInfo ) )
        ( ChainWidth ( ComponentInfoGetWidth ChainInfo ) )
        ( Position ( ComponentInfoGetPosition ChainInfo ) )
        ( BBox ( ComponentInfoGetBBox ChainInfo ) )
        ( HParity ( ComponentInfoGetHParity ChainInfo ) )
        ( BottomCSInfo ( ComponentInfoGetBottomCSInfo ChainInfo ) )
        ( TopCSInfo ( ComponentInfoGetTopCSInfo ChainInfo ) )
        )
    (let (
          ( I 0 )
          ( ChainInfoList nil )
          ( FoldedChainStrengths
            ( PDKFoldChain
              ChainStrength
              1e9
              t
              ChainFolds
              ChainFolds
              ) ) )
      ( foreach
        FoldedChainStrength
        FoldedChainStrengths
        (let (
              ( OldBottomSymbol ( DominoGetBottomSymbol ChainInfo ) )
              ( OldTopSymbol ( DominoGetTopSymbol ChainInfo ) )
              ( OldSourceNetName ( arrayref OldConnTable "S" ) )
              ( OldDrainNetName ( arrayref OldConnTable "D" ) ) )
          (let (
                ( NewConnTable ( TableCopy OldConnTable ) )
                ( NewSourceNetName 
                  OldSourceNetName )
                ( NewDrainNetName   
                  OldDrainNetName )
                ( NewBottomSymbol
                  OldBottomSymbol )
                ( NewTopSymbol
                  OldTopSymbol )
                )
            
            
            ( setarray NewConnTable "S" NewSourceNetName )
            ( setarray NewConnTable "D" NewDrainNetName )
            (let (
                  ( NewChainInfo
                    ( ComponentInfoCreateComponentInfo
                      ( ComponentInfoGetLibCellView ChainInfo )
                      (cond (
                             ( rexMatchp ".*\\.[0-9]+" ChainName )
                             (if ( equal ChainFolds 0 ) ChainName
                               ( sprintf nil "%s%d" ChainName I )
                               ) )
                            (
                             t
                             ( sprintf nil "%s.%d" ChainName I ) ) )
                      ChainParameters
                      (if Parity
                          NewTopSymbol
                        NewBottomSymbol )
                      (if Parity
                          NewBottomSymbol
                        NewTopSymbol )
                      NewConnTable
                      ChainHeight
                      ChainWidth
                      Position
                      BBox
                      HParity
                      BottomCSInfo
                      TopCSInfo
                      ) ) )
              
              ( ComponentInfoSetStrengthForChainInfo
                NewChainInfo
                FoldedChainStrength )
              
              ( setq ChainInfoList 
                     ( tconc ChainInfoList NewChainInfo
                             ) )
              ( setq I ( plus I 1 ) )

              ) ) ) )
      ( car ChainInfoList ) ) ) )



(defun ChainFoldToChainInfos ( CellView Chain ColumnWidth UserUnitsPerMeter RoundToNearest PreferEvenFolds )
  "Converts a chain instance into a <a href=glossary.html#ChainInfo>ChainInfos</a> that isn't part of the database"
  ( ChainFoldChainInfoToChainInfos 
    CellView 
    ( ComponentInfoCreateComponentInfoFromComponent Chain )
    ColumnWidth 
    UserUnitsPerMeter
    RoundToNearest
    PreferEvenFolds ) )

(defun ChainFoldToChainInfosOverrideFolds ( CellView Chain ChainFolds RoundToNearest )
  ( ChainFoldChainInfoToChainInfosOverrideFolds 
    CellView 
    ( ComponentInfoCreateComponentInfoFromComponent Chain )
    ChainFolds
    RoundToNearest ) )

(defun ChainFoldChains ( CellView 
                         Chains
                         ColumnWidth
                         UserUnitsPerMeter
                         RoundToNearest
                         PreferEvenFolds  )
  ( mapcar 
    (lambda ( Chain )
      ( ChainCreateFoldedChainsFromChainInfos 
        CellView 
        ( ChainFoldToChainInfos CellView Chain ColumnWidth UserUnitsPerMeter RoundToNearest PreferEvenFolds )
        ( getq Chain xy ) )
      ( dbDeleteObject Chain ) )
    Chains ) )

(defun ChainFoldChainsOverrideFolds ( CellView 
                                      Chains
                                      ChainFolds
                                      RoundToNearest )
  ( mapcar 
    (lambda ( Chain )
      ( ChainCreateFoldedChainsFromChainInfos 
        CellView
        ( ChainFoldToChainInfosOverrideFolds CellView Chain ChainFolds RoundToNearest )
        ( getq Chain xy )
         )
      ( dbDeleteObject Chain ) )
    Chains ) )

  
(defun ChainFoldAllChains ( CellView
                            ChainLibCellPairRegExs
                            ColumnWidth
                            UserUnitsPerMeter
                            RoundToNearest
                            PreferEvenFolds )
  
  (let (
        ( Chains ( cadr ( NameFilterInstances 
                          ( getq CellView instances )
                          ChainLibCellPairRegExs ) ) ) )
    ( ChainFoldChains 
      CellView
      Chains
      ColumnWidth
      UserUnitsPerMeter 
      RoundToNearest
      PreferEvenFolds ) ) )
