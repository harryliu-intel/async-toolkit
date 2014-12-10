; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

defun( UpdateNetlistTablifyNets ( CellView TargetTable )
  foreach( Net CellView~>nets
    TargetTable[Net~>name] = Net 
  )
)

defun( UpdateNetlistTablifyTerminals ( CellView TargetTable )
  foreach( Term CellView~>terminals 
     TargetTable[Term~>name] = Term  
  )
)

defun( UpdateNetlistTablifyInstanceTerms ( Instance TargetTable )
  foreach( InstTerm Instance~>conns
    TargetTable[InstTerm~>term~>name] = InstTerm  
  ) 
)

defun( UpdateNetlistCompareConnections ( InstTerm1 InstTerm2 )  
  InstTerm1 && InstTerm2 && InstTerm1~>net~>name==InstTerm2~>net~>name && InstTerm1~>name==InstTerm2~>name
)

(defun UpdateNetlistGetInstancesToDelete ( Instances
                                           InstanceMap
                                           ConnectivityInstanceExistsFunc 
                                           ConnectivityInstanceExistsFuncParams )
  ( setof
    Instance
    Instances
    ( null 
      ( apply
        ConnectivityInstanceExistsFunc
        ( cons
          ( NameGetCanonicalInstanceNameForInstance InstanceMap Instance )
          ConnectivityInstanceExistsFuncParams ) ) ) ) )


(defun UpdateNetlistGetInstancesToFix ( Instances )
  nil )


(defun UpdateNetlistGetInstanceCoincidences ( Instances
                                              InstanceMap
                                              ConnectivityGetInstanceMasterLibNameFunc
                                              ConnectivityGetInstanceMasterLibNameFuncParams
                                              ConnectivityGetInstanceMasterCellNameFunc
                                              ConnectivityGetInstanceMasterCellNameFuncParams
                                              ConnectivityGetInstanceTransformFunc
                                              ConnectivityGetInstanceTransformFuncParams
                                              )
  temp=( Partition
    Instances
    (lambda ( Instance )
      (let (
            ( CanonicalInstanceName 
              ( NameGetCanonicalInstanceNameForInstance
                InstanceMap
                Instance ) ) )
        (let (
              ( Transform
                ( apply
                  ConnectivityGetInstanceTransformFunc
                  ( cons 
                    CanonicalInstanceName
                    ConnectivityGetInstanceTransformFuncParams ) ) )
              ( LibName
                ( apply 
                  ConnectivityGetInstanceMasterLibNameFunc
                  ( cons
                    CanonicalInstanceName
                    ConnectivityGetInstanceMasterLibNameFuncParams ) ) )
              ( CellName
                ( apply 
                  ConnectivityGetInstanceMasterCellNameFunc
                  ( cons
                    CanonicalInstanceName
                    ConnectivityGetInstanceMasterCellNameFuncParams ) ) )
              )
          ( list 
            ( list 
              LibName
              CellName )
            (when Transform
              ( list
                ( mapcar
                  (lambda ( Coord ) ( MathRoundToNearest Coord 1e-3 ) )
                  ( car Transform ) )
                ( cadr Transform ) ) ) )

          ) ) ) ) )


(defun UpdateNetlistGetInstancesToChange ( Instances
                                           InstanceMap
                                           ConnectivityGetInstanceMasterLibNameFunc
                                           ConnectivityGetInstanceMasterLibNameFuncParams
                                           ConnectivityGetInstanceMasterCellNameFunc
                                           ConnectivityGetInstanceMasterCellNameFuncParams
                                           ConnectivityGetInstanceTransformFunc
                                           ConnectivityGetInstanceTransformFuncParams

                                           )

  (let (
        ( InstanceCoincidences
          ( UpdateNetlistGetInstanceCoincidences
            Instances
            InstanceMap
            ConnectivityGetInstanceMasterLibNameFunc
            ConnectivityGetInstanceMasterLibNameFuncParams
            ConnectivityGetInstanceMasterCellNameFunc
            ConnectivityGetInstanceMasterCellNameFuncParams
            ConnectivityGetInstanceTransformFunc
            ConnectivityGetInstanceTransformFuncParams
         ) ) )
  ( ListApplyFuncToListAndAccumulateNonNilResults
    Instances
    (lambda ( Instance
              ConnectivityGetInstanceMasterLibNameFunc
              ConnectivityGetInstanceMasterLibNameFuncParams
              ConnectivityGetInstanceMasterCellNameFunc
              ConnectivityGetInstanceMasterCellNameFuncParams
              ConnectivityGetInstanceTransformFunc
              ConnectivityGetInstanceTransformFuncParams )
      (let (
            ( CanonicalInstanceName 
              ( NameGetCanonicalInstanceNameForInstance InstanceMap Instance ) ) )  
        (let (
              ( InstanceTransform ( getq Instance transform ) )
              ( ConnectivityInstanceMasterLibName
                ( apply
                  ConnectivityGetInstanceMasterLibNameFunc
                  ( cons
                    CanonicalInstanceName
                    ConnectivityGetInstanceMasterLibNameFuncParams ) ) )
              ( ConnectivityInstanceMasterCellName
                ( apply
                  ConnectivityGetInstanceMasterCellNameFunc
                  ( cons
                    CanonicalInstanceName
                    ConnectivityGetInstanceMasterCellNameFuncParams ) ) )
              ( ConnectivityInstanceTransform
                ( apply
                  ConnectivityGetInstanceTransformFunc
                  ( cons
                    CanonicalInstanceName
                    ConnectivityGetInstanceTransformFuncParams ) ) ) )
          (when ( or
                  ;coincidences
                  ( and
                    ;the location is not null
                    ( cadr 
                      ( car
                        ( arrayref InstanceCoincidences
                                   Instance  ) ) )
                    ;there is more than one instance at the location
                    ( greaterp
                      ( length
                        ( cadr
                          ( arrayref InstanceCoincidences
                                     Instance  ) ) )
                      1 ) )
                  ( not 
                    ( or
                      ( null ConnectivityInstanceMasterLibName )
                      ( null ConnectivityInstanceMasterCellName )
                      ( and
                        ( equal
                          CanonicalInstanceName
                          ( getq Instance name ) )
                        ( equal
                          ConnectivityInstanceMasterCellName
                          ( getq Instance cellName ) )
                        ( equal
                          ConnectivityInstanceMasterLibName
                          ( getq Instance libName ) )
                        ( or
                          ( null ConnectivityInstanceTransform )
                          (let (
                                ( InstancePos ( car InstanceTransform ) )
                                ( InstanceRotation ( cadr InstanceTransform ) )
                                ( ConnectivityInstancePos ( car ConnectivityInstanceTransform ) )
                                ( ConnectivityInstanceRotation ( cadr ConnectivityInstanceTransform ) ) )
                            ( and
                              ( lessp
                                ( abs
                                  ( difference
                                    ( car InstancePos )
                                    ( car ConnectivityInstancePos ) ) )
                                ( quotient 0.0001 1000000.0 ) )
                              ( lessp
                                ( abs
                                  ( difference
                                    ( cadr InstancePos )
                                    ( cadr ConnectivityInstancePos ) ) )
                                ( quotient 0.0001 1000000.0 ) )
                              ( equal InstanceRotation ConnectivityInstanceRotation ) ) ) ) ) ) ) )
            ( list
              CanonicalInstanceName
              ConnectivityInstanceMasterLibName 
              ConnectivityInstanceMasterCellName
              Instance
              ConnectivityInstanceTransform
              ( arrayref InstanceCoincidences Instance )
              ) ) ) ) )
    ( list
      ConnectivityGetInstanceMasterLibNameFunc
      ConnectivityGetInstanceMasterLibNameFuncParams
      ConnectivityGetInstanceMasterCellNameFunc
      ConnectivityGetInstanceMasterCellNameFuncParams
      ConnectivityGetInstanceTransformFunc
      ConnectivityGetInstanceTransformFuncParams ) ) ) )

(defun UpdateNetlistGetInstancesToCreate ( InstanceMap
                                           ConnectivityInstanceNames 
                                           ConnectivityGetInstanceMasterLibNameFunc
                                           ConnectivityGetInstanceMasterLibNameFuncParams
                                           ConnectivityGetInstanceMasterCellNameFunc
                                           ConnectivityGetInstanceMasterCellNameFuncParams
                                           ConnectivityGetInstanceTransformFunc
                                           ConnectivityGetInstanceTransformFuncParams )

  ( ListApplyFuncToListAndAccumulateNonNilResults
    ConnectivityInstanceNames
    (lambda
      ( ConnectivityInstanceName 
        InstanceMap
        ConnectivityGetInstanceMasterLibNameFunc
        ConnectivityGetInstanceMasterLibNameFuncParams
        ConnectivityGetInstanceMasterCellNameFunc
        ConnectivityGetInstanceMasterCellNameFuncParams
      ConnectivityGetInstanceTransformFunc
      ConnectivityGetInstanceTransformFuncParams )
      (unless ( NameGetInstancesForCanonicalName InstanceMap ConnectivityInstanceName )
        ( list
          ConnectivityInstanceName
          ( apply
            ConnectivityGetInstanceMasterLibNameFunc
            ( cons
              ConnectivityInstanceName
              ConnectivityGetInstanceMasterLibNameFuncParams ) )
          ( apply
            ConnectivityGetInstanceMasterCellNameFunc
            ( cons
              ConnectivityInstanceName
              ConnectivityGetInstanceMasterCellNameFuncParams ) )
          ( apply
            ConnectivityGetInstanceTransformFunc
            ( cons
              ConnectivityInstanceName
              ConnectivityGetInstanceTransformFuncParams ) ) ) ) )
     list(
      InstanceMap
      ConnectivityGetInstanceMasterLibNameFunc
      ConnectivityGetInstanceMasterLibNameFuncParams
      ConnectivityGetInstanceMasterCellNameFunc
      ConnectivityGetInstanceMasterCellNameFuncParams
      ConnectivityGetInstanceTransformFunc
      ConnectivityGetInstanceTransformFuncParams ) ) )

(defun UpdateNetlistGetInstanceParametersToSet ( InstanceMap
                                                 ConnectivityInstanceNames
                                                 ConnectivityGetInstanceParameterPairsFunc
                                                 ConnectivityGetInstanceParameterPairsFuncParams )
  (let (
        ( Ret nil ) )
    ( foreach
      ConnectivityInstanceName
      ConnectivityInstanceNames
      (let (
            ( CurrInstances
              ( NameGetInstancesForCanonicalName
                InstanceMap ConnectivityInstanceName ) )
            ( ConnecitivityInstanceParamPairs 
              ( apply
                ConnectivityGetInstanceParameterPairsFunc
                ( cons
                  ConnectivityInstanceName
                  ConnectivityGetInstanceParameterPairsFuncParams ) ) ) )
        ( foreach
          ParamPair
          ConnecitivityInstanceParamPairs         
          (let (
                ( ParamName ( car ParamPair ) )
                ( ParamValue ( cadr ParamPair ) ) )
            (let (
                  ( ParamRet nil )
                  ( ParamValueNum 
                    (if ( stringp ParamValue )
                        ( StringUtilParseLengthString ParamValue )
                      ParamValue ) ) )
              (cond (
                                        ;chain width, check all folds
                     ( rexMatchp "^[NP]*[Ww]+[0-9]*$" ParamName )
                     ( setq ParamRet
                            (if CurrInstances
                                (let (
                                      ( TotalWidth 
                                        ( PDKCombineFolds
                                          ( mapcar
                                            (lambda ( CurrInstance )
                                              (let (
                                                    ( WidthValue
                                                      ( getq
                                                        ( dbSearchPropByName
                                                          CurrInstance
                                                          ParamName )
                                                          value ) ) )
                                                (if WidthValue 
                                                    (if ( stringp WidthValue )
                                                        ( StringUtilParseLengthString WidthValue )
                                                      WidthValue )
                                                  -1e9 ) ) )
                                            CurrInstances ) ) ) )
                                          
                                  (if ( greaterp
                                        ( abs
                                          ( difference
                                            TotalWidth
                                            ParamValueNum ) )
                                        ( quotient 0.0001 1000000.0 ) )
                                      ( list ConnectivityInstanceName
                                             ParamName 
                                             ( quotient ParamValueNum ( length CurrInstances ) )
                                             TotalWidth ) ) )
                              ( list ConnectivityInstanceName ParamName ParamValueNum ) ) ) )
                    ( 
                     t
                     (if CurrInstances
                         ( foreach CurrInstance CurrInstances 
                                   (let (
                                         ( ExistingParam 
                                           ( dbSearchPropByName
                                             CurrInstance
                                             ParamName ) ) )
                                     ( setq ParamRet
                                            (if ExistingParam
                                                (let (
                                                      ( ExistingValue ( getq ExistingParam value ) ) )
                                                  (let (
                                                        ( ExistingValueNum 
                                                          (if ( stringp ExistingValue )
                                                              ( StringUtilParseLengthString
                                                                ExistingValue )
                                                            ExistingValue ) ) )
                                                    (if ( greaterp
                                                          ( abs
                                                            ( difference
                                                              ExistingValueNum
                                                              ParamValueNum ) )
                                                          ( quotient 0.0001 1000000.0 ) )
                                                        ( list ConnectivityInstanceName ParamName ParamValueNum ExistingValueNum )
                                                      ParamRet ) ) )
                                              ( list ConnectivityInstanceName ParamName ParamValueNum ) ) ) ) )
                       ( setq ParamRet ( list ConnectivityInstanceName ParamName ParamValueNum ) ) ) ) )

              (if ParamRet 
                  ( setq Ret ( cons ParamRet Ret ) ) ) ) ) ) ) )
    Ret ) )

(defun UpdateNetlistCompareCellInstances ( Instances
                                           InstanceMap
                                           ConnectivityInstanceExistsFunc 
                                           ConnectivityInstanceExistsFuncParams
                                           ConnectivityGetInstanceMasterLibNameFunc
                                           ConnectivityGetInstanceMasterLibNameFuncParams
                                           ConnectivityGetInstanceMasterCellNameFunc
                                           ConnectivityGetInstanceMasterCellNameFuncParams
                                           ConnectivityInstanceNames
                                           ConnectivityGetInstanceParameterPairsFunc
                                           ConnectivityGetInstanceParameterPairsFuncParams
                                           ConnectivityGetInstanceTransformFunc
                                           ConnectivityGetInstanceTransformFuncParams )
  (let (
        ( InstancesToDelete ( UpdateNetlistGetInstancesToDelete
                              Instances
                              InstanceMap
                              ConnectivityInstanceExistsFunc 
                              ConnectivityInstanceExistsFuncParams ) )
        ( InstancesToCreate ( UpdateNetlistGetInstancesToCreate
                              InstanceMap
                              ConnectivityInstanceNames 
                              ConnectivityGetInstanceMasterLibNameFunc
                              ConnectivityGetInstanceMasterLibNameFuncParams
                              ConnectivityGetInstanceMasterCellNameFunc
                              ConnectivityGetInstanceMasterCellNameFuncParams
                              ConnectivityGetInstanceTransformFunc
                              ConnectivityGetInstanceTransformFuncParams ) )
        ( InstancesToFix ( UpdateNetlistGetInstancesToFix
                           Instances ) )
        ( ParametersToSet ( UpdateNetlistGetInstanceParametersToSet
                            InstanceMap
                            ConnectivityInstanceNames
                            ConnectivityGetInstanceParameterPairsFunc
                            ConnectivityGetInstanceParameterPairsFuncParams ) )
        )
    (let (
          ( InstancesToChange ( UpdateNetlistGetInstancesToChange
                                ( ListDifferenceNoTableElements
                                  Instances
                                  InstancesToDelete
                                  )
                                InstanceMap
                                ConnectivityGetInstanceMasterLibNameFunc
                                ConnectivityGetInstanceMasterLibNameFuncParams
                                ConnectivityGetInstanceMasterCellNameFunc
                                ConnectivityGetInstanceMasterCellNameFuncParams
                                ConnectivityGetInstanceTransformFunc
                                ConnectivityGetInstanceTransformFuncParams ) )
          )

      (when ( or InstancesToDelete
                 InstancesToChange
                 InstancesToCreate 
                 InstancesToFix
                 ParametersToSet )
        (when InstancesToCreate
          ( printf "%L\n" InstancesToCreate ) )
        tempCompareCellInstances = ( list
          InstancesToDelete
          InstancesToChange
          InstancesToCreate 
          InstancesToFix
          ParametersToSet ) ) ) ) )

(defun UpdateNetlistGetBoundaryLayerBBox ( CellView BoundaryLPP Recurse )
  (when CellView
    (if CellView->prBoundary CellView->prBoundary->bBox ) ) )


(defun UpdateNetlistCompareCellBoundary ( CellView
                                          ConnectivityInstanceNames
                                          BoundaryLayerLPP
                                          UserUnitsPerMeter
                                          ConnectivityGetCellHeightFunc
                                          ConnectivityGetCellHeightFuncParams
                                          ConnectivityEstimateCellWidthFunc
                                          ConnectivityEstimateCellWidthFuncParams
                                          ConnectivityGetBoundaryPositionFunc
                                          ConnectivityGetBoundaryPositionFuncParams
                                          LockBound )
  
  (let (
        ( Shapes ( getq CellView shapes ) )
        ( ExistingBoundaryBBox ( UpdateNetlistGetBoundaryLayerBBox CellView BoundaryLayerLPP nil ) ) 
        ( LibName ( getq CellView libName ) )
        ( CellName ( getq CellView cellName ) )
        ( ViewName ( getq CellView viewName ) ) )
    ;If we are allowed to change the boundary
    ;or if there is no boundary shape then figure
    ;out what the boundayr shape should be and compare
    ;that to the boundary shape in the cell.
    (unless ( and LockBound ExistingBoundaryBBox )
      (let (
            ;Get Height and width of existing shape.
            ( HeightOfExistingBoundary (when ExistingBoundaryBBox
                                         ( quotient
                                           ( RectGetHeight ExistingBoundaryBBox )
                                           ( float UserUnitsPerMeter ) ) ) )
            ( WidthOfExistingBoundary (when ExistingBoundaryBBox
                                        ( quotient
                                          ( RectGetWidth ExistingBoundaryBBox )
                                          ( float UserUnitsPerMeter ) ) ) ) )
        (let (
              ;Ask the netlist what it thinks the height should be.
              ;See UpdateNetlistGetDefaultGetFloorplanViewHeightFunc, 
              ;UpdateNetlistGetDefaultGetLayoutViewHeightFunc, and
              ;UpdateNetlistGetDefaultGetNetlistViewHeightFunc which are all called
              ;by the lambda function returned by UpdateNetlistGetDefaultGetCellHeightFunc.
              ;If the ConnectivityGetCellHeightFunc returns nil then this function will return nil,
              ;otherwise the ConnectivityGetCellHeightFunc should return a pair
              ;whose first element is the desired height of the cell and whose second element is nil
              ;or a string containing a warning message.
              ( ConnectivityCellHeightRet ( apply
                                            ConnectivityGetCellHeightFunc
                                            ( cons
                                              LibName
                                              ( cons
                                                CellName
                                                ( cons
                                                  ViewName
                                                  ( cons
                                                    HeightOfExistingBoundary
                                                    ( cons
                                                      ConnectivityInstanceNames
                                                      ConnectivityGetCellHeightFuncParams ) ) ) ) ) ) ) )
          (when ConnectivityCellHeightRet
            (let (
                  ( ConnectivityCellHeight ( car ConnectivityCellHeightRet ) )
                  ( ConnectivityCellHeightWarning ( cadr ConnectivityCellHeightRet ) ) )
              (let (
                    ( HeightCompareResult (if HeightOfExistingBoundary
                                              (when ( greaterp
                                                      ( abs 
                                                        ( difference 
                                                          HeightOfExistingBoundary
                                                          ConnectivityCellHeight ) )
                                                      ( quotient
                                                        0.0001
                                                        1000000 ) )
                                                ( list 
                                                  ConnectivityCellHeight 
                                                  HeightOfExistingBoundary 
                                                  ConnectivityCellHeightWarning ) )
                                            ( list 
                                              ConnectivityCellHeight
                                              nil
                                              ConnectivityCellHeightWarning ) ) ) )
                ;check width if there aren't any non-pin, non-label, non-boundary shapes
                ;and there are no connectivity instances.  In other words if the cell
                ;contains any shapes that we are not expecting assume the width in the
                ;cell is correct.
                (if ( not 
                      ( or 
                        ( exists 
                          Shape
                          Shapes 
                          ( and 
                            ( not
                              ( equal ( getq Shape lpp )
                                      BoundaryLayerLPP ) )
                            ( null ( getq Shape pin ) )
                            ( not ( equal ( getq Shape objType ) "label" ) ) ) )
                        ConnectivityInstanceNames ) )
                    (let (
                          ;Find the DB object for the existing boundary if there is one
                          ;so that we can put it in our return value so people using our return
                          ;value won't have to go looking for it.
                          ( ExistingBoundary CellView->prBoundary )

                          ;Ask the netlist what it thinks the width should be.
                          ;See UpdateNetlistGetDefaultGetLayoutViewWidthFunc, UpdateNetlistGetDefaultGetFloorplanViewWidthFunc,
                          ;and UpdateNetlistGetDefaultGetNetlistViewWidthFunc which are all called
                          ;by the lambda function returned by UpdateNetlistGetDefaultGetCellWidthFunc.
                          ;If the ConnectivityEstimateCellWidthFunc returns nil then this function will return nil,
                          ;otherwise the ConnectivityEstimateCellWidthFunc should return a pair
                          ;whose first element is the desired width of the cell and whose second elementn is nil
                          ;or a string containing a warning message.
                          ( ConnectivityEstimatedCellWidthRet
                            ( apply
                              ConnectivityEstimateCellWidthFunc
                              ( cons
                                LibName
                                ( cons
                                  CellName
                                  ( cons
                                    ViewName
                                    ( cons
                                      ConnectivityCellHeight
                                      ConnectivityEstimateCellWidthFuncParams ) ) ) ) ) ) )
                      (let (
                            ( ConnectivityEstimatedCellWidth ( car ConnectivityEstimatedCellWidthRet ) )
                            ( ConnectivityCellWidthWarning ( cadr ConnectivityEstimatedCellWidthRet ) ) )
                        (when ConnectivityEstimatedCellWidth
                          ;If the width was successfully estimated
                          (let (
                                ;Ask the netlist wheree the boundary should be positioned in the cell.
                                ;See UpdateNetlistGetDefaultGetNetlistViewPositionFunc, UpdateNetlistGetDefaultGetLayoutViewPositionFunc,
                                ;and UpdateNetlistGetDefaultGetFloorplanViewPositionFunc which are all called by the
                                ;lambda function returned by UpdateNetlistGetDefaultGetCellPositionFunc.
                                ;If the ConnectivityGetBoundaryPositionFunc returns nil then this function will return nil,
                                ;otherwise the ConnectivityGetBoundaryPositionFunc should return a pair
                                ;whose first element is the desired position of the cell boundayr and whose second element is nil
                                ;or a string containing a warning message.
                                ( ConnectivityBoundaryPositionRet ( apply 
                                                                    ConnectivityGetBoundaryPositionFunc
                                                                    ( cons
                                                                      LibName
                                                                      ( cons
                                                                        CellName
                                                                        ( cons
                                                                          ViewName
                                                                          ConnectivityGetBoundaryPositionFuncParams ) ) ) ) ) )
                            (let (
                                  ( ConnectivityBoundaryPosition ( car ConnectivityBoundaryPositionRet ) )
                                  ( ConnectivityBoundaryPositionWarning ( cadr ConnectivityBoundaryPositionRet ) ) )
                              (when ConnectivityBoundaryPosition
                                (let (
                                      ( PositionOfExistingBoundary (when ExistingBoundaryBBox
                                                                     ( list
                                                                       ( quotient
                                                                         ( RectGetLeft ExistingBoundaryBBox )
                                                                         ( float UserUnitsPerMeter ) )
                                                                       ( quotient
                                                                         ( RectGetBottom ExistingBoundaryBBox )
                                                                         ( float UserUnitsPerMeter ) ) ) ) ) )
                                  ;Compare width and position of boundary to netlist.
                                  (let (
                                        ( WidthCompareResult (if WidthOfExistingBoundary                      
                                                                 (when ( greaterp
                                                                         ( abs 
                                                                           ( difference 
                                                                             WidthOfExistingBoundary
                                                                             ConnectivityEstimatedCellWidth ) )
                                                                         ( quotient
                                                                           0.0001
                                                                           1000000 ) )
                                                                   ( list 
                                                                     ConnectivityEstimatedCellWidth 
                                                                     WidthOfExistingBoundary
                                                                     ConnectivityCellWidthWarning ) )
                                                               ( list ConnectivityEstimatedCellWidth nil ConnectivityCellWidthWarning ) ) )
                                        ( PositionCompareResult (if PositionOfExistingBoundary
                                                                    (let (
                                                                          ( ConnectivityBoundaryXPos 
                                                                            ( car ConnectivityBoundaryPosition ) )
                                                                          ( ConnectivityBoundaryYPos 
                                                                            ( cadr ConnectivityBoundaryPosition ) )
                                                                          ( XPosOfExistingBoundary 
                                                                            ( car PositionOfExistingBoundary ) )
                                                                          ( YPosOfExistingBoundary 
                                                                            ( cadr PositionOfExistingBoundary ) ) )
                                                                      (when ( or
                                                                              ( greaterp
                                                                                ( abs
                                                                                  ( difference
                                                                                    ConnectivityBoundaryXPos
                                                                                    XPosOfExistingBoundary ) )
                                                                                ( quotient
                                                                                  0.0001
                                                                                  1000000 ) )
                                                                              ( greaterp
                                                                                ( abs
                                                                                  ( difference
                                                                                    ConnectivityBoundaryYPos
                                                                                    YPosOfExistingBoundary ) )
                                                                                ( quotient
                                                                                  0.0001
                                                                                  1000000 ) ) )
                                                                        ( list
                                                                          ConnectivityBoundaryPosition
                                                                          PositionOfExistingBoundary
                                                                          ConnectivityBoundaryPositionWarning ) ) )
                                                                  ( list ConnectivityBoundaryPosition 
                                                                         nil 
                                                                         ConnectivityBoundaryPositionWarning ) ) ) )
                                    (when ( or HeightCompareResult WidthCompareResult PositionCompareResult )
                                      ( list
                                        (if HeightCompareResult
                                            HeightCompareResult
                                          ( list ConnectivityCellHeight nil ConnectivityCellHeightWarning ) )
                                        (if WidthCompareResult
                                            WidthCompareResult
                                          ( list ConnectivityEstimatedCellWidth nil ConnectivityCellWidthWarning ) )
                                        (if PositionCompareResult
                                            PositionCompareResult
                                          ( list ConnectivityBoundaryPosition nil ConnectivityBoundaryPositionWarning ) )
                                        ExistingBoundary ) ) ) ) ) ) ) ) ) )
                  (when HeightCompareResult
                    ( list
                      HeightCompareResult
                      nil
                      nil
                      nil ) ) ) ) ) ) ) ) ) ) )
          

(defun UpdateNetlistCompareEmptyCellBoundary ( ConnectivityInstanceNames 
                                               LibName 
                                               CellName
                                               ViewName
                                               ConnectivityGetCellHeightFunc
                                               ConnectivityGetCellHeightFuncParams
                                               ConnectivityEstimateCellWidthFunc
                                               ConnectivityEstimateCellWidthFuncParams
                                               ConnectivityGetBoundaryPositionFunc
                                               ConnectivityGetBoundaryPositionFuncParams )
  (unless ConnectivityInstanceNames
    (let (
          ( ConnectivityCellHeightRet ( apply
                                        ConnectivityGetCellHeightFunc
                                        ( cons
                                          LibName
                                          ( cons
                                            CellName
                                            ( cons
                                              ViewName
                                              ( cons
                                                nil
                                                ( cons
                                                  ConnectivityInstanceNames
                                                  ConnectivityGetCellHeightFuncParams ) ) ) ) ) ) ) )



      (let (
            ( ConnectivityCellHeight ( car ConnectivityCellHeightRet ) )
            ( ConnectivityCellHeightWarning ( cadr ConnectivityCellHeightRet ) ) )
        (when ConnectivityCellHeightRet
          (let (
                ( ConnectivityEstimatedCellWidthRet ( apply
                                                      ConnectivityEstimateCellWidthFunc
                                                      ( cons
                                                        LibName
                                                        ( cons
                                                          CellName
                                                          ( cons
                                                            ViewName
                                                            ( cons
                                                              ConnectivityCellHeight
                                                              ConnectivityEstimateCellWidthFuncParams ) ) ) ) ) ) )
            (let (
                  ( ConnectivityEstimatedCellWidth ( car ConnectivityEstimatedCellWidthRet ) )
                  ( ConnectivityEstimatedCellWidthWarning ( cadr ConnectivityEstimatedCellWidthRet ) ) )
              (when ConnectivityEstimatedCellWidthRet
                (let ( 
                      ( ConnectivityBoundaryPositionRet ( apply 
                                                          ConnectivityGetBoundaryPositionFunc
                                                          ( cons
                                                            LibName
                                                            ( cons
                                                              CellName
                                                              ( cons
                                                                ViewName
                                                                ConnectivityGetBoundaryPositionFuncParams ) ) ) ) ) )
                  (let ( 
                        ( ConnectivityBoundaryPosition ( car ConnectivityBoundaryPositionRet ) )
                        ( ConnectivityBoundaryPositionWarning ( cadr ConnectivityBoundaryPositionRet ) ) )
                    (when ConnectivityBoundaryPositionRet
                      ( list
                        ( list ConnectivityCellHeight nil ConnectivityCellHeightWarning )
                        ( list ConnectivityEstimatedCellWidth nil ConnectivityEstimatedCellWidthWarning )
                        ( list ConnectivityBoundaryPosition nil ConnectivityBoundaryPositionWarning )
                        nil ) ) ) ) ) ) ) ) ) ) ) )

(defun UpdateNetlistCompareEmptyCellInstances ( ConnectivityInstanceNames 
                                                ConnectivityGetInstanceMasterLibNameFunc
                                                ConnectivityGetInstanceMasterLibNameFuncParams
                                                ConnectivityGetInstanceMasterCellNameFunc
                                                ConnectivityGetInstanceMasterCellNameFuncParams
                                                ConnectivityGetInstanceParameterPairsFunc
                                                ConnectivityGetInstanceParameterPairsFuncParams
                                                ConnectivityGetInstanceTransformFunc
                                                ConnectivityGetInstanceTransformFuncParams )
  ( list
    nil
    nil
    ( ListApplyFuncToListAndAccumulateResults
      ConnectivityInstanceNames
      (lambda
        ( ConnectivityInstanceName
          ConnectivityGetInstanceMasterLibNameFunc
          ConnectivityGetInstanceMasterLibNameFuncParams
          ConnectivityGetInstanceMasterCellNameFunc
          ConnectivityGetInstanceMasterCellNameFuncParams
          ConnectivityGetInstanceTransformFunc
          ConnectivityGetInstanceTransformFuncParams )
        ( list
          ConnectivityInstanceName
          ( apply
            ConnectivityGetInstanceMasterLibNameFunc
            ( cons
              ConnectivityInstanceName
              ConnectivityGetInstanceMasterLibNameFuncParams ) )
          ( apply
            ConnectivityGetInstanceMasterCellNameFunc
            ( cons
              ConnectivityInstanceName
              ConnectivityGetInstanceMasterCellNameFuncParams ) )
          ( apply
            ConnectivityGetInstanceTransformFunc
            ( cons
              ConnectivityInstanceName
              ConnectivityGetInstanceTransformFuncParams ) ) ) )
      ( list
        ConnectivityGetInstanceMasterLibNameFunc
        ConnectivityGetInstanceMasterLibNameFuncParams
        ConnectivityGetInstanceMasterCellNameFunc
        ConnectivityGetInstanceMasterCellNameFuncParams
        ConnectivityGetInstanceTransformFunc
        ConnectivityGetInstanceTransformFuncParams ) )
    nil
    ( ListApplyFuncToListAndAccumulateResult
      ConnectivityInstanceNames
      (lambda
        ( ConnectivityInstanceName
          CurrParamsToSet
          ConnectivityGetInstanceParameterPairsFunc
          ConnectivityGetInstanceParameterPairsFuncParams )
        (let (
              ( ParamPairs ( apply
                             ConnectivityGetInstanceParameterPairsFunc
                             ( cons
                               ConnectivityInstanceName
                               ConnectivityGetInstanceParameterPairsFuncParams ) ) ) )
          ( ListApplyFuncToListAndAccumulateResult
            ParamPairs
            (lambda
              ( ParamPair CurrResult InstanceName )
              ( cons 
                ( list 
                  InstanceName 
                  ( car ParamPair )
                  (let (
                        ( ParamVal ( cadr ParamPair ) ) )
                    (if ( stringp ParamVal )
                        ( StringUtilParseLengthString ParamVal )
                      ParamVal ) ) )
                CurrResult ) )
            ( list
              ConnectivityInstanceName )
            CurrParamsToSet ) ) )
      ( list
        ConnectivityGetInstanceParameterPairsFunc
        ConnectivityGetInstanceParameterPairsFuncParams )
      nil ) ) )



(defun UpdateNetlistGetConnectionsToDelete ( InstanceMap 
                                             ConnectivityInstanceNames
                                             ConnectivityInstanceHasConnectionFunc
                                             ConnectivityInstanceHasConnectionFuncParams )
  (let (
        ( Ret nil ) )
    ( foreach
      ConnectivityInstanceName
      ConnectivityInstanceNames
      (let (
            ( CurrInstances ( NameGetInstancesForCanonicalName InstanceMap ConnectivityInstanceName ) ) )
        (when CurrInstances
          ( foreach
            CurrInstance
            CurrInstances
            (let (
                  ( CurrInstanceConnectionsToDelete ( setof
                                                      CurrInstanceConnection
                                                      ( getq CurrInstance conns )
                                                      ( not
                                                        ( apply
                                                          ConnectivityInstanceHasConnectionFunc
                                                          ( cons
                                                            ConnectivityInstanceName
                                                            ( cons
                                                              ( getq CurrInstanceConnection name )
                                                              ( cons 
                                                                ( getq ( getq CurrInstanceConnection net ) name )
                                                                ConnectivityInstanceHasConnectionFuncParams ) ) ) ) ) ) ) )
              (when CurrInstanceConnectionsToDelete
                ( setq Ret ( lconc Ret CurrInstanceConnectionsToDelete ) ) ) ) ) ) ) )
    ( car Ret ) ) )

(defun UpdateNetlistInstanceHasMaster ( Instance )
  (let (
        ( MasterLibName ( getq Instance libName ) )
        ( MasterCellName ( getq Instance cellName ) )
        ( MasterViewName ( getq Instance viewName ) ) )
    (let (
          ( MasterDDObj ( ddGetObj MasterLibName MasterCellName MasterViewName ) ) )
      ( and
        MasterDDObj
        ( getq MasterDDObj files ) ) ) ) )

(defun UpdateNetlistGetConnectionsToCreate ( InstanceMap 
                                             ConnectivityInstanceNames
                                             ConnectivityGetInstanceConnectionsFunc
                                             ConnectivityGetInstanceConnectionsFuncParams )
  (let (
        ( Ret nil ) )
    ( foreach
      ConnectivityInstanceName
      ConnectivityInstanceNames
      (let (
            ( CurrInstance ( car ( NameGetInstancesForCanonicalName InstanceMap ConnectivityInstanceName ) ) )
            ( ConnectivityInstanceConnections ( apply
                                                ConnectivityGetInstanceConnectionsFunc
                                                ( cons
                                                  ConnectivityInstanceName
                                                  ConnectivityGetInstanceConnectionsFuncParams ) ) ) )
        (let (
              ( CurrInstanceConnectionsToCreate (if ( and
                                                      CurrInstance
                                                      ( UpdateNetlistInstanceHasMaster
                                                        CurrInstance ) )
                                                    (let (
                                                          ( CurrInstanceConnectionsTable ( makeTable "bar" nil ) ) )
                                                      ( UpdateNetlistTablifyInstanceTerms 
                                                        CurrInstance
                                                        CurrInstanceConnectionsTable )
                                                      ( setof
                                                        ConnectivityInstanceConnection
                                                        ConnectivityInstanceConnections
                                                        (let (
                                                              ( CurrConnection ( arrayref 
                                                                                 CurrInstanceConnectionsTable
                                                                                 ( car ConnectivityInstanceConnection ) ) ) )
                                                          ( not
                                                            ( and
                                                              CurrConnection
                                                              ( equal
                                                                ( getq ( getq CurrConnection net ) name )
                                                                ( cadr ConnectivityInstanceConnection ) ) ) ) ) ) )
                                                  ConnectivityInstanceConnections ) ) )
          ( setq 
            Ret 
            ( lconc
              Ret 
              ( ListApplyFuncToListAndAccumulateResults
                CurrInstanceConnectionsToCreate
                (lambda ( ConnectionToCreate ConnectivityInstanceName )
                  ( cons ConnectivityInstanceName ConnectionToCreate ) )
                ( list ConnectivityInstanceName ) ) ) ) ) ) )
    ( car Ret ) ) )

(defun UpdateNetlistGetTerminalsToDelete ( CellView
                                           ConnectivityTerminalExistsFunc 
                                           ConnectivityTerminalExistsFuncParams )
  ( setof
    Term
    ( getq CellView terminals )
    ( null
      ( apply
        ConnectivityTerminalExistsFunc
        ( cons
          ( getq Term name )
          ConnectivityTerminalExistsFuncParams ) ) ) ) )

(defun UpdateNetlistGetTerminalsToCreate ( CellView ConnectivityTerminalNames )
  (let (
        ( TerminalsTable ( makeTable "foo" nil ) ) )
    ( UpdateNetlistTablifyTerminals CellView TerminalsTable )
    ( setof
      ConnectivityTerminalName
      ConnectivityTerminalNames
      ( null ( arrayref TerminalsTable ConnectivityTerminalName ) ) ) ) )

(defun UpdateNetlistGetNetsToDelete ( CellView 
                                      ConnectivityNetExistsFunc 
                                      ConnectivityNetExistsFuncParams )
  ( setof
    Net
    ( getq CellView nets )
    ( null
      ( apply
        ConnectivityNetExistsFunc
        ( cons 
          ( getq Net name )
          ConnectivityNetExistsFuncParams ) ) ) ) )

(defun UpdateNetlistGetNetsToCreate ( CellView ConnectivityNetNames )
  (let (
        ( NetsTable ( makeTable "foo" nil ) ) )
    ( UpdateNetlistTablifyNets CellView NetsTable )
    ( setof
      ConnectivityNetName
      ConnectivityNetNames
      ( null ( arrayref NetsTable ConnectivityNetName ) ) ) ) )

(defun UpdateNetlistGetNetsToChange ( CellView
                                      ConnectivityNetNames
                                      ConnectivityGetNetWireSpacingFunc
                                      ConnectivityGetNetWireSpacingFuncParams
                                      ConnectivityGetNetWireWidthFunc
                                      ConnectivityGetNetWireWidthFuncParams )
  ( setof 
    Change
    ( mapcar
      (lambda ( NetName )
        (let (
              ( WireSpacing
                ( apply ConnectivityGetNetWireSpacingFunc
                        ( cons NetName
                               ConnectivityGetNetWireSpacingFuncParams ) ) )
              ( WireWidth
                ( apply ConnectivityGetNetWireWidthFunc
                        ( cons NetName
                               ConnectivityGetNetWireWidthFuncParams ) ) )
              ( Net ( dbFindNetByName CellView NetName ) )
              )
          (when ( or 
                  ( null Net )
                  ( not 
                    ( equal
                      WireSpacing
                      ( ConstraintsGetNetWireSpacing
                        Net ) ) )
                  ( not 
                    ( equal
                      WireWidth
                      ( ConstraintsGetNetWireWidth
                        Net ) ) ) )
            ( list 
              NetName
              WireSpacing
              WireWidth ) ) ) )
      ConnectivityNetNames )
    Change ) )


(defun UpdateNetlistCompareCellConnectivity ( CellView
                                              InstanceMap
                                              ConnectivityInstanceNames
                                              ConnectivityInstanceHasConnectionFunc
                                              ConnectivityInstanceHasConnectionFuncParams
                                              ConnectivityGetInstanceConnectionsFunc
                                              ConnectivityGetInstanceConnectionsFuncParams
                                              ConnectivityTerminalExistsFunc 
                                              ConnectivityTerminalExistsFuncParams 
                                              ConnectivityTerminalNames
                                              ConnectivityNetExistsFunc 
                                              ConnectivityNetExistsFuncParams 
                                              ConnectivityNetNames
                                              ConnectivityGetNetWireSpacingFunc
                                              ConnectivityGetNetWireSpacingFuncParams
                                              ConnectivityGetNetWireWidthFunc
                                              ConnectivityGetNetWireWidthFuncParams )
  (let (
        ( ConnectionsToDelete ( UpdateNetlistGetConnectionsToDelete
                                InstanceMap
                                ConnectivityInstanceNames
                                ConnectivityInstanceHasConnectionFunc
                                ConnectivityInstanceHasConnectionFuncParams ) )
        ( ConnectionsToCreate ( UpdateNetlistGetConnectionsToCreate
                                InstanceMap 
                                ConnectivityInstanceNames
                                ConnectivityGetInstanceConnectionsFunc
                                ConnectivityGetInstanceConnectionsFuncParams ) )
        ( TerminalsToDelete ( UpdateNetlistGetTerminalsToDelete
                              CellView 
                              ConnectivityTerminalExistsFunc 
                              ConnectivityTerminalExistsFuncParams ) )
        ( TerminalsToCreate ( UpdateNetlistGetTerminalsToCreate
                              CellView 
                              ConnectivityTerminalNames ) )
        ( NetsToDelete ( UpdateNetlistGetNetsToDelete
                         CellView 
                         ConnectivityNetExistsFunc 
                         ConnectivityNetExistsFuncParams ) )
        ( NetsToCreate ( UpdateNetlistGetNetsToCreate
                         CellView 
                         ConnectivityNetNames ) )
        ( NetsToChange ( UpdateNetlistGetNetsToChange
                         CellView 
                         ConnectivityNetNames
                         ConnectivityGetNetWireSpacingFunc
                         ConnectivityGetNetWireSpacingFuncParams
                         ConnectivityGetNetWireWidthFunc
                         ConnectivityGetNetWireWidthFuncParams ) )
        )
    (when ( or
            ConnectionsToDelete
            ConnectionsToCreate
            TerminalsToDelete
            TerminalsToCreate
            NetsToDelete
            NetsToCreate
            NetsToChange
            )
      ( list
        ( list
          ConnectionsToDelete
          ConnectionsToCreate )
        ( list
          TerminalsToDelete
          TerminalsToCreate )
        ( list
          NetsToDelete
          NetsToCreate
          NetsToChange
          ) ) ) ) )

(defun UpdateNetlistCompareEmptyCellConnectivity ( ConnectivityInstanceNames
                                                   ConnectivityGetInstanceConnectionsFunc
                                                   ConnectivityGetInstanceConnectionsFuncParams
                                                   ConnectivityTerminalNames
                                                   ConnectivityNetNames )
  (let (
        ( InstanceMap ( NameMakeEmptyInstanceMap ) ) )
    (let (
          ( ConnectionsToCreate ( UpdateNetlistGetConnectionsToCreate
                                  InstanceMap
                                  ConnectivityInstanceNames
                                  ConnectivityGetInstanceConnectionsFunc
                                  ConnectivityGetInstanceConnectionsFuncParams ) ) )
      ( list
        ( list
          nil
          ConnectionsToCreate )
        ( list
          nil
          ConnectivityTerminalNames )
        ( list
          nil
          ConnectivityNetNames ) ) ) ) )
                                

(defun UpdateNetlistFilterInstances ( CellView LibCellExpressionPairsToIgnore )
  ( car
    ( NameFilterInstances
      ( getq CellView instances )
      LibCellExpressionPairsToIgnore ) ) )

(defun UpdateNetlistComparePins ( CompareResult
                                  ConnectivityPinChangeFunc
                                  ConnectivityPinChangeFuncParams
                                  SuppressPins
                                  ForcePins )
  (unless SuppressPins
    ( apply ConnectivityPinChangeFunc
            ( cons
              ( or
                ( UpdateNetlistGetConnectionsToCreateFromCompareResult CompareResult )
                ( UpdateNetlistGetConnectionsToDeleteFromCompareResult CompareResult )
                ( UpdateNetlistGetTerminalsToDeleteFromCompareResult CompareResult )
                ( UpdateNetlistGetTerminalsToCreateFromCompareResult CompareResult )
                ( UpdateNetlistGetNetsToDeleteFromCompareResult CompareResult )
                ( UpdateNetlistGetNetsToCreateFromCompareResult CompareResult ) 
                ( UpdateNetlistGetBoundaryHeightFromCompareResult CompareResult )
                ( UpdateNetlistGetBoundaryWidthFromCompareResult CompareResult )
                ( UpdateNetlistGetBoundaryPositionFromCompareResult CompareResult ) )
              ( cons 
                ForcePins
                ConnectivityPinChangeFuncParams ) ) ) ) )

(defun UpdateNetlistCompareCellView ( CellViewToCheck
                                      LibCellExpressionPairsToIgnore
                                      FoldableLibCellExpressionPairs
                                      InstanceMap
                                      Instances
                                      ConnectivityInstanceExistsFunc 
                                      ConnectivityInstanceExistsFuncParams
                                      ConnectivityGetInstanceMasterLibNameFunc
                                      ConnectivityGetInstanceMasterLibNameFuncParams
                                      ConnectivityGetInstanceMasterCellNameFunc
                                      ConnectivityGetInstanceMasterCellNameFuncParams
                                      ConnectivityInstanceNames
                                      ConnectivityGetInstanceParameterPairsFunc
                                      ConnectivityGetInstanceParameterPairsFuncParams
                                      ConnectivityGetInstanceTransformFunc
                                      ConnectivityGetInstanceTransformFuncParams
                                      ConnectivityInstanceHasConnectionFunc
                                      ConnectivityInstanceHasConnectionFuncParams
                                      ConnectivityGetInstanceConnectionsFunc
                                      ConnectivityGetInstanceConnectionsFuncParams 
                                      ConnectivityTerminalExistsFunc 
                                      ConnectivityTerminalExistsFuncParams 
                                      ConnectivityTerminalNames
                                      ConnectivityNetExistsFunc 
                                      ConnectivityNetExistsFuncParams 
                                      ConnectivityNetNames 
                                      ConnectivityGetNetWireSpacingFunc
                                      ConnectivityGetNetWireSpacingFuncParams
                                      ConnectivityGetNetWireWidthFunc
                                      ConnectivityGetNetWireWidthFuncParams
                                      ConnectivityGetLayerWireSpacingFunc
                                      ConnectivityGetLayerWireSpacingFuncParams
                                      ConnectivityGetLayerWireWidthFunc
                                      ConnectivityGetLayerWireWidthFuncParams
                                      ConnectivityGetGlobalWireSpacingFunc
                                      ConnectivityGetGlobalWireSpacingFuncParams
                                      ConnectivityGetGlobalWireWidthFunc
                                      ConnectivityGetGlobalWireWidthFuncParams
                                      ConnectivityGetCellHeightFunc
                                      ConnectivityGetCellHeightFuncParams
                                      ConnectivityEstimateCellWidthFunc
                                      ConnectivityEstimateCellWidthFuncParams
                                      ConnectivityGetBoundaryPositionFunc
                                      ConnectivityGetBoundaryPositionFuncParams
                                      ConnectivityPinChangeFunc
                                      ConnectivityPinChangeFuncParams
                                      BoundaryLayerLPP
                                      UserUnitsPerMeter
                                      LockBound
                                      SuppressPins
                                      ForcePins )
  (let (
        ( InstancesCompareResult ( UpdateNetlistCompareCellInstances
                                   Instances
                                   InstanceMap
                                   ConnectivityInstanceExistsFunc 
                                   ConnectivityInstanceExistsFuncParams
                                   ConnectivityGetInstanceMasterLibNameFunc
                                   ConnectivityGetInstanceMasterLibNameFuncParams
                                   ConnectivityGetInstanceMasterCellNameFunc
                                   ConnectivityGetInstanceMasterCellNameFuncParams
                                   ConnectivityInstanceNames
                                   ConnectivityGetInstanceParameterPairsFunc
                                   ConnectivityGetInstanceParameterPairsFuncParams
                                   ConnectivityGetInstanceTransformFunc
                                   ConnectivityGetInstanceTransformFuncParams ) )
        ( ConnectivityCompareResult ( UpdateNetlistCompareCellConnectivity
                                      CellViewToCheck
                                      InstanceMap
                                      ConnectivityInstanceNames
                                      ConnectivityInstanceHasConnectionFunc
                                      ConnectivityInstanceHasConnectionFuncParams
                                      ConnectivityGetInstanceConnectionsFunc
                                      ConnectivityGetInstanceConnectionsFuncParams 
                                      ConnectivityTerminalExistsFunc 
                                      ConnectivityTerminalExistsFuncParams 
                                      ConnectivityTerminalNames
                                      ConnectivityNetExistsFunc 
                                      ConnectivityNetExistsFuncParams 
                                      ConnectivityNetNames
                                      ConnectivityGetNetWireSpacingFunc
                                      ConnectivityGetNetWireSpacingFuncParams
                                      ConnectivityGetNetWireWidthFunc
                                      ConnectivityGetNetWireWidthFuncParams )
                                    )
        ( BoundaryCompareResult (when ( and
                                        ( null ConnectivityInstanceNames )
                                        ( equal
                                          ( getq CellViewToCheck cellViewType )
                                          "maskLayout" ) )
                                  ( UpdateNetlistCompareCellBoundary
                                    CellViewToCheck
                                    ConnectivityInstanceNames
                                    BoundaryLayerLPP
                                    UserUnitsPerMeter
                                    ConnectivityGetCellHeightFunc
                                    ConnectivityGetCellHeightFuncParams
                                    ConnectivityEstimateCellWidthFunc
                                    ConnectivityEstimateCellWidthFuncParams
                                    ConnectivityGetBoundaryPositionFunc
                                    ConnectivityGetBoundaryPositionFuncParams
                                    LockBound ) ) ) )
    (let (
          ( PinCompareResult
            ( UpdateNetlistComparePins
              ( list InstancesCompareResult
                     ConnectivityCompareResult 
                     BoundaryCompareResult
                     nil )
              ConnectivityPinChangeFunc
              ConnectivityPinChangeFuncParams
              SuppressPins
              ForcePins ) ) )
      (when ( or
              InstancesCompareResult
              ConnectivityCompareResult 
              BoundaryCompareResult
              PinCompareResult )
        ( list
          InstancesCompareResult
          ConnectivityCompareResult
          BoundaryCompareResult
          PinCompareResult
          ( getq CellViewToCheck viewName ) 
          ( getq CellViewToCheck cellName )
          ) ) ) ) )




(defun UpdateNetlistCompareEmptyCellView ( LibName 
                                           CellName
                                           ViewName
                                           ConnectivityInstanceNames 
                                           ConnectivityGetInstanceMasterLibNameFunc
                                           ConnectivityGetInstanceMasterLibNameFuncParams
                                           ConnectivityGetInstanceMasterCellNameFunc
                                           ConnectivityGetInstanceMasterCellNameFuncParams 
                                           ConnectivityGetInstanceConnectionsFunc
                                           ConnectivityGetInstanceConnectionsFuncParams
                                           ConnectivityTerminalNames
                                           ConnectivityNetNames
                                           ConnectivityGetInstanceParameterPairsFunc
                                           ConnectivityGetInstanceParameterPairsFuncParams
                                           ConnectivityGetInstanceTransformFunc
                                           ConnectivityGetInstanceTransformFuncParams
                                           ConnectivityGetCellHeightFunc
                                           ConnectivityGetCellHeightFuncParams
                                           ConnectivityEstimateCellWidthFunc
                                           ConnectivityEstimateCellWidthFuncParams 
                                           ConnectivityGetBoundaryPositionFunc
                                           ConnectivityGetBoundaryPositionFuncParams
                                           ConnectivityPinChangeFunc
                                           ConnectivityPinChangeFuncParams
                                           NetlistViewName 
                                           SuppressPins
                                           ForcePins )
  (let (
        ( InstancesCompareResult ( UpdateNetlistCompareEmptyCellInstances
                                   ConnectivityInstanceNames
                                   ConnectivityGetInstanceMasterLibNameFunc
                                   ConnectivityGetInstanceMasterLibNameFuncParams
                                   ConnectivityGetInstanceMasterCellNameFunc
                                   ConnectivityGetInstanceMasterCellNameFuncParams
                                   ConnectivityGetInstanceParameterPairsFunc
                                   ConnectivityGetInstanceParameterPairsFuncParams
                                   ConnectivityGetInstanceTransformFunc
                                   ConnectivityGetInstanceTransformFuncParams ) )
        ( ConnectivityCompareResult ( UpdateNetlistCompareEmptyCellConnectivity
                                      ConnectivityInstanceNames
                                      ConnectivityGetInstanceConnectionsFunc
                                      ConnectivityGetInstanceConnectionsFuncParams
                                      ConnectivityTerminalNames
                                      ConnectivityNetNames ) )
        ( BoundaryCompareResult (unless ( and NetlistViewName ( equal ViewName NetlistViewName ) )
                                  ( UpdateNetlistCompareEmptyCellBoundary
                                    ConnectivityInstanceNames
                                    LibName 
                                    CellName
                                    ViewName
                                    ConnectivityGetCellHeightFunc
                                    ConnectivityGetCellHeightFuncParams
                                    ConnectivityEstimateCellWidthFunc
                                    ConnectivityEstimateCellWidthFuncParams 
                                    ConnectivityGetBoundaryPositionFunc
                                    ConnectivityGetBoundaryPositionFuncParams ) ) ) )
    (let (
          ( PinCompareResult
            ( UpdateNetlistComparePins
              ( list InstancesCompareResult
                     ConnectivityCompareResult 
                     BoundaryCompareResult
                     nil )
              ConnectivityPinChangeFunc
              ConnectivityPinChangeFuncParams
              SuppressPins
              ForcePins ) ) )
      (when ( or
              InstancesCompareResult
              ConnectivityCompareResult 
              BoundaryCompareResult )
        ( list
          InstancesCompareResult
          ConnectivityCompareResult
          BoundaryCompareResult
          PinCompareResult
          ViewName
          CellName ) ) ) ) )


(defun UpdateNetlistGetViewNameFromCompareResult ( CompareResult )
  ( nth 4 CompareResult ) )

(defun UpdateNetlistGetCellNameFromCompareResult ( CompareResult )
  ( nth 5 CompareResult ) )

(defun UpdateNetlistGetInstancesToDeleteFromCompareResult ( CompareResult )
  ( caar CompareResult ) )

(defun UpdateNetlistGetInstancesToChangeFromCompareResult ( CompareResult )
  ( cadar CompareResult ) )

(defun UpdateNetlistGetInstancesToCreateFromCompareResult ( CompareResult )
  ( caddar CompareResult ) )

(defun UpdateNetlistGetInstancesToFixFromCompareResult ( CompareResult )
  ( nth 3 ( car CompareResult ) ) )

(defun UpdateNetlistGetParametersToSetFromCompareResult ( CompareResult )
  ( nth 4 ( car CompareResult ) ) )

(defun UpdateNetlistGetBoundaryShapeFromCompareResult ( CompareResult ) 
  ( nth 3 ( caddr CompareResult ) ) )

(defun UpdateNetlistGetBoundaryHeightFromCompareResult ( CompareResult )
  ( car ( car ( caddr CompareResult ) ) ) )

(defun UpdateNetlistGetOldBoundaryHeightFromCompareResult ( CompareResult )
  ( cadr ( car ( caddr CompareResult ) ) ) )

(defun UpdateNetlistGetBoundaryHeightWarningFromCompareResult ( CompareResult )
  ( caddr ( car ( caddr CompareResult ) ) ) )

(defun UpdateNetlistGetBoundaryWidthFromCompareResult ( CompareResult )
  ( car ( cadr ( caddr CompareResult ) ) ) )

(defun UpdateNetlistGetOldBoundaryWidthFromCompareResult ( CompareResult )
  ( cadr ( cadr ( caddr CompareResult ) ) ) )

(defun UpdateNetlistGetBoundaryWidthWarningFromCompareResult ( CompareResult )
  ( caddr ( cadr ( caddr CompareResult ) ) ) )

(defun UpdateNetlistGetBoundaryPositionFromCompareResult ( CompareResult )
  ( car ( nth 2 ( caddr CompareResult ) ) ) )

(defun UpdateNetlistGetOldBoundaryPositionFromCompareResult ( CompareResult )
  ( cadr ( nth 2 ( caddr CompareResult ) ) ) )

(defun UpdateNetlistGetBoundaryPositionWarningFromCompareResult ( CompareResult )
  ( caddr ( nth 2 ( caddr CompareResult ) ) ) )

(defun UpdateNetlistGetConnectionsToDeleteFromCompareResult ( CompareResult )
  ( caaadr CompareResult ) )

(defun UpdateNetlistGetConnectionsToCreateFromCompareResult ( CompareResult )
  ( car ( cdr ( car ( car ( cdr CompareResult ) ) ) ) ) )

(defun UpdateNetlistGetTerminalsToDeleteFromCompareResult ( CompareResult )
  ( car ( car ( cdr ( car ( cdr CompareResult ) ) ) ) ) )

(defun UpdateNetlistGetTerminalsToCreateFromCompareResult ( CompareResult )
  ( car ( cdr ( car ( cdr ( car ( cdr CompareResult ) ) ) ) ) ) )

(defun UpdateNetlistGetNetsToDeleteFromCompareResult ( CompareResult )
  ( car ( car ( cdr ( cdr ( car ( cdr CompareResult ) ) ) ) ) ) )

(defun UpdateNetlistGetNetsToCreateFromCompareResult ( CompareResult )
  ( cadr ( car ( cdr ( cdr ( car ( cdr CompareResult ) ) ) ) ) ) )

(defun UpdateNetlistGetNetsToChangeFromCompareResult ( CompareResult )
  ( caddr ( car ( cdr ( cdr ( car ( cdr CompareResult ) ) ) ) ) ) )

(defun UpdateNetlistGetPinChangeFromCompareResult ( CompareResult )
  ( cadddr CompareResult ) )

(defun UpdateNetlistGetPinsFileFromCompareResult ( CompareResult )
  ( car ( cadddr CompareResult ) ) )

(defun UpdateNetlistGetWirePitchFromCompareResult ( CompareResult )
  ( cadr ( cadddr CompareResult ) ) )

(defun UpdateNetlistGetWireWidthFromCompareResult ( CompareResult )
  ( caddr ( cadddr CompareResult ) ) )

(defun UpdateNetlistGetWireSpacingFromCompareResult ( CompareResult )
  ( cadddr ( cadddr CompareResult ) ) )

(defun UpdateNetlistGetPinLPPFromCompareResult ( CompareResult )
  ( car ( cddddr ( cadddr CompareResult ) ) ) )


(defun UpdateNetlistGetInstanceNameFromInstanceToCreate ( InstanceToCreate )
  ( car InstanceToCreate ) )

(defun UpdateNetlistGetLibNameFromInstanceToCreate ( InstanceToCreate )
  ( cadr InstanceToCreate ) )

(defun UpdateNetlistGetCellNameFromInstanceToCreate ( InstanceToCreate )
  ( caddr InstanceToCreate ) )

(defun UpdateNetlistGetInstanceTransformFromInstanceToCreate ( InstanceToCreate )
  ( nth 3 InstanceToCreate ) )

(defun UpdateNetlistGetInstanceNameFromInstanceToChange ( InstanceToChange )
  ( car InstanceToChange ) )

(defun UpdateNetlistGetLibNameFromInstanceToChange ( InstanceToChange )
  ( cadr InstanceToChange ) )

(defun UpdateNetlistGetCellNameFromInstanceToChange ( InstanceToChange )
  ( caddr InstanceToChange ) )

(defun UpdateNetlistGetInstanceTransformFromInstanceToChange ( InstanceToChange )
  ( nth 4 InstanceToChange ) )

(defun UpdateNetlistGetInstanceCoincidencesFromInstanceToChange ( InstanceToChange )
  ( nth 5 InstanceToChange ) )

(defun UpdateNetlistGetInstanceNameFromParameterToSet ( ParameterToSet )
  ( car ParameterToSet ) )

(defun UpdateNetlistGetParameterNameFromParameterToSet ( ParameterToSet )
  ( cadr ParameterToSet ) )

(defun UpdateNetlistGetParameterValueFromParameterToSet ( ParameterToSet )
  ( caddr ParameterToSet ) )

(defun UpdateNetlistGetInstanceDBObjFromInstanceToChange ( InstanceToChange )
  ( cadddr InstanceToChange ) )

(defun UpdateNetlistGetInstanceNameFromConnectionToCreate ( ConnectionToCreate )
  ( car ConnectionToCreate ) )

(defun UpdateNetlistGetTerminalNameFromConnectionToCreate ( ConnectionToCreate )
  ( cadr ConnectionToCreate ) )

(defun UpdateNetlistGetNetNameFromConnectionToCreate ( ConnectionToCreate )
  ( caddr ConnectionToCreate ) )


(defun UpdateNetlistGetErrorsFromCompareResult ( CompareResult )
  
  (let (
        ( CellName ( UpdateNetlistGetCellNameFromCompareResult CompareResult ) )
        ( ViewName ( UpdateNetlistGetViewNameFromCompareResult CompareResult ) )
        ( InstancesToFix ( UpdateNetlistGetInstancesToFixFromCompareResult CompareResult ) )
        ( InstancesToChange ( UpdateNetlistGetInstancesToChangeFromCompareResult CompareResult ) )
        ( BoundaryShape ( UpdateNetlistGetBoundaryShapeFromCompareResult CompareResult ) )
        ( BoundaryHeight ( UpdateNetlistGetBoundaryHeightFromCompareResult CompareResult ) )
        ( BoundaryWidth ( UpdateNetlistGetBoundaryWidthFromCompareResult CompareResult ) )
        ( BoundaryPosition ( UpdateNetlistGetBoundaryPositionFromCompareResult CompareResult ) )
        ( OldBoundaryHeight ( UpdateNetlistGetOldBoundaryHeightFromCompareResult CompareResult ) )
        ( OldBoundaryPosition ( UpdateNetlistGetOldBoundaryPositionFromCompareResult CompareResult ) ) )

    (let (
          ( Ret
            ( ListApplyFuncToListAndAccumulateResults
              InstancesToFix
              (lambda
                ( InstanceToFix
                  CellName
                  ViewName )
                (let (
                      ( InstCellName ( getq InstanceToFix cellName ) )
                      ( InstLibName ( getq InstanceToFix libName ) )
                      ( InstViewName ( getq InstanceToFix viewName ) ) )
                  ( sprintf
                    nil
                    "\"%s\" in library \"%s\" does not have a view \"%s\" which is referenced by instance \"%s\" in \"%s\" \"%s\"."
                    InstCellName
                    InstLibName
                    InstViewName 
                    ( getq InstanceToFix name )
                    CellName
                    ViewName ) ) )
              ( list CellName ViewName ) ) ) )
        
      (when BoundaryShape
        (unless ( and BoundaryHeight BoundaryWidth BoundaryPosition )  
          ( setq
            Ret
            ( cons
              ( sprintf
                nil
                "\"%s\" \"%s\" Boundary needs to change, but either the height( %L ), the width( %L ) or position( %L ) is nil."
                CellName
                ViewName
                BoundaryHeight
                BoundaryWidth
                BoundaryPosition )
              Ret ) ) ) )
      (when BoundaryHeight
        (unless ( and OldBoundaryHeight BoundaryShape )
          (when OldBoundaryHeight
            ( setq
              Ret
              ( cons
                ( sprintf
                  nil
                  "HEIGHT \"%s\" \"%s\" directive specifies %.9e non-empty cell has height of %.9e."
                  CellName
                  ViewName
                  BoundaryHeight
                  OldBoundaryHeight )
                Ret ) ) ) ) )
      ( setq 
        Ret
        ( append 
          Ret
          ( ListApplyFuncToListAndAccumulateNonNilResults
            InstancesToChange
            (lambda ( InstanceToChange )
              (let (                                
                    ( InstName 
                      ( UpdateNetlistGetInstanceNameFromInstanceToChange InstanceToChange ) )
                    ( InstCellName 
                      ( UpdateNetlistGetCellNameFromInstanceToChange InstanceToChange ) )
                    ( InstLibName 
                      ( UpdateNetlistGetLibNameFromInstanceToChange InstanceToChange ) ) 
                    ( InstCoincidences 
                      ( UpdateNetlistGetInstanceCoincidencesFromInstanceToChange 
                        InstanceToChange ) ) )
                (when ( and ( cadr ( car InstCoincidences ) )
                            ( greaterp ( length ( cadr InstCoincidences ) )
                                       1 ) )
                  ( sprintf
                    nil
                    "COINCIDENCE Instance \"%s\" of type \"%s\" in library \"%s\" is coincident with another instance at %L\n"
                    InstName
                    InstCellName
                    InstLibName
                    ( cadr ( car InstCoincidences ) )
                    ) ) ) )
            nil
            ) ) )
      Ret ) ) )
          
        

(defun UpdateNetlistPrintDifferences ( CompareResult
                                       OutputPort )
  (let (
        ( InstancePrefix "INSTANCE:" )
        ( NetPrefix "NET:" )
        ( ParamPrefix "PARAM:" )
        ( ErrorPrefix "ERROR:" )
        ( BoundaryPrefix "BOUNDARY:" )
        ( CellsToCreateTable ( makeTable "foo" nil ) )
        ( CellName ( UpdateNetlistGetCellNameFromCompareResult CompareResult ) )
        ( ViewName ( UpdateNetlistGetViewNameFromCompareResult CompareResult ) )
        ( InstancesToDelete ( UpdateNetlistGetInstancesToDeleteFromCompareResult CompareResult ) )
        ( InstancesToChange ( UpdateNetlistGetInstancesToChangeFromCompareResult CompareResult ) )
        ( InstancesToCreate ( UpdateNetlistGetInstancesToCreateFromCompareResult CompareResult ) )
        ( ParametersToSet ( UpdateNetlistGetParametersToSetFromCompareResult CompareResult ) )
        ( ConnectionsToDelete ( UpdateNetlistGetConnectionsToDeleteFromCompareResult CompareResult ) )
        ( ConnectionsToCreate ( UpdateNetlistGetConnectionsToCreateFromCompareResult CompareResult ) )
        ( TerminalsToDelete ( UpdateNetlistGetTerminalsToDeleteFromCompareResult CompareResult ) )
        ( TerminalsToCreate ( UpdateNetlistGetTerminalsToCreateFromCompareResult CompareResult ) )
        ( NetsToDelete ( UpdateNetlistGetNetsToDeleteFromCompareResult CompareResult ) )
        ( NetsToCreate ( UpdateNetlistGetNetsToCreateFromCompareResult CompareResult ) ) 
        ( NetsToChange ( UpdateNetlistGetNetsToChangeFromCompareResult CompareResult ) ) 
        ( BoundaryShape ( UpdateNetlistGetBoundaryShapeFromCompareResult CompareResult ) )
        ( BoundaryHeight ( UpdateNetlistGetBoundaryHeightFromCompareResult CompareResult ) )
        ( BoundaryWidth ( UpdateNetlistGetBoundaryWidthFromCompareResult CompareResult ) )
        ( BoundaryWidthWarning ( UpdateNetlistGetBoundaryWidthWarningFromCompareResult CompareResult ) )
        ( BoundaryPosition ( UpdateNetlistGetBoundaryPositionFromCompareResult CompareResult ) )
        ( BoundaryPositionWarning ( UpdateNetlistGetBoundaryPositionWarningFromCompareResult CompareResult ) )
        ( OldBoundaryHeight ( UpdateNetlistGetOldBoundaryHeightFromCompareResult CompareResult ) )
        ( OldBoundaryWidth ( UpdateNetlistGetOldBoundaryWidthFromCompareResult CompareResult ) )
        ( OldBoundaryPosition ( UpdateNetlistGetOldBoundaryPositionFromCompareResult CompareResult ) ) 
        ( BoundaryHeightWarning ( UpdateNetlistGetBoundaryHeightWarningFromCompareResult CompareResult ) ) 
        ( PinChange ( UpdateNetlistGetPinChangeFromCompareResult CompareResult ) ) 
        ( ErrorList ( UpdateNetlistGetErrorsFromCompareResult CompareResult ) ) )

    ( foreach 
      InstanceToDelete
      InstancesToDelete
      ( fprintf 
        OutputPort 
        "%L Delete Instance \"%L\" of type \"%L\" in library \"%L\".\n"
        InstancePrefix
        ( getq InstanceToDelete name )
        ( getq InstanceToDelete cellName )
        ( getq InstanceToDelete libName ) ) )

    ( foreach
      InstanceToChange
      InstancesToChange
      (let (
            ( InstName ( UpdateNetlistGetInstanceNameFromInstanceToChange InstanceToChange ) )
            ( InstCellName ( UpdateNetlistGetCellNameFromInstanceToChange InstanceToChange ) )
            ( InstLibName ( UpdateNetlistGetLibNameFromInstanceToChange InstanceToChange ) ) 
            ( InstToChangeDBObj ( UpdateNetlistGetInstanceDBObjFromInstanceToChange InstanceToChange ) )
            ( InstTransform ( UpdateNetlistGetInstanceTransformFromInstanceToChange InstanceToChange ) )
            )
        ( fprintf
          OutputPort
          "%L Change Instance \"%L\" of type \"%L\" in library \"%L\" to \"%L\" of type \"%L\" in library \"%L\".\n"
          InstancePrefix
          ( getq InstToChangeDBObj name ) 
          ( getq InstToChangeDBObj cellName )
          ( getq InstToChangeDBObj libName )
          InstName
          InstCellName
          InstLibName )
        (when InstTransform
          ( fprintf
            OutputPort
            "%s Move Instance \"%s\" from %L to %L.\n"
            InstancePrefix
            InstName
            ( getq InstToChangeDBObj transform )
            InstTransform ) )
        ) )
            
    ( foreach
      InstanceToCreate
      InstancesToCreate
      (let (
            ( InstName ( UpdateNetlistGetInstanceNameFromInstanceToCreate InstanceToCreate ) )
            ( InstLibName ( UpdateNetlistGetLibNameFromInstanceToCreate InstanceToCreate ) )
            ( InstCellName ( UpdateNetlistGetCellNameFromInstanceToCreate InstanceToCreate ) )
            ( InstTransform ( UpdateNetlistGetInstanceTransformFromInstanceToCreate InstanceToCreate ) ) )
        (if InstTransform
            ( fprintf
              OutputPort
              "%s Create Instance \"%s\" of type \"%s\" in library \"%s\" at %L.\n"
              InstancePrefix
              InstName 
              InstCellName
              InstLibName
              InstTransform )
          ( fprintf
            OutputPort
            "%s Create Instance \"%s\" of type \"%s\" in library \"%s\".\n"
            InstancePrefix
            InstName 
            InstCellName 
            InstLibName ) ) ) )

    
    ( foreach
      ParameterToSet
      ParametersToSet
      (let (
            ( InstanceName ( UpdateNetlistGetInstanceNameFromParameterToSet ParameterToSet ) )
            ( ParamName ( UpdateNetlistGetParameterNameFromParameterToSet ParameterToSet ) )
            ( ParamValue ( UpdateNetlistGetParameterValueFromParameterToSet ParameterToSet ) ) )
        ( fprintf
          OutputPort
          "%s Set \"%s\" to \"%e\" on instance \"%s\".\n"
          ParamPrefix
          ParamName
          ParamValue
          InstanceName ) ) )
    ( foreach
      ConnectionToDelete
      ConnectionsToDelete
      ( fprintf
        OutputPort
        "%s Delete Connection of \"%s\" to \"%s.%s\".\n"
        NetPrefix
        ( getq ( getq ConnectionToDelete net ) name )
        ( getq ( getq ConnectionToDelete inst ) name )
        ( getq ConnectionToDelete name ) ) )
    ( foreach
      ConnectionToCreate
      ConnectionsToCreate
      (let (
            ( ConnectionInstanceName ( UpdateNetlistGetInstanceNameFromConnectionToCreate
                                       ConnectionToCreate ) )
            ( ConnectionTerminalName ( UpdateNetlistGetTerminalNameFromConnectionToCreate
                                       ConnectionToCreate ) )
            ( ConnectionNetName ( UpdateNetlistGetNetNameFromConnectionToCreate
                                  ConnectionToCreate ) ) )
        ( fprintf
          OutputPort
          "%s Create Connection of \"%s\" to \"%s.%s\".\n"
          NetPrefix
          ConnectionNetName
          ConnectionInstanceName
          ConnectionTerminalName ) ) )
    ( foreach
      TerminalToDelete
      TerminalsToDelete
      ( fprintf
        OutputPort
        "%s Delete Terminal \"%L\".\n"
        NetPrefix
        ( getq TerminalToDelete name ) ) )
    ( foreach
      TerminalToCreate
      TerminalsToCreate
      ( fprintf
        OutputPort
        "%s Create Terminal \"%s\".\n"
        NetPrefix
        TerminalToCreate ) )
    ( foreach
      NetToDelete
      NetsToDelete
      ( fprintf
        OutputPort
        "%s Delete Net \"%L\".\n"
        NetPrefix
        ( getq NetToDelete name ) ) )
    ( foreach
      NetToCreate
      NetsToCreate
      ( fprintf
        OutputPort
        "%s Create Net \"%s\".\n"
        NetPrefix
        NetToCreate ) )
    ( foreach
      NetToChangeSpec
      NetsToChange
      (let (
            ( NetToChange ( car NetToChangeSpec ) )
            ( WireSpacing ( cadr NetToChangeSpec ) )
            ( WireWidth ( caddr NetToChangeSpec ) )
            )
        (when WireSpacing
          ( fprintf
            OutputPort
            "%s Change Net \"%s\" wire spacing to %g meters.\n"
            NetPrefix
            NetToChange
            WireSpacing ) )
        (when WireWidth
          ( fprintf
            OutputPort
            "%s Change Net \"%s\" wire width to %g meters.\n"
            NetPrefix
            NetToChange
            WireWidth ) )
        ) )

    (when BoundaryShape
      (when ( and BoundaryHeight BoundaryWidth BoundaryPosition )
        ( fprintf
          OutputPort
          "%s CHANGE.\n"
          BoundaryPrefix ) ) )

    (when BoundaryHeight
      (if ( and OldBoundaryHeight BoundaryShape )
          ( fprintf
            OutputPort
            "%s CHANGE HEIGHT from %.9e to %.9e.\n"
            BoundaryPrefix
            OldBoundaryHeight
            BoundaryHeight )
        (unless OldBoundaryHeight
          ( fprintf
            OutputPort
            "%s HEIGHT will be %.9e.\n" 
            BoundaryPrefix
            BoundaryHeight ) ) ) )

    (when BoundaryHeightWarning
      ( fprintf
        OutputPort
        "%s HEIGHT WARNING %s\n"
        BoundaryPrefix
        BoundaryHeightWarning ) )

    (when BoundaryWidth
      (if ( and OldBoundaryWidth BoundaryShape )
          ( fprintf
            OutputPort
            "%s CHANGE WIDTH from %.9e to %.9e.\n"
            BoundaryPrefix
            OldBoundaryWidth
            BoundaryWidth )
        (unless BoundaryShape
          ( fprintf
            OutputPort
            "%s WIDTH will be %.9e.\n" 
            BoundaryPrefix
            BoundaryWidth ) ) ) )
    (when BoundaryWidthWarning
      ( fprintf
        OutputPort
        "%s WIDTH WARNING %s\n"
        BoundaryPrefix
        BoundaryWidthWarning ) )
    (when BoundaryPosition
      (if ( and OldBoundaryPosition BoundaryShape )
          ( fprintf
            OutputPort
            "%s CHANGE POSITION from %L to %L.\n"
            BoundaryPrefix
            OldBoundaryPosition
            BoundaryPosition )
        (unless BoundaryShape
          ( fprintf
            OutputPort
            "%s POSITION will be %L.\n"
            BoundaryPrefix
            BoundaryPosition ) ) ) )
    (when BoundaryPositionWarning
      ( fprintf
        OutputPort
        "%s POSITION WARNING %L\n"
        BoundaryPrefix
        BoundaryPositionWarning ) )

    (when PinChange
      ( fprintf
        OutputPort
        "PINS: will change\n" ) )
    
    ( foreach
      ErrorStr
      ErrorList
      ( fprintf
        OutputPort
        "%s %s\n"
        ErrorPrefix
        ErrorStr ) ) ) )


(defun UpdateNetlistGetInstanceNamesFromCellView ( CellView )
  ( ListApplyFuncToListAndAccumulateResults
    ( getq CellView instances )
    (lambda
      ( Instance )
      ( getq Instance name ) )
    nil ) )

(defun UpdateNetlistCompareCellViews ( LibName 
                                       CellName
                                       LibCellExpressionPairsToIgnore
                                       FoldableLibCellExpressionPairs
                                       RequiredViews
                                       ViewsToUpdate
                                       UpdateOnlyViews
                                       NetlistViewName
                                       ConnectivityInstanceExistsFunc 
                                       ConnectivityInstanceExistsFuncParams
                                       ConnectivityGetInstanceMasterLibNameFunc
                                       ConnectivityGetInstanceMasterLibNameFuncParams
                                       ConnectivityGetInstanceMasterCellNameFunc
                                       ConnectivityGetInstanceMasterCellNameFuncParams
                                       ConnectivityGetInstanceTripplesFunc
                                       ConnectivityGetInstanceTripplesFuncParams
                                       ConnectivityGetInstanceParameterPairsFunc
                                       ConnectivityGetInstanceParameterPairsFuncParams
                                       ConnectivityGetInstanceTransformFunc
                                       ConnectivityGetInstanceTransformFuncParams
                                       ConnectivityInstanceHasConnectionFunc
                                       ConnectivityInstanceHasConnectionFuncParams
                                       ConnectivityGetInstanceConnectionsFunc
                                       ConnectivityGetInstanceConnectionsFuncParams 
                                       ConnectivityTerminalExistsFunc 
                                       ConnectivityTerminalExistsFuncParams 
                                       ConnectivityGetTerminalNamesFunc
                                       ConnectivityGetTerminalNamesFuncParams
                                       ConnectivityNetExistsFunc 
                                       ConnectivityNetExistsFuncParams 
                                       ConnectivityGetNetNamesFunc
                                       ConnectivityGetNetNamesFuncParams
                                       ConnectivityGetNetWireSpacingFunc
                                       ConnectivityGetNetWireSpacingFuncParams
                                       ConnectivityGetNetWireWidthFunc
                                       ConnectivityGetNetWireWidthFuncParams
                                       ConnectivityGetLayerWireSpacingFunc
                                       ConnectivityGetLayerWireSpacingFuncParams
                                       ConnectivityGetLayerWireWidthFunc
                                       ConnectivityGetLayerWireWidthFuncParams
                                       ConnectivityGetGlobalWireSpacingFunc
                                       ConnectivityGetGlobalWireSpacingFuncParams
                                       ConnectivityGetGlobalWireWidthFunc
                                       ConnectivityGetGlobalWireWidthFuncParams
                                       ConnectivityGetCellHeightFunc
                                       ConnectivityGetCellHeightFuncParams
                                       ConnectivityEstimateCellWidthFunc
                                       ConnectivityEstimateCellWidthFuncParams
                                       ConnectivityGetBoundaryPositionFunc
                                       ConnectivityGetBoundaryPositionFuncParams
                                       ConnectivityPinChangeFunc
                                       ConnectivityPinChangeFuncParams
                                       BoundaryLayerLPP
                                       UserUnitsPerMeter
                                       LockBound
                                       SuppressPins
                                       ForcePins )
  (let (
        ( ViewInfoTable ( makeTable "viewInfoTable" nil ) )
        ( Ret nil ) )

    ( foreach
      ViewName
      RequiredViews
      (let (
            ( ViewObj ( ddGetObj LibName CellName ViewName ) )
            ( InstanceMap ( NameMakeEmptyInstanceMap ) ) )
        (let (
              ( ConnectivityInstanceTripples ( apply
                                               ConnectivityGetInstanceTripplesFunc
                                               ( cons
                                                 LibName
                                                 ( cons
                                                   CellName
                                                   ( cons
                                                     ViewName
                                                     ConnectivityGetInstanceTripplesFuncParams ) ) ) ) )
              ( MyLibCellExpressionPairsToIgnore (if ( and 
                                                       NetlistViewName 
                                                       ( equal ViewName NetlistViewName ) )
                                                     ( list )
                                                   LibCellExpressionPairsToIgnore ) ) )
          (let (
                ( FilteredInstanceNames ( ListApplyFuncToListAndAccumulateResults
                                          ( car
                                            ( NameFilterInstanceTripples
                                              ConnectivityInstanceTripples
                                              LibCellExpressionPairsToIgnore ) )
                                          (lambda
                                            ( InstanceTripple )
                                            ( car InstanceTripple ) )
                                          nil ) )
                ( InstanceNames ( ListApplyFuncToListAndAccumulateResults
                                  ConnectivityInstanceTripples
                                  (lambda
                                    ( InstanceTripple )
                                    ( car InstanceTripple ) )
                                  nil ) )
                ( ConnectivityTerminalNames ( apply
                                              ConnectivityGetTerminalNamesFunc
                                              ( cons
                                                LibName
                                                ( cons
                                                  CellName
                                                  ( cons
                                                    ViewName
                                                    ConnectivityGetTerminalNamesFuncParams ) ) ) ) )
                ( ConnectivityNetNames ( apply
                                         ConnectivityGetNetNamesFunc
                                         ( cons
                                           LibName
                                           ( cons
                                             CellName
                                             ( cons
                                               ViewName
                                               ConnectivityGetNetNamesFuncParams ) ) ) ) ) )
            (let (
                  ( ConnectivityInstanceNames (if ( and 
                                                    NetlistViewName 
                                                    ( equal ViewName NetlistViewName ) )
                                                  InstanceNames
                                                FilteredInstanceNames ) )
                  ( CellView (when ( and
                                     ( car ( getq ViewObj files ) )
                                     ( ddIsObjReadable ( car ( getq ViewObj files ) ) ) )
                               ( dbOpenCellViewByType
                                 LibName
                                 CellName
                                 ViewName
                                 nil
                                 "r" ) ) ) )
              (let (
                    ( Instances (when CellView
                                  ( UpdateNetlistFilterInstances
                                    CellView
                                    MyLibCellExpressionPairsToIgnore ) ) ) )
                ( NamePopulateInstanceMapWithInstances
                  InstanceMap
                  Instances
                  FoldableLibCellExpressionPairs )
                                       
                ( setarray
                  ViewInfoTable
                  ViewName
                  ( list
                    CellView
                    MyLibCellExpressionPairsToIgnore
                    InstanceMap
                    Instances
                    ConnectivityInstanceNames
                    ConnectivityTerminalNames
                    ConnectivityNetNames ) )
                ) ) ) ) ) )


    ( foreach
      ViewName
      ViewsToUpdate
      (let (
            ( ViewInfo ( arrayref ViewInfoTable ViewName ) ) )
        (let (
              ( CellView ( car ViewInfo ) )
              ( MyLibCellExpressionPairsToIgnore ( cadr ViewInfo ) )
              ( InstanceMap ( caddr ViewInfo ) )
              ( Instances ( nth 3 ViewInfo ) )
              ( ConnectivityInstanceNames ( nth 4 ViewInfo ) )
              ( ConnectivityTerminalNames ( nth 5 ViewInfo ) )
              ( ConnectivityNetNames ( nth 6 ViewInfo ) ) )
          ( printf "Comparing %L %L\n" CellName ViewName )
          ( setq
            Ret
            ( tconc
              Ret
              ( list
                ViewName
                (if CellView
                  ( SyncNetlistCompareCellView
                    CellView
                    MyLibCellExpressionPairsToIgnore
                    FoldableLibCellExpressionPairs
                    InstanceMap
                    Instances
                    BoundaryLayerLPP
                    UserUnitsPerMeter
                    LockBound
                    SuppressPins
                    ForcePins )
                  (when ( forall
                          UpdateOnlyView
                          UpdateOnlyViews
                          ( not ( equal ViewName UpdateOnlyView ) ) )
                    ( UpdateNetlistCompareEmptyCellView
                      LibName
                      CellName
                      ViewName
                      ConnectivityInstanceNames
                      ConnectivityGetInstanceMasterLibNameFunc
                      ( cons ViewName ConnectivityGetInstanceMasterLibNameFuncParams )
                      ConnectivityGetInstanceMasterCellNameFunc
                      ( cons ViewName ConnectivityGetInstanceMasterCellNameFuncParams ) 
                      ConnectivityGetInstanceConnectionsFunc
                      ( cons ViewName ConnectivityGetInstanceConnectionsFuncParams )
                      ConnectivityTerminalNames
                      ConnectivityNetNames
                      ConnectivityGetInstanceParameterPairsFunc
                      ( cons ViewName ConnectivityGetInstanceParameterPairsFuncParams )
                      ConnectivityGetInstanceTransformFunc
                      ( cons 
                        ViewName 
                        ( cons ViewInfoTable ConnectivityGetInstanceTransformFuncParams ) )
                      ConnectivityGetCellHeightFunc
                      ConnectivityGetCellHeightFuncParams
                      ConnectivityEstimateCellWidthFunc
                      ConnectivityEstimateCellWidthFuncParams 
                      ConnectivityGetBoundaryPositionFunc
                      ConnectivityGetBoundaryPositionFuncParams
                      ConnectivityPinChangeFunc
                      ConnectivityPinChangeFuncParams
                      NetlistViewName
                      SuppressPins
                      ForcePins
                      ) ) )
                InstanceMap ) ) ) ) ) ) 
    ( list ( list LibName CellName ) ( car Ret ) ) ) )

(defun UpdateNetlistGetSkillNetlistInstanceExistsFunc ( )
  (lambda
    ( InstanceName ViewName NetlistsTablesTable )
    ( not
      ( null
        ( NetlistTableGetInstance 
          ( arrayref NetlistsTablesTable ViewName ) 
          InstanceName ) ) ) ) )

defun( UpdateNetlistGetSkillNetlistInstanceExists ( InstanceName ViewName NetlistsTablesTable )
  NetlistsTablesTable[ViewName]["i"][InstanceName] != nil
)


(defun UpdateNetlistGetSkillNetlistGetInstanceMasterLibNameFunc ( )
  (lambda
    ( InstanceName ViewName NetlistsTablesTable )
    ( NetlistTableGetInstanceInNetlistMasterLibName
      ( arrayref NetlistsTablesTable ViewName )
      InstanceName ) ) )

defun( UpdateNetlistGetSkillNetlistGetInstanceMasterLibName ( InstanceName ViewName NetlistsTablesTable )
  NetlistTableGetInstanceInNetlistMasterLibName( NetlistsTablesTable[ViewName] InstanceName ) 
) 

(defun UpdateNetlistGetSkillNetlistGetInstanceMasterCellNameFunc ( )
  (lambda
    ( InstanceName ViewName NetlistsTablesTable )
    ( NetlistTableGetInstanceInNetlistMasterCellName
      ( arrayref NetlistsTablesTable ViewName )
      InstanceName ) ) )

defun( UpdateNetlistGetSkillNetlistGetInstanceMasterCellName ( InstanceName ViewName NetlistsTablesTable )
  NetlistTableGetInstanceInNetlistMasterCellName( NetlistsTablesTable[ViewName] InstanceName ) 
) 

(defun UpdateNetlistGetSkillNetlistGetInstanceTripplesFunc ()
  (lambda
    ( LibName CellName ViewName NetlistsTablesTable )
    ( NetlistTableGetInstanceTripples
      ( arrayref NetlistsTablesTable ViewName ) ) ) )

defun( UpdateNetlistGetSkillNetlistGetInstanceTripples ( LibName CellName ViewName NetlistsTablesTable )
  NetlistTableGetInstanceTripples( NetlistsTablesTable[ViewName] ) 
)  

(defun UpdateNetlistGetSkillNetlistGetInstanceParameterPairsFunc ( )
  (lambda
    ( InstanceName ViewName NetlistsTablesTable )
    ( NetlistTableGetInstanceInNetlistParametersPairs
      ( arrayref NetlistsTablesTable ViewName )
      InstanceName ) ) )

(defun UpdateNetlistGetSkillNetlistInstanceHasConnectionFunc ( )
  (lambda
    ( InstanceName TerminalName NetName ViewName NetlistsTablesTable )
    (let (
          ( ConnectivityNetName ( NetlistTableGetNetNameForInstanceInNetlistTerminalName
                                  ( arrayref NetlistsTablesTable ViewName )
                                  InstanceName
                                  TerminalName ) ) )
      ( and
        ConnectivityNetName
        ( equal ConnectivityNetName NetName ) ) ) ) )

(defun UpdateNetlistGetSkillNetlistGetInstanceConnectionsFunc ( )
  (lambda
    ( InstanceName ViewName NetlistsTablesTable )
    ( NetlistTableGetInstanceInNetlistConnectionPairs
      ( arrayref NetlistsTablesTable ViewName )
      InstanceName ) ) )

(defun UpdateNetlistGetSkillNetlistTerminalExistsFunc ( )
  (lambda
    ( TerminalName ViewName NetlistsTablesTable )
    ( NetlistTableTerminalExists
      ( arrayref NetlistsTablesTable ViewName )
      TerminalName ) ) )

(defun UpdateNetlistGetSkillNetlistGetTerminalNamesFunc ( )
  (lambda
    ( LibName CellName ViewName NetlistsTablesTable )
    ( NetlistTableGetTerminalNames
      ( arrayref NetlistsTablesTable ViewName ) ) ) )

(defun UpdateNetlistGetSkillNetlistNetExistsFunc ( )
  (lambda
    ( NetName ViewName NetlistsTablesTable )
    ( NetlistTableNetExists
      ( arrayref NetlistsTablesTable ViewName )
      NetName ) ) )

(defun UpdateNetlistGetSkillGetNetlistNetNamesFunc ( )
  (lambda
    ( LibName CellName ViewName NetlistsTablesTable )
    ( NetlistTableGetNetNames
      ( arrayref NetlistsTablesTable ViewName ) ) ) )

(defun UpdateNetListGetSkillGetNetlistNetWireSpacingFunc ()
  (lambda ( NetName
            DirectiveTable
            DirectiveUnitsPerMeter )
    ( CellInfoGetNetWireSpacingInMeters
      DirectiveTable
      NetName
      DirectiveUnitsPerMeter ) ) )

(defun UpdateNetListGetSkillGetNetlistNetWireWidthFunc ()
  (lambda ( NetName
            DirectiveTable
            DirectiveUnitsPerMeter )
    ( CellInfoGetNetWireWidthInMeters
      DirectiveTable
      NetName
      DirectiveUnitsPerMeter ) ) )

(defun UpdateNetListGetSkillGetNetlistLayerWireSpacingFunc ()
  (lambda ( Layer
            DirectiveTable
            DirectiveUnitsPerMeter )
    ( CellInfoGetLayerWireSpacingInMeters
      DirectiveTable
      Layer
      DirectiveUnitsPerMeter ) ) )

(defun UpdateNetListGetSkillGetNetlistLayerWireWidthFunc ()
  (lambda ( Layer
            DirectiveTable
            DirectiveUnitsPerMeter )
    ( CellInfoGetLayerWireWidthInMeters
      DirectiveTable
      Layer
      DirectiveUnitsPerMeter ) ) )

(defun UpdateNetListGetSkillGetNetlistGlobalWireSpacingFunc ()
  (lambda ( DirectiveTable
            DirectiveUnitsPerMeter )
    ( CellInfoGetGlobalWireSpacingInMeters
      DirectiveTable
      DirectiveUnitsPerMeter ) ) )

(defun UpdateNetListGetSkillGetNetlistGlobalWireWidthFunc ()
  (lambda ( DirectiveTable
            DirectiveUnitsPerMeter )
    ( CellInfoGetGlobalWireWidthInMeters
      DirectiveTable
      DirectiveUnitsPerMeter ) ) )

(defun UpdateNetlistGetSkillNetlistGetCellHeightFunc ( )
  (lambda
    ( LibName 
      CellName 
      ViewName
      HeightOfExistingBoundaryInMeters
      ConnectivityInstanceNames
      DirectivesTable 
      DirectiveUnitsPerMeter 
      GetCellHeightFunc
      GetCellHeightFuncParams )
    (let (
          ( BitPitch ( arrayref DirectivesTable "bitpitch" ) )
          ( HeightInBitPitches ( arrayref DirectivesTable "height" ) ) )
      ( apply
        GetCellHeightFunc
        ( cons
          LibName
          ( cons
            CellName
            ( cons
              ViewName
              ( cons
                HeightOfExistingBoundaryInMeters
                ( cons
                  ConnectivityInstanceNames
                  ( cons
                    (when ( and BitPitch ( numberp BitPitch ) )
                      ( quotient BitPitch DirectiveUnitsPerMeter ) )
                    ( cons
                      (when ( and HeightInBitPitches ( numberp HeightInBitPitches ) )
                        HeightInBitPitches )
                      GetCellHeightFuncParams ) ) ) ) ) ) ) ) ) ) )

(defun UpdateNetlistGetSkillNetlistEstimateCellWidthFunc ( )
  (lambda
    ( LibName
      CellName
      ViewName
      CellHeightInMeters
      NetlistsTablesTable
      DirectivesTable
      DirectiveUnitsPerMeter
      EstimationFunc
      EstimationFuncParams )
    (let (
          ( TotalTransistorGateAreaInMetersSquared ( NetlistTableGetTotalTransistorGateArea
                                                     ( arrayref NetlistsTablesTable ViewName ) ) )
          ( TotalTransistorGateLengthInMeters ( NetlistTableGetTotalTransistorLength
                                                ( arrayref NetlistsTablesTable ViewName ) ) )
          )
      (when ( and TotalTransistorGateAreaInMetersSquared
                  TotalTransistorGateLengthInMeters )
        ( apply
          EstimationFunc
          ( cons
            LibName
            ( cons
              CellName
              ( cons
                ViewName
                ( cons
                  TotalTransistorGateAreaInMetersSquared
                  ( cons
                    TotalTransistorGateLengthInMeters
                    ( cons
                      CellHeightInMeters
                      ( cons
                        DirectivesTable
                        ( cons
                          DirectiveUnitsPerMeter
                          EstimationFuncParams ) ) ) ) ) ) ) ) ) ) ) ) )

(defun UpdateNetlistGetSkillNetlistEstimateCellPositionFunc ( )
  (lambda
    ( LibName
      CellName
      ViewName
      EstimationFunc
      EstimationFuncParams )
    ( apply
      EstimationFunc
      ( cons
        LibName
        ( cons
          CellName
          ( cons
            ViewName
            EstimationFuncParams ) ) ) ) ) )

(defun UpdateNetlistGetPinChangeFunc ( )
  (lambda ( ConnectivityChanged
            ForcePins
            PinsFile
            DirectiveTable
            DirectiveUnitsPerMeter )
    (if ( or 
          ForcePins
          ConnectivityChanged )
        (let (
              ( PinLPP ( CellInfoGetPinLayer DirectiveTable ) ) )
          (let (
                ( WirePitch ( CellInfoGetLayerWirePitchInMeters DirectiveTable PinLPP DirectiveUnitsPerMeter ) )
                ( WireWidth ( CellInfoGetLayerWireWidthInMeters DirectiveTable PinLPP DirectiveUnitsPerMeter ) )
                ( WireSpacing ( CellInfoGetLayerWireSpacingInMeters DirectiveTable PinLPP DirectiveUnitsPerMeter ) ) )
            ( list PinsFile WirePitch WireWidth WireSpacing PinLPP )
            ) ) ) ) )



(defun UpdateNetlistCompareCellViewsToSkillNetlist ( LibName 
                                                     CellName
                                                     LibCellExpressionPairsToIgnore
                                                     FoldableLibCellExpressionPairs
                                                     RequiredViews
                                                     ViewsToUpdate
                                                     UpdateOnlyViews
                                                     NetlistViewName
                                                     SkillNetlistDir
                                                     DirectivesSkillDir
                                                     AutopinsSkillDir
                                                     BoundaryLayerLPP
                                                     UserUnitsPerMeter
                                                     DirectiveUnitsPerMeter
                                                     GetInstanceTransformFunc
                                                     GetInstanceTransformFuncParams
                                                     GetCellHeightFunc
                                                     GetCellHeightFuncParams
                                                     WidthEstimationFunc
                                                     WidthEstimationFuncParams
                                                     PositionFunc
                                                     PositionFuncParams
                                                     NetlistTableTransformationFunc
                                                     NetlistTableTransformationFuncParams
                                                     LockBound
                                                     SuppressPins
                                                     ForcePins )
  ( printf "Loading Netlist for %L\n" CellName )
  (let (
        ( NetlistTable ( NetlistTableGetSkillNetlistTableForCellName
                         CellName
                         SkillNetlistDir ) ) )
    (if ( tablep NetlistTable )
        (let (
              ( PinsFile ( sprintf nil
                                   "%s/%s.pins.il"
                                   AutopinsSkillDir
                                   CellName ) )
              ( DirectivesTable ( CellInfoGetTableForCellName
                                  CellName
                                  DirectivesSkillDir ) ) )
          (if ( tablep DirectivesTable )
              (let (
                    ( ErrorStr nil )
                    ( TransformedNetlistsTablesTable ( makeTable "tableOfNetlistTables" nil ) ) )
                ( foreach
                  ViewName
                  RequiredViews
                  (unless ErrorStr
                    (let (
                          ( TransformedNetlistTable ( apply
                                                      NetlistTableTransformationFunc
                                                      ( cons
                                                        NetlistTable
                                                        ( cons
                                                          SkillNetlistDir
                                                          ( cons 
                                                            DirectivesTable
                                                            ( cons
                                                              ViewName
                                                              NetlistTableTransformationFuncParams ) ) ) ) ) ) )
                      (if ( tablep TransformedNetlistTable )
                          ( setarray TransformedNetlistsTablesTable ViewName TransformedNetlistTable )
                        ( setq ErrorStr TransformedNetlistTable ) ) ) ) )
                (if ( not  ErrorStr )
                    ( cons
                      nil
                      ( UpdateNetlistCompareCellViews
                        LibName 
                        CellName
                        LibCellExpressionPairsToIgnore
                        FoldableLibCellExpressionPairs
                        RequiredViews
                        ViewsToUpdate
                        UpdateOnlyViews
                        NetlistViewName
                        ( UpdateNetlistGetSkillNetlistInstanceExistsFunc )
                        ( list TransformedNetlistsTablesTable )
                        ( UpdateNetlistGetSkillNetlistGetInstanceMasterLibNameFunc )
                        ( list TransformedNetlistsTablesTable )
                        ( UpdateNetlistGetSkillNetlistGetInstanceMasterCellNameFunc )
                        ( list TransformedNetlistsTablesTable )
                        ( UpdateNetlistGetSkillNetlistGetInstanceTripplesFunc )
                        ( list TransformedNetlistsTablesTable )
                        ( UpdateNetlistGetSkillNetlistGetInstanceParameterPairsFunc )
                        ( list TransformedNetlistsTablesTable )
                        GetInstanceTransformFunc
                        GetInstanceTransformFuncParams
                        ( UpdateNetlistGetSkillNetlistInstanceHasConnectionFunc )
                        ( list TransformedNetlistsTablesTable )
                        ( UpdateNetlistGetSkillNetlistGetInstanceConnectionsFunc )
                        ( list TransformedNetlistsTablesTable )
                        ( UpdateNetlistGetSkillNetlistTerminalExistsFunc )
                        ( list TransformedNetlistsTablesTable )
                        ( UpdateNetlistGetSkillNetlistGetTerminalNamesFunc )
                        ( list TransformedNetlistsTablesTable )
                        ( UpdateNetlistGetSkillNetlistNetExistsFunc )
                        ( list TransformedNetlistsTablesTable )
                        ( UpdateNetlistGetSkillGetNetlistNetNamesFunc )
                        ( list TransformedNetlistsTablesTable )

                        ( UpdateNetListGetSkillGetNetlistNetWireSpacingFunc )
                        ( list DirectivesTable DirectiveUnitsPerMeter )
                        ( UpdateNetListGetSkillGetNetlistNetWireWidthFunc )
                        ( list DirectivesTable DirectiveUnitsPerMeter )

                        ( UpdateNetListGetSkillGetNetlistLayerWireSpacingFunc )
                        ( list DirectivesTable DirectiveUnitsPerMeter )
                        ( UpdateNetListGetSkillGetNetlistLayerWireWidthFunc )
                        ( list DirectivesTable DirectiveUnitsPerMeter )

                        ( UpdateNetListGetSkillGetNetlistGlobalWireSpacingFunc )
                        ( list DirectivesTable DirectiveUnitsPerMeter )
                        ( UpdateNetListGetSkillGetNetlistGlobalWireWidthFunc )
                        ( list DirectivesTable DirectiveUnitsPerMeter )

                        ( UpdateNetlistGetSkillNetlistGetCellHeightFunc )
                        ( list 
                          DirectivesTable
                          DirectiveUnitsPerMeter
                          GetCellHeightFunc
                          GetCellHeightFuncParams )
                        ( UpdateNetlistGetSkillNetlistEstimateCellWidthFunc )
                        ( list 
                          TransformedNetlistsTablesTable 
                          DirectivesTable
                          DirectiveUnitsPerMeter
                          WidthEstimationFunc
                          WidthEstimationFuncParams )
                        ( UpdateNetlistGetSkillNetlistEstimateCellPositionFunc )
                        ( list
                          PositionFunc
                          PositionFuncParams )
                        ( UpdateNetlistGetPinChangeFunc )
                        ( list 
                          PinsFile
                          DirectivesTable
                          DirectiveUnitsPerMeter )
                        BoundaryLayerLPP
                        UserUnitsPerMeter
                        LockBound
                        SuppressPins
                        ForcePins ) )
                  ( list
                    ErrorStr
                    ( list LibName CellName )
                    nil ) ) )
            ( list
              DirectivesTable
              ( list LibName CellName )
              nil ) ) )
      ( list
        NetlistTable
        ( list LibName CellName ) 
        nil ) ) ) )

(defun UpdateNetlistCompareCellsToSkillNetlists ( LibCellPairList
                                                  LibCellExpressionPairsToIgnore
                                                  FoldableLibCellExpressionPairs
                                                  RequiredViews
                                                  ViewsToUpdate
                                                  UpdateOnlyViews
                                                  NetlistViewName
                                                  SkillNetlistDir
                                                  DirectivesSkillDir
                                                  AutopinsSkillDir
                                                  BoundaryLayerLPP
                                                  UserUnitsPerMeter
                                                  DirectiveUnitsPerMeter
                                                  GetInstanceTransformFunc
                                                  GetInstanceTransformFuncParams
                                                  GetCellHeightFunc
                                                  GetCellHeightFuncParams
                                                  WidthEstimationFunc
                                                  WidthEstimationFuncParams
                                                  PositionFunc
                                                  PositionFuncParams
                                                  NetlistTableTransformationFunc
                                                  NetlistTableTransformationFuncParams
                                                  LockBound
                                                  SuppressPins
                                                  ForcePins )
  (let (
        ( Ret nil ) )
    ( foreach
      LibCellPair
      LibCellPairList
      ( setq 
        Ret 
        ( tconc 
          Ret 
          (let (
                ( LibName ( car LibCellPair ) )
                ( CellName ( cadr LibCellPair ) ) )
            ( UpdateNetlistCompareCellViewsToSkillNetlist
              ( car LibCellPair ) 
              ( cadr LibCellPair )
              LibCellExpressionPairsToIgnore
              FoldableLibCellExpressionPairs
              RequiredViews
              ViewsToUpdate
              UpdateOnlyViews
              NetlistViewName
              SkillNetlistDir
              DirectivesSkillDir
              AutopinsSkillDir
              BoundaryLayerLPP
              UserUnitsPerMeter
              DirectiveUnitsPerMeter
              GetInstanceTransformFunc
              GetInstanceTransformFuncParams
              GetCellHeightFunc
              GetCellHeightFuncParams
              WidthEstimationFunc
              WidthEstimationFuncParams
              PositionFunc
              PositionFuncParams
              NetlistTableTransformationFunc
              NetlistTableTransformationFuncParams
              LockBound
              SuppressPins
              ForcePins ) ) ) ) )
    ( car Ret ) ) )

(defun UpdateNetlistAppendCompareResultsToPort ( CompareResults OutputPort )
  ( foreach
    CellCompareResult
    CompareResults
    (let (
          ( LibCellPair ( cadr CellCompareResult ) )
          ( ViewCheckResults ( caddr CellCompareResult ) ) )
      ( foreach
        ViewCheckResult
        ViewCheckResults
        (let (
              ( ViewName ( car ViewCheckResult ) )
              ( CompareResult ( cadr ViewCheckResult ) ) )
          (if CompareResult
              (let ()
                (if ( ddGetObj ( car LibCellPair ) ( cadr LibCellPair ) ViewName )
                    ( fprintf OutputPort "CELL: %s %s %s\n" ( car LibCellPair ) ( cadr LibCellPair ) ViewName )
                  ( fprintf OutputPort "CREATE: %s %s %s\n" ( car LibCellPair ) ( cadr LibCellPair ) ViewName ) )
                ( UpdateNetlistPrintDifferences
                  CompareResult
                  OutputPort ) )
            (let ()
              ( fprintf OutputPort "IGNORE: %s %s %s\n" ( car LibCellPair ) ( cadr LibCellPair ) ViewName ) ) ) ) ) ) ) )

(defun UpdateNetlistAppendCompareResultsToFile ( CompareResults FileName )
  (let (
        ( OutputPort ( outfile FileName "a" ) ) )
    ( UpdateNetlistAppendCompareResultsToPort CompareResults OutputPort )
    ( close OutputPort ) ) )

(defun UpdateNetlistGetDescendantLibCellPairs ( RootCellView
                                                LibCellExpressionPairsToIgnore )
  ( HierarchyGetDescendantLibCellPairs
    RootCellView
    LibCellExpressionPairsToIgnore
    (lambda ( CellView ) 
      nil )
    nil ) )


(defun UpdateNetlistSafeOpenCellViewForReading ( LibName CellName ViewName )
  (let (
        ( ViewDDObj ( ddGetObj LibName CellName ViewName ) ) )
    (when ( and
            ViewDDObj
            ( ddIsObjReadable ViewDDObj )
            ( car ( getq ViewDDObj files ) )
            ( ddIsObjReadable ( car ( getq ViewDDObj files ) ) ) )
      ( dbOpenCellViewByType
        LibName
        CellName
        ViewName
        nil
        "r" ) ) ) )

(defun UpdateNetlistGetDefaultGetFloorplanInstanceTransformFunc ()
  (lambda
    ( InstanceName ViewInfoTable LayoutViewName )
    (let (
          ( LayoutViewInfo ( arrayref ViewInfoTable LayoutViewName ) ) )
      (when LayoutViewInfo
        (let (
              ( LayoutCellView ( car LayoutViewInfo ) )
              ( LayoutInstanceMap ( caddr LayoutViewInfo ) ) )
          (when LayoutCellView
            (let (
                  ( LayoutInstance ( car 
                                     ( NameGetInstancesForCanonicalName
                                       LayoutInstanceMap
                                       InstanceName ) ) ) )
              (when LayoutInstance
                (let (
                      ( LayoutTransform ( getq LayoutInstance transform ) ) )
                  LayoutTransform ) ) ) ) ) ) ) ) )


(defun UpdateNetlistGetDefaultGetFloorplanInstanceTransformFuncParams ( LayoutViewName )
  ( list LayoutViewName ) )

(defun UpdateNetlistGetNullGetInstanceTransformFunc ()
  (lambda
    ( InstanceName ViewInfoTable )
    nil ) )

(defun UpdateNetlistGetNullGetInstanceTransformFuncParams ()
  nil )


(defun UpdateNetlistGetDefaultGetInstanceTransformFunc ()
  (lambda
    ( InstanceName ViewName ViewInfoTable FunctionTable )
    (let (
          ( ViewFunctions ( arrayref FunctionTable ViewName ) ) )
      (when ViewFunctions
        (let (
              ( GetInstanceTransformFuncEntry ( car ViewFunctions ) ) )
          (let (
                ( GetInstanceTransformFunc ( car GetInstanceTransformFuncEntry ) )
                ( GetInstanceTransformFuncParams ( cadr GetInstanceTransformFuncEntry ) ) )
            ( apply
              GetInstanceTransformFunc
              ( cons
                InstanceName
                ( cons ViewInfoTable GetInstanceTransformFuncParams ) ) ) ) ) ) ) ) )

(defun UpdateNetlistGetFunctionTableForDefaultInstanceTransformFunctions ( NetlistViewName
                                                                          LayoutViewName
                                                                          FloorplanViewName )
  (let (
        ( Ret ( makeTable "instancePositionFunctionTable" nil ) ) )
    (when NetlistViewName
      ( setarray
        Ret
        NetlistViewName
        ( list
          ( list
            ( UpdateNetlistGetNullGetInstanceTransformFunc )
            ( UpdateNetlistGetNullGetInstanceTransformFuncParams ) ) ) ) )
    ( setarray
      Ret
      LayoutViewName
      ( list
        ( list
          ( UpdateNetlistGetNullGetInstanceTransformFunc )
          ( UpdateNetlistGetNullGetInstanceTransformFuncParams ) ) ) )
    ( setarray
      Ret
      FloorplanViewName
      ( list
        ( list
          ( UpdateNetlistGetDefaultGetFloorplanInstanceTransformFunc )
          ( UpdateNetlistGetDefaultGetFloorplanInstanceTransformFuncParams 
            LayoutViewName ) ) ) )
    Ret ) )


(defun UpdateNetlistGetDefaultGetNetlistViewHeightFunc ()
  (lambda
    ( LibName
      CellName
      ViewName
      HeightOfExistingBoundaryInMeters
      ConnectivityInstanceNames
      BitPitchInMeters
      HeightInBitPitches )
    nil ) )

(defun UpdateNetlistGetDefaultGetNetlistViewHeightFuncParams ()
  nil )

(defun UpdateNetlistGetDefaultGetNetlistViewWidthFunc ()
  (lambda
    ( LibName
      CellName
      ViewName
      TotalTransistorGateAreaInMetersSquared
      TotalTransistorGateLengthInMeters
      CellHeightInMeters )
    nil ) )

(defun UpdateNetlistGetDefaultGetNetlistViewWidthFuncParams ()
  nil )

(defun UpdateNetlistGetDefaultGetNetlistViewPositionFunc ()
  (lambda
    ( LibName
      CellName
      ViewName )
    nil ) )

(defun UpdateNetlistGetDefaultGetNetlistViewPositionFuncParams ()
  nil )

(defun UpdateNetlistGetDefaultGetLayoutViewHeightFunc ()
  (lambda
    ( LibName
      CellName
      ViewName
      HeightOfExistingBoundaryInMeters
      ConnectivityInstanceNames
      BitPitchInMeters
      HeightInBitPitches 
      BoundaryLPP 
      UserUnitsPerMeter
      SmallCellWarningCutOff )

    (let (
          ( DirectivesHeightInMeters (when ( and BitPitchInMeters HeightInBitPitches )
                                       ( times BitPitchInMeters HeightInBitPitches ) ) ) )
      (if ( and HeightOfExistingBoundaryInMeters DirectivesHeightInMeters )
          (if ( lessp HeightOfExistingBoundaryInMeters DirectivesHeightInMeters )
              ( list
                HeightOfExistingBoundaryInMeters
                (unless ( lessp HeightInBitPitches SmallCellWarningCutOff )
                  ( sprintf 
                    nil
                    "Height of boundary of \"%s\" \"%s\" \"%s\" is %0.9e, which is smaller than the %0.9e."
                    LibName
                    CellName
                    ViewName
                    HeightOfExistingBoundaryInMeters
                    DirectivesHeightInMeters ) ) )
            ( list DirectivesHeightInMeters nil ) )
        (if HeightOfExistingBoundaryInMeters
            ( list HeightOfExistingBoundaryInMeters nil )
          (when ( and 
                  DirectivesHeightInMeters
                  ( null ConnectivityInstanceNames ) )
            ( list DirectivesHeightInMeters nil ) ) ) ) ) ) )


(defun UpdateNetlistGetDefaultGetLayoutViewHeightHeightFuncParams ( BoundaryLPP 
                                                                    UserUnitsPerMeter 
                                                                    SmallCellWarningCutOff )
  ( list BoundaryLPP UserUnitsPerMeter SmallCellWarningCutOff ) )

(defun UpdateNetlistGetDefaultGetLayoutViewWidthFunc ()
  (lambda
    ( LibName
      CellName
      ViewName
      TotalTransistorGateAreaInMetersSquared
      TotalTransistorGateLengthInMeters
      CellHeightInMeters
      DirectivesTable
      DirectiveUnitsPerMeter )
    nil ) )

(defun UpdateNetlistGetDefaultGetLayoutViewWidthFuncParams ()
  nil )

(defun UpdateNetlistGetDefaultGetLayoutViewPositionFunc ()
  (lambda
    ( LibName
      CellName
      ViewName )
    nil ) )

(defun UpdateNetlistGetDefaultGetLayoutViewPositionFuncParams ()
  nil )

;If this function returns nil, UpdateNetlistCompareCellBoundary will
;return nil which means that if there is a boundary shape it will not
;be change and if there is not a boundary shape none will be created.
;This function should otherwise return a pair whose first element
;is the desire height of the boundary shape and whose second element
;is nil or a string containing a warning message.
(defun UpdateNetlistGetDefaultGetFloorplanViewHeightFunc ()
  (lambda
    ( LibName
      CellName
      ViewName
      HeightOfExistingBoundaryInMeters
      ConnectivityInstanceNames
      BitPitchInMeters
      HeightInBitPitches 
      BoundaryLPP 
      UserUnitsPerMeter
      SmallCellWarningCutOff
      UseLayoutHeight
      LayoutViewName )

    (let (
          ;What does the height directive say?
          ( DirectivesHeightInMeters (when ( and BitPitchInMeters HeightInBitPitches )
                                       ( times BitPitchInMeters HeightInBitPitches ) ) )
          ;Find the height of the boundary in the layout view if there is one.
          ( BoundaryHeightInLayoutViewInMeters 
            (let (
                  ( LayoutCellView ( UpdateNetlistSafeOpenCellViewForReading
                                     LibName
                                     CellName
                                     LayoutViewName ) ) )
              (when LayoutCellView
                (let (
                      ( LayoutBoundaryBBox ( UpdateNetlistGetBoundaryLayerBBox
                                             LayoutCellView
                                             BoundaryLPP
                                             t ) ) )
                  (when LayoutBoundaryBBox
                    ( quotient
                      ( RectGetHeight LayoutBoundaryBBox )
                      UserUnitsPerMeter ) ) ) ) ) ) )
      (if ( and BoundaryHeightInLayoutViewInMeters DirectivesHeightInMeters )
          ;If the layout has a boundary and the directive is specified
          ;Or the user specified that we use the layout height
          (if ( or
                UseLayoutHeight
                ( lessp BoundaryHeightInLayoutViewInMeters DirectivesHeightInMeters )
                )
              ;If boundary shape in the layout had a height less than
              ;the height specified by the directive, use the height in the layout.
              ( list
                BoundaryHeightInLayoutViewInMeters
                ;If the height specified by the directive is smaller
                ;than the SmallCellWarningCutOff then don't emit a warning.
                (unless ( lessp HeightInBitPitches SmallCellWarningCutOff )
                  ( sprintf
                    nil
                    "Height of boundary of \"%s\" \"%s\" \"%s\" is %0.9e, which is smaller than the height directive value %0.9e."
                    LibName
                    CellName
                    LayoutViewName
                    BoundaryHeightInLayoutViewInMeters
                    DirectivesHeightInMeters ) ) )
            ;The height of the boundary shape in the layout was greater than
            ;or equal to the height specified by the directive so just use the
            ;directive value.
            ( list DirectivesHeightInMeters nil ) )
        ;Either there is no Boundary shape in the layout
        ;or the height directive did not have a value.
        
        (if BoundaryHeightInLayoutViewInMeters
            ;If the layout does have a boundary shape,
            ;use the height of that shape as the height for the floorplane view.
            ( list BoundaryHeightInLayoutViewInMeters nil )
          ;There was no boundary shape in the layout so
          ;use the directive value for the height if there is one
          ;unless the cell is not a leaf cells.
          (when ( and
                  DirectivesHeightInMeters
                  ( null ConnectivityInstanceNames ) )
            ( list DirectivesHeightInMeters nil ) ) ) ) ) ) )

(defun UpdateNetlistGetDefaultGetFloorplanViewHeightFuncParams ( BoundaryLPP 
                                                                 UserUnitsPerMeter 
                                                                 SmallCellWarningCutOff
                                                                 UseLayoutHeight
                                                                 LayoutViewName )
  ( list
    BoundaryLPP 
    UserUnitsPerMeter 
    SmallCellWarningCutOff
    UseLayoutHeight
    LayoutViewName ) )



(defun UpdateNetlistGetDefaultGetFloorplanViewWidthFunc ()
  (lambda
    ( LibName
      CellName
      ViewName
      TotalTransistorGateAreaInMetersSquared
      TotalTransistorGateLengthInMeters
      CellHeightInMeters
      DirectivesTable
      DirectiveUnitsPerMeter
      BoundaryLPP
      LayoutViewName
      UserUnitsPerMeter
      WidthIncrement
      BoundaryWidthWarningCutOff
      DefaultDensityFactor
      BoundaryScalingFactor )

    (let (
          ;Get the width of the boundary shape in the layout if there is on.
          ( WidthFromLayoutViewInMeters (let (
                                              ( LayoutCellView ( UpdateNetlistSafeOpenCellViewForReading
                                                                 LibName
                                                                 CellName
                                                                 LayoutViewName ) ) )
                                          (when LayoutCellView
                                            (let (
                                                  ( LayoutBoundaryBBox ( UpdateNetlistGetBoundaryLayerBBox
                                                                         LayoutCellView
                                                                         BoundaryLPP
                                                                         t ) ) )
                                              (when LayoutBoundaryBBox
                                                ( quotient
                                                  ( RectGetWidth LayoutBoundaryBBox )
                                                  UserUnitsPerMeter ) ) ) ) ) )
          ( DensityScaleFactorToUse (let (
                                          ( DensityScaleFactor ( arrayref DirectivesTable "density_scale_factor" ) ) )
                                      (if ( and
                                            DensityScaleFactor
                                            ( numberp DensityScaleFactor ) )
                                          ( float DensityScaleFactor )
                                        1.0 ) ) )
          ( TransistorBaseWidthInMeters (let (
                                              ( TransistorBaseWidthInMeters
                                                ( arrayref DirectivesTable "base_transistor_layout_width" ) ) )
                                          (if ( and
                                                TransistorBaseWidthInMeters
                                                ( numberp TransistorBaseWidthInMeters ) )
                                              ( float TransistorBaseWidthInMeters )
                                            0.0 ) ) )
          ( DensityFactorToUse (let (
                                     ( DensityFactor ( arrayref DirectivesTable "density_factor" ) ) )
                                 (if ( and 
                                       DensityFactor
                                       ( numberp DensityFactor ) )
                                     DensityFactor
                                   DefaultDensityFactor ) ) ) )
      (if WidthFromLayoutViewInMeters
          ;If the layout view had a boundary shape, use the width of that shape.
          ( list WidthFromLayoutViewInMeters nil )
        ;If the layout view did not have a boundary shape then we'll have to guess.
        (let (
              ;Guess the width.  You can read the code as well as I can.
              ;Notice that width is always rounded up to nearest multiple
              ;of WidthIncrement.
              ( Width ( times
                        ( ceiling
                          ( quotient
                            ( times
                              ( quotient
                                ( plus 
                                  ( times
                                    TotalTransistorGateLengthInMeters
                                    TransistorBaseWidthInMeters )
                                  TotalTransistorGateAreaInMetersSquared )
                                CellHeightInMeters )
                              ( times 
                                DensityFactorToUse
                                DensityScaleFactorToUse
                                BoundaryScalingFactor ) )
                            WidthIncrement ) )
                        WidthIncrement ) ) )
          (if ( geqp Width BoundaryWidthWarningCutOff )
              ;If the guessed width is greater than minimum allowed width
              ;use the guessed width.
              ( list Width nil )
            ;If the guessed width is smaller than the minimum allowed width
            ;use the minimum allowed width and generate a warning.
            ( list BoundaryWidthWarningCutOff 
                   ( sprintf nil
                             "Estimated width %.9e below cutoff of %.9e...enforcing cutoff" 
                             Width 
                             BoundaryWidthWarningCutOff ) ) ) ) ) ) ) )

(defun UpdateNetlistGetDefaultGetFloorplanViewWidthFuncParams ( BoundaryLPP
                                                                LayoutViewName
                                                                UserUnitsPerMeter
                                                                WidthIncrement
                                                                BoundaryWidthWarningCutOff
                                                                DefaultDensityFactor
                                                                BoundaryScalingFactor )
  ( list 
    BoundaryLPP
    LayoutViewName
    UserUnitsPerMeter
    WidthIncrement
    BoundaryWidthWarningCutOff
    DefaultDensityFactor
    BoundaryScalingFactor ) )

(defun UpdateNetlistGetDefaultGetFloorplanViewPositionFunc (  )
  ( lambda 
    ( LibName
      CellName
      ViewName
      UserUnitsPerMeter
      LayoutViewName         
      BoundaryLPP 
      DefaultBoundaryPosition )
    (let (
          ( BoundaryPositionInLayoutViewInMeters 
            (let (
                  ( LayoutCellView ( UpdateNetlistSafeOpenCellViewForReading
                                     LibName
                                     CellName
                                     LayoutViewName ) ) )
              (when LayoutCellView
                (let (
                      ( LayoutBoundaryBBox ( UpdateNetlistGetBoundaryLayerBBox
                                             LayoutCellView
                                             BoundaryLPP
                                             t ) ) )
                  (when LayoutBoundaryBBox
                    ( list 
                      ( quotient ( caar LayoutBoundaryBBox ) UserUnitsPerMeter )
                      ( quotient ( cadar LayoutBoundaryBBox ) UserUnitsPerMeter ) ) ) ) ) ) ) )
      (if BoundaryPositionInLayoutViewInMeters 
          ( list BoundaryPositionInLayoutViewInMeters nil )
        ( list DefaultBoundaryPosition nil ) ) ) ) )


(defun UpdateNetlistGetDefaultGetFloorplanViewPositionFuncParams ( UserUnitsPerMeter
                                                                   LayoutViewName         
                                                                   BoundaryLPP 
                                                                   DefaultBoundaryPosition )
  ( list 
    UserUnitsPerMeter
    LayoutViewName         
    BoundaryLPP 
    DefaultBoundaryPosition ) )


(defun UpdateNetlistGetDefaultGetCellHeightFunc ( )
  (lambda
    ( LibName
      CellName
      ViewName
      HeightOfExistingBoundaryInMeters
      ConnectivityInstanceNames
      BitPitchInMeters
      HeightInBitPitches
      FunctionTable )
    (let (
          ( ViewFunctions ( arrayref FunctionTable ViewName ) ) )
      (when ViewFunctions
        (let (
              ( HeightFunctionEntry ( car ViewFunctions ) ) )
          (let (
                ( HeightFunction ( car HeightFunctionEntry ) )
                ( HeightFunctionParams ( cadr HeightFunctionEntry ) ) )
            ( apply
              HeightFunction
              ( cons
                LibName
                ( cons
                  CellName
                  ( cons
                    ViewName
                    ( cons
                      HeightOfExistingBoundaryInMeters
                      ( cons
                        ConnectivityInstanceNames
                        ( cons
                          BitPitchInMeters
                          ( cons
                            HeightInBitPitches
                            HeightFunctionParams ) ) ) ) ) ) ) ) ) ) ) ) ) )

(defun UpdateNetlistGetDefaultGetCellWidthFunc ( )
  (lambda
    ( LibName
      CellName
      ViewName
      TotalTransistorGateAreaInMetersSquared
      TotalTransistorGateLengthInMeters
      CellHeightInMeters
      DirectivesTable
      DirectiveUnitsPerMeter
      FunctionTable )
    (let (
          ( ViewFunctions ( arrayref FunctionTable ViewName ) ) )
      (when ViewFunctions
        (let (
              ( WidthFunctionEntry ( cadr ViewFunctions ) ) )
          (let (
                ( WidthFunction ( car WidthFunctionEntry ) )
                ( WidthFunctionParams ( cadr WidthFunctionEntry ) ) )
            ( apply
              WidthFunction
              ( cons
                LibName
                ( cons
                  CellName
                  ( cons
                    ViewName
                    ( cons
                      TotalTransistorGateAreaInMetersSquared
                      ( cons
                        TotalTransistorGateLengthInMeters
                        ( cons
                          CellHeightInMeters
                          ( cons
                            DirectivesTable
                            ( cons
                              DirectiveUnitsPerMeter
                              WidthFunctionParams ) ) ) ) ) ) ) ) ) ) ) ) ) ) )

(defun UpdateNetlistGetDefaultGetCellPositionFunc ( )
  (lambda
    ( LibName
      CellName
      ViewName
      FunctionTable )
    (let (
          ( ViewFunctions ( arrayref FunctionTable ViewName ) ) )
      (when ViewFunctions
        (let (
              ( PositionFunctionEntry ( caddr ViewFunctions ) ) )
          (let (
                ( PositionFunction ( car PositionFunctionEntry ) )
                ( PositionFunctionParams ( cadr PositionFunctionEntry ) ) )
            ( apply
              PositionFunction
              ( cons
                LibName
                ( cons
                  CellName
                  ( cons
                    ViewName
                    PositionFunctionParams ) ) ) ) ) ) ) ) ) )


(defun UpdateNetlistGetFunctionTableForDefaultWidthAndHeightAndPositionFunctions ( NetlistViewName 
                                                                                   LayoutViewName
                                                                                   FloorplanViewName
                                                                                   BoundaryLPP
                                                                                   UserUnitsPerMeter
                                                                                   WidthIncrement
                                                                                   BoundaryWidthWarningCutOff
                                                                                   DefaultDensityFactor
                                                                                   BoundaryScalingFactor
                                                                                   SmallCellWarningCutOff
                                                                                   UseLayoutHeight
                                                                                   DefaultBoundaryPosition )
  (let (
        ( Ret ( makeTable "boundaryFunctionTable" nil ) ) )
    (when NetlistViewName
      ( setarray 
        Ret
        NetlistViewName
        ( list
          ( list
            ( UpdateNetlistGetDefaultGetNetlistViewHeightFunc )
            ( UpdateNetlistGetDefaultGetNetlistViewHeightFuncParams ) )
          ( list
            ( UpdateNetlistGetDefaultGetNetlistViewWidthFunc )
            ( UpdateNetlistGetDefaultGetNetlistViewWidthFuncParams ) )
          ( list
            ( UpdateNetlistGetDefaultGetNetlistViewPositionFunc )
            ( UpdateNetlistGetDefaultGetNetlistViewPositionFuncParams ) ) ) ) )
    ( setarray
      Ret
      LayoutViewName
      ( list
        ( list
          ( UpdateNetlistGetDefaultGetLayoutViewHeightFunc )
          ( UpdateNetlistGetDefaultGetLayoutViewHeightHeightFuncParams
            BoundaryLPP
            UserUnitsPerMeter
            SmallCellWarningCutOff ) )
        ( list
          ( UpdateNetlistGetDefaultGetLayoutViewWidthFunc )
          ( UpdateNetlistGetDefaultGetLayoutViewWidthFuncParams ) )
        ( list
          ( UpdateNetlistGetDefaultGetLayoutViewPositionFunc )
          ( UpdateNetlistGetDefaultGetLayoutViewPositionFuncParams ) ) ) )
    ( setarray
      Ret
      FloorplanViewName
      ( list
        ( list
          ( UpdateNetlistGetDefaultGetFloorplanViewHeightFunc )
          ( UpdateNetlistGetDefaultGetFloorplanViewHeightFuncParams 
            BoundaryLPP
            UserUnitsPerMeter
            SmallCellWarningCutOff
            UseLayoutHeight
            LayoutViewName ) )
        ( list
          ( UpdateNetlistGetDefaultGetFloorplanViewWidthFunc )
          ( UpdateNetlistGetDefaultGetFloorplanViewWidthFuncParams
            BoundaryLPP
            LayoutViewName
            UserUnitsPerMeter
            WidthIncrement
            BoundaryWidthWarningCutOff
            DefaultDensityFactor
            BoundaryScalingFactor ) )
        ( list
          ( UpdateNetlistGetDefaultGetFloorplanViewPositionFunc )
          ( UpdateNetlistGetDefaultGetFloorplanViewPositionFuncParams 
            UserUnitsPerMeter
            LayoutViewName         
            BoundaryLPP 
            DefaultBoundaryPosition ) ) ) )
    Ret ) )


(defun UpdateNetlistGetNullNetlistTransformationFunc ()
  (lambda
    ( OriginalNetlistTable 
      SkillNetlistDir 
      DirectivesTable
      ViewName )
    OriginalNetlistTable ) )

(defun UpdateNetlistTransformNetlistTableForLayoutView ()
  (lambda
    ( OriginalNetlistTable 
      SkillNetlistDir 
      DirectivesTable
      ViewName
      ShouldInlineConnectionsFunc
      ShouldInlineConnectionsFuncParams )
    ( InlineConnectionsHierTransformTable
      OriginalNetlistTable 
      ShouldInlineConnectionsFunc
      ( cons
        DirectivesTable
        ShouldInlineConnectionsFuncParams ) 
      SkillNetlistDir ) ) )

(defun UpdateNetlistGetDefaultNetlistTransformationFunc ()
  (lambda
    ( OriginalNetlistTable 
      SkillNetlistDir 
      DirectivesTable
      ViewName
      TransformationFunctionTable )
    (let (
          ( TransformationFuncEntry ( arrayref TransformationFunctionTable ViewName ) ) )
      (let (
            ( TransformationFunc ( car TransformationFuncEntry ) )
            ( TransformationFuncParams ( cadr TransformationFuncEntry ) ) )
        ( apply
          TransformationFunc
          ( cons
            OriginalNetlistTable
            ( cons
              SkillNetlistDir
              ( cons
                DirectivesTable
                ( cons
                  ViewName
                  TransformationFuncParams ) ) ) ) ) ) ) ) )


(defun UpdateNetlistGetDefaultShouldInLineConnectionsFunc ()
  (lambda
    ( LibName
      CellName
      InstanceName
      InstanceMasterLibName
      InstanceMasterCellName
      DirectivesTable
      DirectivesSkillDir )
;    (let (
;          ( CurrDirectivesTable ( CellInfoGetTableForCellName
;                                  CellName
;                                  DirectivesSkillDir ) ) )
;      ( and 
;        nil
;        ( CellInfoLookupParametrizedInBlock
;          DirectivesTable
;          "subcell"
;          "inline_layout"
;          "instance"
;          InstanceName )
;        ) )
       nil
  ) )

(defun UpdateNetlistGetFunctionTableForNetlistTransformationFunctions ( NetlistViewName 
                                                                        LayoutViewName
                                                                        FloorplanViewName
                                                                        DirectivesSkillDir )
  (let (
        ( NetlistTransformationFunctionsTable ( makeTable "functionTable" nil ) ) )
    (when NetlistViewName
      ( setarray
        NetlistTransformationFunctionsTable
        NetlistViewName
        ( list
          ( UpdateNetlistGetNullNetlistTransformationFunc )
          nil ) ) )
    ( setarray
      NetlistTransformationFunctionsTable
      FloorplanViewName
      ( list
        ( UpdateNetlistGetNullNetlistTransformationFunc )
        nil ) )
    ( setarray
      NetlistTransformationFunctionsTable
      LayoutViewName
      ( list
        ( UpdateNetlistTransformNetlistTableForLayoutView )
        ( list
          ( UpdateNetlistGetDefaultShouldInLineConnectionsFunc )
          ( list DirectivesSkillDir ) ) ) ) 
    NetlistTransformationFunctionsTable ) )

; lookup a CAST directive by name, if not found, use default value
(defun CellInfoLookupWithDefault ( DirectivesTable name default )
  (let ((x (CellInfoLookup DirectivesTable name)))
    (cond (x x) ((not x) default))
    )
  )

(defun UpdateNetlistCompareCellsToSkillNetlistsInToFileUsingPDKInfo ( LibCellPairList
                                                                      NetlistViewName
                                                                      LayoutViewName
                                                                      FloorplanViewName
                                                                      SkillNetlistDir
                                                                      DirectivesSkillDir
                                                                      AutopinsSkillDir
                                                                      DefaultDensityFactor
                                                                      BoundaryScalingFactor
                                                                      OutputFileName
                                                                      LockBound
                                                                      SuppressPins
                                                                      ForcePins
                                                                      UseLayoutHeight
                                                                      LockLayoutView )
  (let (
        ( FilteredLibCellPairList 
          ( setof
            LibCellPair
            LibCellPairList
            ( not
              ( or
                ( equal ( car LibCellPair ) StackLibraryName )
                ( equal ( car LibCellPair ) GateLibraryName ) ) ) ) ) )
    (let (
          ( DirectivesTable
            ( CellInfoGetTableForCellName
              ( cadr ( car FilteredLibCellPairList ) )
              DirectivesSkillDir ) ) )
      (let (
          ( BoundaryPosition
            (CellInfoLookupWithDefault
             DirectivesTable "boundary_position" BoundaryPosition))
          ( BoundaryWidthIncrement
            (CellInfoLookupWithDefault
             DirectivesTable "boundary_width_increment" BoundaryWidthIncrement))
          ( BoundaryWidthWarningCutOff
            (CellInfoLookupWithDefault
             DirectivesTable "boundary_width_warning_cutoff" BoundaryWidthWarningCutOff))
          ( BoundarySmallCellWarningCutOff
            (CellInfoLookupWithDefault
             DirectivesTable "boundary_small_cell_warning_cutoff" BoundarySmallCellWarningCutOff))
          )
          

    (let (
          ( InstanceTransformFunctionTable ( UpdateNetlistGetFunctionTableForDefaultInstanceTransformFunctions
                                            NetlistViewName
                                            LayoutViewName
                                            FloorplanViewName ) )
                                            
          ( HeightWidthFunctionTable ( UpdateNetlistGetFunctionTableForDefaultWidthAndHeightAndPositionFunctions
                                       NetlistViewName
                                       LayoutViewName
                                       FloorplanViewName
                                       BoundaryLPP
                                       UserUnitsPerMeter
                                       BoundaryWidthIncrement
                                       BoundaryWidthWarningCutOff
                                       DefaultDensityFactor
                                       BoundaryScalingFactor
                                       BoundarySmallCellWarningCutOff
                                       UseLayoutHeight
                                       BoundaryPosition ) )
          ( NetlistTransformationFunctionTable ( UpdateNetlistGetFunctionTableForNetlistTransformationFunctions
                                                 NetlistViewName
                                                 LayoutViewName
                                                 FloorplanViewName
                                                 DirectivesSkillDir ) ) )
      (let (
            ( CompareResults ( UpdateNetlistCompareCellsToSkillNetlists
                               FilteredLibCellPairList\
                               ( cons
                                 ( list ".*" ".+\\.wires\\..+" )
                                 ( cons
                                   ( list 
                                     ( sprintf nil "^%s$" GateLibraryName )
                                     ".*" )
                                   ( cons 
                                     ( list 
                                       ( sprintf nil "^%s$" StackLibraryName )
                                       ".*" )
                                     ( cons
                                       ( list TechLibName ".*" )
                                     ( cons
                                       ( list "^$" "" )
                                       TransistorLibCellPairs
                                       ) ) ) ) )
                               TransistorLibCellPairs
                               ( setof
                                 ViewName
                                 ( list
                                   FloorplanViewName
                                   NetlistViewName
                                   LayoutViewName )
                                 ViewName )
                               ( setof
                                 ViewName
                                 ( list
                                   FloorplanViewName
                                   NetlistViewName
                                   (unless LockLayoutView
                                     LayoutViewName ) )
                                 ViewName )
                               ( list LayoutViewName )
                               NetlistViewName
                               SkillNetlistDir
                               DirectivesSkillDir
                               AutopinsSkillDir
                               BoundaryLPP
                               UserUnitsPerMeter
                               DirectiveUnitsPerMeter
                               ( UpdateNetlistGetDefaultGetInstanceTransformFunc )
                               ( list InstanceTransformFunctionTable )
                               ( UpdateNetlistGetDefaultGetCellHeightFunc )
                               ( list HeightWidthFunctionTable )
                               ( UpdateNetlistGetDefaultGetCellWidthFunc )
                               ( list HeightWidthFunctionTable )
                               ( UpdateNetlistGetDefaultGetCellPositionFunc )
                               ( list HeightWidthFunctionTable )
                               ( UpdateNetlistGetDefaultNetlistTransformationFunc )
                               ( list NetlistTransformationFunctionTable )
                               LockBound
                               SuppressPins
                               ForcePins ) ) )
        ( UpdateNetlistAppendCompareResultsToFile
          CompareResults
          OutputFileName )
        nil ) ) ) ) ) )

; Resolve BUG 13423: Compute GetPinLength = min(PinLength,BoundaryWidth/2-WireSpacing)
(defun GetPinLength (CellView BoundaryLPP WireSpacing)
  (let ((BBox (UpdateNetlistGetBoundaryLayerBBox CellView BoundaryLPP nil)))
    (if BBox (min PinLength (RectGetWidth BBox)/UserUnitsPerMeter/2-WireSpacing)
      PinLength)))

(defun UpdateNetlistUpdateCellViewWithCompareResult ( CellView
                                                      CompareResult
                                                      InstanceMap
                                                      BoundaryLPP
                                                      UserUnitsPerMeter
                                                      @key
                                                      ( DoNotMoveExistingInstances t )
                                                    )
  (let (
        ( ErrorStr nil )
        ( CurrY 0 )
        ( InstancesToDelete ( UpdateNetlistGetInstancesToDeleteFromCompareResult CompareResult ) )
        ( InstancesToChange ( UpdateNetlistGetInstancesToChangeFromCompareResult CompareResult ) )
        ( InstancesToCreate ( UpdateNetlistGetInstancesToCreateFromCompareResult CompareResult ) )
        ( ParametersToSet ( UpdateNetlistGetParametersToSetFromCompareResult CompareResult ) )
        ( ConnectionsToDelete ( UpdateNetlistGetConnectionsToDeleteFromCompareResult CompareResult ) )
        ( ConnectionsToCreate ( UpdateNetlistGetConnectionsToCreateFromCompareResult CompareResult ) )
        ( TerminalsToDelete ( UpdateNetlistGetTerminalsToDeleteFromCompareResult CompareResult ) )
        ( TerminalsToCreate ( UpdateNetlistGetTerminalsToCreateFromCompareResult CompareResult ) )
        ( NetsToDelete ( UpdateNetlistGetNetsToDeleteFromCompareResult CompareResult ) )
        ( NetsToCreate ( UpdateNetlistGetNetsToCreateFromCompareResult CompareResult ) )
        ( NetsToChange ( UpdateNetlistGetNetsToChangeFromCompareResult CompareResult ) )
        ( BoundaryShape ( UpdateNetlistGetBoundaryShapeFromCompareResult CompareResult ) )
        ( BoundaryHeight ( UpdateNetlistGetBoundaryHeightFromCompareResult CompareResult ) )
        ( BoundaryWidth ( UpdateNetlistGetBoundaryWidthFromCompareResult CompareResult ) )
        ( BoundaryPosition ( UpdateNetlistGetBoundaryPositionFromCompareResult CompareResult ) ) 
        ( OldBoundaryHeight ( UpdateNetlistGetOldBoundaryHeightFromCompareResult CompareResult ) ) )
    
    ( foreach 
      TerminalToDelete
      TerminalsToDelete
      ( when TerminalToDelete ( dbDeleteObject
        TerminalToDelete ) ) )

    ( foreach
      ConnectionToDelete
      ConnectionsToDelete
      ( when ConnectionToDelete ( dbDeleteObject
        ConnectionToDelete ) ) )
    
    ( foreach
      NetToDelete
      NetsToDelete
      ( when NetToDelete ( dbDeleteObject
        NetToDelete ) ) )
    
    ( foreach
      InstanceToDelete
      InstancesToDelete
      ( when InstanceToDelete ( dbDeleteObject
        InstanceToDelete ) ) )

    if( InstancesToChange then println("Updating InstancesToChange..."))

    ( foreach
      InstanceToChange
      InstancesToChange
      (unless ErrorStr
        (let (
              ( InstName ( UpdateNetlistGetInstanceNameFromInstanceToChange InstanceToChange ) )
              ( InstCellName ( UpdateNetlistGetCellNameFromInstanceToChange InstanceToChange ) )
              ( InstLibName ( UpdateNetlistGetLibNameFromInstanceToChange InstanceToChange ) ) 
              ( InstToChangeDBObj ( UpdateNetlistGetInstanceDBObjFromInstanceToChange InstanceToChange ) )
              ( InstTransform ( UpdateNetlistGetInstanceTransformFromInstanceToChange InstanceToChange ) ) )

          ;TODO - remove...this will prevent swapping out of subtypes in
          ; layout that have no counterpart in the spec
          (when nil 
            (when ( rexMatchp ( sprintf nil "%s.*" InstCellName )
                              InstToChangeDBObj->cellName )
              ( setq InstCellName InstToChangeDBObj->cellName )
              ( printf "Not actually changing %s of type %s\n" 
                       InstToChangeDBObj->name
                       InstToChangeDBObj->cellName )
              )
            )

          (let (
                ( NewMasterCellViewDDObj
                  (let (
                        ( MasterViewDDObjWithSameViewName 
                          ( ddGetObj
                            InstLibName
                            InstCellName
                            InstToChangeDBObj~>viewName
                          ) ) )
                    (if MasterViewDDObjWithSameViewName
                        MasterViewDDObjWithSameViewName
                      (if ( equal ( getq CellView cellViewType ) "schematic" )
                          ( ddGetObj
                            InstLibName
                            InstCellName
                            "symbol" )
                        (if ( equal ( getq CellView cellViewType ) "maskLayout" )
                            ( ddGetObj
                              InstLibName
                              InstCellName
                              "layout" ) ) ) ) ) ) )
            (if NewMasterCellViewDDObj
                (let (
                      ( NewMasterCellView ( dbOpenCellViewByType
                                            InstLibName
                                            InstCellName
                                            ( getq NewMasterCellViewDDObj name )
                                            nil
                                            "r" ) ) )
                  (if NewMasterCellView
                      (if ( UpdateNetlistInstanceHasMaster
                            InstToChangeDBObj )
                          (let ()
                            ( dbSetq InstToChangeDBObj NewMasterCellView master )
                            ( dbSetq InstToChangeDBObj InstName name )
                            (when InstTransform
                              ( dbSetq InstToChangeDBObj InstTransform transform ) ) )
                        (let (
                              ( MyInstTransform (if InstTransform InstTransform ( getq InstToChangeDBObj transform ) ) )
                              ( InstOrient ( getq InstToChangeDBObj orient ) ) )
                          ( dbDeleteObject InstToChangeDBObj )
                          ( dbCreateInst
                            CellView
                            NewMasterCellView
                            InstName
                            ( car MyInstTransform )
                            ( cadr MyInstTransform ) ) ) )
                    ( setq
                      ErrorStr 
                      ( sprintf
                        nil
                        "DDObj for %L %L %L exists but the CellView could not be opened." 
                        InstLibName
                        InstCellName
                        ( getq CellView viewName ) ) ) ) )
              ( setq 
                ErrorStr
                ( sprintf
                  nil
                  "%L in %L does not have a %L view for %L in %L."
                  InstCellName
                  InstLibName
                  ( getq CellView viewName ) 
                  InstName
                  ( getq CellView cellName ) ) ) ) ) ) ) )

    if( InstancesToCreate then println("Updating InstancesToCreate..."))

    ( foreach
      InstanceToCreate
      InstancesToCreate
      (unless ErrorStr
        (let (
              ( InstName ( UpdateNetlistGetInstanceNameFromInstanceToCreate InstanceToCreate ) )
              ( InstLibName ( UpdateNetlistGetLibNameFromInstanceToCreate InstanceToCreate ) )
              ( InstCellName ( UpdateNetlistGetCellNameFromInstanceToCreate InstanceToCreate ) )
              ( InstTransform ( UpdateNetlistGetInstanceTransformFromInstanceToCreate InstanceToCreate ) ) )
          (let (
                ( NewMasterCellViewDDObj
                  (let (
                        ( MasterViewDDObjWithSameViewName
                          ( ddGetObj
                            InstLibName
                            InstCellName
                            ( getq CellView viewName ) ) ) )
                    (if MasterViewDDObjWithSameViewName
                        MasterViewDDObjWithSameViewName
                      (if ( equal ( getq CellView cellViewType ) "schematic" )
                          ( ddGetObj
                            InstLibName
                            InstCellName
                            "symbol" )
                        (if ( equal ( getq CellView cellViewType ) "maskLayout" )
                            ( ddGetObj
                              InstLibName
                              InstCellName
                              "layout" ) ) ) ) ) ) )
            (if NewMasterCellViewDDObj
                (let (
                      ( NewMasterCellView ( dbOpenCellViewByType
                                            InstLibName
                                            InstCellName
                                            ( getq NewMasterCellViewDDObj name )
                                            nil
                                            "r" ) ) )
                  (if NewMasterCellView
                      (let (
                            ( NewMasterBBox ( dbComputeBBoxNoNLP NewMasterCellView ) ) )
                        (let (
                              ( NewInst
                                ( dbCreateInst
                                  CellView
                                  NewMasterCellView
                                  InstName
                                  (if InstTransform
                                      ( car InstTransform )
                                    ( list
                                      ( difference 0 ( RectGetRight NewMasterBBox ) )
                                      ( difference CurrY ( RectGetTop NewMasterBBox ) ) ) )
                                  (if InstTransform
                                      ( cadr InstTransform ) 
                                    "R0" ) ) ) )
                          ( NameAddNonFoldableInstanceToInstanceMap 
                            InstanceMap
                            NewInst )
                          (unless InstTransform
                            ( setq CurrY ( difference 
                                           ( RectGetBottom ( getq NewInst bBox ) )
                                           6.0 ) )
                            

                           ) ) )
                    ( setq 
                      ErrorStr
                      ( sprintf
                        nil
                        "DDObj for %L %L %L exists but the CellView could not be opened." 
                        InstLibName
                        InstCellName
                        ( getq CellView viewName ) ) ) ) )
              ( setq
                ErrorStr
                ( sprintf
                  nil
                  "%L in %L does not have a %L view for %L in %L."
                  InstCellName
                  InstLibName
                  ( getq CellView viewName ) 
                  InstName
                  ( getq CellView cellName ) ) ) ) ) ) ) )
    if( ParametersToSet then println("Updating ParemetersToSet..."))
    
    ( foreach
      ParameterToSet
      ParametersToSet
      (let (
            ( InstanceName ( UpdateNetlistGetInstanceNameFromParameterToSet ParameterToSet ) )
            ( ParamName ( UpdateNetlistGetParameterNameFromParameterToSet ParameterToSet ) )
            ( ParamValue ( UpdateNetlistGetParameterValueFromParameterToSet ParameterToSet ) ) )
        (let (
              ( Instances ( NameGetInstancesForCanonicalName InstanceMap InstanceName ) ) )
          ( foreach Instance Instances 
                    (let (
                          ( ParamType ( PCellGetParamTypeForSchematicInstance Instance ParamName ) ) )
                      ( dbReplaceProp
                        Instance
                        ParamName
                        (if ParamType
                            ParamType
                          "float" )
                        (cond
                         (
                          ( equal ParamType "boolean" )
                          (if ( equal ParamValue 0.0 )
                              nil
                            t ) )
                         (
                          ( equal ParamType "string" )
                          ( sprintf nil "%e" ParamValue ) )
                         (
                          t
                          ParamValue ) ) ) ) ) ) ) )

    SyncNetlistInitFloorplan( CellView InstancesToCreate InstanceMap 
                              ?DoNotMoveExistingInstances DoNotMoveExistingInstances
                            )
      
    (unless ErrorStr
      ( foreach
        NetToCreate
        NetsToCreate
        ( dbCreateNet CellView NetToCreate ) ) )

    ( foreach
      NetToChangeSpec
      NetsToChange
      (let (
            ( NetToChange ( car NetToChangeSpec ) )
            ( WireSpacing ( cadr NetToChangeSpec ) )
            ( WireWidth ( caddr NetToChangeSpec ) )
            )
        (when WireSpacing
          ( ConstraintsReplaceNetWireSpacing
            ( dbFindNetByName CellView NetToChange )
            WireSpacing ) )
        (when WireWidth
          ( ConstraintsReplaceNetWireWidth
            ( dbFindNetByName CellView NetToChange )
            WireWidth )
        ) ) )

    ( foreach
      ConnectionToCreate
      ConnectionsToCreate
      (unless ErrorStr
        (let (
              ( ConnectionInstanceName ( UpdateNetlistGetInstanceNameFromConnectionToCreate
                                         ConnectionToCreate ) )
              ( ConnectionTerminalName ( UpdateNetlistGetTerminalNameFromConnectionToCreate
                                         ConnectionToCreate ) )
              ( ConnectionNetName ( UpdateNetlistGetNetNameFromConnectionToCreate
                                    ConnectionToCreate ) ) )
          (let (
                ( InstanceToConnectTo ( car
                                        ( NameGetInstancesForCanonicalName
                                          InstanceMap
                                          ConnectionInstanceName ) ) ) )
            (if InstanceToConnectTo
                (let (
                      ( TerminalToConnectTo ( ListFindElement
                                              ( getq ( getq InstanceToConnectTo master ) terminals )
                                              (lambda
                                                ( Terminal TerminalToFind )
                                                ( equal
                                                  ( getq Terminal name )
                                                  TerminalToFind ) )
                                              ( list ConnectionTerminalName ) ) ) )
                  (if TerminalToConnectTo then
                      OldInstTerm=car(setof( instTerm InstanceToConnectTo~>instTerms instTerm~>name==ConnectionTerminalName))
                      if(  OldInstTerm dbDeleteObject( OldInstTerm ))
                      ( dbCreateConn
                        ( dbMakeNet CellView ConnectionNetName )
                        InstanceToConnectTo
                        TerminalToConnectTo )
                   else ( printf
                      "WARNING: The master of instance %L in %L %L, %L %L %L, does not have a terminal %L.\n"
                      ( getq InstanceToConnectTo name )
                      ( getq CellView cellName )
                      ( getq CellView viewName )
                      ( getq InstanceToConnectTo libName )
                      ( getq InstanceToConnectTo cellName )
                      ( getq InstanceToConnectTo viewName )
                      ConnectionTerminalName ) ) )
              ( setq
                ErrorStr
                ( sprintf
                  nil
                  "Unable to find instance %L in %L %L %L."
                  ConnectionInstanceName
                  ( getq CellView libName )
                  ( getq CellView cellName )
                  ( getq CellView viewName ) ) ) ) ) ) ) )
    (unless ErrorStr
      ( foreach
        TerminalToCreate
        TerminalsToCreate
        ( dbCreateTerm
          ( dbMakeNet CellView TerminalToCreate )
          TerminalToCreate
          "inputOutput" ) ) )
    
    (unless ErrorStr
      (when BoundaryShape
        (if ( and BoundaryHeight BoundaryWidth BoundaryPosition )
            ( dbDeleteObject
              BoundaryShape )
          ( setq
            ErrorStr
            ( sprintf
              nil
              "%L %L Boundary needs to change, but either the height( %L ), width( %L ), or position( %L ) is nil."
              ( getq CellView cellName )
              ( getq CellView viewName )
              BoundaryHeight
              BoundaryWidth 
              BoundaryPosition ) ) ) ) )

    (unless ErrorStr
      (when ( and BoundaryHeight BoundaryWidth BoundaryPosition ) 
          println("Updating PRBoundary...")
          ( dbCreatePRBoundary
            CellView
            ( RectGetPolygonPoints ( RectMakeRect      
              ( times UserUnitsPerMeter ( car BoundaryPosition ) )
              ( times UserUnitsPerMeter ( cadr BoundaryPosition ) )
              ( times UserUnitsPerMeter BoundaryWidth )
              ( times UserUnitsPerMeter BoundaryHeight ) ) ) )
          ( SetXAlignmentProp CellView DefaultWiringPitch*2 )
          ( SetYAlignmentProp CellView GridPitch )
          ( SetLeafProp CellView t )
      )
    )

    (unless ErrorStr
      (when ( and 
              ( equal ( getq CellView cellViewType ) "maskLayout" )
              ( UpdateNetlistGetPinChangeFromCompareResult CompareResult )
              ( stringp ( UpdateNetlistGetPinsFileFromCompareResult CompareResult) )
              ( isFile ( UpdateNetlistGetPinsFileFromCompareResult CompareResult) )
              ( numberp ( UpdateNetlistGetWirePitchFromCompareResult CompareResult) )
              ( numberp ( UpdateNetlistGetWireWidthFromCompareResult CompareResult) )
              ( numberp ( UpdateNetlistGetWireSpacingFromCompareResult CompareResult) ) )

;        ( errset
          ( PinPlaceDrawUsingPDKInfo
            CellView
            ?PowerGrid nil
            ?InPlace t
            ?PinsDirectory nil ;use dfii cell dir
            ?ConnectivityViewName ( getq CellView viewName )
            ?PinsFile ( UpdateNetlistGetPinsFileFromCompareResult CompareResult)
            ?PinLength (GetPinLength CellView BoundaryLPP (UpdateNetlistGetWireSpacingFromCompareResult CompareResult))
            ?PinLPP
            ( list 
              ( car ( UpdateNetlistGetPinLPPFromCompareResult CompareResult) )
              "pin" )
            ?BoundaryPinHorizontalSpacing 
            ( quotient 
              ( UpdateNetlistGetWireSpacingFromCompareResult CompareResult)
              2.0 )
            ?BoundaryPinVerticalSpacing 0.0
            )
          ;)
        ) )
      
    ErrorStr ) )


(defun UpdateNetlistGetErrorsFromCompareResults ( CompareResults )
  (let (
        ( Result nil ) )
    ( foreach
      CellCompareResult
      CompareResults
      (let (
            ( LibName ( car ( cadr CellCompareResult ) ) )
            ( CellName ( cadr ( cadr CellCompareResult ) ) )
            ( ViewCheckResults ( caddr CellCompareResult ) ) )
        ( foreach
          ViewCheckResult
          ViewCheckResults
          (let (
                ( ViewName ( car ViewCheckResult ) )
                ( ViewCompareResult ( cadr ViewCheckResult ) )
                ( InstanceMap ( caddr ViewCheckResult ) ) )
            (when ViewCompareResult
              (let (
                    ( ErrorsForView ( UpdateNetlistGetErrorsFromCompareResult ViewCompareResult ) ) )
                (when ErrorsForView
                  ( setq
                    Result
                    ( tconc
                      Result
                      ErrorsForView ) ) ) ) ) ) ) ) )
                  
    ( car Result ) ) )

(defun UpdateNetlistUpdateCellsFromCompareResults ( CompareResults
                                                    WriteableCellViewTable
                                                    BoundaryLPP
                                                    UserUnitsPerMeter 
                                                    OutputPort )
  (let (
        ( Result nil ) )
    
    ( foreach
      CellCompareResult
      CompareResults
      (let (
            ( LibName ( car ( cadr CellCompareResult ) ) )
            ( CellName ( cadr ( cadr CellCompareResult ) ) )
            ( ViewCheckResults ( caddr CellCompareResult ) ) )
        ( foreach
          ViewCheckResult
          ViewCheckResults
          (let (
                ( ViewName ( car ViewCheckResult ) )
                ( ViewCompareResult ( cadr ViewCheckResult ) )
                ( InstanceMap ( caddr ViewCheckResult ) ) )
            (when ViewCompareResult
              ( setq
                Result
                ( tconc
                  Result
                  ( list
                    ( list LibName CellName ViewName )
                    (let (
                          ( CurrCellView ( arrayref 
                                           WriteableCellViewTable
                                           ( list LibName CellName ViewName ) ) ) )
                      (if CurrCellView
                          (let ()
                            ( printf "Updating %L %L\n" CellName ViewName )
                            (let (
                                  ( UpdateResult
                                    ( UpdateNetlistUpdateCellViewWithCompareResult
                                      CurrCellView
                                      ViewCompareResult
                                      InstanceMap
                                      BoundaryLPP
                                      UserUnitsPerMeter ) ) )
                              ( printf "Saving and Purging %L %L\n" CellName ViewName )
                              (if ( dbSave CurrCellView )
                                ( fprintf OutputPort "SAVED: %s %s\n" CellName ViewName ) )
                              ( dbReopen CurrCellView "r" )
                              UpdateResult ) )
                        ( sprintf
                          nil 
                          "Unable to get writeable view for %L %L %L.\n"
                          LibName 
                          CellName 
                          ViewName ) ) ) ) ) ) ) ) ) ) )
    ( car Result ) ) )

(defun UpdateNetlistUpdateCellsFromCompareResultsToPort ( CompareResults
                                                          WriteableCellViewTable
                                                          BoundaryLPP
                                                          UserUnitsPerMeter
                                                          OutputPort )
  (let (
        ( HadError nil ) )
    ( foreach
      UpdateResult
      ( UpdateNetlistUpdateCellsFromCompareResults
        CompareResults
        WriteableCellViewTable
        BoundaryLPP
        UserUnitsPerMeter 
        OutputPort )
      (let (
            ( ErrorStr ( cadr UpdateResult ) ) )
        (when ErrorStr
          ( setq HadError t )
          ( fprintf OutputPort "Error: %s\n" ErrorStr ) ) ) )
    HadError ) )

(defun UpdateNetlistGetCellViewTripplesToAddAndEditFromCompareResults ( CompareResults )
  (let (
        ( Result nil ) )
    ( foreach
      CellCompareResult
      CompareResults
      (let (
            ( LibName ( car ( cadr CellCompareResult ) ) )
            ( CellName ( cadr ( cadr CellCompareResult ) ) )
            ( ViewCheckResults ( caddr CellCompareResult ) ) )
        ( foreach
          ViewCheckResult
          ViewCheckResults
          (let (
                ( ViewName ( car ViewCheckResult ) )
                ( ViewCompareResult ( cadr ViewCheckResult ) ) )
            (when ViewCompareResult
              ( setq Result ( cons ( list LibName CellName ViewName ) Result ) ) ) ) ) ) )
    Result ) )
    
    
     
(defun UpdateNetlistMakeWriteableCellViewTableFromCompareResults ( CompareResult
                                                                   NetlistViewName )
  (let (
        ( Result ( makeTable "cellViewTable" nil ) )
        ( ErrorStr nil )
        ( ListOfLibCellViewTripplesToCreate nil )
        ( ListOfLibCellViewTripplesToEdit nil ) )
    ( foreach
      CellCompareResult
      CompareResults
      (let (
            ( LibName ( car ( cadr CellCompareResult ) ) )
            ( CellName ( cadr ( cadr CellCompareResult ) ) )
            ( ViewCheckResults ( caddr CellCompareResult ) ) )
        ( foreach
          ViewCheckResult
          ViewCheckResults
          (let (
                ( ViewName ( car ViewCheckResult ) )
                ( ViewCompareResult ( cadr ViewCheckResult ) ) )
            (when ViewCompareResult
              (let (
                    ( CellViewDDObj ( ddGetObj LibName CellName ViewName ) ) )
                (if CellViewDDObj
                    ( setq
                      ListOfLibCellViewTripplesToEdit
                      ( tconc
                        ListOfLibCellViewTripplesToEdit
                        ( list LibName CellName ViewName ) ) )
                  ( setq
                    ListOfLibCellViewTripplesToCreate
                    ( tconc
                      ListOfLibCellViewTripplesToCreate
                      ( list LibName CellName ViewName ) ) ) ) ) ) ) ) ) )
    ( foreach
      LibCellViewTripple
      ( car ListOfLibCellViewTripplesToCreate )
      (let ()
        ( printf
          "Creating %L %L\n"
          ( cadr LibCellViewTripple )
          ( caddr LibCellViewTripple ) )
        (let (
              ( CellView ( dbOpenCellViewByType
                           ( car LibCellViewTripple )
                           ( cadr LibCellViewTripple )
                           ( caddr LibCellViewTripple )
                           (if ( and 
                                 NetlistViewName 
                                 ( equal 
                                   ( caddr LibCellViewTripple ) 
                                   NetlistViewName ) )
                               "schematic"
                             "maskLayout" )
                           "w" ) ) )
          (when CellView
            ( setarray Result LibCellViewTripple CellView ) ) ) ) )
    ( foreach
      LibCellViewTripple
      ( car ListOfLibCellViewTripplesToEdit )
      (let (
            ( LibName ( car LibCellViewTripple ) )
            ( CellName ( cadr LibCellViewTripple ) )
            ( ViewName ( caddr LibCellViewTripple ) ) )
        (let (
              ( CellViewDDObj ( ddGetObj LibName CellName ViewName ) ) )
          (when ( and
                  CellViewDDObj
                  ( getq CellViewDDObj files )
                  ( forall
                    FileDDObj
                    ( getq CellViewDDObj files )
                    ( ddIsObjWritable FileDDObj ) ) )
            ( printf "Opening %L %L %L for append.\n" LibName CellName ViewName )
            (let (
                  ( CellView ( dbOpenCellViewByType
                               LibName
                               CellName
                               ViewName
                               (if ( and NetlistViewName ( equal ViewName NetlistViewName ) )
                                   "schematic"
                                 "maskLayout" )
                               "a" ) ) )
              ( setarray Result LibCellViewTripple CellView ) ) ) ) ) )
    (if ErrorStr
        ErrorStr
      Result ) ) )

(defun UpdateNetlistUpdateCellsFromSkillNetlistsToFileUsingPDKInfo ( LibCellPairList
                                                                     NetlistViewName
                                                                     LayoutViewName
                                                                     FloorplanViewName
                                                                     SkillNetlistDir
                                                                     DirectivesSkillDir
                                                                     AutopinsSkillDir
                                                                     DefaultDensityFactor
                                                                     BoundaryScalingFactor
                                                                     FileName
                                                                     LockBound
                                                                     SuppressPins
                                                                     ForcePins
                                                                     UseLayoutHeight
                                                                     LockLayoutView )



  (let (
        ( FilteredLibCellPairList 
          ( setof
            LibCellPair
            LibCellPairList
            ( not
              ( or
                ( equal ( car LibCellPair ) StackLibraryName )
                ( equal ( car LibCellPair ) GateLibraryName ) ) ) ) ) )
    (let (
          ( DirectivesTable
            ( CellInfoGetTableForCellName
              ( cadr ( car FilteredLibCellPairList ) )
              DirectivesSkillDir ) ) )
      (let (
          ( BoundaryPosition
            (CellInfoLookupWithDefault
             DirectivesTable "boundary_position" BoundaryPosition))
          ( BoundaryWidthIncrement
            (CellInfoLookupWithDefault
             DirectivesTable "boundary_width_increment" BoundaryWidthIncrement))
          ( BoundaryWidthWarningCutOff
            (CellInfoLookupWithDefault
             DirectivesTable "boundary_width_warning_cutoff" BoundaryWidthWarningCutOff))
          ( BoundarySmallCellWarningCutOff
            (CellInfoLookupWithDefault
             DirectivesTable "boundary_small_cell_warning_cutoff" BoundarySmallCellWarningCutOff))
          )
        
    (let (
          ( InstanceTransformFunctionTable ( UpdateNetlistGetFunctionTableForDefaultInstanceTransformFunctions
                                            NetlistViewName
                                            LayoutViewName
                                            FloorplanViewName ) )
          ( HeightWidthFunctionTable ( UpdateNetlistGetFunctionTableForDefaultWidthAndHeightAndPositionFunctions
                                       NetlistViewName
                                       LayoutViewName
                                       FloorplanViewName
                                       BoundaryLPP
                                       UserUnitsPerMeter
                                       BoundaryWidthIncrement
                                       BoundaryWidthWarningCutOff
                                       DefaultDensityFactor
                                       BoundaryScalingFactor
                                       BoundarySmallCellWarningCutOff
                                       UseLayoutHeight
                                       BoundaryPosition ) )
          ( NetlistTransformationFunctionTable ( UpdateNetlistGetFunctionTableForNetlistTransformationFunctions
                                                 NetlistViewName
                                                 LayoutViewName
                                                 FloorplanViewName
                                                 DirectivesSkillDir ) ) )
      (let (
            ( OutputPort ( outfile FileName "a" ) )
            ( CompareResults ( UpdateNetlistCompareCellsToSkillNetlists
                               FilteredLibCellPairList
                               ( cons
                                 ( list ".*" ".+\\.wires\\..+" )
                                 ( cons
                                   ( list 
                                     ( sprintf nil "^%s$" GateLibraryName )
                                     ".*" )
                                   ( cons 
                                     ( list 
                                       ( sprintf nil "^%s$" StackLibraryName )
                                       ".*" )
                                     ( cons
                                       ( list TechLibName ".*" )
                                     ( cons
                                       ( list "^$" "" )
                                       TransistorLibCellPairs
                                       ) ) ) ) )
                               TransistorLibCellPairs
                               ( setof
                                 ViewName
                                 ( list
                                   FloorplanViewName
                                   NetlistViewName
                                   LayoutViewName )
                                 ViewName )
                               ( setof
                                 ViewName
                                 ( list
                                   FloorplanViewName
                                   NetlistViewName
                                   (unless LockLayoutView
                                     LayoutViewName ) )
                                 ViewName )
                               ( list LayoutViewName )
                               NetlistViewName
                               SkillNetlistDir
                               DirectivesSkillDir
                               AutopinsSkillDir
                               BoundaryLPP
                               UserUnitsPerMeter
                               DirectiveUnitsPerMeter
                               ( UpdateNetlistGetDefaultGetInstanceTransformFunc )
                               ( list InstanceTransformFunctionTable )
                               ( UpdateNetlistGetDefaultGetCellHeightFunc )
                               ( list HeightWidthFunctionTable )
                               ( UpdateNetlistGetDefaultGetCellWidthFunc )
                               ( list HeightWidthFunctionTable )
                               ( UpdateNetlistGetDefaultGetCellPositionFunc )
                               ( list HeightWidthFunctionTable )
                               ( UpdateNetlistGetDefaultNetlistTransformationFunc )
                               ( list NetlistTransformationFunctionTable )
                               LockBound
                               SuppressPins
                               ForcePins ) ) )
        ( UpdateNetlistAppendCompareResultsToPort
          CompareResults
          OutputPort )
        (unless ( UpdateNetlistGetErrorsFromCompareResults CompareResults )
          (let (
                ( CellViewTable ( UpdateNetlistMakeWriteableCellViewTableFromCompareResults
                                  CompareResults
                                  NetlistViewName ) ) )
            (if ( tablep CellViewTable )
                ( UpdateNetlistUpdateCellsFromCompareResultsToPort
                  CompareResults
                  CellViewTable
                  BoundaryLPP
                  UserUnitsPerMeter
                  OutputPort )
              ( fprintf OutputPort "Error: %s\n" CellViewTable ) ) ) )
        ( close OutputPort ) ) ) )
  nil ) ) )


(defun UpdateNetlistGenFromSource ( TargetCellView
                                    LibCellExpressionPairsToIgnore
                                    FoldableLibCellExpressionPairs
                                    ConnectivityInstanceExistsFunc 
                                    ConnectivityInstanceExistsFuncParams
                                    ConnectivityGetInstanceMasterLibNameFunc
                                    ConnectivityGetInstanceMasterLibNameFuncParams
                                    ConnectivityGetInstanceMasterCellNameFunc
                                    ConnectivityGetInstanceMasterCellNameFuncParams
                                    ConnectivityInstanceTripples
                                    ConnectivityGetInstanceParameterPairsFunc
                                    ConnectivityGetInstanceParameterPairsFuncParams
                                    ConnectivityGetInstanceTransformFunc
                                    ConnectivityGetInstanceTransformFuncParams
                                    ConnectivityInstanceHasConnectionFunc
                                    ConnectivityInstanceHasConnectionFuncParams
                                    ConnectivityGetInstanceConnectionsFunc
                                    ConnectivityGetInstanceConnectionsFuncParams 
                                    ConnectivityTerminalExistsFunc 
                                    ConnectivityTerminalExistsFuncParams 
                                    ConnectivityTerminalNames
                                    ConnectivityNetExistsFunc 
                                    ConnectivityNetExistsFuncParams 
                                    ConnectivityNetNames 
                                    ConnectivityGetNetWireSpacingFunc
                                    ConnectivityGetNetWireSpacingFuncParams
                                    ConnectivityGetNetWireWidthFunc
                                    ConnectivityGetNetWireWidthFuncParams
                                    ConnectivityGetLayerWireSpacingFunc
                                    ConnectivityGetLayerWireSpacingFuncParams
                                    ConnectivityGetLayerWireWidthFunc
                                    ConnectivityGetLayerWireWidthFuncParams
                                    ConnectivityGetGlobalWireSpacingFunc
                                    ConnectivityGetGlobalWireSpacingFuncParams
                                    ConnectivityGetGlobalWireWidthFunc
                                    ConnectivityGetGlobalWireWidthFuncParams
                                    BoundaryLPP
                                    UserUnitsPerMeter
                                    @key
                                    ( Verbose t ) )
  
  (let (
        ( Ret nil )
        ( InstanceNames 
          ( ListApplyFuncToListAndAccumulateResults
            ( car
              ( NameFilterInstanceTripples
                ConnectivityInstanceTripples
                LibCellExpressionPairsToIgnore ) )
            (lambda
              ( InstanceTripple )
              ( car InstanceTripple ) )
            nil ) )
        ( InstanceMap ( NameMakeEmptyInstanceMap ) )
        ( Instances (when TargetCellView
                      ( UpdateNetlistFilterInstances
                        TargetCellView
                        LibCellExpressionPairsToIgnore ) ) ) )
    ( NamePopulateInstanceMapWithInstances
        InstanceMap
        Instances
        FoldableLibCellExpressionPairs )
      (let (
            ( CompareResult ( UpdateNetlistCompareCellView
                              TargetCellView
                              LibCellExpressionPairsToIgnore
                              FoldableLibCellExpressionPairs
                              InstanceMap
                              Instances
                              ConnectivityInstanceExistsFunc 
                              ConnectivityInstanceExistsFuncParams
                              ConnectivityGetInstanceMasterLibNameFunc
                              ConnectivityGetInstanceMasterLibNameFuncParams
                              ConnectivityGetInstanceMasterCellNameFunc
                              ConnectivityGetInstanceMasterCellNameFuncParams
                              InstanceNames
                              ConnectivityGetInstanceParameterPairsFunc
                              ConnectivityGetInstanceParameterPairsFuncParams
                              ConnectivityGetInstanceTransformFunc
                              ConnectivityGetInstanceTransformFuncParams
                              ConnectivityInstanceHasConnectionFunc
                              ConnectivityInstanceHasConnectionFuncParams
                              ConnectivityGetInstanceConnectionsFunc
                              ConnectivityGetInstanceConnectionsFuncParams 
                              ConnectivityTerminalExistsFunc 
                              ConnectivityTerminalExistsFuncParams 
                              ConnectivityTerminalNames
                              ConnectivityNetExistsFunc 
                              ConnectivityNetExistsFuncParams 
                              ConnectivityNetNames 
                              ConnectivityGetNetWireSpacingFunc
                              ConnectivityGetNetWireSpacingFuncParams
                              ConnectivityGetNetWireWidthFunc
                              ConnectivityGetNetWireWidthFuncParams
                              ConnectivityGetLayerWireSpacingFunc
                              ConnectivityGetLayerWireSpacingFuncParams
                              ConnectivityGetLayerWireWidthFunc
                              ConnectivityGetLayerWireWidthFuncParams
                              ConnectivityGetGlobalWireSpacingFunc
                              ConnectivityGetGlobalWireSpacingFuncParams
                              ConnectivityGetGlobalWireWidthFunc
                              ConnectivityGetGlobalWireWidthFuncParams
                              (lambda
                                ( LibName CellName ViewName HeightOfExistingBoundaryInMeters BlaBlabl )
                                nil )
                              nil
                              (lambda
                                ( LibName
                                  CellName
                                  ViewName
                                  CellHeightInMeters )
                                nil )
                              nil
                              (lambda
                                ()
                                nil )
                              nil
                              (lambda
                                ( x y)
                                nil )
                              nil
                              BoundaryLPP
                              UserUnitsPerMeter
                              nil 
                              nil
                              nil ) ) )
        (when CompareResult
          (when Verbose
            ( UpdateNetlistPrintDifferences 
              CompareResult
              poport ) )
          ( setq
            Ret
            ( UpdateNetlistUpdateCellViewWithCompareResult
              TargetCellView
              CompareResult
              InstanceMap
              BoundaryLPP
              UserUnitsPerMeter ) )
          (when Ret ( printf "Error: %L\n" Ret ) )
          )
        Ret ) ) )

(defun UpdateNetlistGenFromSkillNetlist ( TargetCellView
                                          LibCellExpressionPairsToIgnore
                                          FoldableLibCellExpressionPairs
                                          BoundaryLPP
                                          WidthEstimationFunc 
                                          WidthEstimationFuncParams
                                          SkillNetlistDir
                                          SkillDirectivesDir
                                          UserUnitsPerMeter
                                          DirectiveUnitsPerMeter
                                          )
    NetlistTable = NetlistTableGetSkillNetlistTableForCellName( TargetCellView~>cellName SkillNetlistDir ) 
    if( tablep( NetlistTable ) then
      DirectivesTable= CellInfoGetTableForCellName( TargetCellView~>cellName SkillDirectivesDir )
      if( tablep( DirectivesTable ) then
         TransformedNetlistTable=InlineConnectionsHierTransformTable(
                        NetlistTable
                        UpdateNetlistGetDefaultShouldInLineConnectionsFunc( )
                        list( DirectivesTable SkillDirectivesDir ) 
                        SkillNetlistDir ) 
         TransformedNetlistsTablesTable=makeTable( "tableOfNetlistTables" nil )
         TransformedNetlistsTablesTable[TargetCellView~>viewName]=TransformedNetlistTable
         InstancesTable=NetListTable["i"] 
         NetsTable=NetListTable["n"] 
         StatisticsTable=NetlistTable["s"]
         UpdateNetlistGenFromSource(
                    TargetCellView
                    LibCellExpressionPairsToIgnore
                    FoldableLibCellExpressionPairs
                    ( UpdateNetlistGetSkillNetlistInstanceExistsFunc )
                    ( list ( getq TargetCellView viewName ) TransformedNetlistsTablesTable )
                    ( UpdateNetlistGetSkillNetlistGetInstanceMasterLibNameFunc )
                    ( list ( getq TargetCellView viewName ) TransformedNetlistsTablesTable )
                    ( UpdateNetlistGetSkillNetlistGetInstanceMasterCellNameFunc )
                    ( list ( getq TargetCellView viewName ) TransformedNetlistsTablesTable )
                    ( NetlistTableGetInstanceTripples TransformedNetlistTable )
                    ( UpdateNetlistGetSkillNetlistGetInstanceParameterPairsFunc )
                    ( list ( getq TargetCellView viewName ) TransformedNetlistsTablesTable )
                    (lambda ( InstanceName ) nil )
                    nil
                    ( UpdateNetlistGetSkillNetlistInstanceHasConnectionFunc )
                    ( list ( getq TargetCellView viewName ) TransformedNetlistsTablesTable )
                    ( UpdateNetlistGetSkillNetlistGetInstanceConnectionsFunc )
                    ( list ( getq TargetCellView viewName ) TransformedNetlistsTablesTable )
                    ( UpdateNetlistGetSkillNetlistTerminalExistsFunc )
                    ( list ( getq TargetCellView viewName ) TransformedNetlistsTablesTable )
                    ( NetlistTableGetTerminalNames TransformedNetlistTable )
                    ( UpdateNetlistGetSkillNetlistNetExistsFunc )
                    ( list ( getq TargetCellView viewName ) TransformedNetlistsTablesTable )
                    ( NetlistTableGetNetNames TransformedNetlistTable )
                    ( UpdateNetListGetSkillGetNetlistNetWireSpacingFunc )
                    ( list DirectivesTable DirectiveUnitsPerMeter )
                    ( UpdateNetListGetSkillGetNetlistNetWireWidthFunc )
                    ( list DirectivesTable DirectiveUnitsPerMeter )
                    ( UpdateNetListGetSkillGetNetlistLayerWireSpacingFunc )
                    ( list DirectivesTable DirectiveUnitsPerMeter )
                    ( UpdateNetListGetSkillGetNetlistLayerWireWidthFunc )
                    ( list DirectivesTable DirectiveUnitsPerMeter )
                    ( UpdateNetListGetSkillGetNetlistGlobalWireSpacingFunc )
                    ( list DirectivesTable DirectiveUnitsPerMeter )
                    ( UpdateNetListGetSkillGetNetlistGlobalWireWidthFunc )
                    ( list DirectivesTable DirectiveUnitsPerMeter )
                    BoundaryLPP
                    UserUnitsPerMeter ) 
            
      else DirectivesTable )
    else NetlistTable ) 
) 


(defun UpdateNetlistGenFromSkillNetlistUsingPDKInfo ( TargetCellView
                                                      SkillNetlistDir
                                                      SkillDirectivesDir
                                                      @key
                                                      ( LibCellsToIgnore nil )
                                                      )
  ( UpdateNetlistGenFromSkillNetlist
    TargetCellView
    ( append 
      LibCellsToIgnore
      ( append
        WiringCellLibCellPairRegExs
        TechLibExceptTransistorsLibCellPairRegExs
        ) )
    FoldableCellLibCellPairRegExs
    BoundaryLPP
    (lambda
      ( LibName
        CellName
        ViewName
        TotalTransistorGateAreaInMetersSquared
        TotalTransistorGateLengthInMeters
        CellHeightInMeters
        DirectivesTable )
      nil )
    nil
    SkillNetlistDir
    SkillDirectivesDir
    UserUnitsPerMeter
    DirectiveUnitsPerMeter ) )

(defun UpdateNetlistEasyGen ( CellName
                              ViewName
                              SkillNetlistDir
                              SkillDirectivesDir )
  
  (let (
        ( LibCellPair ( NameParseCellName CellName ) ) )
    (let ( 
          ( CellView
            ( dbOpenCellViewByType  
              ( car LibCellPair )
              ( cadr LibCellPair )
              ViewName
              "maskLayout"
              "w" ) ) )
      ( UpdateNetlistGenFromSkillNetlistUsingPDKInfo
        CellView
        SkillNetlistDir
        SkillDirectivesDir ) ) ) )
