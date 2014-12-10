; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$


(defun UpdateFloorplanTablifyInstances ( Instances TargetTable )
  ( foreach
    Instance
    Instances
    ( setarray
      TargetTable
      ( NameCanonicalizeNonFoldableInstanceName ( getq Instance name ) )
      Instance ) ) )

(defun UpdateFloorplanComparePRBounds ( FloorplanCellView CellView PRBoundLPP )
  (let (
        ( FloorplanBoundShape ( car 
                                ( setof
                                  Shape
                                  ( getq FloorplanCellView shapes )
                                  ( and
                                    ( equal ( car ( getq Shape lpp ) ) ( car PRBoundLPP ) )
                                    ( equal ( cadr ( getq Shape lpp ) ) ( cadr PRBoundLPP ) ) ) ) ) )
        ( LayoutBoundShape ( car 
                             ( setof
                               Shape
                               ( getq CellView  shapes )
                               ( and
                                 ( equal ( car ( getq Shape lpp ) ) ( car PRBoundLPP ) )
                                 ( equal ( cadr ( getq Shape lpp ) ) ( cadr PRBoundLPP ) ) ) ) ) ) )
    ( dbComputeBBox CellView )
    (when FloorplanBoundShape
      (let (
            ( FloorplanBBox ( getq FloorplanBoundShape bBox ) )
            ( LayoutBBox (if LayoutBoundShape
                             ( getq LayoutBoundShape bBox )
                           ( getq CellView bBox ) ) ) )
        (unless ( and
                  ( equal ( RectGetLeft FloorplanBBox ) ( RectGetLeft LayoutBBox ) )
                  ( equal ( RectGetRight FloorplanBBox ) ( RectGetRight LayoutBBox ) )
                  ( equal ( RectGetBottom FloorplanBBox ) ( RectGetBottom LayoutBBox ) )
                  ( equal ( RectGetTop FloorplanBBox ) ( RectGetTop LayoutBBox ) ) )
          ( list
            LayoutBBox
            FloorplanBBox ) ) ) ) ) )
        

(defun UpdateFloorplanCompareInstances ( FloorplanInstance Instance )
  (let (
        ( FloorplanLibName ( getq FloorplanInstance libName ) )
        ( LibName ( getq Instance libName ) )
        ( FloorplanCellName ( getq FloorplanInstance cellName ) )
        ( CellName ( getq Instance cellName ) )
        ( FloorplanPosition ( getq FloorplanInstance xy ) )
        ( Position ( getq Instance xy ) )
        ( FloorplanOrientation ( getq FloorplanInstance orient ) )
        ( Orientation ( getq Instance orient ) ) )
    (let (
          ( CompareResult
            ( list
              (unless ( and
                        ( equal FloorplanLibName LibName )
                        ( equal FloorplanCellName CellName  ) )
                ( list
                  ( list
                    LibName
                    CellName )
                  ( list
                    FloorplanLibName
                    FloorplanCellName ) ) )
              (unless ( and
                        ( equal ( car FloorplanPosition ) ( car Position ) )
                        ( equal ( cadr FloorplanPosition ) ( cadr Position ) ) )
                ( list
                  Position
                  FloorplanPosition ) )
              (unless ( equal FloorplanOrientation Orientation )
                ( list
                  Orientation
                  FloorplanOrientation ) ) ) ) )
      (unless ( forall Entry CompareResult ( null Entry ) )
        ( cons
          ( list Instance FloorplanInstance )
          CompareResult ) ) ) ) )

(defun UpdateFloorplanGetInstancesFromInstanceCompareResult ( InstanceCompareResult )
  ( car InstanceCompareResult ) )

(defun UpdateFloorplanGetMasterCellLibCellPairsFromInstanceCompareResult ( InstanceCompareResult )
  ( nth 1 InstanceCompareResult ) )

(defun UpdateFloorplanGetPositionsFromInstanceCompareResult ( InstanceCompareResult )
  ( nth 2 InstanceCompareResult ) )

(defun UpdateFloorplanGetOrientationsFromInstanceCompareResult ( InstanceCompareResult )
  ( nth 3 InstanceCompareResult ) )

(defun UpdateFloorplanMakeCompareResult ( CellView
                                          FloorplanCellView
                                          ExtraLayoutInstances
                                          MissingLayoutInstances
                                          InstancesCompareResult
                                          BoundaryCompareResult )
  ( list
    CellView
    FloorplanCellView
    ( getq CellView cellName )
    ExtraLayoutInstances
    MissingLayoutInstances
    InstancesCompareResults
    BoundaryCompareResult ) )

(defun UpdateFloorplanGetCellViewFromCompareResult ( CompareResult )
  ( car CompareResult ) )

(defun UpdateFloorplanGetFloorplanCellViewFromCompareResult ( CompareResult )
  ( nth 1 CompareResult ) )

(defun UpdateFloorplanGetCellNameFromCompareResult ( CompareResult )
  ( nth 2 CompareResult ) )

(defun UpdateFloorplanGetExtaLayoutInstancesFromCompareResult ( CompareResult )
  ( nth 3 CompareResult ) )

(defun UpdateFloorplanGetMissingLayoutInstancesFromCompareResult ( CompareResult )
  ( nth 4 CompareResult ) )

(defun UpdateFloorplanGetInstancesCompareResultsFromCompareResult ( CompareResult )
  ( nth 5 CompareResult ) )

(defun UpdateFloorplanGetBoundaryCompareResultFromCompareResult ( CompareResult )
  ( nth 6 CompareResult ) )
 
(defun UpdateFloorplanCompareCellViewToFloorplanView ( FloorplanCellView 
                                                       CellView 
                                                       LibCellExpressionPairsToIgnore
                                                       PRBoundLPP )
  (let (
        ( FloorplanInstances ( getq 
                               FloorplanCellView
                               instances ) )
        ( Instances ( car
                      ( NameFilterInstances
                        ( getq CellView instances )
                        LibCellExpressionPairsToIgnore ) ) )
        ( FloorplanInstanceTable ( makeTable "instanceTable" nil ) )
        ( InstanceTable ( makeTable "instanceTable" nil ) ) )
    ( UpdateFloorplanTablifyInstances FloorplanInstances FloorplanInstanceTable )
    ( UpdateFloorplanTablifyInstances Instances InstanceTable )
    (let (
          ( ExtraLayoutInstances ( setof
                                   Instance
                                   Instances
                                   ( null 
                                     ( arrayref 
                                       FloorplanInstanceTable
                                       ( NameCanonicalizeNonFoldableInstanceName
                                         ( getq Instance name ) ) ) ) ) )
          ( MissingLayoutInstances ( setof
                                     FloorplanInstance
                                     FloorplanInstances
                                     ( null
                                       ( arrayref
                                         InstanceTable
                                         ( NameCanonicalizeNonFoldableInstanceName
                                           ( getq FloorplanInstance name ) ) ) ) ) )
          ( FloorplanInstancesToCheck ( setof
                               FloorplanInstance
                               FloorplanInstances
                               ( arrayref 
                                 InstanceTable
                                 ( NameCanonicalizeNonFoldableInstanceName
                                   ( getq FloorplanInstance name ) ) ) ) ) )      
      (let (
            ( BoundaryCompareResult ( UpdateFloorplanComparePRBounds
                                      FloorplanCellView
                                      CellView
                                      PRBoundLPP ) )
            ( InstancesCompareResults ( ListApplyFuncToListAndAccumulateNonNilResults
                                        FloorplanInstancesToCheck
                                        (lambda ( FloorplanInstanceToCheck InstanceTable )
                                          (let (
                                                ( Instance ( arrayref
                                                             InstanceTable
                                                             ( NameCanonicalizeNonFoldableInstanceName
                                                               ( getq FloorplanInstanceToCheck name ) ) ) ) )
                                            ( UpdateFloorplanCompareInstances
                                              FloorplanInstanceToCheck
                                              Instance ) ) )
                                        ( list InstanceTable ) ) ) )
        (when ( or InstancesCompareResults ExtraLayoutInstances MissingLayoutInstances BoundaryCompareResult )
          ( UpdateFloorplanMakeCompareResult
            CellView
            FloorplanCellView
            ExtraLayoutInstances
            MissingLayoutInstances
            InstancesCompareResults
            BoundaryCompareResult ) ) ) ) ) )


(defun UpdateFloorplanHierCompareCellViewsToFloorplanViews ( RootCellViews
                                                             LibCellExpressionPairsToIgnore
                                                             FloorplanViewName
                                                             PRBoundLPP )
  (let (
        ( CellViewsInTree ( HierarchyGetCellDescendantsOfCells
                            RootCellViews
                            LibCellExpressionPairsToIgnore
                            (lambda ( CellView ) nil )
                            nil ) ) )
    ( ListApplyFuncToListAndAccumulateNonNilResults
      CellViewsInTree
      (lambda
        ( CellView LibCellExpressionPairsToIgnore FloorplanViewName )
        (let (
              ( LibName ( getq CellView libName ) )
              ( CellName ( getq CellView cellName ) ) )
          (let (
                ( FloorplanCellViewDDObj ( ddGetObj LibName CellName FloorplanViewName ) ) )
            (if FloorplanCellViewDDObj
                (let (
                      ( FloorplanCellView ( dbOpenCellViewByType
                                            LibName
                                            CellName
                                            FloorplanViewName
                                            nil
                                            "r" ) ) )
                  ( UpdateFloorplanCompareCellViewToFloorplanView
                    FloorplanCellView
                    CellView
                    LibCellExpressionPairsToIgnore
                    PRBoundLPP ) )
              ( list CellView nil ( getq CellView cellName ) nil nil nil nil ) ) ) ) )
      ( list LibCellExpressionPairsToIgnore FloorplanViewName ) ) ) )

(defun UpdateFloorplanPrintDifferences ( OutputPort CompareResults )
  ( foreach
    CompareResult
    CompareResults
    (let (
          ( CellName ( UpdateFloorplanGetCellNameFromCompareResult CompareResult ) )
          ( FloorplanCellView ( UpdateFloorplanGetFloorplanCellViewFromCompareResult CompareResult ) )
          ( ExtraInstances ( UpdateFloorplanGetExtaLayoutInstancesFromCompareResult CompareResult ) )
          ( MissingInstances ( UpdateFloorplanGetMissingLayoutInstancesFromCompareResult CompareResult ) )
          ( InstancesCompareResults ( UpdateFloorplanGetInstancesCompareResultsFromCompareResult
                                      CompareResult ) )
          ( BoundaryCompareResult ( UpdateFloorplanGetBoundaryCompareResultFromCompareResult
                                    CompareResult ) ) )
      (if FloorplanCellView
          (let ()
            ( foreach
              ExtraInstance
              ExtraInstances
              ( fprintf OutputPort "%L EXTRA %L\n" CellName ( getq ExtraInstance name ) ) )
            ( foreach
              MissingInstance
              MissingInstances
              ( fprintf OutputPort "%L MISSING %L\n" CellName ( getq MissingInstance name ) ) )
            
            ( foreach
              InstanceCompareResult
              InstancesCompareResults
              (let (
                    ( Instances ( UpdateFloorplanGetInstancesFromInstanceCompareResult
                                  InstanceCompareResult ) )
                    ( MasterCellLibCellPairs ( UpdateFloorplanGetMasterCellLibCellPairsFromInstanceCompareResult
                                               InstanceCompareResult ) )
                    ( Positions ( UpdateFloorplanGetPositionsFromInstanceCompareResult
                                  InstanceCompareResult ) )
                    ( Orientations ( UpdateFloorplanGetOrientationsFromInstanceCompareResult
                                     InstanceCompareResult ) ) )
                (when MasterCellLibCellPairs
                  ( fprintf 
                    OutputPort 
                    "%L MASTER %L %L %L\n" 
                    CellName 
                    ( getq ( car Instances ) name ) 
                    ( cadr MasterCellLibCellPairs ) 
                    ( car MasterCellLibCellPairs ) ) )
                (when Positions
                  ( fprintf 
                    OutputPort
                    "%L POS %L %L %L\n"
                    CellName
                    ( getq ( car Instances ) name )
                    ( cadr Positions )
                    ( car Positions ) ) )
                (when Orientations
                  ( fprintf 
                    OutputPort 
                    "%L ORIENT %L %L %L\n" 
                    CellName 
                    ( getq ( car Instances ) name ) 
                    ( cadr Orientations ) 
                    ( car Orientations ) ) ) ) )
            (when BoundaryCompareResult
              ( fprintf OutputPort "%L BOUND %L %L\n" CellName ( cadr BoundaryCompareResult ) ( car BoundaryCompareResult ) ) ) )
        ( fprintf OutputPort "#%L did not have a floorplan view.\n" CellName ) ) ) ) 
  nil )
      
(defun UpdateFloorplanDoUpdate ( CompareResults PRBoundLPP )
  (let (
        ( Errors nil ) )
    ( foreach
      CompareResult
      CompareResults
      (let (
            ( CellName ( UpdateFloorplanGetCellNameFromCompareResult CompareResult ) )
            ( FloorplanCellView ( UpdateFloorplanGetFloorplanCellViewFromCompareResult CompareResult ) )
            ( ExtraInstances ( UpdateFloorplanGetExtaLayoutInstancesFromCompareResult CompareResult ) )
            ( MissingInstances ( UpdateFloorplanGetMissingLayoutInstancesFromCompareResult CompareResult ) )
            ( InstancesCompareResults ( UpdateFloorplanGetInstancesCompareResultsFromCompareResult
                                        CompareResult ) )
            ( BoundaryCompareResult ( UpdateFloorplanGetBoundaryCompareResultFromCompareResult
                                      CompareResult ) ) )
        (if FloorplanCellView
            (let (
                  ( FloorplanCellViewDDObj ( ddGetObj
                                             ( getq FloorplanCellView libName )
                                             ( getq FloorplanCellView cellName )
                                             ( getq FloorplanCellView viewName ) ) ) )
              (when ( not ( equal ( getq FloorplanCellView viewName ) "floorplan" ) )
                ( error "floorplan view was not named \"floorplan\"" ) )
              (if ( and
                    FloorplanCellViewDDObj
                    ( car ( getq FloorplanCellViewDDObj files ) )
                    ( forall
                      File
                      ( getq FloorplanCellViewDDObj files )
                      ( ddIsObjWritable File ) ) )
                  (let ()
                    ( dbReopen FloorplanCellView "a" )
                    ( foreach
                      InstanceCompareResult
                      InstancesCompareResults
                      (let (
                            ( Instances ( UpdateFloorplanGetInstancesFromInstanceCompareResult
                                          InstanceCompareResult ) )
                            ( MasterCellLibCellPairs ( UpdateFloorplanGetMasterCellLibCellPairsFromInstanceCompareResult
                                                       InstanceCompareResult ) )
                            ( Positions ( UpdateFloorplanGetPositionsFromInstanceCompareResult
                                          InstanceCompareResult ) )
                            ( Orientations ( UpdateFloorplanGetOrientationsFromInstanceCompareResult
                                             InstanceCompareResult ) ) )
                        (let (
                              ( FloorplanInstance ( cadr Instances ) ) )
                          (unless ( equal ( getq FloorplanInstance viewName ) "floorplan" )
                            ( error "Not instance of floorplan view." ) )
                          (when MasterCellLibCellPairs
                            (let (
                                  ( LayoutLibCellPair ( car MasterCellLibCellPairs ) ) )
                              (let (
                                    ( NewMasterCellViewDDObj ( ddGetObj
                                                               ( car LayoutLibCellPair )
                                                               ( cadr LayoutLibCellPair )
                                                               ( getq FloorplanInstance viewName ) ) ) )
                                (if ( and
                                      NewMasterCellViewDDObj
                                      ( getq NewMasterCellViewDDObj files ) )
                                    (let (
                                          ( NewMasterCellView ( dbOpenCellViewByType
                                                                ( car LayoutLibCellPair )
                                                                ( cadr LayoutLibCellPair )
                                                                ( getq FloorplanInstance viewName )
                                                                nil
                                                                "r" ) ) )
                                      (if NewMasterCellView
                                          ( dbSetq FloorplanInstance NewMasterCellView master )
                                        ( setq 
                                          Errors 
                                          ( cons
                                            ( sprintf
                                              nil
                                              "Unable to open %L %L %L for instance %L in %L %L %L"
                                              ( car LayoutLibCellPair )
                                              ( cadr LayoutLibCellPair )
                                              ( getq FloorplanInstance viewName )
                                              ( getq FloorplanInstance name )
                                              ( getq FloorplanCellView libName )
                                              ( getq FloorplanCellView cellName )
                                              ( getq FloorplanCellView viewName ) )
                                            Errors ) ) ) )
                                  ( setq 
                                    Errors
                                    ( cons
                                      ( sprintf
                                        nil
                                        "%L %L %L for instance %L in %L %L %L does not exist."
                                        ( car LayoutLibCellPair )
                                        ( cadr LayoutLibCellPair )
                                        ( getq FloorplanInstance viewName )
                                        ( getq FloorplanInstance name )
                                        ( getq FloorplanCellView libName )
                                        ( getq FloorplanCellView cellName )
                                        ( getq FloorplanCellView viewName ) )
                                      Errors ) ) ) ) ) )
                          (when Positions
                            ( dbSetq FloorplanInstance ( car Positions ) xy ) )
                          (when Orientations
                            ( dbSetq FloorplanInstance ( car Orientations ) orient ) ) ) ) )
                    (when BoundaryCompareResult
                      ( foreach
                        Shape
                        ( getq FloorplanCellView shapes )
                        (when ( and
                                ( equal ( car ( getq Shape lpp ) ) ( car PRBoundLPP ) )
                                ( equal ( cadr ( getq Shape lpp ) ) ( cadr PRBoundLPP ) ) )
                          ( dbDeleteObject Shape ) ) )
                      ( dbCreateRect
                        FloorplanCellView
                        PRBoundLPP
                        ( car BoundaryCompareResult ) ) )
                    ( dbSave FloorplanCellView ) )
                ( setq
                  Errors
                  ( cons
                    ( sprintf 
                      nil
                      "Unable to open %L %L %L for append."
                      ( getq FloorplanCellView libName )
                      ( getq FloorplanCellView cellName )
                      ( getq FloorplanCellView viewName ) )
                    Errors ) ) ) )
          ( setq
            Errors
            ( cons
              ( sprintf nil "%L did not have a floorplan view." CellName )
              Errors ) ) ) ) )
    Errors ) )
