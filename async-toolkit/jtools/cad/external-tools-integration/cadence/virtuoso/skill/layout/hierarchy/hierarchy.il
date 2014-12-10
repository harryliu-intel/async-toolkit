; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$


(defun HierarchyGetMaxDepth ( CellView 
                              @key
                              ( LibCellsToIgnore nil )
                              ( DescendIntoInstancePredicate
                                (lambda ( Instance CurrTransform ) t) )
                              )
  (cond (
         ( ListFindMaximum
           ( car ( NameFilterInstances
                   ( getq CellView instances )
                   LibCellsToIgnore ) )
           (lambda ( Instance )
             (cond (
                    ( null ( getq ( getq Instance master ) instances ) )
                    1 )
                   (
                    ( plus 1 ( HierarchyGetMaxDepth 
                               ( getq Instance master )
                               ?LibCellsToIgnore LibCellsToIgnore
                               ) ) ) ) ) ) )
        (
         0
         )
        ) )

(defun HierarchyGetCellDescendantsMakeStackFrame ( Cell Instances )
  ( list
    Cell
    Instances ) )

(defun HierarchyGetCellDescendantsPush ( Stack
                                         Cell
                                         Instances )
  ( cons ( list Cell Instances ) Stack ) )

(defun HierarchyGetCellDescendantsSmartPush ( Stack 
                                              CellTable
                                              Cell
                                              LibCellExpressionPairsToIgnore
                                              CellIgnoreFunc
                                              CellIgnoreFuncParams )
  (if ( arrayref CellTable ( HierarchyMakeKeyStringForCellView Cell ) )
      Stack
    ( HierarchyGetCellDescendantsPush
      Stack
      Cell
      (let (
            ( FilterResult ( NameFilterInstances 
                             ( getq Cell instances )
                             LibCellExpressionPairsToIgnore ) ) )
        ( foreach
          Instance
          ( cadr FilterResult )
          ( setarray CellTable ( HierarchyMakeKeyStringForInstance Instance ) t ) )
        ( setof
          Instance
          ( car FilterResult )
          (unless ( or
                    ( null ( getq Instance master  ) )
                    ( arrayref
                      CellTable
                      ( HierarchyMakeKeyStringForInstance
                        Instance ) ) )
            (if ( apply
                  CellIgnoreFunc
                  ( cons
                    ( getq Instance master )
                    CellIgnoreFuncParams ) )
                ( setarray CellTable ( HierarchyMakeKeyStringForInstance Instance ) t )
              t ) ) ) ) ) ) )
           

(defun HierarchyGetCellDescendantsGetCurrInstance ( Stack )
  ( car ( car ( cdr ( car Stack ) ) ) ) )

(defun HierarchyGetCellDescendantsNextInstance ( Stack )
  ( HierarchyGetCellDescendantsPush
    ( cdr
      Stack )
    ( car
      ( car
        Stack ) )
    ( cdr
      ( car
        ( cdr
          ( car
            Stack ) ) ) ) ) )

(defun HierarchyGetCellDescendantsGetCurrCell ( Stack )
  ( car ( car Stack ) ) )

(defun HierarchyMakeKeyStringForCellView ( CellView )
  ( sprintf
    nil
    "%s,%s,%s"
    ( getq CellView libName )
    ( getq CellView cellName )
    ( getq CellView viewName ) ) )

(defun HierarchyMakeKeyStringForInstance ( Instance )
  ( HierarchyMakeKeyStringForCellView ( getq Instance master ) ) )


(defun HierarchyInnerGetCellDescendants ( RootCellView
                                          LibCellExpressionPairsToIgnore
                                          CellIgnoreFunc
                                          CellIgnoreFuncParams
                                          TouchedCells )
  (let (
        ( Result nil )
        ( Stack nil )
        ( RootInstances ( getq RootCellView instances ) ) )
    ( setq Stack ( HierarchyGetCellDescendantsSmartPush 
                   Stack 
                   TouchedCells
                   RootCellView 
                   LibCellExpressionPairsToIgnore
                   CellIgnoreFunc
                   CellIgnoreFuncParams ) )
    
    (while Stack
      (let (
            ( CurrInstance ( HierarchyGetCellDescendantsGetCurrInstance Stack ) ) )
        (if CurrInstance
            (let ()
              ( setq Stack ( HierarchyGetCellDescendantsNextInstance Stack ) )
              ( setq Stack ( HierarchyGetCellDescendantsSmartPush 
                             Stack 
                             TouchedCells
                             ( getq CurrInstance master ) 
                             LibCellExpressionPairsToIgnore
                             CellIgnoreFunc
                             CellIgnoreFuncParams ) ) )
          (let (
                ( CurrCell ( HierarchyGetCellDescendantsGetCurrCell Stack ) )
                )
            ( setarray TouchedCells ( HierarchyMakeKeyStringForCellView CurrCell ) t )
            ( setq Result ( tconc Result CurrCell ) )
            ( setq Stack ( cdr Stack ) ) ) ) ) )
    ( car Result ) ) )

(defun HierarchyGetCellDescendants ( RootCellView
                                     LibCellExpressionPairsToIgnore
                                     CellIgnoreFunc
                                     CellIgnoreFuncParams )
  ( HierarchyInnerGetCellDescendants 
    RootCellView
    LibCellExpressionPairsToIgnore
    CellIgnoreFunc
    CellIgnoreFuncParams
    ( makeTable "HierarchyGetCellDescendantsTable" nil ) ) )

(defun HierarchyGetCellDescendantsOfCells ( ListOfRootCellViews
                                            LibCellExpressionPairsToIgnore
                                            CellIgnoreFunc
                                            CellIgnoreFuncParams )
  (let (
        ( TouchedCells ( makeTable "HierarchyGetCellDescendantsTable" nil ) )
        ( Ret nil ) )
    ( foreach
      RootCellView
      ListOfRootCellViews
      ( setq 
        Ret
        ( lconc 
          Ret 
          ( HierarchyInnerGetCellDescendants
            RootCellView
            LibCellExpressionPairsToIgnore
            CellIgnoreFunc
            CellIgnoreFuncParams
            TouchedCells ) ) ) )
    ( car Ret ) ) )

(defun HierarchyGetCellDescendantsOfLibCellViewTripples ( ListOfRootLibCellViewTripples
                                                          LibCellExpressionPairsToIgnore
                                                          CellIgnoreFunc
                                                          CellIgnoreFuncParams )
  ( HierarchyGetCellDescendantsOfCells
    ( ListApplyFuncToListAndAccumulateNonNilResults
      ListOfRootLibCellViewTripples
      (lambda ( Tripple )
        (let (
              ( LibName ( car Tripple ) )
              ( CellName ( cadr Tripple ) )
              ( ViewName ( caddr Tripple ) ) )
          (let (
                ( CellViewDDObj ( ddGetObj LibName CellName ViewName ) ) )
            (when ( and
                    CellViewDDObj
                    ( getq CellViewDDObj files ) )
              ( dbOpenCellViewByType
                LibName
                CellName
                ViewName
                nil
                "r" ) ) ) ) )
      nil )
    LibCellExpressionPairsToIgnore
    CellIgnoreFunc
    CellIgnoreFuncParams ) )
              
(defun HierarchyGetAllCellsInTree ( RootCellView
                                    LibCellExpressionPairsToIgnore )
  ( HierarchyGetCellDescendants
    RootCellView
    LibCellExpressionPairsToIgnore
    (lambda
      ( CellView )
      nil )
    nil ) )

(defun HierarchyGetDescendantLibCellPairs ( RootCellView
                                            LibCellExpressionPairsToIgnore
                                            CellIgnoreFunc
                                            CellIgnoreFuncParams )
  ( ListRemoveDuplicates 
    ( ListApplyFuncToListAndAccumulateResults 
      ( HierarchyGetCellDescendants 
        RootCellView
        LibCellExpressionPairsToIgnore
        CellIgnoreFunc
        CellIgnoreFuncParams ) 
      (lambda ( CellView ) 
        ( list ( getq CellView libName ) ( getq CellView cellName ) ) ) 
      nil ) 
    (lambda ( LibCellPair0 LibCellPair1 ) 
      ( and 
        ( equal 
          ( car LibCellPair0 ) 
          ( car LibCellPair1 ) ) 
        ( equal 
          ( cadr LibCellPair0 ) 
          ( cadr LibCellPair1 ) ) ) ) 
    nil ) )

(defun HierarchyPrintCells ( RootCellView LibCellExpressionPairsToIgnore )
  (let (
        ( OrderedViews
          ( HierarchyGetCellDescendants
            RootCellView
            LibCellExpressionPairsToIgnore
            (lambda
              ( CellView )
              nil )
            nil ) ) )
    (let (
          ( ViewNames ( ListApplyFuncToListAndAccumulateResults
                        OrderedViews
                        (lambda
                          ( CellView )
                          ( getq CellView cellName ) )
                        nil ) ) )
      ( foreach
        Name
        ViewNames
        ( printf "%s\n" Name ) ) ) )
  nil )



(defun HierarchyGICMakeStackFrame ( CellView InstanceCount )
  (let (
        ( InstanceCountsTable ( HierarchyCountInstancesInCell
                                CellView ) ) )
    ( list 
      CellView
      InstanceCount
      ( ListApplyFuncToListAndAccumulateNonNilResults
        ( tableToList InstanceCountsTable )
        (lambda
          ( Entry )
          (let (
                ( LibCellViewTripple ( car Entry ) )
                ( InstanceCount ( cadr Entry ) ) )
            (let (
                  ( LibName ( car LibCellViewTripple ) )
                  ( CellName ( cadr LibCellViewTripple ) )
                  ( ViewName ( caddr LibCellViewTripple ) ) )
              (let (
                    ( CellViewDDObj ( ddGetObj LibName CellName ViewName ) ) )
                (when ( and
                        CellViewDDObj
                        ( getq CellViewDDObj files ) )
                  ( list
                    InstanceCount
                    ( dbOpenCellViewByType
                      LibName
                      CellName
                      ViewName
                      nil
                      "r" ) ) ) ) ) ) )
        nil )
      InstanceCountsTable ) ) )
  
(defun HierarchyGICGetCellViewFromStackFrame ( StackFrame )
  ( car StackFrame ) )

(defun HierarchyGICGetInstanceCountFromStackFrame ( StackFrame )
  ( cadr StackFrame ) )

(defun HierarchyGICGetCellViewsToRecurseOnFromStackFrame ( StackFrame )
  ( caddr StackFrame ) )

(defun HierarchyGICGetInstanceCountsTableFromStrackFrame ( StackFrame )
  ( cadddr StackFrame ) )


(defun HierarchyGICMergeTwoFrames ( TopFrame NextFrame )
  (let (
        ( TopInstanceCountsTable
          ( HierarchyGICGetInstanceCountsTableFromStrackFrame
            TopFrame ) )
        ( TopInstanceCount
          ( HierarchyGICGetInstanceCountFromStackFrame
            TopFrame ) )
        ( NextCellView
          ( HierarchyGICGetCellViewFromStackFrame
            NextFrame ) )
        ( NextInstanceCount
          ( HierarchyGICGetInstanceCountFromStackFrame
            NextFrame ) )
        ( NextCellViewsToRecurseOn
          ( HierarchyGICGetCellViewsToRecurseOnFromStackFrame
            NextFrame ) )
        ( NextInstanceCountsTable
          ( HierarchyGICGetInstanceCountsTableFromStrackFrame
            NextFrame ) ) )
    (let (
          ( MergedInstanceCounts
            ( HierarchyGICMergeInstanceCountsTables
              NextInstanceCountsTable
              TopInstanceCountsTable
              TopInstanceCount ) ) )
      ( list
        NextCellView
        NextInstanceCount
        NextCellViewsToRecurseOn
        MergedInstanceCounts ) ) ) )



(defun HierarchyGICMergeInstanceCountsTables ( TargetTable SrcTable Mult )
  ( foreach
    SrcEntry
    ( tableToList SrcTable )
    (let (
          ( LibCellViewTripple ( car SrcEntry ) )
          ( SrcCount ( times ( cadr SrcEntry ) Mult ) ) )
      (let (
            ( CurrCount ( arrayref TargetTable LibCellViewTripple ) ) )
        ( setarray
          TargetTable
          LibCellViewTripple
          (if CurrCount
              ( plus CurrCount SrcCount )
            SrcCount ) ) ) ) )
  TargetTable )

                                        ;Merges Stack Frames until the top of the stack has
                                        ;some cells to recurse on.
(defun HierarchyGICMergeStackFrames ( Stack )
  (let (
        ( MyStack Stack ) )
    (while ( and
             ( cadr MyStack )
             ( null
               ( HierarchyGICGetCellViewsToRecurseOnFromStackFrame
                 ( car MyStack ) ) ) )
      (let (
            ( TopFrame ( car MyStack ) )
            ( NextFrame ( cadr MyStack ) )
            ( RestOfStack ( cddr MyStack ) ) )
        (let (
              ( NewTopFrame ( HierarchyGICMergeTwoFrames
                              TopFrame
                              NextFrame ) ) )
          ( setq MyStack ( cons NewTopFrame RestOfStack ) ) ) ) )
    MyStack ) )
          
                                        ;Returns ( list NextCellView Stack )
(defun HierarchyGICGetNextCellViewToRecurseOnFromStack ( Stack )
  (let (
        ( MyStack ( HierarchyGICMergeStackFrames
                    Stack ) ) )
    (let (
          ( TopFrame ( car MyStack ) )
          ( RestOfMyStack ( cdr MyStack ) ) )
      (let (
            ( CurrNextCellView
              ( car
                ( HierarchyGICGetCellViewsToRecurseOnFromStackFrame
                  TopFrame ) ) ) )
        (if CurrNextCellView
            (let (
                  ( TopCellView
                    ( HierarchyGICGetCellViewFromStackFrame
                      TopFrame ) )
                  ( TopInstanceCount
                    ( HierarchyGICGetInstanceCountFromStackFrame
                      TopFrame ) )
                  ( TopCellViewsToRecurseOn
                    ( HierarchyGICGetCellViewsToRecurseOnFromStackFrame
                      TopFrame ) )
                  ( TopInstanceCountsTable
                    ( HierarchyGICGetInstanceCountsTableFromStrackFrame
                      TopFrame ) ) )
              ( list
                CurrNextCellView
                ( cons
                  ( list
                    TopCellView
                    TopInstanceCount
                    ( cdr TopCellViewsToRecurseOn )
                    TopInstanceCountsTable )
                  RestOfMyStack ) ) )
          ( list
            nil
            MyStack ) ) ) ) ) )

(defun HierarchyCountInstancesInCell ( CellView )
  (let (
        ( CountTable ( makeTable "InstanceCountsTable" nil ) ) )
    ( foreach
      Instance
      ( getq CellView instances )
      (let (
            ( MasterLibName ( getq Instance libName ) )
            ( MasterCellName ( getq Instance cellName ) )
            ( MasterViewName ( getq Instance viewName ) ) )
        (let (
              ( CurrCount ( arrayref
                            CountTable
                            ( list MasterLibName MasterCellName MasterViewName ) ) ) )
          ( setarray
            CountTable
            ( list
              MasterLibName
              MasterCellName
              MasterViewName )
            (if CurrCount
                ( plus CurrCount 1 )
              1 ) ) ) ) )
    CountTable ) )

(defun HierarchyGetInstanceCounts ( RootCellView )
  (let (
        ( Stack ( list
                  ( HierarchyGICMakeStackFrame
                    RootCellView
                    1 ) ) ) )
    (while ( or
             ( cadr Stack )
             ( HierarchyGICGetCellViewsToRecurseOnFromStackFrame
               ( car Stack ) ) )
      (let (
            ( NextCellViewResult ( HierarchyGICGetNextCellViewToRecurseOnFromStack
                                   Stack ) ) )
        (let (
              ( NextCellViewAndCount ( car NextCellViewResult ) ) )
          (if NextCellViewAndCount
              (let (
                    ( InstanceCount ( car NextCellViewAndCount ) )
                    ( NextCellView ( cadr NextCellViewAndCount ) ) )
                ( setq
                  Stack
                  ( cons
                    ( HierarchyGICMakeStackFrame
                      NextCellView
                      InstanceCount )
                    ( cadr NextCellViewResult ) ) ) )
            ( setq
              Stack
              ( cadr NextCellViewResult ) ) ) ) ) )
    (let (
          ( ResultTable ( HierarchyGICGetInstanceCountsTableFromStrackFrame
                          ( car Stack ) ) ) )
      ( setarray
        ResultTable
        ( list
          ( getq RootCellView libName )
          ( getq RootCellView cellName )
          ( getq RootCellView viewName ) )
        1 )
      ResultTable ) ) )
      

(defun HierarchyMakeCellsInstantiatorsTable ( HierarchyViewName 
                                              ViewName
                                              LibCellRegExsToIgnore
                                              )
  (let (
        ( CellsInstantiatorTable ( makeTable "foo" nil ) )
        ( LibCellPairs
          ( car
            ( NameFilterObjects
              ( ListNonDestructiveMapCan
                (lambda ( Lib )
                  ( mapcar
                    (lambda ( Cell )
                      ( list Lib->name Cell->name )
                      )
                    ( getq Lib cells ) ) )
                ( ddGetLibList ) )
            LibCellRegExsToIgnore
            `car
            `cadr ) ) ) )

    ( foreach
      LibCellPair
      LibCellPairs
      (let (
            ( LibName ( car LibCellPair ) )
            ( CellName ( cadr LibCellPair ) ) )
        (let (
              ( HierarchyViewDDObj 
                ( ddGetObj
                  LibName
                  CellName
                  HierarchyViewName ) )
              ( ViewDDObj 
                ( ddGetObj
                  LibName
                  CellName
                  ViewName ) ) )
          (when ( and
                  HierarchyViewDDObj
                  ViewDDObj
                  ( getq HierarchyViewDDObj files ) 
                  ( getq ViewDDObj files ) 
                  )
            (let (
                  ( CellView
                    ( dbOpenCellViewByType
                      LibName
                      CellName
                      HierarchyViewName
                      nil
                      "r" ) ) )
              (when CellView
                (let (
                      ( Instances ( getq CellView instances ) ) )
                  ( foreach
                    Instance
                    Instances
                    (let (
                          ( MasterCellName ( getq Instance cellName ) ) )
                      (let (
                            ( ExistingEntry ( arrayref
                                              CellsInstantiatorTable
                                              MasterCellName ) ) )
                        (let (
                              ( TableForCell
                                (if ExistingEntry
                                    ExistingEntry
                                  (let (
                                        ( NewTableForCell
                                          ( makeTable
                                            "CellInstantiators"
                                            nil ) ) )
                                    ( setarray
                                      CellsInstantiatorTable
                                      MasterCellName
                                      NewTableForCell )
                                    NewTableForCell ) ) ) )
                          ( setarray
                            TableForCell
                            ( getq CellView cellName )
                            ( cons 
                              ( getq Instance name )
                              ( arrayref TableForCell ( getq CellView cellName ) )
                              )
                            ) ) ) ) ) ) ) ) ) ) ) )

    CellsInstantiatorTable ) )

(defun HierarchyGetTopLevelCellNames ( CellsInstantiatorsTable FloorplanViewName )
  (let (
        ( Ret nil ) )
    ( foreach
      Lib
      ( ddGetLibList )
      ( foreach
        Cell
        ( getq Lib cells )
        (let (
              ( FloorplanViewDDObj ( ddGetObj
                                     ( getq Lib name )
                                     ( getq Cell name )
                                     FloorplanViewName ) ) )
          (when ( and
                  FloorplanViewDDObj
                  ( getq FloorplanViewDDObj files ) )
            (unless ( arrayref CellsInstantiatorsTable ( getq Cell name ) )
              ( setq Ret ( cons ( getq Cell name ) Ret ) ) ) ) ) ) )
    Ret ) )

(defun HierarchyGetInstantiatorsOfCell ( CellsInstantiatorsTable CellName )
  (let (
        ( CellInstantiatorsTable ( arrayref CellsInstantiatorsTable CellName ) ) )
    (when CellInstantiatorsTable
      ( ListApplyFuncToListAndAccumulateResults
        ( tableToList CellInstantiatorsTable )
        (lambda ( Entry )
          ( car Entry ) )
        nil ) ) ) )

(defun HierarchyGetTopLevelCellsThatContainCell ( CellsInstantiatorsTable 
                                                  CellName
                                                  GoAboveThisCellPredicate
                                                  )
  ( ListUnionNoTableElements 
    nil
    ( mapcar
      `car
      ( HierarchyGetInstancePathChoicesThatContainCell 
        CellsInstantiatorsTable 
        CellName
        GoAboveThisCellPredicate ) ) ) )

(defun HierarchyGetInstancePathChoicesThatContainCell ( CellsInstantiatorsTable 
                                                        CellName
                                                        GoAboveThisCellPredicate
                                                        )
  (let (
        ( CellNameInstanceNameChoices nil )
        )
                                        ;initialize queue
    (let (
          ( CellNameInstanceNameChoicesQueue
            (let (
                  ( InstantiatorTable 
                    ( arrayref
                      CellsInstantiatorsTable
                      CellName ) ) )
              (if InstantiatorTable
                  ( lconc 
                    nil
                    ( mapcar
                      (lambda ( CellNameInstancesPair )
                        ( list ( car CellNameInstancesPair )
                               ( cdr CellNameInstancesPair ) ) )
                      ( tableToList InstantiatorTable )
                      ) )
                nil ) ) ) )
      (while ( car CellNameInstanceNameChoicesQueue )

        (let (
              ( CurrCellNameInstanceNameChoices 
                ( car ( car CellNameInstanceNameChoicesQueue ) ) ) )
                                        ;dequeue
          ( setq
            CellNameInstanceNameChoicesQueue
            ( lconc nil ( cdr ( car CellNameInstanceNameChoicesQueue ) ) ) )

          (let (
                ( CurrCell 
                  ( car CurrCellNameInstanceNameChoices ) )
                ( CurrInstanceCellNameInstanceNameChoices
                  ( cadr CurrCellNameInstanceNameChoices ) ) )
            (let (
                  ( InstantiatorsOfCurrCell 
                    ( arrayref
                      CellsInstantiatorsTable
                      CurrCell ) ) )
              (if ( and InstantiatorsOfCurrCell
                        ( apply GoAboveThisCellPredicate
                                ( list CellsInstantiatorsTable
                                       CurrCell ) ) )
                                        ;enqueue with children added
                  ( foreach NextCellNameInstanceNameChoices
                            ( tableToList InstantiatorsOfCurrCell )
                            (let (
                                  ( NextCell
                                    ( car NextCellNameInstanceNameChoices ) )
                                  ( NextInstances
                                    ( cadr NextCellNameInstanceNameChoices ) ) )

                              ( setq
                                CellNameInstanceNameChoicesQueue
                                ( tconc
                                  CellNameInstanceNameChoicesQueue
                                  ( list NextCell
                                         ( cons NextInstances
                                                CurrInstanceCellNameInstanceNameChoices ) ) )
                                ) ) )
                                        ;add to unique, and don't enqueue
                ( setq CellNameInstanceNameChoices
                       ( cons CurrCellNameInstanceNameChoices
                              CellNameInstanceNameChoices ) ) ) ) ) ) )
      CellNameInstanceNameChoices ) ) )

(defun HierarchyExpandPathChoices ( Choices )
  (let (
        ( CurrPaths 
          ( mapcar
            (lambda ( Choice ) ( list Choice ) )
            ( car Choices ) ) )
        ( CurrChoices ( cdr Choices ) )
        )
    (while CurrChoices
      (let (
            ( NewPaths nil ) )

        ( setq NewPaths
               ( ListNonDestructiveMapCan
                 (lambda ( Choice )
                   ( mapcar
                     (lambda ( Path )
                       ( append1 Path Choice ) )
                     CurrPaths ) )
                 ( car CurrChoices ) ) )
        ( setq CurrPaths NewPaths )
        ( setq CurrChoices ( cdr CurrChoices ) ) ) )
    CurrPaths ) )

(defun HierarchyPredicateSimplifier ( SimplePredicate
                                      CellInstantiatorsTable
                                      CellName
                                      ViewName )
  ( ExpressionReplaceSymbolsWithValues
    `(lambda ( CellInstantiatorsTable
               CellName )
       ( apply
         SimplePredicate
         ( list CellInstantiatorsTable
                ( dbOpenCellViewByType
                  ( car ( NameParseCellName CellName ) )
                  CellName
                  ViewName
                  nil
                  "r" ) ) ) )
    ( list `ViewName `SimplePredicate )
    ) )


; change an instance to a preferred view or list of views
(defun ChangeView (inst view @key (verbose t))
  (let (views to_cv)
    views = (if (atom view) (list view) view)
    (foreach v views
             (unless to_cv 
               to_cv=(nrOpenCellViewReadable inst->libName inst->cellName v)
               )
             )
    (cond (to_cv inst->master=to_cv)
          (verbose (printf "Can't find subcell view for %s\n" inst->cellName))
          )
    )
  )

(defun ReplaceSubcells (cv from_view to_view @key (verbose t))
 (let (to from_master to_master)
  (when (nrIsCellViewWritable cv)
    (foreach inst cv->instances
      (when inst->viewName==from_view && inst->libName!=TechLibName
        (ChangeView inst to_view ?verbose verbose)
      )
    )
    (dbSave cv)
  )
 )
)

(defun RecursiveReplaceSubcells (from_view to_view @key (cv nil))
 (let (cellview uniqueInstances cell)
   (if cv then
     cellview=cv
   else
     cellview=(geGetEditCellView)
   )

   (when (nrIsCellViewWritable cellview)
     cell = (nrOpenCellViewWritable cellview->libName cellview->cellName cellview->viewName)
     (ReplaceSubcells cell from_view to_view)
   )

   uniqueInstances=nil
   (foreach inst cellview->instances
     (when !(member inst->cellName uniqueInstances->cellName)
             && inst->libName!=TechLibName 
             && inst->libName!=GateLibName
             && inst->libName!=StackLibName
       uniqueInstances= (cons inst uniqueInstances )
     )
   )
   (foreach inst uniqueInstances
     cell = (nrOpenCellViewReadable inst->libName inst->cellName inst->viewName)
     (when cell 
       (RecursiveReplaceSubcells from_view to_view ?cv cell)
     )
   )
 t
 )
)

(defun shit (from to)
(RecursiveReplaceSubcells from to)
)
