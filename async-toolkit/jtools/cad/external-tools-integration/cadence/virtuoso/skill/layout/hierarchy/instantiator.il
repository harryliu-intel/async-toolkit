; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

/*DOC
<h2> Purpose </h2>
<p> This module is used to create instantiator views, which help in <flow eco>s of low level cells.</p>
<p>The problem is when high level cells route over lower level cells.  In order to redo the lower level cell, we must either reroute the upper level cell or constrain the routing of the lower level cell by taking into account the wiring on top of it.</p>

<h2> Description </h2>
<p>An instantiator view for cell X will have keepout ('boundary' layer) every where a higher level cell draws shapes over X that don't connect to X, and 'drawing' layer everywhere a higher level cell connects to a shape in X (or a subcell of X).</p>
<p> They keep the pins from the source cell, and can be instantiated in the cell you want to route provided the conductor-depth>=1.  The <package ui>/Fulcrum/route menu has an option to instantiate the instantiator view.</p>

<h2> Other References </h2>
<ul>
<li> See the <module layout.keepout.keepout> for the SKILL that does the layer post processing.
<li> Used only by <tool gen_instantiator_views>.
</ul>
*/



(defun InstantiatorsCreateInstantiatorsView ( CellView
                                              TargetCellView
                                              EmptyCellView
                                              InstantiatorsTable
                                              ViewName
                                              HierarchyViewName
                                              ConductorPurpose
                                              BoundaryLPP
                                              LPPs
                                              PinNetNamesToIgnoreLPP
                                              UseExistingInstantiatorViews
                                              OutPort
                                              LibCellsToIgnore
                                              MaxDepth
                                              )
  "Looks through the InstantiatorsTable for cells that instantiate CellView, and copies shapes in these cells that overlap the bounding box of instances of CellView. \
   Shapes turn into these layers: \
   subcell shapes and signal pins become 'drawing' purpose (metal)\
   all other shaopes in the cell become 'dummy' purpose (conductor)\
   metal in instantiators become 'boundary' purpose (keepout)"
  (let (
        ( BoundaryShape nil )
        ( NoWriteMode t )
        ( PruneCacheTable ( makeTable `foo nil ) )
        ( CellName ( getq CellView cellName ) )
        )
    (let (
          ( ViewNameCellNameInstanceNamePathChoicesList nil )
          ( CellNameInstanceNamePathChoicesList
            ( HierarchyGetInstancePathChoicesThatContainCell 
              InstantiatorsTable 
              CellName
              ( HierarchyPredicateSimplifier
                (lambda ( CellInstantiatorsTable
                          CellView )
                  ;go above this cell provided...
                  ( not
                    ( and UseExistingInstantiatorViews
                          ( ddGetObj 
                            ( getq CellView libName )
                            ( getq CellView cellName )
                            "instantiator" ) ) ) )
                InstantiatorsTable 
                CellName
                ViewName )
              ) )
          ( PrunePredicate
            ( InstantiatorDefaultPrunePredicate 
              PruneCacheTable
              LPPs ) )
          )

    (when ( equal ( getq CellView mode ) "r" )
      ( setq NoWriteMode t )
      )

    ;copy everything over
    ( betterCopyCellView 
      CellView
      ( getq TargetCellView libName )
      ( getq TargetCellView cellName )
      ( getq TargetCellView viewName )
      nil nil t )

    (unless NoWriteMode 
      ( printf "Warning: deleting stuff in %s %s %s\n"
               ( getq CellView libName )
               ( getq CellView cellName )
               ( getq CellView viewName ) )
      ;clear the original
      ( betterCopyCellView 
        EmptyCellView
        ( getq CellView libName )
        ( getq CellView cellName )
        ( getq CellView viewName )
        nil nil t )
      )

    ;save the original - we'll restore it later
    ( betterCopyCellView 
      TargetCellView
       ( getq EmptyCellView libName )
       ( getq EmptyCellView cellName )
       ( getq EmptyCellView viewName )
       nil nil t )

    ( setq
      BoundaryShape
      ( PinUtilFindPRBoundShape
        BoundaryLPP
        TargetCellView ) )

    
    ;we need to keep the boundary shape in the original cell
    ; so we know how big it is
    (unless NoWriteMode
      ( dbCopyFig
        BoundaryShape
        CellView )
      )
    
    ( foreach 
      Shape 
      ( getq TargetCellView shapes )
      (cond (
             ( exists LPP LPPs
                      ( equal 
                        ( car ( getq Shape lpp ) )
                        ( car LPP ) ) )
             (when ( and
                     ( equal 
                       ( cadr ( getq Shape lpp ) )
                       "drawing" )
                     ;convert non signal-pin shapes to conductor(dummy) purpose
                     ( or 
                       ( not ( getq Shape pin ) )
                       ( exists 
                         NetName
                         PinNetNamesToIgnoreLPP
                         ( equal 
                           Shape->pin->net->name
                           NetName ) ) )
                     )
               ( dbSetq
                 Shape
                 ( list ( car ( getq Shape lpp ) )
                        ConductorPurpose )
                 lpp ) )
             
             )
            (
             ( equal ( getq Shape lpp ) BoundaryLPP ) )
            (
             t
             ( dbDeleteObject Shape )
             ) ) )

    (unless BoundaryShape
      (error "No boundary shape" ) )


    ;add instantiator views
    ( foreach 
      CellNameInstanceNamePathChoices
      CellNameInstanceNamePathChoicesList

      (let (
            ( CellName 
              ( car CellNameInstanceNamePathChoices ) ) )
        ( setq 
          ViewNameCellNameInstanceNamePathChoicesList
          ( cons
            ( cons ViewName CellNameInstanceNamePathChoices )
            ViewNameCellNameInstanceNamePathChoicesList ) )

        (when ( and 
                UseExistingInstantiatorViews
                ( ddGetObj
                  ( car ( NameParseCellName 
                          CellName ) )
                  CellName
                  "instantiator" ) )
          ( setq 
            ViewNameCellNameInstanceNamePathChoicesList
            ( cons
              ( cons "instantiator" CellNameInstanceNamePathChoices )
              ViewNameCellNameInstanceNamePathChoicesList ) ) ) )
      )
    
    ;get prunings
    ( setq 
      ViewNameCellNameInstanceNamePathChoicesList
      ( mapcar
        (lambda ( ViewNameCellNameInstanceNamePathChoices )
          ( InstantiatorGetPrunings 
            ViewNameCellNameInstanceNamePathChoices
            HierarchyViewName
            PrunePredicate ) )
          ViewNameCellNameInstanceNamePathChoicesList ) )

    ( println
      ViewNameCellNameInstanceNamePathChoicesList )

    
    ;copy the overlaps
    ( foreach 
      ViewNameCellNameInstanceNamePathChoices
      ViewNameCellNameInstanceNamePathChoicesList
      (let (
            ( ViewNameToSearch
              ( car ViewNameCellNameInstanceNamePathChoices ) )
            ( CellName 
              ( cadr ViewNameCellNameInstanceNamePathChoices ) )
            ( InstanceNamePathChoices
              ( caddr ViewNameCellNameInstanceNamePathChoices ) ) )
        (let ( 
              ( TopLevelLayoutCellView
                ( dbOpenCellViewByType
                  ( car ( NameParseCellName 
                          CellName ) )
                  CellName
                  ViewName
                  nil
                  "r" ) )
              ( TopLevelCellViewToSearch
                ( dbOpenCellViewByType
                  ( car ( NameParseCellName 
                          CellName ) )
                  CellName
                  ViewNameToSearch
                  nil
                  "r" ) )
              )
            
          ( println ( list CellName ViewNameToSearch ) )

          ( foreach 
            InstanceNamePath
            ( HierarchyExpandPathChoices
              InstanceNamePathChoices )

            ( printf "instances: %L\n" InstanceNamePath )
            
            (let (
                  ( InstancePath
                    ( TransformGetInstancePathFromInstanceNamePath
                      TopLevelCellViewToSearch
                      InstanceNamePath ) )
                  )
              (let (
                    ( TargetCellToTopLevelTransform
                      ( TransformGetTransformFromInstancePath
                        InstancePath ) )
                    )
              (let (
                    ( InstanceBBoxAtTopLevel
                      ( dbTransformBBox
                        ( getq CellView bBox )
                        TargetCellToTopLevelTransform ) )
                    ( TopLevelToTargetCellTransform
                      ( TransformGetInverseTransform
                        TargetCellToTopLevelTransform ) ) )
                (let (
                    ;if layout search for drawing
                    ;if not layout search for keepout
                      ( LPPsToSearch
                        (cond ( 
                               ( equal ViewName ViewNameToSearch )
                               LPPs )
                            (
                             ( mapcar
                               (lambda ( LPP )
                                 ( list ( car LPP ) "boundary" ) )
                               LPPs ) ) ) )
                    ;expand search area
                      ( RectToSearch
                        ( RectExpandRight
                          InstanceBBoxAtTopLevel
                          10
                          ) )
                    ;look until we get to gates,stacks
                      ( DepthRange
                        ( list 
                          0 
                          ( min
                            ( max ( length InstanceNamePath )
                                  MaxDepth )
                            ( HierarchyGetMaxDepth
                              TopLevelCellViewToSearch
                              ?LibCellsToIgnore LibCellsToIgnore
                              ?DescendIntoInstancePredicate
                              (lambda ( Instance CurrTransform )
                                (when ( RectDoRectsOverlap
                                        InstanceBBoxAtTopLevel
                                        ( dbTransformBBox
                                          ( getq Instance bBox )
                                          CurrTransform ) ) t ) )
                              ) ) ) )
                      )
                  (let (
                        ( Overlaps
                          ( ListNonDestructiveMapCan
                            (lambda ( LPP )
                              ( dbProduceOverlap
                                TopLevelCellViewToSearch
                                RectToSearch
                                DepthRange
                                LPP
                                ) )
                            LPPsToSearch
                            ) ) )


                ( println ( list InstanceBBoxAtTopLevel 
                                 TopLevelToTargetCellTransform ) )

                ( println "Finding overlaps..." )
                ( foreach
                  Overlap
                  Overlaps
                  (let (
                        ( FigStack
                          ( reverse
                            ( TransformCanonicalizeInstancePath Overlap ) ) ) )
                  ;If we couldn't clear out the cell for which we're 
                  ;making the instantiator view, then make sure we don't copy 
                  ;figs that are in that cell
                    (unless ( and 
                              NoWriteMode
                              ( exists 
                                Component 
                                FigStack
                                ( equal
                                  ( getq Component cellName )
                                  ( getq CellView cellName ) ) ) )
                      (let (
                            ( FigInSomeCell ( car FigStack ) )
                            )
                        (when ( and
                                ( or 
                                  ( equal ( getq FigInSomeCell objType ) "rect" )
                                  ( equal ( getq FigInSomeCell objType ) "polygon" )
                                  ( equal ( getq FigInSomeCell objType ) "path" ) )
                                t )
                          (let (
                                ( SomeCellToTopLevelTransform
                                  ( TransformGetTransformFromInstanceStack
                                    ( cdr FigStack ) ) ) )
                            
                          (let (
                                ( FigInTargetCell
                                  ( dbCopyFig
                                    FigInSomeCell
                                    TargetCellView
                                    ( dbConcatTransform
                                      SomeCellToTopLevelTransform
                                      TopLevelToTargetCellTransform ) ) ) )
                            ;change to keepout
                            ( dbSetq
                              FigInTargetCell
                              ( list ( car ( getq FigInTargetCell lpp ) )
                                     "boundary" )
                              lpp )
                            ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) )

    ;restore the original
    (unless NoWriteMode
      ( betterCopyCellView 
        EmptyCellView
        ( getq CellView libName )
        ( getq CellView cellName )
        ( getq CellView viewName )
        nil nil t ) 
      )

    ( foreach 
      Key
      PruneCacheTable
      (when ( equal ( arrayref PruneCacheTable Key ) 2 )
        ( fprintf 
          OutPort
          "WARNING...seriously: %L was not examined for cell %s because it's names don't m!\n"
          Key
          ( getq CellView cellName )
          ) ) )
    ) ) )


(defun InstantiatorDefaultPrunePredicate ( PrunePredicateCache
                                           LPPs )
  ( ExpressionReplaceSymbolsWithValues
    `(lambda ( ViewNameCellNameInstanceNamePathChoices )
       (let (
             ( ViewName
               ( car ViewNameCellNameInstanceNamePathChoices ) )
             ( CellName
               ( cadr ViewNameCellNameInstanceNamePathChoices ) )
             ( InstanceNamePathChoices
               ( caddr ViewNameCellNameInstanceNamePathChoices ) )
             ( PruneMeCached
               ( arrayref
                 PrunePredicateCache
                 ViewNameCellNameInstanceNamePathChoices
                 ) ) )
         (cond (
                ( numberp PruneMeCached )
                ( not ( equal 0 PruneMeCached ) )
                )
               (
                (let (
                      ( CellView
                        ( dbOpenCellViewByType
                          ( car ( NameParseCellName 
                                  CellName ) )
                          CellName
                          ViewName
                          nil
                          "r" ) ) )
                  (let (
                        ( AffectsCell
                          ( exists
                            LPP
                            LPPs
                            ( PinUtilGetAllShapesOnLPP
                              CellView
                              LPP ) ) )
                        ( LayoutMatchesHierarchy
                          ( forall
                            InstanceNamePath
                            ( HierarchyExpandPathChoices
                              InstanceNamePathChoices )
                            ( TransformIsValidInstanceNamePath
                              CellView
                              InstanceNamePath ) ) )
                        )
                  (let ( 
                        ( PruneMe (if AffectsCell
                                      (if LayoutMatchesHierarchy 0 1 ) 2 ) ) )
                      
                      ( setarray 
                        PrunePredicateCache
                        ViewNameCellNameInstanceNamePathChoices
                        PruneMe )

                      ( println 
                        ( list CellName AffectsCell LayoutMatchesHierarchy ( equal 1 PruneMe ) ) )
                          
                      ( not ( equal 0 PruneMe ) )
                            ) ) ) ) ) ) )
    ( list `PrunePredicateCache
           ) )
  )

(defun InstantiatorGetPrunings ( ViewNameCellNameInstanceNamePathChoices
                                 HierarchyViewName
                                 PrunePredicate )
  (let (
        ( ViewNameToSearch
          ( car ViewNameCellNameInstanceNamePathChoices ) )
        ( CellName
          ( cadr ViewNameCellNameInstanceNamePathChoices ) )
        ( InstanceNamePathChoices
          ( caddr ViewNameCellNameInstanceNamePathChoices ) )
        )
    (let (
          ( TopLevelCellView
            ( dbOpenCellViewByType
              ( car ( NameParseCellName 
                      CellName ) )
              CellName
              ViewNameToSearch
              nil
              "r" ) ) )
      (cond (
             ( apply PrunePredicate ( list ViewNameCellNameInstanceNamePathChoices ) )
             ( InstantiatorGetPrunings
               ( InstantiatorGetSubViewNameCellNameInstanceNamePathChoices
                 ViewNameCellNameInstanceNamePathChoices
                 HierarchyViewName
                 )
               HierarchyViewName
               PrunePredicate ) )
            (
             t
             ViewNameCellNameInstanceNamePathChoices ) ) ) ) )
      
(defun InstantiatorGetSubViewNameCellNameInstanceNamePathChoices
  ( InstanceNamePathChoices
    HierarchyViewName )
  (let (
        ( ViewName
          ( car ViewNameCellNameInstanceNamePathChoices ) )
        ( CellName
          ( cadr ViewNameCellNameInstanceNamePathChoices ) )
        ( InstanceNamePathChoices
          ( caddr ViewNameCellNameInstanceNamePathChoices ) )
        )
    (let (
          ( CellView
            ( dbOpenCellViewByType
              ( car ( NameParseCellName 
                      CellName ) )
              CellName
              HierarchyViewName
              nil
              "r" ) ) )
      (let (
            ( Instance
              ( dbFindAnyInstByName 
                CellView
                ( car ( car InstanceNamePathChoices ) ) ) ) )
        ( list ViewName ( getq Instance cellName ) ( cdr InstanceNamePathChoices ) )
        ) ) ) )
