; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

(defun GetCellNameMappingsGetBiDirectionalCellNameMappings ( NewRootCellView
                                                             BottomUpListOfNewRootCellDescendants
                                                             OldRootCellView
                                                             TargetOldToNewTable
                                                             TargetNewToOldTable 
                                                             GetOldCellViewFunc
                                                             GetOldCellViewFuncParams )
  (let (
        ( TopDownListOfNewCells ( reverse BottomUpListOfNewRootCellDescendants ) ) )
    ( setarray 
      TargetNewToOldTable
      ( getq NewRootCellView cellName )
      OldRootCellView )
    ( setarray 
      TargetOldToNewTable
      ( getq OldRootCellView cellName )
      NewRootCellView )
    ( foreach
      Cell
      TopDownListOfNewCells
      (let (
            ( OldCellView ( arrayref
                            TargetNewToOldTable
                            ( getq Cell cellName ) ) ) )
        (when OldCellView
          (let (
                ( OldInstances ( getq OldCellView instances ) ) )
            ( foreach
              Instance
              ( getq Cell instances )
              (unless ( arrayref TargetNewToOldTable ( getq Instance cellName ) )
                (let (
                      ( OldInstance ( ListFindElement
                                      OldInstances
                                      (lambda
                                        ( OldInstance Instance )
                                        (let (
                                              ( MungedOldInstanceName ( MungeInstanceName
                                                                        ( getq OldInstance name ) ) ) )
                                                                          
                                          ( equal
                                            MungedOldInstanceName
                                            ( getq Instance name ) ) ) )
                                      ( list Instance ) ) ) )
                  (when OldInstance
                    (let (
                          ( OldMasterCellView ( apply
                                                GetOldCellViewFunc
                                                ( cons
                                                  ( getq OldInstance libName )
                                                  ( cons
                                                    ( getq OldInstance cellName )
                                                    ( cons
                                                      ( getq OldInstance viewName )
                                                      ( cons 
                                                        ( getq Instance cellName )
                                                        GetOldCellViewFuncParams ) ) ) ) ) ) )
                      (when OldMasterCellView
                        ( setarray
                          TargetOldToNewTable
                          ( getq OldInstance cellName )
                          ( getq Instance master ) )
                        ( setarray 
                            TargetNewToOldTable
                            ( getq Instance cellName )
                            OldMasterCellView ) ) ) ) ) ) ) ) ) ) ) ) 
  t )

(defun GetCellNameMappings ( NewRootCellView LibsToIgnore OldRootCellView TargetTable )
  ( GetCellNameMappingsGetBiDirectionalCellNameMappings
    NewRootCellView
    ( HierarchyGetCellDescendants
      RootConnectivityCellView
      LibsToIgnore
      (lambda ( CellView ) 
        nil )
      nil ) 
    OldRootCellView
    ( makeTable "foo" nil )
    TargetTable
    (lambda 
      ( OldLibName OldCellName OldViewName NewCellName )
      (when ( ddGetObj OldLibNAme OldCellName OldViewName )
        ( printf "%L=%L\n" NewCellName OldCellName )
        ( dbOpenCellViewByType
          OldLibName
          OldCellName
          OldViewName
          nil
          "r" ) ) )
    nil ) )
