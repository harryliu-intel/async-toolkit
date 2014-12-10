; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

; Function: CreateSuperStacks
; Parameter: CellView (CVId)
; Return: CellView (CVId)
;
; Create Super Stacks
;
;
;

defun( CreateSuperStacks ( CellView )

  ;stack the clumps
  ( mapcar
    (lambda ( Clump )
      ( SuperStackCreateStackGroupsFromClumpUsingPDKInfo 
        CellView
        Clump ) )
    ;inline the sub-clumps
    ( ListNonDestructiveMapCan
      (lambda ( X )
        X )
      ;break the clumps into sub-clumps
      ( mapcar
        (lambda ( Clump )
          ( ListSplitOnPredicate
            ( sort Clump (lambda ( C1 C2 ) ( lessp ( RectGetBottom ( getq C1 bBox ) )
                                                   ( RectGetBottom ( getq C2 bBox ) ) ) ) )
            (lambda ( C1 C2  )
              ( and
                ( exists C ( list C1 C2 )
                         ( equal ( getq C libName ) GateLibraryName ) )
                ( exists C ( list C1 C2 )
                         ( not ( equal ( getq C libName ) GateLibraryName ) ) ) ) ) ) )
        ;clumps of components
        ( mapcar
          `cadr
          ( tableToList
            ( FigGetClumpTable
              ( cadr
                ( NameFilterInstances
                  ( getq CellView instances )
                  ( append
                    GateLibCellPairRegExs
                    ( append
                      NChainLibCellPairRegExs
                      PChainLibCellPairRegExs ) ) ) ) ) ) ) ) ) )

                
  ( dbSave CellView )
  CellView
)


