; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

( list "Lock Folds" "UILockFolds" )

( UICreateMenuItemCallBack
  `UILockFoldsCB
  (lambda ()
    (let (
          ( CellView ( UIGetCellView ) ) )
      (let (
            ( Gates ( cadr ( NameFilterInstances 
                             ( setof Fig ( geGetSelectedSet ) ( equal ( getq Fig objType ) "inst" ) )
                             GateLibCellPairRegExs ) ) ) )
        ( foreach Gate Gates
                  (let (
                        ( Folds ( NPSearchGateGetFolds Gate ) ) )
                    (when Folds
                      ( NPSearchGateSetFolds Gate Folds )
                      ( NPSearchGateSetOverrideFolds Gate t ) ) ) ) ) ) ) )
