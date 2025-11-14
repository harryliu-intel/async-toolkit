; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

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
