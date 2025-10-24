; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

( load "/p/eth400g/mby/romanpar/work/mby-mby-x0/wm/src/main/m3/hardrada/Similix-master/system/sim-scm.scm" )
( load "hw_lib/bcam.sim" )
( display "Result before PE: " )
( display ( cascade_lookup 3 ( list ( list 3 ) ) ) )
( newline )
( similix 'cascade_lookup ( list '*** '*** ) "hw_lib/bcam.sim" )
( load-residual-program )
( car ( residual-program ) )
( display "Result after PE: " )
( display ( cascade_lookup-0 3 ( list ( list 3 ) ) ) )
( newline )
( exit )
