; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id: //depot/sw/intel/cad/external-tools-integration/cadence/virtuoso/skill/ui/Fulcrum/Pins/DrawPins.il#4 $
; $DateTime: 2012/07/11 17:39:18 $
; $Author: pankala $

( list "4. Make Abstract" "UIMakeAbstract" )


( UICreateMenuItemCallBack 
  `UIMakeAbstractCB
  (lambda ()
    (MakeMidAbstract)
    (View "abstract")
  )
)


