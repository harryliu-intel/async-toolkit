; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

( list "Get Cell Area" "UIGetCellArea" )

( UICreateMenuItemCallBack 
  `UIGetCellAreaCB
  (lambda ()
       (let (area)
        area=(GetCellArea (geGetWindowCellView))
        (printf "%s -- %3.3fmm^2\n" (geGetWindowCellView)~>cellName area/1000000)
       )
))

