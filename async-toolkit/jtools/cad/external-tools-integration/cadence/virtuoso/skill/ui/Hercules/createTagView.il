; Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
; SPDX-License-Identifier: Apache-2.0

list( "0. Create Tag View" "createTagView" )


( UICreateMenuItemCallBack
  `createTagViewCB
  (lambda ()
    let(( CellView cellName newCV)
      CellView= UIGetCellView()
      MakeLayoutTag(CellView)
    )
) )
