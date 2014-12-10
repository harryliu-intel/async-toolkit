( list "Get Cell Area" "UIGetCellArea" )

( UICreateMenuItemCallBack 
  `UIGetCellAreaCB
  (lambda ()
       (let (area)
        area=(GetCellArea (geGetWindowCellView))
        (printf "%s -- %3.3fmm^2\n" (geGetWindowCellView)~>cellName area/1000000)
       )
))

