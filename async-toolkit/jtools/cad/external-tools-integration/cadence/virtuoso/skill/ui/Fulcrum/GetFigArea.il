( list "Get Selected Fig Area" "UIGetFigArea" )

( UICreateMenuItemCallBack 
  `UIGetFigAreaCB
  (lambda ()
       (let (area fig)
        fig = car(geGetSelectedSet())
        area=GetFigArea(fig)
        if( fig~>objType == "inst"
         then
          (printf "%s -- %3.3fmm^2\n" fig~>cellName area/1000000)
         else   
          (printf "Selected Fig Area -- %3.3fmm^2\n" area/1000000)
        )
       )
))

