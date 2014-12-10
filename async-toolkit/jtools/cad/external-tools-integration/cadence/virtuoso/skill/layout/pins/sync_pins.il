(defun DrawSyncPins (@key (CV (geGetEditCellView)))
 (let (props points layer isPin startVias endVias doubleVias flipX flipY pattern layers
              pitch busShield rotateMetals newpoints paths guideInst guideInstName
              patlist prelist pre buspat lo hi lohi options evens odds s pat I brkIdx path) 

 
   (rexCompile "^\\(.*\\)\\[\\([0-9]+\\)[\\.]*\\([0-9]*\\)\\]$")
   (DeleteBusWires) 
    paths         = nil
    pattern       = nil
    layers        = nil
    lohi          = nil
    paths = (GetSyncGuides ?CV CV)
    (foreach path paths
             lohi          =  nil
             props         =  (car path)
             points        =  (cadr path)
             patlist       =  (parseString (nth 0 props) " ")
             layer         =  (nth 1 props)
             prelist       =  (parseString (nth 2 props) " ")
             isPin         =  (nth 3 props)
             startVias     =  (nth 4 props)
             endVias       =  (nth 5 props)
             flipX         =  (nth 6 props)
             flipY         =  (nth 7 props)
             doubleVias    =  (nth 9 props)
             busShield     =  (nth 10 props)
             rotateMetals  =  (nth 11 props)

             ; iterate over BusPattern/BusPrefix lists
             (for n 0 (length patlist)-1
                  pat = (nth n patlist)
                  pre = (if prelist (nth (mod n (length prelist)) prelist) nil)

                  ; pick pattern and array range automatically
                  (when (and pattern==nil lohi==nil (rexExecute pat))
                    pat = (rexSubstitute "\\1")
                    lo = (rexSubstitute "\\2")
                    hi = (rexSubstitute "\\3")
                    (cond (hi=="" hi=(atoi lo)-1 lo=0)
                          (t      hi=(atoi hi)   lo=(atoi lo))
                          )
                    lohi = (list lo hi)
                    )
                  buspat = (if pattern==nil (MapBusPattern pat) pattern)
                           println(pat)
                           println(pre)
                           println(lohi)

                  ; draw
                  (when (and buspat pre lohi)
                    when( pat == "node_1W" ; && length(points)==2
                      brkIdx = expandSyncIdx(lo hi)
                      for( I 0 length(brkIdx)-2
                         when( I == 0
                           println("hello")
                           println(pre)
                           DrawChannelArray((ChooseLayers layers layer ?rotateMetals rotateMetals) 
                                             buspat pre nth(0 brkIdx):nth(1 brkIdx) list(0 0.13 0) 
                                             points ?isPin t ?drawShield nil)
                           newpoints = points
                            )
                         when( I > 0
                           println(newpoints)
                           newpoints = computeNewPts(newpoints)
                           DrawChannelArray((ChooseLayers layers layer ?rotateMetals rotateMetals) 
                                             buspat pre nth(I brkIdx)+1:nth(I+1 brkIdx) list(0 0.13 0) 
                                             newpoints ?isPin t ?drawShield nil)
                            )
                        ) 
                       )
                    )
                  when( pat == "node_1W" && lohi == nil
                      DrawChannel((ChooseLayers layers layer ?rotateMetals rotateMetals) 
                                    buspat pre points ?isPin t ?drawShield nil)                 
                   )
                 )
    )

fixSyncPinDirection(?CV CV)
t
 )
)

(defun expandSyncIdx (lo hi)
  (let ( s curr ) 

   s = list(lo)
   curr = lo + 9
   while( curr < hi
     s = append(s list(curr))
     curr = curr+10
    )

   s = append(s list(hi))

s
 )
)

(defun computeNewPts (points)
 (let ( newpoints )

   when(car(ClassifyPoints(points)) == "EH" || car(ClassifyPoints(points)) == "HE"
       newpoints = list(caar(points)+3.12:cadar(points) caadr(points)+3.12:cadadr(points))
      )

   when(car(ClassifyPoints(points)) == "EV" || car(ClassifyPoints(points)) == "VE"
       newpoints = list(caar(points):cadar(points)+3.12 caadr(points):cadadr(points)+3.12)
      )

newpoints
 )
)

(defun fixSyncPinDirection (@key (CV (geGetEditCellView))) 
  (let ( ne pname d_prefix e_prefix pnew )

  foreach( ne CV~>nets
      when( ne~>term~>name
       pname = parseString(ne~>term~>name "_")
;      when( member("i"   pname ) ne~>term~>direction = "input"  )
;      when( member("o"   pname ) ne~>term~>direction = "output" )
       when( nth(0 pname) == "i"  ne~>term~>direction = "input"  )
       when( nth(0 pname) == "o"  ne~>term~>direction = "output" )
;      when( member("in"  pname ) ne~>term~>direction = "input"  )
;      when( member("out" pname ) ne~>term~>direction = "output" )
       )
     )    
t
 )
)

(defun GetSyncGuides (@key (CV (geGetEditCellView)))
  (let (paths shapes pattern pre isPin startVias endVias flipX flipY pitch double shield rotate props)
     paths = nil
     (foreach obj CV~>shapes
             pattern   = (GetProp obj "BusPattern"             nil        )
             pre       = (GetProp obj "BusPrefix"              ""         )
             isPin     = (GetProp obj "BusPin"                 nil        )
             startVias = (GetProp obj "BusStartVias"           nil        )
             endVias   = (GetProp obj "BusEndVias"             nil        )
             flipX     = (GetProp obj "BusFlipX"               nil        )
             flipY     = (GetProp obj "BusFlipY"               nil        )
             pitch     = (GetProp obj "BusArrayPitch"          TrackPitch )
             double    = (GetProp obj "BusDoubleVias"          nil        )
             shield    = (GetProp obj "DrawBusShields"         nil        )
             rotate    = (GetProp obj "RotateMetalOrientation" nil        )
             props = (list pattern obj->lpp pre isPin startVias endVias flipX flipY pitch double shield rotate)
             (cond ((and obj->objType=="path" pattern!=nil ) ; (or name==nil pattern==name))
                    paths = (cons (list props obj->points) paths))
                   )
             )   
  
 paths
 )
)
