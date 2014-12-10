; Copyright 2003 Fulcrum Microsy\stems.  All rights reserved.
; $Id: //depot/sw/intel/cad/external-tools-integration/cadence/virtuoso/skill/layout/bus/shield.il#1 $
; $DateTime: 2012/07/27 17:24:08 $
; $Author: pankala $

(defun DrawBusShield ( points layers prefix @key (CellVue (UIGetCellView)) (shieldM2 t)) 
  let( ( Idx NPoints HLayer VLayer 
         CurrPoint NextPoint CurrLayer Segments
         ShieldDir ShieldPoint ShieldPointX ShieldPointY ShieldLength ShieldInst ShieldInstName
         CurrXQuad CurrYQuad (types (ClassifyPoints points))
        )
  
   HLayer = car(nth(0 layers))
   VLayer = car(nth(1 layers))
   NPoints = length(points)

  for( Idx 0 NPoints-2 

    CurrPoint = nth(Idx points)

    CurrXQuad = car(ShieldFindQuad(CurrPoint))
    CurrYQuad = cadr(ShieldFindQuad(CurrPoint))

  when( Idx == 0 
    NextPoint = nth(Idx+1 points)

    if( almostEqual(car(CurrPoint) car(NextPoint))
     then
       CurrLayer = VLayer
        if( NPoints > 2
         then
          Segments = round((abs(cadr(NextPoint)*1000 - cadr(CurrPoint)*1000) - 780)/(TrackPitch*1000))
         else
          Segments = round(abs(cadr(NextPoint)*1000 - cadr(CurrPoint)*1000)/(TrackPitch*1000))
         )
       if( cadr(NextPoint) > cadr(CurrPoint)
        then
          ShieldDir = "V_R0"
          if( CurrYQuad > 0
            then
              ShieldPointY = truncate((cadr(CurrPoint)*1000)/(TrackPitch*1000))*TrackPitch
            else
              ShieldPointY = round((cadr(CurrPoint)*1000)/(TrackPitch*1000))*TrackPitch
            )
          if( CurrXQuad > 0
            then
              ShieldPointX = truncate((car(CurrPoint)*1000)/(TrackPitch*1000))*TrackPitch
            else
              ShieldPointX = (truncate((car(CurrPoint)*1000)/(TrackPitch*1000)) + 1*CurrXQuad)*TrackPitch
            )
        else
          ShieldDir = "V_MX"
          if( CurrYQuad > 0
            then
              ShieldPointY = round((cadr(CurrPoint)*1000)/(TrackPitch*1000))*TrackPitch
            else
              ShieldPointY = truncate((cadr(CurrPoint)*1000)/(TrackPitch*1000))*TrackPitch
            )
          if( CurrXQuad > 0
            then
              ShieldPointX = truncate((car(CurrPoint)*1000)/(TrackPitch*1000))*TrackPitch
            else
              ShieldPointX = (truncate((car(CurrPoint)*1000)/(TrackPitch*1000)) + 1*CurrXQuad)*TrackPitch
            )
         )
     else
       CurrLayer = HLayer
        if( NPoints > 2
         then
          Segments = round((abs(car(NextPoint)*1000 - car(CurrPoint)*1000) - 780)/(TrackPitch*1000))
         else
          Segments = round(abs(car(NextPoint)*1000 - car(CurrPoint)*1000)/(TrackPitch*1000))
         )
       if( car(NextPoint) > car(CurrPoint)
        then
          ShieldDir = "H_R0"
          if( CurrYQuad > 0
            then
              ShieldPointY = truncate((cadr(CurrPoint)*1000)/(TrackPitch*1000))*TrackPitch
            else
              ShieldPointY = (truncate((cadr(CurrPoint)*1000)/(TrackPitch*1000)) + 1*CurrYQuad)*TrackPitch
            )
          if( CurrXQuad > 0
            then
              ShieldPointX = truncate((car(CurrPoint)*1000)/(TrackPitch*1000))*TrackPitch
            else
              ShieldPointX = round((car(CurrPoint)*1000)/(TrackPitch*1000))*TrackPitch
            )
        else
          ShieldDir = "H_MY"
          if( CurrYQuad > 0
            then
              ShieldPointY = truncate((cadr(CurrPoint)*1000)/(TrackPitch*1000))*TrackPitch
            else
              ShieldPointY = (truncate((cadr(CurrPoint)*1000)/(TrackPitch*1000)) + 1*CurrYQuad)*TrackPitch
            )
          if( CurrXQuad > 0
            then
              ShieldPointX = round((car(CurrPoint)*1000)/(TrackPitch*1000))*TrackPitch
            else
              ShieldPointX = truncate((car(CurrPoint)*1000)/(TrackPitch*1000))*TrackPitch
            )
         )
       )
   ShieldPoint = list(ShieldPointX ShieldPointY)
   when( !OverlapShield(CellVue ShieldPoint CurrLayer ShieldDir Segments)
       DrawShieldShapes(CellVue Segments ShieldPoint ShieldDir CurrLayer ?shieldM2 shieldM2)
     )
   )

  when( Idx > 0 && Idx < NPoints-2
    NextPoint = nth(Idx+1 points)

    if( almostEqual(car(CurrPoint) car(NextPoint))
     then
       CurrLayer = VLayer
       Segments = round(abs(cadr(NextPoint) - cadr(CurrPoint))/TrackPitch) - 1
       if( cadr(NextPoint) > cadr(CurrPoint)
        then
          ShieldDir = "V_R0"
          if( CurrYQuad > 0
            then
              ShieldPointY = (truncate((cadr(CurrPoint)*1000)/(TrackPitch*1000)) + 1*CurrYQuad)*TrackPitch
            else
              ShieldPointY = truncate((cadr(CurrPoint)*1000)/(TrackPitch*1000))*TrackPitch
            )
          if( CurrXQuad > 0
            then
              ShieldPointX = truncate((car(CurrPoint)*1000)/(TrackPitch*1000))*TrackPitch
            else
              ShieldPointX = (truncate((car(CurrPoint)*1000)/(TrackPitch*1000)) + 1*CurrXQuad)*TrackPitch
            )
        else
          ShieldDir = "V_MX"
          if( CurrYQuad > 0
            then
              ShieldPointY = truncate((cadr(CurrPoint)*1000)/(TrackPitch*1000))*TrackPitch
            else
              ShieldPointY = (truncate((cadr(CurrPoint)*1000)/(TrackPitch*1000)) + 1*CurrYQuad)*TrackPitch
            )
          if( CurrXQuad > 0
            then
              ShieldPointX = truncate((car(CurrPoint)*1000)/(TrackPitch*1000))*TrackPitch
            else
              ShieldPointX = (truncate((car(CurrPoint)*1000)/(TrackPitch*1000)) + 1*CurrXQuad)*TrackPitch
            )
         )
     else
       CurrLayer = HLayer
       Segments = round(abs(car(NextPoint) - car(CurrPoint))/TrackPitch) - 1
       if( car(NextPoint) > car(CurrPoint)
        then
          ShieldDir = "H_R0"
          if( CurrYQuad > 0
            then
              ShieldPointY = truncate((cadr(CurrPoint)*1000)/(TrackPitch*1000))*TrackPitch
            else
              ShieldPointY = (truncate((cadr(CurrPoint)*1000)/(TrackPitch*1000)) + 1*CurrYQuad)*TrackPitch
            )
          if( CurrXQuad > 0
            then
              ShieldPointX = (truncate((car(CurrPoint)*1000)/(TrackPitch*1000)) + 1*CurrXQuad)*TrackPitch
            else
              ShieldPointX = truncate((car(CurrPoint)*1000)/(TrackPitch*1000))*TrackPitch
            )
        else
          ShieldDir = "H_MY"
          if( CurrYQuad > 0
            then
              ShieldPointY = truncate((cadr(CurrPoint)*1000)/(TrackPitch*1000))*TrackPitch
            else
              ShieldPointY = (truncate((cadr(CurrPoint)*1000)/(TrackPitch*1000)) + 1*CurrYQuad)*TrackPitch
            )
          if( CurrXQuad > 0
            then
              ShieldPointX = truncate((car(CurrPoint)*1000)/(TrackPitch*1000))*TrackPitch
            else
              ShieldPointX = (truncate((car(CurrPoint)*1000)/(TrackPitch*1000)) + 1*CurrXQuad)*TrackPitch
            )
         )
       )
   ShieldPoint = list(ShieldPointX ShieldPointY)
   when( !OverlapShield(CellVue ShieldPoint CurrLayer ShieldDir Segments)
   DrawShieldShapes(CellVue Segments ShieldPoint ShieldDir CurrLayer ?shieldM2 shieldM2)
    )
   )

   when( Idx == NPoints-2 && Idx != 0
    NextPoint = nth(Idx+1 points)

    if( almostEqual(car(CurrPoint) car(NextPoint))
     then
       CurrLayer = VLayer
        if( NPoints > 2
         then
          Segments = round((abs(cadr(NextPoint)*1000 - cadr(CurrPoint)*1000) - 780)/(TrackPitch*1000))
         else
          Segments = round(abs(cadr(NextPoint)*1000 - cadr(CurrPoint)*1000)/(TrackPitch*1000))
         )
       if( cadr(NextPoint) > cadr(CurrPoint)
        then
          ShieldDir = "V_R0"
          if( CurrYQuad > 0
            then
              ShieldPointY = (truncate((cadr(CurrPoint)*1000)/(TrackPitch*1000)) + 1)*TrackPitch
            else
              ShieldPointY = truncate((cadr(CurrPoint)*1000)/(TrackPitch*1000))*TrackPitch
            )
          if( CurrXQuad > 0
            then
              ShieldPointX = truncate((car(CurrPoint)*1000)/(TrackPitch*1000))*TrackPitch
            else
              ShieldPointX = (truncate((car(CurrPoint)*1000)/(TrackPitch*1000)) + 1*CurrXQuad)*TrackPitch
            )
        else
          ShieldDir = "V_MX"
          if( CurrYQuad > 0
            then
              ShieldPointY = truncate((cadr(CurrPoint)*1000)/(TrackPitch*1000))*TrackPitch
            else
              ShieldPointY = (truncate((cadr(CurrPoint)*1000)/(TrackPitch*1000)) - 1)*TrackPitch
            )
          if( CurrXQuad > 0
            then
              ShieldPointX = truncate((car(CurrPoint)*1000)/(TrackPitch*1000))*TrackPitch
            else
              ShieldPointX = (truncate((car(CurrPoint)*1000)/(TrackPitch*1000)) + 1*CurrXQuad)*TrackPitch
            )
         )
     else
       CurrLayer = HLayer
        if( NPoints > 2
         then
          Segments = round((abs(car(NextPoint)*1000 - car(CurrPoint)*1000) - 780)/(TrackPitch*1000))
         else
          Segments = round(abs(car(NextPoint)*1000 - car(CurrPoint)*1000)/(TrackPitch*1000))
         )
       if( car(NextPoint) > car(CurrPoint)
        then
          ShieldDir = "H_R0"
          if( CurrYQuad > 0
            then
              ShieldPointY = truncate((cadr(CurrPoint)*1000)/(TrackPitch*1000))*TrackPitch
            else
              ShieldPointY = (truncate((cadr(CurrPoint)*1000)/(TrackPitch*1000)) + 1*CurrYQuad)*TrackPitch
            )
          if( CurrXQuad > 0
            then
              ShieldPointX = (truncate((car(CurrPoint)*1000)/(TrackPitch*1000)) + 1)*TrackPitch
            else
              ShieldPointX = truncate((car(CurrPoint)*1000)/(TrackPitch*1000))*TrackPitch
            )
        else
          ShieldDir = "H_MY"
          if( CurrYQuad > 0
            then
              ShieldPointY = truncate((cadr(CurrPoint)*1000)/(TrackPitch*1000))*TrackPitch
            else
              ShieldPointY = (truncate((cadr(CurrPoint)*1000)/(TrackPitch*1000)) + 1*CurrYQuad)*TrackPitch
            )
          if( CurrXQuad > 0
            then
              ShieldPointX = truncate((car(CurrPoint)*1000)/(TrackPitch*1000))*TrackPitch
            else
              ShieldPointX = (truncate((car(CurrPoint)*1000)/(TrackPitch*1000)) - 1)*TrackPitch
            )
         )
       )
   ShieldPoint = list(ShieldPointX ShieldPointY)
   when( !OverlapShield(CellVue ShieldPoint CurrLayer ShieldDir Segments)
   DrawShieldShapes(CellVue Segments ShieldPoint ShieldDir CurrLayer ?shieldM2 shieldM2)
    )
   )


    )
  t
 )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun OverlapShield (CellVue ShieldPoint CurrLayer ShieldDir Segments)
 let( ( ShieldbBoxFirst ShieldbBoxLast ShieldPresent ShieldShape SegInc ) 
   if( cadr(parseString(ShieldDir "_")) == "R0"
    then
      SegInc = 1
    else
      SegInc = -1
    )
     when( car(parseString(ShieldDir "_")) == "H"
       ShieldbBoxFirst = list(car(ShieldPoint)+(0.78*SegInc):cadr(ShieldPoint)+0.045 car(ShieldPoint)+(0.80*SegInc):cadr(ShieldPoint)+0.095)
       ShieldbBoxLast  = list(car(ShieldPoint)+(0.78*(Segments-1)*SegInc):cadr(ShieldPoint)+0.045 car(ShieldPoint)+(0.80*(Segments-1)*SegInc):cadr(ShieldPoint)+0.095)
      )
     when( car(parseString(ShieldDir "_")) == "V"
       ShieldbBoxFirst = list(car(ShieldPoint)+0.045:cadr(ShieldPoint)+(0.78*SegInc) car(ShieldPoint)+0.095:cadr(ShieldPoint)+(0.80*SegInc))
       ShieldbBoxLast  = list(car(ShieldPoint)+0.045:cadr(ShieldPoint)+(0.78*(Segments-1)*SegInc) car(ShieldPoint)+0.095:cadr(ShieldPoint)+(0.80*(Segments-1)*SegInc))
      )    
    if( setof(ShieldShape dbGetOverlaps(CellVue ShieldbBoxFirst) ShieldShape~>BusScriptObject&&car(ShieldShape~>lpp)==CurrLayer) && setof(ShieldShape dbGetOverlaps(CellVue ShieldbBoxLast) ShieldShape~>BusScriptObject&&car(ShieldShape~>lpp)==CurrLayer)
      then
        ShieldPresent = t
      else
        ShieldPresent = nil
     )
 ShieldPresent
 )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun DrawShieldShapes (CellVue Segments ShieldPoint ShieldDir CurrLayer @key (shieldM2 t))
  let( ( Idx  ShieldbBox1 ShieldbBox2 ShieldObj1 ShieldObj2 SegInc ShieldbBox1M2 ShieldbBox2M2 ShieldViolationM2 ShieldViolation1M2 ShieldViolation2M2
         RBlockObj1 RBlockObj2 VBlockObj1 VBlockObj2 M2Block1 M2Block2 viaid ShieldVia1 ShieldVia2
        )

   if( cadr(parseString(ShieldDir "_")) == "R0"
    then
      SegInc = 1
    else
      SegInc = -1
    )

   for( Idx 0 Segments-1
     when( car(parseString(ShieldDir "_")) == "H"
       ShieldbBox1 = list(car(ShieldPoint)+TrackPitch*Idx*SegInc:cadr(ShieldPoint)+0.045 car(ShieldPoint)+(TrackPitch*Idx+1.355)*SegInc:cadr(ShieldPoint)+0.095)
       ShieldbBox2 = list(car(ShieldPoint)+TrackPitch*Idx*SegInc:cadr(ShieldPoint)+1.465 car(ShieldPoint)+(TrackPitch*Idx+1.355)*SegInc:cadr(ShieldPoint)+1.515)
       when( SegInc == -1
       ShieldbBox1 = list(caar(ShieldbBox1)-0.205:cadar(ShieldbBox1) caadr(ShieldbBox1)-0.205:cadadr(ShieldbBox1))
       ShieldbBox2 = list(caar(ShieldbBox2)-0.205:cadar(ShieldbBox2) caadr(ShieldbBox2)-0.205:cadadr(ShieldbBox2))
       )
       ShieldObj1 = dbCreateRect(CellVue CurrLayer ShieldbBox1)
       ShieldObj2 = dbCreateRect(CellVue CurrLayer ShieldbBox2)
       RBlockObj1 = dbCreateLayerBlockage(CellVue CurrLayer "routing" BBoxToPoints(ShieldbBox1))
       RBlockObj2 = dbCreateLayerBlockage(CellVue CurrLayer "routing" BBoxToPoints(ShieldbBox2))
       SetBusProp(ShieldObj1)
       SetBusProp(ShieldObj2)
       SetBusProp(RBlockObj1)
       SetBusProp(RBlockObj2)
      )

       if( SegInc == 1
       then
       ShieldbBox1M2 = list(car(ShieldPoint)-0.105:cadr(ShieldPoint)+(TrackPitch*Idx-0.105)*SegInc car(ShieldPoint)+0.105:cadr(ShieldPoint)+(TrackPitch*Idx+1.365)*SegInc)
       ShieldbBox2M2 = list(car(ShieldPoint)+1.455:cadr(ShieldPoint)+(TrackPitch*Idx-0.105)*SegInc car(ShieldPoint)+1.665:cadr(ShieldPoint)+(TrackPitch*Idx+1.365)*SegInc)
       else
       ShieldbBox1M2 = list(car(ShieldPoint)-0.105:cadr(ShieldPoint)-1.56+(TrackPitch*Idx-0.105)*SegInc car(ShieldPoint)+0.105:cadr(ShieldPoint)-1.56+(TrackPitch*Idx+1.365)*SegInc)
       ShieldbBox2M2 = list(car(ShieldPoint)+1.455:cadr(ShieldPoint)-1.56+(TrackPitch*Idx-0.105)*SegInc car(ShieldPoint)+1.665:cadr(ShieldPoint)-1.56+(TrackPitch*Idx+1.365)*SegInc)
       )
;      when( CurrLayer == "M2"
;       ShieldObj1 = dbCreateRect(CellVue "PO" ShieldbBox1M2)
;       ShieldObj2 = dbCreateRect(CellVue "PO" ShieldbBox2M2)
;       SetBusProp(ShieldObj1)
;       SetBusProp(ShieldObj2)
;
;       )
       ShieldViolationM2 = nil 
       if( CurrLayer == "M2" && dbGetOverlaps(CellVue ShieldbBox1M2 "M2" 32) == nil 
         then 
           ShieldViolationM2 = nil
           ShieldViolation1M2 = nil
         else
           ShieldViolationM2 = t
           ShieldViolation1M2 = t
        )
  
       if( CurrLayer == "M2" && dbGetOverlaps(CellVue ShieldbBox2M2 "M2" 32) == nil
         then 
           ShieldViolationM2 = nil
           ShieldViolation2M2 = nil
         else
           ShieldViolationM2 = t
           ShieldViolation1M2 = t
        )

     when( ( car(parseString(ShieldDir "_")) == "V" && CurrLayer != "M2" ) || (CurrLayer == "M2" && !ShieldViolationM2)
       ShieldbBox1 = list(car(ShieldPoint)+0.045:cadr(ShieldPoint)+TrackPitch*Idx*SegInc car(ShieldPoint)+0.095:cadr(ShieldPoint)+(TrackPitch*Idx+1.365)*SegInc)
       ShieldbBox2 = list(car(ShieldPoint)+1.465:cadr(ShieldPoint)+TrackPitch*Idx*SegInc car(ShieldPoint)+1.515:cadr(ShieldPoint)+(TrackPitch*Idx+1.365)*SegInc)
       when( SegInc == -1
       ShieldbBox1 = list(caar(ShieldbBox1):cadar(ShieldbBox1)-0.195 caadr(ShieldbBox1):cadadr(ShieldbBox1)-0.195)
       ShieldbBox2 = list(caar(ShieldbBox2):cadar(ShieldbBox2)-0.195 caadr(ShieldbBox2):cadadr(ShieldbBox2)-0.195)
       )
       when( ( car(parseString(ShieldDir "_")) == "V" && CurrLayer != "M2" ) || (CurrLayer == "M2" && !ShieldViolation1M2)
         ShieldObj1 = dbCreateRect(CellVue CurrLayer ShieldbBox1)
         SetBusProp(ShieldObj1)
         RBlockObj1 = dbCreateLayerBlockage(CellVue CurrLayer "routing" BBoxToPoints(ShieldbBox1))
         SetBusProp(RBlockObj1)
        )
       when( ( car(parseString(ShieldDir "_")) == "V" && CurrLayer != "M2" ) || (CurrLayer == "M2" && !ShieldViolation2M2)
         ShieldObj2 = dbCreateRect(CellVue CurrLayer ShieldbBox2)
         SetBusProp(ShieldObj2)
         RBlockObj2 = dbCreateLayerBlockage(CellVue CurrLayer "routing" BBoxToPoints(ShieldbBox2))
         SetBusProp(RBlockObj2)
        )
       when( CurrLayer == "M2" && shieldM2
         ShieldbBox1 = list(car(ShieldPoint)-0.105:cadr(ShieldPoint)+(TrackPitch*Idx-0.105)*SegInc car(ShieldPoint)+0.105:cadr(ShieldPoint)+(TrackPitch*Idx+0.105)*SegInc)
         ShieldbBox2 = list(car(ShieldPoint)+1.455:cadr(ShieldPoint)+(TrackPitch*Idx-0.105)*SegInc car(ShieldPoint)+1.665:cadr(ShieldPoint)+(TrackPitch*Idx+0.105)*SegInc)
       when( SegInc == -1
         ShieldbBox1 = list(car(ShieldPoint)-0.105:cadr(ShieldPoint)-1.56+(TrackPitch*Idx-0.105)*SegInc car(ShieldPoint)+0.105:cadr(ShieldPoint)-1.56+(TrackPitch*Idx+0.105)*SegInc)
         ShieldbBox2 = list(car(ShieldPoint)+1.455:cadr(ShieldPoint)-1.56+(TrackPitch*Idx-0.105)*SegInc car(ShieldPoint)+1.665:cadr(ShieldPoint)-1.56+(TrackPitch*Idx+0.105)*SegInc)
       )
       when( ( car(parseString(ShieldDir "_")) == "V" && CurrLayer != "M2" ) || (CurrLayer == "M2" && !ShieldViolation1M2)
         M2Block1 = dbCreateRect(CellVue CurrLayer ShieldbBox1)
         RBlockObj1 = dbCreateLayerBlockage(CellVue CurrLayer "routing" BBoxToPoints(ShieldbBox1))
         SetBusProp(RBlockObj1)
         SetBusProp(M2Block1)
        )
       when( ( car(parseString(ShieldDir "_")) == "V" && CurrLayer != "M2" ) || (CurrLayer == "M2" && !ShieldViolation2M2)
         M2Block2 = dbCreateRect(CellVue CurrLayer ShieldbBox2)
         RBlockObj2 = dbCreateLayerBlockage(CellVue CurrLayer "routing" BBoxToPoints(ShieldbBox2))
         SetBusProp(RBlockObj2)
         SetBusProp(M2Block2)
        )
       viaid = techFindViaDefByName(techGetTechFile(CellVue) "M3_M2_R_H")
       if( SegInc == 1
        then
         when( ( car(parseString(ShieldDir "_")) == "V" && CurrLayer != "M2" ) || (CurrLayer == "M2" && !ShieldViolation1M2)
            ShieldVia1 = dbCreateVia(CellVue viaid list(car(ShieldPoint) cadr(ShieldPoint)+(TrackPitch*Idx+0.065)*SegInc) "R0")
            )
         when( ( car(parseString(ShieldDir "_")) == "V" && CurrLayer != "M2" ) || (CurrLayer == "M2" && !ShieldViolation2M2)
            ShieldVia2 = dbCreateVia(CellVue viaid list(car(ShieldPoint)+TrackPitch cadr(ShieldPoint)+(TrackPitch*Idx+0.065)*SegInc) "R0")
            )
        else
         when( ( car(parseString(ShieldDir "_")) == "V" && CurrLayer != "M2" ) || (CurrLayer == "M2" && !ShieldViolation1M2)
          ShieldVia1 = dbCreateVia(CellVue viaid list(car(ShieldPoint) cadr(ShieldPoint)-1.56+(TrackPitch*Idx-0.065)*SegInc) "R0")
            )
         when( ( car(parseString(ShieldDir "_")) == "V" && CurrLayer != "M2" ) || (CurrLayer == "M2" && !ShieldViolation2M2)
          ShieldVia2 = dbCreateVia(CellVue viaid list(car(ShieldPoint)+TrackPitch cadr(ShieldPoint)-1.56+(TrackPitch*Idx-0.065)*SegInc) "R0")           
            )
         )
       SetBusProp(ShieldVia1)
       SetBusProp(ShieldVia2)
        )

      ) ;when



    ) ;for
   
  t
 )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun BBoxToPoints (bBox)
   list(car(bBox) caadr(bBox):cadar(bBox) cadr(bBox) caar(bBox):cadadr(bBox))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ShieldSetProps (ShieldInst CurrLayer ShieldLength)
  let( (ShieldLayer layer)

    ShieldInst~>SegLength = ShieldLength

    for( layer 2 7
      sprintf(ShieldLayer "M%d" layer)
      if( ShieldLayer == CurrLayer
       then
         dbReplaceProp(ShieldInst ShieldLayer "boolean" "TRUE")
         println(ShieldLayer)
       else
         dbReplaceProp(ShieldInst ShieldLayer "boolean" "FALSE")
      )
    )
  t
 )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ShieldFindQuad (point)
  let( (XQuad YQuad)

    if( car(CurrPoint) >= 0
     then
       XQuad = 1
     else
       XQuad = -1
    )

    if( cadr(CurrPoint) >= 0
     then
       YQuad = 1
     else
       YQuad = -1
    )

  list(XQuad YQuad)
 )
)
