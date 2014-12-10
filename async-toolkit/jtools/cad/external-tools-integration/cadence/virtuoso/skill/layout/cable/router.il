; Copyright 2003 Fulcrum Microsy\stems.  All rights reserved.
; $Id: //depot/sw/intel/cad/external-tools-integration/cadence/virtuoso/skill/layout/cable/router.il#1 $
; $DateTime: 2012/07/27 17:24:08 $
; $Author: pankala $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
defun( DrawCablePins (CableShape CellVue Template)
  let( ( 
        IpProbeXP IpProbeXN IpProbeYP IpProbeYN CableWidth
        OpProbeXP OpProbeXN OpProbeYP OpProbeYN
        IpRouteList OpRouteList
        Idx TempArray
        )

   TempArray = CBLCreateTempArray(Template)
   StartPt = nth(0 CableShape~>points)
   EndPt   = nth(CableShape~>nPoints-1 CableShape~>points)


      CableWidth = CableShape~>CableWidth
      when( stringp(CableWidth)
       CableWidth = evalstring(CableShape~>CableWidth)
       )

   IpRouteList = list(StartPt car(CBLLocateAtDist(CableShape StartPt 0.39)))

   OpRouteList = list(EndPt car(CBLLocateAtDist(CableShape EndPt -0.39)))

   for( Idx 0 CableWidth - 1
     DrawChannel(nil DefChan(list(TempArray[Idx])) "L" IpRouteList ?isPin t)
     DrawChannel(nil DefChan(list(TempArray[Idx])) "R" OpRouteList ?isPin t)
    )

  t
 )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
defun( RouteCableBuffers (@optional (CableShape car(geGetSelectedSet())) (CellVue (UIGetCellView))) 
  let( ( BufIdx HopIdx
         Hops Bufs BufArray CableWidth
         temp 
        )

      (DeleteBusWires)
      DrawCablePins(CableShape CellVue "Template_A")
      BufArray = CBLCreateBufArray()

      CableWidth = CableShape~>CableWidth
      when( stringp(CableWidth)
       CableWidth = evalstring(CableShape~>CableWidth)
       )
      Hops = CableShape~>BufferHops      
      when( stringp(Hops)
         Hops = evalstring(CableShape~>BufferHops)
       )

      if( evenp(CableWidth)
       then
         Bufs = truncate(CableWidth)/2 - 1
       else
         Bufs = truncate(CableWidth)/2 - 1
      )

      for( BufIdx 0 Bufs
          TrBufIdx = BufArray[2*BufIdx]
          when( BufIdx <= CableWidth
          CBLPlaceWiringInst(CableShape 0 2*BufIdx "Template_A")
          temp = CBLRouteBufferInput(CableShape 0 2*BufIdx "Template_A" "Input")
          )
         )

      for( HopIdx 1 Hops-1
        for( BufIdx 0 Bufs
          TrBufIdx = BufArray[2*BufIdx]
          when( BufIdx <= CableWidth
          CBLPlaceWiringInst(CableShape HopIdx 2*BufIdx "Template_A")
          temp = CBLRouteBufferInput(CableShape HopIdx 2*BufIdx "Template_A" "Buffer")
           )
         )
       )

        for( BufIdx 0 Bufs
          TrBufIdx = BufArray[2*BufIdx]
          when( BufIdx <= CableWidth
          temp = CBLRouteBufferInput(CableShape Hops 2*BufIdx "Template_A" "Output")
         )
        )
  t
 )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
defun( CBLFindCableSegPoints (CableShape StartPt EndPt)
  let( ( 
         StartIdx
         EndIdx
         SegPoints
        )
  
    StartIdx = cadr(CBLFindPtIdxs(CableShape StartPt))
    EndIdx   = cadr(CBLFindPtIdxs(CableShape EndPt))

    SegPoints = list(EndPt)
    I = EndIdx 

   when( EndIdx - StartIdx > 0
    while( I > StartIdx
       SegPoints = cons(nth(I-1 CableShape~>points) SegPoints)
       I = I - 1
     )
    )

    SegPoints = cons(StartPt SegPoints)

   SegPoints
 )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
defun( CBLRoundPts (RouteList)
  let( (
        TrRouteList AdjustedXY
        )

    TrRouteList = nil
  
    for( I 0 length(RouteList)-1
     
;   println(I)
    AdjustedXY  = nil
 
     when( I == 0
        if( cadr(nth(I RouteList)) == cadr(nth(I+1 RouteList))
          then
             when( (truncate(cadr(nth(I RouteList))/3.12)*3.12)+0.78 != cadr(nth(I RouteList))
                TrRouteList = append(TrRouteList list(list(car(nth(I RouteList)) (truncate(cadr(nth(I RouteList))/3.12)*3.12)+0.78)))
                AdjustedXY = t
               )
          else
             when( (truncate(car(nth(I RouteList))/3.12)*3.12)+0.78 != car(nth(I RouteList))
                TrRouteList = append(TrRouteList list(list((truncate(car(nth(I RouteList))/3.12)*3.12)+0.78 cadr(nth(I RouteList))))) 
                AdjustedXY = t
               )
           )
          when( AdjustedXY == nil
                TrRouteList = append(TrRouteList list(nth(I RouteList)))
           )
         )

     when( I == length(RouteList)-1
        if( cadr(nth(I RouteList)) == cadr(nth(I-1 RouteList))
          then
             when( (truncate(cadr(nth(I RouteList))/3.12)*3.12)+0.78 != cadr(nth(I RouteList))
                TrRouteList = append(TrRouteList list(list(car(nth(I RouteList)) (truncate(cadr(nth(I RouteList))/3.12)*3.12)+0.78)))
               )
          else
             when( (truncate(car(nth(I RouteList))/3.12)*3.12)+0.78 != car(nth(I RouteList))
                TrRouteList = append(TrRouteList list(list((truncate(car(nth(I RouteList))/3.12)*3.12)+0.78 cadr(nth(I RouteList)))))
               )
           )
          when( AdjustedXY == nil
                TrRouteList = append(TrRouteList list(nth(I RouteList)))
           )
         )

     when( I > 0 && I < length(RouteList)-1
       when( (truncate(cadr(nth(I RouteList))/3.12)*3.12)+0.78 != cadr(nth(I RouteList)) && (truncate(car(nth(I RouteList))/3.12)*3.12)+0.78 != car(nth(I RouteList))
             TrRouteList = append(TrRouteList list(list((truncate(car(nth(I RouteList))/3.12)*3.12)+0.78 (truncate(cadr(nth(I RouteList))/3.12)*3.12)+0.78)))
             AdjustedXY = t
               )
        when( (truncate(cadr(nth(I RouteList))/3.12)*3.12)+0.78 != cadr(nth(I RouteList)) && AdjustedXY == nil
             TrRouteList = append(TrRouteList list(list(car(nth(I RouteList)) (truncate(cadr(nth(I RouteList))/3.12)*3.12)+0.78)))
             AdjustedXY = t
               )
        when( (truncate(car(nth(I RouteList))/3.12)*3.12)+0.78 != car(nth(I RouteList)) && AdjustedXY == nil
             TrRouteList = append(TrRouteList list(list((truncate(car(nth(I RouteList))/3.12)*3.12)+0.78 cadr(nth(I RouteList)))))
             AdjustedXY = t
               )
          when( AdjustedXY == nil
                TrRouteList = append(TrRouteList list(nth(I RouteList)))
              )
         )

      )
    TrRouteList
 )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
defun( CBLRouteBufferInput (CableShape HopIdx BufIdx TempType RouteType @key (CellVue (UIGetCellView)))
  let( ( TempArray
         OpEvInst OpEvInstName
         OpOdInst OpOdInstName
         IpEvInst IpEvInstName
         IpOdInst IpOdInstName
         StartPt StartSeg TrStartPt
         EndPt EndSeg TrEndPt
         RouteList TrRouteList
         IpSegOp OpSegOp
         EvNetName OdNetName
        )

   TempArray = CBLCreateTempArray(TempType)

   sprintf(OpEvInstName "mbuf[%d][%d]" HopIdx BufIdx)
   sprintf(OpOdInstName "mbuf[%d][%d]" HopIdx BufIdx+1)
   sprintf(IpEvInstName "mbuf[%d][%d]" HopIdx-1 BufIdx)
   sprintf(IpOdInstName "mbuf[%d][%d]" HopIdx-1 BufIdx+1)

   OpEvInst = dbFindAnyInstByName(CellVue OpEvInstName)
   OpOdInst = dbFindAnyInstByName(CellVue OpOdInstName)
   IpEvInst = dbFindAnyInstByName(CellVue IpEvInstName)
   IpOdInst = dbFindAnyInstByName(CellVue IpOdInstName)
 
  when( IpEvInst != nil || OpEvInst != nil

   IpSegOp = CBLFindInstCableSeg(CableShape IpEvInst)
   OpSegOp = CBLFindInstCableSeg(CableShape OpEvInst)
      

   StartPt = cadr(IpSegOp)
   EndPt   = cadr(OpSegOp)

   StartSeg = car(IpSegOp)
   EndSeg   = car(OpSegOp)

   case( StartSeg

      ( "H" 
        if( IpEvInst~>orient == "R0"
         then
          TrStartPt = list(car(StartPt)+6.24 cadr(StartPt))
         else
          TrStartPt = list(car(StartPt)-6.24 cadr(StartPt))
         )
       )

      ( "V" 
        if( IpEvInst~>orient == "R0"
         then
          TrStartPt = list(car(StartPt) cadr(StartPt)+6.24)
         else
          TrStartPt = list(car(StartPt) cadr(StartPt)-6.24)
         )
       )

      )

   case( RouteType
      
      ( "Buffer"
        RouteList = CBLFindCableSegPoints(CableShape TrStartPt EndPt)
       )

      ( "Input"
        RouteList = CBLFindCableSegPoints(CableShape nth(0 CableShape~>points) EndPt)
       )

      ( "Output"
        RouteList = CBLFindCableSegPoints(CableShape TrStartPt nth(length(CableShape~>points)-1 CableShape~>points))
       )

     )

    ;   RouteList = CBLFindCableSegPoints(CableShape TrStartPt EndPt)


  ; TrRouteList = CBLRoundPts(RouteList)

   sprintf(EvNetName "wrA[%d]" HopIdx-1)
   DrawChannel(nil DefChan(list(TempArray[BufIdx])) EvNetName RouteList)

    )
 
   when( OpOdInst != nil || IpOdInst != nil
   sprintf(OdNetName "wrA[%d]" HopIdx-1)
   DrawChannel(nil DefChan(list(TempArray[BufIdx+1])) OdNetName RouteList)
    )
  
 )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
defun( CBLPlaceWiringInst (CableShape HopIdx BufIdx TempType @key (CellVue (UIGetCellView)))
  let( ( TempArray 
         Pts PtFound
         I Metals
         EvInst EvInstName
         OdInst OdInstName
         WrInst WrInstName WrCellName
         CurrIdx CurrSeg
        )

   declare(Pts[4])
   I = 0
   TempArray = CBLCreateTempArray(TempType)

   WrCellName = "globals.cable.wires.M"

   sprintf(EvInstName "mbuf[%d][%d]" HopIdx BufIdx)
   sprintf(OdInstName "mbuf[%d][%d]" HopIdx BufIdx+1)

   sprintf(WrInstName "wr[%d][%d]" HopIdx BufIdx)

   EvInst = dbFindAnyInstByName(CellVue EvInstName)
   OdInst = dbFindAnyInstByName(CellVue OdInstName)

    when( EvInst != nil
    CurrSeg = car(CBLFindInstCableSeg(CableShape EvInst))
     

    println(CurrSeg)

    Metals = parseString(nth(2 nth(3 TempArray[BufIdx])) "M")
    WrCellName = strcat(WrCellName cadr(Metals) car(Metals) CurrSeg "_0")
    println(WrInstName)

    WrInst = dbCreateInstByMasterName(CellVue "globals.cable" WrCellName "layout" WrInstName EvInst~>xy EvInst~>orient)

    WrInst~>Pitch = nth(2 TempArray[BufIdx])
    when( OdInst == nil 
        WrInst~>Odd_Channel = "FALSE"
      )
    )
    WrInst
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
defun( CBLFindInstCableSeg (CableShape EvInst @key (CellVue (UIGetCellView)))
 let( (
       Probe_Bot_0 Probe_Bot_1
       Probe_Top_0 Probe_Top_1
       Ovp_Bot_0 Ovp_Bot_1
       Ovp_Top_0 Ovp_Top_1
       CurrSeg CurrLoc
       )

   CurrSeg = nil

   when( EvInst != nil
   case( EvInst~>orient

       ( "R0"
        Probe_Bot_0 = list(car(EvInst~>xy)+0.39:cadr(EvInst~>xy)+0.39 car(EvInst~>xy)+2.73:cadr(EvInst~>xy)+1.17)
        Probe_Bot_1 = list(car(EvInst~>xy)+0.39:cadr(EvInst~>xy)+1.95 car(EvInst~>xy)+2.73:cadr(EvInst~>xy)+2.73)

        Probe_Top_0 = list(car(EvInst~>xy)+0.39:cadr(EvInst~>xy)+3.51 car(EvInst~>xy)+1.17:cadr(EvInst~>xy)+5.85)
        Probe_Top_1 = list(car(EvInst~>xy)+1.95:cadr(EvInst~>xy)+3.51 car(EvInst~>xy)+2.73:cadr(EvInst~>xy)+5.85)
        )

       ( "MY"
        Probe_Bot_0 = list(car(EvInst~>xy)-0.39:cadr(EvInst~>xy)+0.39 car(EvInst~>xy)-2.73:cadr(EvInst~>xy)+1.17)
        Probe_Bot_1 = list(car(EvInst~>xy)-0.39:cadr(EvInst~>xy)+1.95 car(EvInst~>xy)-2.73:cadr(EvInst~>xy)+2.73)

        Probe_Top_0 = list(car(EvInst~>xy)-0.39:cadr(EvInst~>xy)+3.51 car(EvInst~>xy)-1.17:cadr(EvInst~>xy)+5.85)
        Probe_Top_1 = list(car(EvInst~>xy)-1.95:cadr(EvInst~>xy)+3.51 car(EvInst~>xy)-2.73:cadr(EvInst~>xy)+5.85)
    
        CurrSeg = "H"
        )

       ( "MX"
        Probe_Bot_0 = list(car(EvInst~>xy)+0.39:cadr(EvInst~>xy)-0.39 car(EvInst~>xy)+2.73:cadr(EvInst~>xy)-1.17)
        Probe_Bot_1 = list(car(EvInst~>xy)+0.39:cadr(EvInst~>xy)-1.95 car(EvInst~>xy)+2.73:cadr(EvInst~>xy)-2.73)

        Probe_Top_0 = list(car(EvInst~>xy)+0.39:cadr(EvInst~>xy)-3.51 car(EvInst~>xy)+1.17:cadr(EvInst~>xy)-5.85)
        Probe_Top_1 = list(car(EvInst~>xy)+1.95:cadr(EvInst~>xy)-3.51 car(EvInst~>xy)+2.73:cadr(EvInst~>xy)-5.85)
       
        CurrSeg = "V"
        )

     )

   Ovp_Bot_0 = CBLOveralapExists(dbGetTrueOverlaps(CellVue Probe_Bot_0) "router")
   Ovp_Bot_1 = CBLOveralapExists(dbGetTrueOverlaps(CellVue Probe_Bot_1) "router")
   Ovp_Top_0 = CBLOveralapExists(dbGetTrueOverlaps(CellVue Probe_Top_0) "router")
   Ovp_Top_1 = CBLOveralapExists(dbGetTrueOverlaps(CellVue Probe_Top_1) "router")

   when( EvInst~>orient == "R0"
      if( (Ovp_Bot_0 == t || Ovp_Bot_1 == t) && (Ovp_Top_0 == t || Ovp_Top_1 == t)
        then
          CurrSeg = "V"
        else
          CurrSeg = "H"
        )
      )
 
   case( CurrSeg
  
        ( "H" 
          if( Ovp_Bot_0 == t
           then
             CurrLoc = list(car(EvInst~>xy) cadr(EvInst~>xy)+0.78)
           else
             CurrLoc = list(car(EvInst~>xy) cadr(EvInst~>xy)+2.34)
           )
         )

        ( "V" 
          if( Ovp_Top_0 == t
           then
             CurrLoc = list(car(EvInst~>xy)+0.78 cadr(EvInst~>xy))
           else
             CurrLoc = list(car(EvInst~>xy)+2.34 cadr(EvInst~>xy))
           )
         )
      )
   )
  list(CurrSeg CurrLoc)
 )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun RouteBufReset (@optional (CableShape car(geGetSelectedSet())) (CellVue (UIGetCellView)))
 let( ( BufResetPts BufToRampPts Hops 
        CurrInstName NextInstName CurrInst RampInstName RampInst NextInst ResNetName
        StartPt EndPt
       )

   Hops = evalstring(CableShape~>BufferHops)

   for( HopIdx 0 Hops-1
     when( HopIdx == 0 
             sprintf(NextInstName "bufReset[%d]", HopIdx)

             NextInst = dbFindAnyInstByName(CellVue NextInstName)

             StartPt = nth(0 CableShape~>points)
             EndPt = car(FindResetPts(NextInst))
  
             BufResetPts = TransformCablePoints(CableShape StartPt EndPt 1 NextInst~>CableIdx 1.56 0)
             DrawChannel(nil DefChan(list(list( "" node_4W 0*TrackPitch BusMetal34))) "L_RESET" BufResetPts)
         )
     when( HopIdx != Hops-1
             sprintf(CurrInstName "bufReset[%d]", HopIdx)
             sprintf(NextInstName "bufReset[%d]", HopIdx+1)

             CurrInst = dbFindAnyInstByName(CellVue CurrInstName)
             NextInst = dbFindAnyInstByName(CellVue NextInstName)
        
              StartPt = cadr(FindResetPts(CurrInst))
                EndPt = car(FindResetPts(NextInst))
  
              BufResetPts = TransformCablePoints(CableShape StartPt EndPt CurrInst~>CableIdx NextInst~>CableIdx 1.56 0)
              sprintf(ResNetName "w_BufReset[%d]", HopIdx+1)
              DrawChannel(nil DefChan(list(list( "" node_4W 0*TrackPitch BusMetal34))) ResNetName BufResetPts)
              ConnectBufToRamp(CurrInst HopIdx)
         )
     when( HopIdx == Hops-1
             sprintf(CurrInstName "bufReset[%d]", HopIdx)

             CurrInst = dbFindAnyInstByName(CellVue CurrInstName)
        
              StartPt = cadr(FindResetPts(CurrInst))
                EndPt = nth(CableShape~>nPoints-1 CableShape~>points)
  
              BufResetPts = TransformCablePoints(CableShape StartPt EndPt CurrInst~>CableIdx CableShape~>nPoints-1 1.56 0)
              DrawChannel(nil DefChan(list(list( "" node_4W 0*TrackPitch BusMetal34))) "R_RESET" BufResetPts)
              ConnectBufToRamp(CurrInst HopIdx)
         )
     )
 t
 )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ConnectBufToRamp ( BufResInst Hop )
  let( ( StartPt EndPt ResNetName )

   case( BufResInst~>Segment
           ( "H"  
             if( BufResInst~>orient == "R0"
              then
                StartPt = list(car(BufResInst~>xy)+2.34 cadr(BufResInst~>xy)+2.34)
                EndPt = list(car(BufResInst~>xy)+2.34 cadr(BufResInst~>xy)+3.90)
                sprintf(ResNetName "w_BufReset[%d]", Hop+1)
                DrawChannel(nil DefChan(list(list( "" node_4W 0*TrackPitch BusMetal23))) ResNetName list(StartPt EndPt))
              else 
                StartPt = list(car(BufResInst~>xy)-2.34 cadr(BufResInst~>xy)+2.34)
                EndPt = list(car(BufResInst~>xy)-2.34 cadr(BufResInst~>xy)+3.90)
                sprintf(ResNetName "w_BufReset[%d]", Hop+1)
                DrawChannel(nil DefChan(list(list( "" node_4W 0*TrackPitch BusMetal23))) ResNetName list(StartPt EndPt))
             )
           )
           ( "V"  
             if( BufResInst~>orient == "R0"
              then
                StartPt = list(car(BufResInst~>xy)+2.34 cadr(BufResInst~>xy)+0.78)
                EndPt = list(car(BufResInst~>xy)+3.90 cadr(BufResInst~>xy)+0.78)
                sprintf(ResNetName "w_BufReset[%d]", Hop+1)
                DrawChannel(nil DefChan(list(list( "" node_4W 0*TrackPitch BusMetal23))) ResNetName list(StartPt EndPt))
              else
                StartPt = list(car(BufResInst~>xy)+2.34 cadr(BufResInst~>xy)-0.78)
                EndPt = list(car(BufResInst~>xy)+3.90 cadr(BufResInst~>xy)-0.78)
                sprintf(ResNetName "w_BufReset[%d]", Hop+1)
                DrawChannel(nil DefChan(list(list( "" node_4W 0*TrackPitch BusMetal23))) ResNetName list(StartPt EndPt)) 
             )
           )
   )

 t
 )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun RouteRampReset (@optional (CableShape car(geGetSelectedSet())) (CellVue (UIGetCellView)))
 let( ( RampResetPts Hops Bufs BufIdx RampInstName RampInst
        FirstInstName LastInstName FirstInst LastInst ResNetName
        StartPt EndPt HopIdx FirstIdx LastIdx EvInstName EvInst
       )

   Hops = CableShape~>BufferHops
      when( stringp(Hops)
         Hops = evalstring(CableShape~>BufferHops)
       )

   Bufs = CableShape~>CableWidth
      when( stringp(Bufs)
       Bufs = evalstring(CableShape~>CableWidth)
       )

   BufArray = CBLCreateBufArray()

   if( Bufs <= CableBufferSplitIdx
     then
       FirstIdx = 0
       LastIdx = truncate((Bufs-1)/2)*2
     else
       FirstIdx = CableBufferSplitIdx
       LastIdx = truncate((CableBufferSplitIdx - 1)/2)*2
   )

   println(LastIdx)

   for( HopIdx 0 Hops-1
        
          sprintf(FirstInstName "mbuf[%d][%d]", HopIdx,FirstIdx)
          sprintf(LastInstName "mbuf[%d][%d]", HopIdx,LastIdx)
          FirstInst = dbFindAnyInstByName(CellVue FirstInstName)
          LastInst = dbFindAnyInstByName(CellVue LastInstName)
          sprintf(RampInstName "rampReset[%d]", HopIdx)
          RampInst = dbFindAnyInstByName(CellVue RampInstName)


          StartPt = cadr(FindResetPts(FirstInst))
          EndPt = car(FindResetPts(LastInst))
  
          RampResetPts = TransformCablePoints(CableShape StartPt EndPt FirstInst~>CableIdx LastInst~>CableIdx -0.325 4.68)
          sprintf(ResNetName "w_rampReset[%d]", HopIdx+1)
          DrawChannel(nil DefChan(list(list( "" node_2W 0*TrackPitch BusMetal23))) ResNetName RampResetPts)

          ConnectRampToMbuf(RampInst HopIdx)

        for( BufIdx 0 Bufs-1
          when( evenp(BufIdx)
            sprintf(EvInstName "mbuf[%d][%d]", HopIdx,BufIdx)
            EvInst = dbFindAnyInstByName(CellVue EvInstName)        
            ConnectEvenOddResets(EvInst HopIdx)
          )
       )
    )
 t
 )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ConnectEvenOddResets ( BufInst Hop ) 
  let( ( StartPt EndPt ResNetName  )
 
   case( BufInst~>Segment
           ( "H"  
             if( BufInst~>orient == "R0"
              then
                StartPt = list(car(BufInst~>xy)+2.34 cadr(BufInst~>xy)+5.46)
                EndPt = list(car(BufInst~>xy)+3.90 cadr(BufInst~>xy)+5.46)
                sprintf(ResNetName "w_rampReset[%d]", Hop)
                DrawChannel(nil DefChan(list(list( "" node_2W 0*TrackPitch BusMetal23))) ResNetName list(StartPt EndPt))
              else 
                StartPt = list(car(BufInst~>xy)-2.34 cadr(BufInst~>xy)+5.46)
                EndPt = list(car(BufInst~>xy)-3.90 cadr(BufInst~>xy)+5.46)
                sprintf(ResNetName "w_rampReset[%d]", Hop)
                DrawChannel(nil DefChan(list(list( "" node_2W 0*TrackPitch BusMetal23))) ResNetName list(StartPt EndPt))
             )
           )
           ( "V"  
             if( BufInst~>orient == "R0"
              then
                StartPt = list(car(BufInst~>xy)+2.34 cadr(BufInst~>xy)+5.46)
                EndPt = list(car(BufInst~>xy)+3.90 cadr(BufInst~>xy)+5.46)
                sprintf(ResNetName "w_rampReset[%d]", Hop)
                DrawChannel(nil DefChan(list(list( "" node_2W 0*TrackPitch BusMetal23))) ResNetName list(StartPt EndPt))
              else
                StartPt = list(car(BufInst~>xy)+2.34 cadr(BufInst~>xy)-5.46)
                EndPt = list(car(BufInst~>xy)+3.90 cadr(BufInst~>xy)-5.46)
                sprintf(ResNetName "w_rampReset[%d]", Hop)
                DrawChannel(nil DefChan(list(list( "" node_2W 0*TrackPitch BusMetal23))) ResNetName list(StartPt EndPt))
             )
           )
   )    
 t 
 )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ConnectRampToMbuf ( RampResInst Hop ) 
 let( (  StartPt EndPt ResNetName )

   case( RampResInst~>Segment
           ( "H"  
             if( RampResInst~>orient == "R0"
              then
                StartPt = list(car(RampResInst~>xy)+2.34 cadr(RampResInst~>xy)+2.34)
                EndPt = list(car(RampResInst~>xy)+3.90 cadr(RampResInst~>xy)+2.34)
                sprintf(ResNetName "w_BufReset[%d]", Hop)
                DrawChannel(nil DefChan(list(list( "" node_2W 0*TrackPitch BusMetal23))) ResNetName list(StartPt EndPt))
              else 
                StartPt = list(car(RampResInst~>xy)-2.34 cadr(RampResInst~>xy)+2.34)
                EndPt = list(car(RampResInst~>xy)-3.90 cadr(RampResInst~>xy)+2.34)
                sprintf(ResNetName "w_BufReset[%d]", Hop)
                DrawChannel(nil DefChan(list(list( "" node_2W 0*TrackPitch BusMetal23))) ResNetName list(StartPt EndPt))
             )
           )
           ( "V"  
             if( RampResInst~>orient == "R0"
              then
                StartPt = list(car(RampResInst~>xy)+0.455 cadr(RampResInst~>xy)+2.34)
                EndPt = list(car(RampResInst~>xy)+0.455 cadr(RampResInst~>xy)+3.90)
                sprintf(ResNetName "w_BufReset[%d]", Hop)
                DrawChannel(nil DefChan(list(list( "" node_2W 0*TrackPitch BusMetal23))) ResNetName list(StartPt EndPt))
              else
                StartPt = list(car(RampResInst~>xy)+0.455 cadr(RampResInst~>xy)-2.34)
                EndPt = list(car(RampResInst~>xy)+0.455 cadr(RampResInst~>xy)-3.90)
                sprintf(ResNetName "w_BufReset[%d]", Hop)
                DrawChannel(nil DefChan(list(list( "" node_2W 0*TrackPitch BusMetal23))) ResNetName list(StartPt EndPt)) 
             )
           )
   )

 t
 )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun FindResetPts ( Inst )
  let( ( StartPt EndPt )

   case( car(parseString(Inst~>name "["))

     ( "bufReset"
        case( Inst~>Segment
           ( "H"  
             if( Inst~>orient == "R0"
              then
                StartPt = list(car(Inst~>CablePt)+0.78 cadr(Inst~>CablePt))
                  EndPt = list(car(Inst~>CablePt)+2.34 cadr(Inst~>CablePt))
              else
                StartPt = list(car(Inst~>CablePt)-0.78 cadr(Inst~>CablePt))
                  EndPt = list(car(Inst~>CablePt)-2.34 cadr(Inst~>CablePt))                
             )
           )
           ( "V"  
             if( Inst~>orient == "R0"
              then
                StartPt = list(car(Inst~>CablePt) cadr(Inst~>CablePt)+0.78)
                  EndPt = list(car(Inst~>CablePt) cadr(Inst~>CablePt)+2.34)
              else
                StartPt = list(car(Inst~>CablePt) cadr(Inst~>CablePt)-0.78)
                  EndPt = list(car(Inst~>CablePt) cadr(Inst~>CablePt)-2.34)                
             )
           )
         )
      )

     ( "rampReset"
        case( Inst~>Segment
           ( "H"  
             if( Inst~>orient == "R0"
              then
                StartPt = list(car(Inst~>CablePt)+0.78 cadr(Inst~>CablePt))
                  EndPt = list(car(Inst~>CablePt)+2.34 cadr(Inst~>CablePt))
              else
                StartPt = list(car(Inst~>CablePt)-0.78 cadr(Inst~>CablePt))
                  EndPt = list(car(Inst~>CablePt)-2.34 cadr(Inst~>CablePt))                
             )
           )
           ( "V"  
             if( Inst~>orient == "R0"
              then
                StartPt = list(car(Inst~>CablePt) cadr(Inst~>CablePt)+0.78)
                  EndPt = list(car(Inst~>CablePt) cadr(Inst~>CablePt)+2.34)
              else
                StartPt = list(car(Inst~>CablePt) cadr(Inst~>CablePt)-0.78)
                  EndPt = list(car(Inst~>CablePt) cadr(Inst~>CablePt)-2.34)                
             )
           )
         )
      )

     ( "mbuf"
        case( Inst~>Segment
           ( "H"  
             if( Inst~>orient == "R0"
              then
                StartPt = list(car(Inst~>CablePt)+0.78 cadr(Inst~>CablePt))
                  EndPt = list(car(Inst~>CablePt)+2.34 cadr(Inst~>CablePt))
              else
                StartPt = list(car(Inst~>CablePt)-0.78 cadr(Inst~>CablePt))
                  EndPt = list(car(Inst~>CablePt)-2.34 cadr(Inst~>CablePt))                
             )
           )
           ( "V"  
             if( Inst~>orient == "R0"
              then
                StartPt = list(car(Inst~>CablePt) cadr(Inst~>CablePt)+0.78)
                  EndPt = list(car(Inst~>CablePt) cadr(Inst~>CablePt)+5.46)
              else
                StartPt = list(car(Inst~>CablePt) cadr(Inst~>CablePt)-0.78)
                  EndPt = list(car(Inst~>CablePt) cadr(Inst~>CablePt)-5.46)                
             )
           )
         )
      )

   )

list(StartPt EndPt)
 )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun TransformCablePoints (CableShape StartPt EndPt StartIdx EndIdx HOffset VOffset)
  let( ( OutPts NOutPts CablePt I )
    
   NOutPts = EndIdx - StartIdx + 2

   for( I 0 NOutPts-1
   println(I)
     when( I == 0 
       if( CBLCurrSegment(CableShape StartIdx) == "H"
        then
          OutPts = list(list(car(StartPt) cadr(StartPt)+VOffset))
        else
          OutPts = list(list(car(StartPt)+HOffset cadr(StartPt)))
         )
       )

     when( I != 0 && I != NOutPts-1
        CablePt = nth((StartIdx + I - 1) CableShape~>points)
        OutPts = append(OutPts list(list(car(CablePt)+HOffset cadr(CablePt)+VOffset)))
       )     

     when( I == NOutPts-1
       if( CBLCurrSegment(CableShape EndIdx) == "H"
        then
          OutPts = append(OutPts list(list(car(EndPt) cadr(EndPt)+VOffset)))
        else
          OutPts = append(OutPts list(list(car(EndPt)+HOffset cadr(EndPt))))
         )
       )

    )

  OutPts
 )
)
