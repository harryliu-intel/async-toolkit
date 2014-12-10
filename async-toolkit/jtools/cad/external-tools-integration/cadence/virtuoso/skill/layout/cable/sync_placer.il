;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun PlaceSyncCableBuffers ( @key (CableShape car(geGetSelectedSet())) (CV (UIGetCellView)) (ResetDist t) )
  let( (
        Hops CableWidth
        BufIdx HopIdx
        I J 
        StartDist EndDist HopDist HopDistX BufType
        StartLoc
        CurrLoc CurrIdx CurrSeg CurrDir
        LocatorOP
        EvInst EvInstName
        OdInst ODInstName
        Inst   InstName
        CableLength SearchSeed SearchDist SearchLoc SearchBuf SearchBufName
        TemplateArray BufArray
        StartDistX MInst BlkShape
        )

;     Move all the Instances to origin initially.
      foreach( MInst CV~>instances MInst~>Orient = "R0" MInst~>xy = 0:0 )
;     Delete old buffer blockage shapes.
      foreach( BlkShape CV~>shapes  when( car(BlkShape~>lpp) == "y5" dbDeleteObject(BlkShape)) )

      CBLCreateBufBlockage(?CableShape CableShape ?CV CV)

      Hops          =   CableShape~>ChannelHops 
      CableWidth    =   CableShape~>ChannelWidth
      StartDist     =   CableShape~>InputDistance_um
      EndDist       =   CableShape~>OutputDistance_um
      HopDist       =   CableShape~>HopDistance_um
      BufType       =   CableShape~>ChannelBuf
      StartDistX    =   round(StartDist/1.56)*1.56
      HopDistX      =   round(HopDist/3.12)*3.12 
      SearchSeed    =   0
      BufIdx        =   0
      CableLength   =   CAComputeCableLength(CableShape)
      StartLoc      =   nth(0 CableShape~>points)
      BufArray      =   CBLCreateBufArray(?Size CableWidth)

    for( HopIdx 0 Hops-1

     I = 0
     if( HopIdx == 0
       then
         SearchDist     =  StartDistX+(SearchSeed*I*3.12)+(HopIdx*HopDist)
         SearchLoc      =  StartLoc
       else             
          t
;         SearchBufName  =  sprintf(nil "mbuf[%d][%d]" HopIdx-1 BufArray[0])
;         SearchBuf      =  dbFindAnyInstByName(CV SearchBufName)
;         SearchLoc      =  SearchBuf~>CablePt
;         SearchDist     =  HopDist
     )

     println(SearchDist)
     println(SearchLoc)

     while( I <=  length(BufArray) - 1 

      if( HopIdx == 0  
       then
         BufIdx          =  BufArray[I]
         SearchMultiple  =  1
       else
         BufIdx = BufArray[length(BufArray)-I-1]
         println(BufIdx)
         SearchBufName  =  sprintf(nil "mbuf[%d][%d]" HopIdx-1 BufArray[length(BufArray)-1])
         SearchBuf      =  dbFindAnyInstByName(CV SearchBufName)
         SearchLoc      =  SearchBuf~>CablePt
         SearchDist     =  HopDist
         SearchMultiple =  -1
        )

     when( BufIdx <=  CableWidth
          InstName  = sprintf(nil "mbuf[%d][%d]" HopIdx BufIdx)
          Inst      = dbFindAnyInstByName(CV InstName)
          LocatorOP = CBLLocateBuffer(CableShape SearchLoc SearchDist ?SearchMultiple SearchMultiple ?BufType BufType ?CV CV)
          CurrLoc   = nth(0 LocatorOP)
          CurrIdx   = nth(1 LocatorOP)
          CurrSeg   = nth(2 LocatorOP)
          CurrDir   = nth(3 LocatorOP)

      case( CurrSeg
         ( "H"
           if( CurrDir == "P"
            then
               Inst~>orient = "MXR90"
               Inst~>xy = CurrLoc
               Inst~>CablePt = list(car(CurrLoc) cadr(CurrLoc)+0.78)
               Inst~>CableIdx = CurrIdx
               dbSet(Inst "H" "Segment")
            else 
               Inst~>orient = "R90"
               Inst~>xy = CurrLoc
               Inst~>CablePt = list(car(CurrLoc) cadr(CurrLoc)+0.78)
               Inst~>CableIdx = CurrIdx
               dbSet(Inst "H" "Segment")
              )
          )
         ( "V"
           if( CurrDir == "P"
            then
               Inst~>orient = "MXR90"
               Inst~>xy = CurrLoc
               Inst~>CablePt = list(car(CurrLoc)+0.78 cadr(CurrLoc))
               Inst~>CableIdx = CurrIdx
               dbSet(Inst "V" "Segment")
            else 
               Inst~>orient = "R270"
               Inst~>xy = CurrLoc
               Inst~>CablePt = list(car(CurrLoc)+0.78 cadr(CurrLoc))
               Inst~>CableIdx = CurrIdx
               dbSet(Inst "V" "Segment")
              )
            )
          )      
        )  
            I = I + 1 ; 2 
        )
   )

; when( ResetDist
;    PlaceSyncResetBufs(?CableShape CableShape ?CV CV)
;  )

  )
 t
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun RouteSyncCableBuffers (@optional (CableShape car(geGetSelectedSet())) (CV (UIGetCellView))) 
  let( ( BufIdx HopIdx
         Hops Bufs BufArray
         temp 
        )

      (DeleteBusWires)
      DrawCablePins(CableShape CV "Template_A_R90")
      BufArray = CBLCreateBufArray()

      if( evenp(evalstring(CableShape~>CableWidth))
       then
         Bufs = truncate(evalstring(CableShape~>CableWidth))/2 - 1
       else
         Bufs = truncate(evalstring(CableShape~>CableWidth))/2 - 1
      )

      Hops = CableShape~>BufferHops      
      when( type(Hops) == "string" 
         Hops = evalstring(CableShape~>BufferHops)
       )

        for( BufIdx 0 Bufs
          TrBufIdx = BufArray[2*BufIdx]
          when( BufIdx <= evalstring(CableShape~>CableWidth)
          CBLPlaceSyncWiringInst(CableShape 0 2*BufIdx "Template_A")
          temp = CBLRouteSyncBufferInput(CableShape 0 2*BufIdx "Template_A_R90" "Input")
          )
         )

      for( HopIdx 1 Hops-1
        for( BufIdx 0 Bufs
          TrBufIdx = BufArray[2*BufIdx]
          when( BufIdx <= evalstring(CableShape~>CableWidth)
          CBLPlaceSyncWiringInst(CableShape HopIdx 2*BufIdx "Template_A")
          temp = CBLRouteSyncBufferInput(CableShape HopIdx 2*BufIdx "Template_A_R90" "Buffer")
           )
         )
       )

        for( BufIdx 0 Bufs
          TrBufIdx = BufArray[2*BufIdx]
          when( BufIdx <= evalstring(CableShape~>CableWidth)
          temp = CBLRouteSyncBufferInput(CableShape Hops 2*BufIdx "Template_A_R90" "Output")
         )
        )
  t
 )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

defun( DrawSyncCablePins (@optional (CableShape car(geGetSelectedSet())) (CV (UIGetCellView)))
  let( ( 
        IpProbeXP IpProbeXN IpProbeYP IpProbeYN
        OpProbeXP OpProbeXN OpProbeYP OpProbeYN
        IpRouteList OpRouteList
        Idx TempArray
        )

   TempArray = CBLCreateTempArray("Template_A_R90")
   StartPt = nth(0 CableShape~>points)
   EndPt   = nth(CableShape~>nPoints-1 CableShape~>points)

   IpRouteList = list(StartPt car(CBLLocateAtDist(CableShape StartPt 0.39)))

   OpRouteList = list(EndPt car(CBLLocateAtDist(CableShape EndPt -0.39)))

   for( Idx 0 evalstring(CableShape~>CableWidth) - 1
     DrawChannel(nil DefChan(list(TempArray[Idx])) "L" IpRouteList ?isPin t)
     DrawChannel(nil DefChan(list(TempArray[Idx])) "R" OpRouteList ?isPin t)
    )

  t
 )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun CBLRouteSyncBufferInput (CableShape HopIdx BufIdx TempType RouteType @key (CV (UIGetCellView)))
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

   OpEvInst = dbFindAnyInstByName(CV OpEvInstName)
   OpOdInst = dbFindAnyInstByName(CV OpOdInstName)
   IpEvInst = dbFindAnyInstByName(CV IpEvInstName)
   IpOdInst = dbFindAnyInstByName(CV IpOdInstName)
 
  when( IpEvInst != nil || OpEvInst != nil

   StartPt = IpEvInst~>CablePt
   EndPt   = OpEvInst~>CablePt

   StartSeg = IpEvInst~>Segment
   EndSeg   = OpEvInst~>Segment

   case( StartSeg

      ( "H" 
        if( IpEvInst~>orient == "MXR90"
         then
          TrStartPt = list(car(StartPt)+6.24 cadr(StartPt))
         else
          TrStartPt = list(car(StartPt)-6.24 cadr(StartPt))
         )
       )

      ( "V" 
        if( IpEvInst~>orient == "MXR90"
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

   sprintf(EvNetName "wrA[%d]" HopIdx-1)
   DrawChannel(nil DefChan(list(TempArray[BufIdx])) EvNetName RouteList)

    )
 
   when( OpOdInst != nil || IpOdInst != nil
   sprintf(OdNetName "wrA[%d]" HopIdx-1)
   DrawChannel(nil DefChan(list(TempArray[BufIdx+1])) OdNetName RouteList)
    )
  
 )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun CBLPlaceSyncWiringInst (CableShape HopIdx BufIdx TempType @key (CV (UIGetCellView)))
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

   EvInst = dbFindAnyInstByName(CV EvInstName)
   OdInst = dbFindAnyInstByName(CV OdInstName)

    when( EvInst != nil
    
     if( EvInst~>Segment == "H"
      then
        CurrSeg = "V"
      else
        CurrSeg = "H"
      )

    Metals = parseString(nth(2 nth(3 TempArray[BufIdx])) "M")
    WrCellName = strcat(WrCellName cadr(Metals) car(Metals) CurrSeg "_0")
    WrInst = dbCreateInstByMasterName(CV "globals.cable" WrCellName "layout" WrInstName EvInst~>xy EvInst~>orient)
    WrInst~>Pitch = nth(2 TempArray[BufIdx])
    when( OdInst == nil 
        WrInst~>Odd_Channel = "FALSE"
      )
    )
    WrInst
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; to be called only once all the MBUFs are placed on the cable guide. 

(defun PlaceSyncResetBufs ( @key (CableShape car(geGetSelectedSet())) (CV (UIGetCellView)) )
  let( ( Hops HopIdx BufInst BufInstName BufArray
         StartDist StartLoc StartDistX HopDist LocatorOP Multiple
         ResetBufInst ResetBufInstName ResetRampInst ResetRampInstName 
         CurrLoc CurrIdx CurrSeg CurrDir
        )
      Hops = CableShape~>BufferHops      
      when( type(Hops) == "string" 
         Hops = evalstring(CableShape~>BufferHops)
       )

      StartDist = CableShape~>InputDistance_um
      when( stringp(StartDist)
        StartDist = evalstring(CableShape~>InputDistance_um)
       )

   StartDistX = round(StartDist/1.56)*1.56
   HopDist = round(300/3.12)*3.12
   StartLoc = nth(0 CableShape~>points)
   BufArray = CBLCreateBufArray()

   for( HopIdx 0 Hops-1
     sprintf(BufInstName "mbuf[%d][%d]", HopIdx, BufArray[0])  
     sprintf(ResetBufInstName "bufReset[%d]", HopIdx)
     sprintf(ResetRampInstName "rampReset[%d]", HopIdx)

     if( HopIdx < Hops-1
      then
        Multiple = 1
      else
        Multiple = -1
     )

     println("helo")
     println(BufInstName)
     println(ResetBufInstName)

     BufInst = dbFindAnyInstByName(CV BufInstName)
     ResetBufInst = dbFindAnyInstByName(CV ResetBufInstName)
     ResetRampInst = dbFindAnyInstByName(CV ResetRampInstName)

     LocatorOP = CBLLocateBuffer(CableShape BufInst~>CablePt 0 ?SearchMultiple Multiple ?BufType "RESET" ?CV CV)
      CurrLoc = nth(0 LocatorOP)
      CurrIdx = nth(1 LocatorOP)
      CurrSeg = nth(2 LocatorOP)
      CurrDir = nth(3 LocatorOP)

      case( CurrSeg
         ( "H"
           if( CurrDir == "P"
            then
               ResetBufInst~>orient = "MXR90"
               ResetRampInst~>orient = "MXR90"
               ResetBufInst~>xy = CurrLoc
               ResetRampInst~>xy = list(car(CurrLoc) cadr(CurrLoc)+3.12)
               ResetBufInst~>CablePt = list(car(CurrLoc) cadr(CurrLoc)+0.78)
               ResetRampInst~>CablePt = list(car(CurrLoc) cadr(CurrLoc)+0.78)
               ResetBufInst~>CableIdx = CurrIdx
               ResetRampInst~>CableIdx = CurrIdx
               dbSet(ResetBufInst "H" "Segment")
               dbSet(ResetRampInst "H" "Segment")
            else 
               ResetBufInst~>orient = "R90"
               ResetRampInst~>orient = "R90"
               ResetBufInst~>xy = CurrLoc
               ResetRampInst~>xy = list(car(CurrLoc) cadr(CurrLoc)+3.12)
               ResetBufInst~>CablePt = list(car(CurrLoc) cadr(CurrLoc)+0.78)
               ResetRampInst~>CablePt = list(car(CurrLoc) cadr(CurrLoc)+0.78)
               ResetBufInst~>CableIdx = CurrIdx
               ResetRampInst~>CableIdx = CurrIdx
               dbSet(ResetBufInst "H" "Segment")
               dbSet(ResetRampInst "H" "Segment")
              )
          )
         ( "V"
           if( CurrDir == "P"
            then
               ResetBufInst~>orient = "MXR90"
               ResetRampInst~>orient = "MXR90"
               ResetBufInst~>xy = CurrLoc
               ResetRampInst~>xy = list(car(CurrLoc)+3.12 cadr(CurrLoc))
               ResetBufInst~>CablePt = list(car(CurrLoc)+0.78 cadr(CurrLoc))
               ResetRampInst~>CablePt = list(car(CurrLoc)+0.78 cadr(CurrLoc))
               ResetBufInst~>CableIdx = CurrIdx
               ResetRampInst~>CableIdx = CurrIdx
               dbSet(ResetBufInst "V" "Segment")
               dbSet(ResetRampInst "V" "Segment")
            else 
               ResetBufInst~>orient = "R270"
               ResetRampInst~>orient = "R270"
               ResetBufInst~>xy = CurrLoc
               ResetRampInst~>xy = list(car(CurrLoc)+3.12 cadr(CurrLoc))
               ResetBufInst~>CablePt = list(car(CurrLoc)+0.78 cadr(CurrLoc))
               ResetRampInst~>CablePt = list(car(CurrLoc)+0.78 cadr(CurrLoc))
               ResetBufInst~>CableIdx = CurrIdx
               ResetRampInst~>CableIdx = CurrIdx
               dbSet(ResetBufInst "V" "Segment")
               dbSet(ResetRampInst "V" "Segment")
              )
            )
          )   

   )

 t
 )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun RouteSyncBufReset (@optional (CableShape car(geGetSelectedSet())) (CV (UIGetCellView)))
 let( ( BufResetPts BufToRampPts Hops 
        CurrInstName NextInstName CurrInst RampInstName RampInst NextInst ResNetName
        StartPt EndPt
       )

      Hops = CableShape~>BufferHops      
      when( type(Hops) == "string" 
         Hops = evalstring(CableShape~>BufferHops)
       )
   for( HopIdx 0 Hops-1
     when( HopIdx == 0 
             sprintf(NextInstName "bufReset[%d]", HopIdx)

             NextInst = dbFindAnyInstByName(CV NextInstName)

             StartPt = nth(0 CableShape~>points)
             EndPt = car(FindSyncResetPts(NextInst))
  
             BufResetPts = TransformSyncCablePoints(CableShape StartPt EndPt 1 NextInst~>CableIdx 0 1.56)
             DrawChannel(nil DefChan(list(list( "" node_4W 0*TrackPitch BusMetal43))) "L_RESET" BufResetPts)
         )
     when( HopIdx != Hops-1
             sprintf(CurrInstName "bufReset[%d]", HopIdx)
             sprintf(NextInstName "bufReset[%d]", HopIdx+1)

             CurrInst = dbFindAnyInstByName(CV CurrInstName)
             NextInst = dbFindAnyInstByName(CV NextInstName)
        
              StartPt = cadr(FindSyncResetPts(CurrInst))
                EndPt = car(FindSyncResetPts(NextInst))
  
              BufResetPts = TransformSyncCablePoints(CableShape StartPt EndPt CurrInst~>CableIdx NextInst~>CableIdx 0 1.56)
              sprintf(ResNetName "w_BufReset[%d]", HopIdx+1)
              DrawChannel(nil DefChan(list(list( "" node_4W 0*TrackPitch BusMetal43))) ResNetName BufResetPts)
              ConnectSyncBufToRamp(CurrInst HopIdx)
         )
     when( HopIdx == Hops-1
             sprintf(CurrInstName "bufReset[%d]", HopIdx)

             CurrInst = dbFindAnyInstByName(CV CurrInstName)
        
              StartPt = cadr(FindSyncResetPts(CurrInst))
                EndPt = nth(CableShape~>nPoints-1 CableShape~>points)
  
              BufResetPts = TransformSyncCablePoints(CableShape StartPt EndPt CurrInst~>CableIdx CableShape~>nPoints-1 0 1.56)
              DrawChannel(nil DefChan(list(list( "" node_4W 0*TrackPitch BusMetal43))) "R_RESET" BufResetPts)
              ConnectSyncBufToRamp(CurrInst HopIdx)
         )
     )
 t
 )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ConnectSyncBufToRamp ( BufResInst Hop )
  let( ( StartPt EndPt ResNetName )

   case( BufResInst~>Segment
           ( "H"  
             if( BufResInst~>orient == "R0"
              then
                StartPt = list(car(BufResInst~>xy)+2.34 cadr(BufResInst~>xy)+2.34)
                EndPt = list(car(BufResInst~>xy)+2.34 cadr(BufResInst~>xy)+3.90)
                sprintf(ResNetName "w_BufReset[%d]", Hop+1)
                DrawChannel(nil DefChan(list(list( "" node_4W 0*TrackPitch BusMetal32))) ResNetName list(StartPt EndPt))
              else 
                StartPt = list(car(BufResInst~>xy)-2.34 cadr(BufResInst~>xy)+2.34)
                EndPt = list(car(BufResInst~>xy)-2.34 cadr(BufResInst~>xy)+3.90)
                sprintf(ResNetName "w_BufReset[%d]", Hop+1)
                DrawChannel(nil DefChan(list(list( "" node_4W 0*TrackPitch BusMetal32))) ResNetName list(StartPt EndPt))
             )
           )
           ( "V"  
             if( BufResInst~>orient == "R0"
              then
                StartPt = list(car(BufResInst~>xy)+2.34 cadr(BufResInst~>xy)+0.78)
                EndPt = list(car(BufResInst~>xy)+3.90 cadr(BufResInst~>xy)+0.78)
                sprintf(ResNetName "w_BufReset[%d]", Hop+1)
                DrawChannel(nil DefChan(list(list( "" node_4W 0*TrackPitch BusMetal32))) ResNetName list(StartPt EndPt))
              else
                StartPt = list(car(BufResInst~>xy)+2.34 cadr(BufResInst~>xy)-0.78)
                EndPt = list(car(BufResInst~>xy)+3.90 cadr(BufResInst~>xy)-0.78)
                sprintf(ResNetName "w_BufReset[%d]", Hop+1)
                DrawChannel(nil DefChan(list(list( "" node_4W 0*TrackPitch BusMetal32))) ResNetName list(StartPt EndPt)) 
             )
           )
   )

 t
 )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun RouteSyncRampReset (@optional (CableShape car(geGetSelectedSet())) (CV (UIGetCellView)))
 let( ( RampResetPts Hops Bufs BufIdx RampInstName RampInst
        FirstInstName LastInstName FirstInst LastInst ResNetName
        StartPt EndPt HopIdx FirstIdx LastIdx EvInstName EvInst
       )
      Hops = CableShape~>BufferHops      
      when( type(Hops) == "string" 
         Hops = evalstring(CableShape~>BufferHops)
       )
   Bufs = evalstring(CableShape~>CableWidth)
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
          FirstInst = dbFindAnyInstByName(CV FirstInstName)
          LastInst = dbFindAnyInstByName(CV LastInstName)
          sprintf(RampInstName "rampReset[%d]", HopIdx)
          RampInst = dbFindAnyInstByName(CV RampInstName)


          StartPt = cadr(FindResetPts(FirstInst))
          EndPt = car(FindResetPts(LastInst))
  
          RampResetPts = TransformCablePoints(CableShape StartPt EndPt FirstInst~>CableIdx LastInst~>CableIdx -0.325 4.68)
          sprintf(ResNetName "w_rampReset[%d]", HopIdx+1)
          DrawChannel(nil DefChan(list(list( "" node_2W 0*TrackPitch BusMetal23))) ResNetName RampResetPts)

          ConnectRampToMbuf(RampInst HopIdx)

        for( BufIdx 0 Bufs-1
          when( evenp(BufIdx)
            sprintf(EvInstName "mbuf[%d][%d]", HopIdx,BufIdx)
            EvInst = dbFindAnyInstByName(CV EvInstName)        
            ConnectEvenOddResets(EvInst HopIdx)
          )
       )
    )
 t
 )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ConnectSyncEvenOddResets ( BufInst Hop ) 
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
(defun ConnectSyncRampToMbuf ( RampResInst Hop ) 
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
(defun FindSyncResetPts ( Inst )
  let( ( StartPt EndPt )

   case( car(parseString(Inst~>name "["))

     ( "bufReset"
        case( Inst~>Segment
           ( "H"  
             case( Inst~>orient
                ( "MXR90"
                StartPt = list(car(Inst~>CablePt)+0.78 cadr(Inst~>CablePt))
                  EndPt = list(car(Inst~>CablePt)+2.34 cadr(Inst~>CablePt))
                )
               ( "R90"
                StartPt = list(car(Inst~>CablePt)-0.78 cadr(Inst~>CablePt))
                  EndPt = list(car(Inst~>CablePt)-2.34 cadr(Inst~>CablePt))                
                 )
                 )
           )
           ( "V"  
             case( Inst~>orient 
               ( "MXR90"
                StartPt = list(car(Inst~>CablePt) cadr(Inst~>CablePt)+0.78)
                  EndPt = list(car(Inst~>CablePt) cadr(Inst~>CablePt)+2.34)
                )
               ( "R270"
                StartPt = list(car(Inst~>CablePt) cadr(Inst~>CablePt)-0.78)
                  EndPt = list(car(Inst~>CablePt) cadr(Inst~>CablePt)-2.34)                
               )
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
(defun TransformSyncCablePoints (CableShape StartPt EndPt StartIdx EndIdx HOffset VOffset)
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
