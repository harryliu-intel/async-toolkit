; Copyright 2003 Fulcrum Microsy\stems.  All rights reserved.
; $Id: //depot/sw/intel/cad/external-tools-integration/cadence/virtuoso/skill/layout/cable/placer.il#1 $
; $DateTime: 2012/07/27 17:24:08 $
; $Author: pankala $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
defun( PlaceCableBuffers ( @key (CableShape car(geGetSelectedSet())) (CV (UIGetCellView)) )
  let( (
        Hops CableWidth
        BufIdx HopIdx
        I J 
        StartDist EndDist HopDist
        StartLoc
        CurrLoc CurrIdx CurrSeg CurrDir
        LocatorOP
        EvInst EvInstName
        OdInst ODInstName
        CableLength SearchSeed
        TemplateArray BufArray
        StartDistX MInst BlkShape
        )

      foreach( MInst CV~>instances
       MInst~>Orient = "R0"
       MInst~>xy = 0:0
      )

      foreach( BlkShape CV~>shapes
       when( car(BlkShape~>lpp) == "y5"
         dbDeleteObject(BlkShape)
       )
      )

      CBLCreateBufBlockage(?CableShape CableShape)
      EndDist   = CableShape~>OutputDistance_um

      Hops = CableShape~>BufferHops      
      when( stringp(Hops)
         Hops = evalstring(CableShape~>BufferHops)
       )

      StartDist = CableShape~>InputDistance_um
      when( stringp(StartDist)
        StartDist = evalstring(CableShape~>InputDistance_um)
       )

      EndDist   = CableShape~>OutputDistance_um
      when( stringp(EndDist)
      EndDist   = evalstring(CableShape~>OutputDistance_um)
       )

      CableWidth = CableShape~>CableWidth
      when( stringp(CableWidth)
       CableWidth = evalstring(CableShape~>CableWidth)
       )

      println(CableWidth)

      StartDistX = round(StartDist/1.56)*1.56

      SearchSeed = 0
      CableLength = CAComputeCableLength(CableShape)
      HopDist = round(300.8/3.12)*3.12 ; 250.8 ; round(((CableLength - (StartDist + EndDist))/(Hops-1))/3.12) * 3.12
      BufIdx = 0
      StartLoc = nth(0 CableShape~>points)
      BufArray = CBLCreateBufArray()

    for( HopIdx 0 Hops-1

  ;  println(HopIdx)
     I = 0

     while( I <=  length(BufArray) - 1 ; evalstring(CableShape~>CableWidth) - 1 
 
     BufIdx = BufArray[I]
  ;   println(BufIdx)

    when( BufIdx <= CableWidth ; evalstring(CableShape~>CableWidth)
     sprintf(EvInstName "mbuf[%d][%d]" HopIdx BufIdx)
     sprintf(OdInstName "mbuf[%d][%d]" HopIdx BufIdx+1)

     EvInst = dbFindAnyInstByName(CV EvInstName)
     OdInst = dbFindAnyInstByName(CV OdInstName)

      LocatorOP = CBLLocateBuffer(CableShape StartLoc StartDistX+(SearchSeed*I*3.12)+(HopIdx*HopDist))
      CurrLoc = nth(0 LocatorOP)
      CurrIdx = nth(1 LocatorOP)
      CurrSeg = nth(2 LocatorOP)
      CurrDir = nth(3 LocatorOP)

      case( CurrSeg
         ( "H"
           if( CurrDir == "P"
            then
               EvInst~>orient = "R0"
               OdInst~>orient = "R0"
               EvInst~>xy = CurrLoc
               OdInst~>xy = list(car(CurrLoc)+3.12 cadr(CurrLoc))
               EvInst~>CablePt = list(car(CurrLoc) cadr(CurrLoc)+0.78)
               OdInst~>CablePt = list(car(CurrLoc)+3.12 cadr(CurrLoc)+0.78)
               EvInst~>CableIdx = CurrIdx
               OdInst~>CableIdx = CurrIdx
               dbSet(EvInst "H" "Segment")
               dbSet(OdInst "H" "Segment")
            else 
               EvInst~>orient = "MY"
               OdInst~>orient = "MY"
               EvInst~>xy = CurrLoc
               OdInst~>xy = list(car(CurrLoc)-3.12 cadr(CurrLoc))
               EvInst~>CablePt = list(car(CurrLoc) cadr(CurrLoc)+0.78)
               OdInst~>CablePt = list(car(CurrLoc)-3.12 cadr(CurrLoc)+0.78)
               EvInst~>CableIdx = CurrIdx
               OdInst~>CableIdx = CurrIdx
               dbSet(EvInst "H" "Segment")
               dbSet(OdInst "H" "Segment")
              )
          )
         ( "V"
           if( CurrDir == "P"
            then
               EvInst~>orient = "R0"
               OdInst~>orient = "R0"
               EvInst~>xy = CurrLoc
               OdInst~>xy = list(car(CurrLoc)+3.12 cadr(CurrLoc))
               EvInst~>CablePt = list(car(CurrLoc)+0.78 cadr(CurrLoc))
               OdInst~>CablePt = list(car(CurrLoc)+0.78 cadr(CurrLoc))
               EvInst~>CableIdx = CurrIdx
               OdInst~>CableIdx = CurrIdx
               dbSet(EvInst "V" "Segment")
               dbSet(OdInst "V" "Segment")
            else 
               EvInst~>orient = "MX"
               OdInst~>orient = "MX"
               EvInst~>xy = CurrLoc
               OdInst~>xy = list(car(CurrLoc)+3.12 cadr(CurrLoc))
               EvInst~>CablePt = list(car(CurrLoc)+0.78 cadr(CurrLoc))
               OdInst~>CablePt = list(car(CurrLoc)+0.78 cadr(CurrLoc))
               EvInst~>CableIdx = CurrIdx
               OdInst~>CableIdx = CurrIdx
               dbSet(EvInst "V" "Segment")
               dbSet(OdInst "V" "Segment")
              )
            )
          )      
        )  
            I = I + 2
        )
   )

   PlaceResetBufs(?CableShape CableShape)


  )
 t
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
defun( CBLLocateBuffer (CableShape Loc Dist @key (CV (UIGetCellView)) (SearchMultiple 1) (BufType "DENSE"))
defun( CBLCreateBufShape (CurrLoc CurrSeg CurrDir @key (CV (UIGetCellView)) (BufType "DENSE"))
 let( (
       BufShape
       CurrLocX CurrLocY
       TrCurrLoc
       )
case( BufType

  ( "DENSE" || "NYB" || "XBUS"

   while( BufLocFound == nil

       LocatorOP = CBLLocateAtDist(CableShape Loc Dist ?CV CV)

       CurrLoc = car(LocatorOP)
       CurrSeg = CBLCurrSegment(CableShape cadr(LocatorOP))
       CurrDir = CBLCurrDir(CableShape cadr(LocatorOP))
    
       BufOP = CBLCreateBufShape(CurrLoc CurrSeg CurrDir ?BufType BufType ?CV CV)
       BufShape = nth(0 BufOP)

       Overlaps = dbGetTrueOverlaps(CV BufShape~>bBox)

     if( CBLOveralapExists(Overlaps "placer")
      then
        dbDeleteObject(BufShape)
        Dist = Dist + SearchMultiple*1.56
      else
        BufLocFound = t
        FinalIdx = cadr(LocatorOP)
        FinalLoc = nth(3 BufOP)
        FinalSeg = nth(1 BufOP)
        FinalDir = nth(2 BufOP)
        dbDeleteObject(BufShape)
        )
       )

  list(FinalLoc FinalIdx FinalSeg FinalDir)

 )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
defun( CBLCreateBufShape (CurrLoc CurrSeg CurrDir @key (CV (UIGetCellView)) (BufType "DENSE"))
 let( (
       BufShape
       CurrLocX CurrLocY
       TrCurrLoc
       )
case( BufType

  ( "DENSE" || "NYB"
   case( CurrSeg
     
      ( "H"
        if( CurrDir == "P"
         then
          CurrLocX = truncate(car(CurrLoc)/3.12)*3.12
          CurrLocY = truncate(cadr(CurrLoc)/3.12)*3.12
          BufShape = dbCreateRect(CV "y4" list(CurrLocX+0.78:CurrLocY+0.78 CurrLocX+5.46:CurrLocY+5.46))
         else 
          CurrLocX = (truncate(car(CurrLoc)/3.12)+1)*3.12
          CurrLocY = truncate(cadr(CurrLoc)/3.12)*3.12
          BufShape = dbCreateRect(CV "y4" list(CurrLocX-0.78:CurrLocY+0.78 CurrLocX-5.46:CurrLocY+5.46))
           )
       )
      ( "V"
        if( CurrDir == "P"
         then
          CurrLocX = truncate(car(CurrLoc)/3.12)*3.12
          CurrLocY = truncate(cadr(CurrLoc)/3.12)*3.12
          BufShape = dbCreateRect(CV "y4" list(CurrLocX+0.78:CurrLocY+0.78 CurrLocX+5.46:CurrLocY+5.46))
         else 
          CurrLocX = truncate(car(CurrLoc)/3.12)*3.12
          CurrLocY = (truncate(cadr(CurrLoc)/3.12)+1)*3.12
          BufShape = dbCreateRect(CV "y4" list(CurrLocX+0.78:CurrLocY-0.78 CurrLocX+5.46:CurrLocY-5.46))
           )
       )
     )
    )

  ( "TS" || "TS_18"
   case( CurrSeg
     
      ( "H"
        if( CurrDir == "P"
         then
          CurrLocX = truncate(car(CurrLoc)/3.12)*3.12
          CurrLocY = truncate(cadr(CurrLoc)/1.56)*1.56
          BufShape = dbCreateRect(CV "y4" list(CurrLocX+0.78:CurrLocY+0.78 CurrLocX+5.46:CurrLocY+7.02))
         else 
          CurrLocX = (truncate(car(CurrLoc)/3.12)+1)*3.12
          CurrLocY = truncate(cadr(CurrLoc)/1.56)*1.56
          BufShape = dbCreateRect(CV "y4" list(CurrLocX-0.78:CurrLocY+0.78 CurrLocX-5.46:CurrLocY+7.02))
           )
       )
      ( "V"
        if( CurrDir == "P"
         then
          CurrLocX = truncate(car(CurrLoc)/3.12)*3.12
          CurrLocY = truncate(cadr(CurrLoc)/1.56)*1.56
          BufShape = dbCreateRect(CV "y4" list(CurrLocX+0.78:CurrLocY+0.78 CurrLocX+5.46:CurrLocY+7.02))
         else 
          CurrLocX = truncate(car(CurrLoc)/3.12)*3.12
          CurrLocY = (truncate(cadr(CurrLoc)/1.56)+1)*1.56
          BufShape = dbCreateRect(CV "y4" list(CurrLocX+0.78:CurrLocY-0.78 CurrLocX+5.46:CurrLocY-7.02))
           )
       )
     )
    )

  ( "SYNC_NODE" 
   case( CurrSeg
     
      ( "H"
        if( CurrDir == "P"
         then
          CurrLocX = truncate(car(CurrLoc)/3.12)*3.12
          CurrLocY = truncate(cadr(CurrLoc)/1.56)*1.56
          BufShape = dbCreateRect(CV "y4" list(CurrLocX+0.39:CurrLocY+0.39 CurrLocX+2.76:CurrLocY+2.76))
         else 
          CurrLocX = (truncate(car(CurrLoc)/3.12)+1)*3.12
          CurrLocY = truncate(cadr(CurrLoc)/1.56)*1.56
          BufShape = dbCreateRect(CV "y4" list(CurrLocX-0.39:CurrLocY+0.39 CurrLocX-2.76:CurrLocY+2.76))
           )
      ( "V"
        if( CurrDir == "P"
         then
          CurrLocX = truncate(car(CurrLoc)/3.12)*3.12
          CurrLocY = truncate(cadr(CurrLoc)/1.56)*1.56
          BufShape = dbCreateRect(CV "y4" list(CurrLocX+0.39:CurrLocY+0.39 CurrLocX+2.76:CurrLocY+2.76))
         else 
          CurrLocX = truncate(car(CurrLoc)/3.12)*3.12
          CurrLocY = (truncate(cadr(CurrLoc)/1.56)+1)*1.56
          BufShape = dbCreateRect(CV "y4" list(CurrLocX+0.39:CurrLocY-0.39 CurrLocX+2.76:CurrLocY-2.76))
           )
     )
    )
  ( "RESET"
   case( CurrSeg
     
      ( "H"
        if( CurrDir == "P"
         then
          CurrLocX = truncate(car(CurrLoc)/3.12)*3.12
          CurrLocY = truncate(cadr(CurrLoc)/3.12)*3.12
          BufShape = dbCreateRect(CV "y4" list(CurrLocX+0.78:CurrLocY+0.78 CurrLocX+2.34:CurrLocY+2.34))
         else 
          CurrLocX = (truncate(car(CurrLoc)/3.12)+1)*3.12
          CurrLocY = truncate(cadr(CurrLoc)/3.12)*3.12
          BufShape = dbCreateRect(CV "y4" list(CurrLocX-0.78:CurrLocY+0.78 CurrLocX-2.34:CurrLocY+2.34))
           )
       )

      ( "V"
        if( CurrDir == "P"
         then
          CurrLocX = truncate(car(CurrLoc)/3.12)*3.12
          CurrLocY = truncate(cadr(CurrLoc)/3.12)*3.12
          BufShape = dbCreateRect(CV "y4" list(CurrLocX+0.78:CurrLocY+0.78 CurrLocX+2.34:CurrLocY+2.34))
         else 
          CurrLocX = truncate(car(CurrLoc)/3.12)*3.12
          CurrLocY = (truncate(cadr(CurrLoc)/3.12)+1)*3.12
          BufShape = dbCreateRect(CV "y4" list(CurrLocX+0.78:CurrLocY-0.78 CurrLocX+2.34:CurrLocY-2.34))
           )
       )

     )
   )
  )

  TrCurrLoc = list(CurrLocX CurrLocY)

  list(BufShape CurrSeg CurrDir TrCurrLoc)
 )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; to be called only once all the MBUFs are placed on the cable guide. 

(defun PlaceResetBufs ( @key (CableShape car(geGetSelectedSet())) (CV (UIGetCellView)) )
  let( ( Hops HopIdx BufInst BufInstName 
         StartDist StartLoc StartDistX HopDist LocatorOP 
         ResetBufInst ResetBufInstName ResetRampInst ResetRampInstName 
         CurrLoc CurrIdx CurrSeg CurrDir
        )

      Hops = CableShape~>BufferHops      
      when( stringp(Hops)
         Hops = evalstring(CableShape~>BufferHops)
       )      
      StartDist = CableShape~>InputDistance_um
      when( stringp(StartDist)
        StartDist = evalstring(CableShape~>InputDistance_um)
       )

   StartDistX = round(StartDist/1.56)*1.56
   HopDist = round(300.8/3.12)*3.12
   StartLoc = nth(0 CableShape~>points)

   for( HopIdx 0 Hops-1
     sprintf(BufInstName "mbuf[%d][0]", HopIdx)  
     sprintf(ResetBufInstName "bufReset[%d]", HopIdx)
     sprintf(ResetRampInstName "rampReset[%d]", HopIdx)

     BufInst = dbFindAnyInstByName(CV BufInstName)
     ResetBufInst = dbFindAnyInstByName(CV ResetBufInstName)
     ResetRampInst = dbFindAnyInstByName(CV ResetRampInstName)

     LocatorOP = CBLLocateBuffer(CableShape StartLoc StartDistX+(HopIdx*HopDist) ?SearchMultiple -1 ?BufType "RESET")
      CurrLoc = nth(0 LocatorOP)
      CurrIdx = nth(1 LocatorOP)
      CurrSeg = nth(2 LocatorOP)
      CurrDir = nth(3 LocatorOP)

      case( CurrSeg
         ( "H"
           if( CurrDir == "P"
            then
               ResetBufInst~>orient = "R0"
               ResetRampInst~>orient = "R0"
               ResetBufInst~>xy = CurrLoc
               ResetRampInst~>xy = list(car(CurrLoc) cadr(CurrLoc)+3.12)
               ResetBufInst~>CablePt = list(car(CurrLoc) cadr(CurrLoc)+0.78)
               ResetRampInst~>CablePt = list(car(CurrLoc) cadr(CurrLoc)+0.78)
               ResetBufInst~>CableIdx = CurrIdx
               ResetRampInst~>CableIdx = CurrIdx
               dbSet(ResetBufInst "H" "Segment")
               dbSet(ResetRampInst "H" "Segment")
            else 
               ResetBufInst~>orient = "MY"
               ResetRampInst~>orient = "MY"
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
               ResetBufInst~>orient = "R0"
               ResetRampInst~>orient = "R0"
               ResetBufInst~>xy = CurrLoc
               ResetRampInst~>xy = list(car(CurrLoc)+3.12 cadr(CurrLoc))
               ResetBufInst~>CablePt = list(car(CurrLoc)+0.78 cadr(CurrLoc))
               ResetRampInst~>CablePt = list(car(CurrLoc)+0.78 cadr(CurrLoc))
               ResetBufInst~>CableIdx = CurrIdx
               ResetRampInst~>CableIdx = CurrIdx
               dbSet(ResetBufInst "V" "Segment")
               dbSet(ResetRampInst "V" "Segment")
            else 
               ResetBufInst~>orient = "MX"
               ResetRampInst~>orient = "MX"
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(defun AnalyzeCable ( @key (CableShape car(geGetSelectedSet())) )
; let( ( )
;
;    
;
;
; )
;)
;
