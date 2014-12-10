; Copyright 2003 Fulcrum Microsy\stems.  All rights reserved.
; $Id: //depot/sw/intel/cad/external-tools-integration/cadence/virtuoso/skill/layout/cable/util.il#1 $
; $DateTime: 2012/07/27 17:24:08 $
; $Author: pankala $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Generic function for overlaps. Use the Toggle for different checks

defun( CBLOveralapExists (Overlaps Toggle)
  let( ( lay Overlap OverlapFound )

   Overlap = nil
   OverlapFound = nil
 
   foreach( lay Overlaps

   case( Toggle

     ( "placer"
       ;  add other blockage layers in the condition
      when( (car(lay~>lpp) == "y5" || car(lay~>lpp) == "OBS") && !OverlapFound
        Overlap = t
        OverlapFound = t
      )
      when( lay~>isAnyInst && !OverlapFound
        if( lay~>libName == "lib.buffer.smr" || lay~>libName == "lib.buffer.nyb" ||   lay~>libName == "lib.buffer.ts"
         then
           Overlap = t
           OverlapFound = t
         else
           Overlap = nil
         )
        )
       )

     ( "router"
       ;  add other blockage layers in the condition
      when( cadr(lay~>lpp) == "bus"
        Overlap = t
         )
       )

      )
    )

  Overlap
 )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
defun( CBLCurrDir (CableShape CurrIdx)
 let( (Direction
       Seg  
       )
   
    Seg = CBLCurrSegment(CableShape CurrIdx)

   if( Seg == "H"
    then
      if( car(nth(CurrIdx CableShape~>points)) > car(nth(CurrIdx-1 CableShape~>points))
       then
         Direction = "P"
       else
         Direction = "N"
         )
    else 
      if( cadr(nth(CurrIdx CableShape~>points)) > cadr(nth(CurrIdx-1 CableShape~>points))
       then
         Direction = "P"
       else
         Direction = "N"
         )
     )

 Direction
 )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

defun( CBLCurrSegment (CableShape CurrIdx)
 let( ( Segment ) 
  
;  if( CurrIdx < CableShape~>nPoints-1
;   then
    if( car(nth(CurrIdx CableShape~>points)) == car(nth(CurrIdx-1 CableShape~>points))
     then
      Segment = "V"
     else
      Segment = "H"
     )
;  else
;    Segment = nil
;   )
 Segment
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

defun( CBLSetCableProp (Shape Prp)
    
       dbReplaceProp(Shape Prp "boolean" "TRUE")

t
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
defun( CBLFindPtIdxs ( CableShape Point @key (CV (UIGetCellView)) )
  let( ( LwIdx UpIdx FoundIdx
         I N SegLen  
         StartPt EndPt StartX StartY EndX EndY PtX PtY
        )

  N = CableShape~>nPoints
  LwIdx = nil
  UpIdx = nil
  FoundIdx = nil

 when( cadr(Point) != nil
  PtX = round(car(Point)*1000)
  PtY = round(cadr(Point)*1000)

  for( I 0 N-2
  
    StartPt = nth(I CableShape~>points)
    StartX = round(car(StartPt)*1000)
    StartY = round(cadr(StartPt)*1000)

    EndPt   = nth(I+1 CableShape~>points)
    EndX = round(car(EndPt)*1000)
    EndY = round(cadr(EndPt)*1000)

    if( StartY == EndY ; && !FoundIdx 
      then
       SegLen = abs(EndX - StartX)
       when( (PtY == StartY) && ( (abs(PtX - StartX) + abs(PtX - EndX)) == SegLen )
            LwIdx = I
            UpIdx = I+1
            FoundIdx = t
           )
      else
       SegLen = abs(EndY - StartY)
       when( (PtX == StartX) && ( (abs(PtY - StartY) + abs(PtY - EndY)) == SegLen )
            LwIdx = I
            UpIdx = I+1
            FoundIdx = t
         )
       )
      )
   )

  list(LwIdx UpIdx)
 )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

defun( CBLLocateAtDist ( CableShape StartLoc Distance @key (CV (UIGetCellView)) )
 let( (
       XYCoOrd
       FinalIdx
       LwIdx UpIdx 
       CurrIdx CurrDist CurrLoc
       CablePoints
       LocFound
       shp
       )

     CablePoints = CableShape~>points
     UpIdx = cadr(CBLFindPtIdxs(CableShape StartLoc))
     CurrIdx = UpIdx
     CurrDist = Distance
     CurrLoc = StartLoc

     while( !LocFound  && (CurrIdx != length(CablePoints))
      if( car(CurrLoc) == car(nth(CurrIdx CablePoints))
       then 
        if( CurrDist <= abs(difference(cadr(CurrLoc) cadr(nth(CurrIdx CablePoints))))
          then
           if( negativep(difference(cadr(CurrLoc) cadr(nth(CurrIdx CablePoints))))
            then
             XYCoOrd = list(car(CurrLoc) cadr(CurrLoc)+CurrDist)
             FinalIdx = CurrIdx
             LocFound = t
            else
             XYCoOrd = list(car(CurrLoc) difference(cadr(CurrLoc) CurrDist))
             FinalIdx = CurrIdx
             LocFound = t             
            )
          else
           CurrDist = CurrDist - abs(difference(cadr(CurrLoc) cadr(nth(CurrIdx CablePoints))))
           CurrLoc = nth(CurrIdx CablePoints)
           CurrIdx = CurrIdx + 1
         )
        else
         if( CurrDist <= abs(difference(car(CurrLoc) car(nth(CurrIdx CablePoints))))
          then
           if( negativep(difference(car(CurrLoc) car(nth(CurrIdx CablePoints))))
            then
             XYCoOrd = list(car(CurrLoc)+CurrDist cadr(CurrLoc))
             FinalIdx = CurrIdx
             LocFound = t
            else
             XYCoOrd = list(difference(car(CurrLoc) CurrDist) cadr(CurrLoc))
             FinalIdx = CurrIdx
             LocFound = t             
            )
          else
           CurrDist =  CurrDist - abs(difference(car(CurrLoc) car(nth(CurrIdx CablePoints))))
           CurrLoc = nth(CurrIdx CablePoints)
           CurrIdx = CurrIdx + 1
         )

       )


     )

  ; shp = dbCreateRect(CV "M5" list(XYCoOrd list(car(XYCoOrd)+1.56 cadr(XYCoOrd)+1.56)))

 list(XYCoOrd FinalIdx)
 )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

defun( CBLCreateBufBlockage ( @key (CableShape car(geGetSelectedSet())) (CV (UIGetCellView)) (BlockLPP list("y5" "drawing")))

  let( (
        CablePoints
        PathWidth
        XCord YCord
        I N
        BufBlock BlockList
      )

    PathWidth   = CBLGetPathWidth(?CableShape CableShape)
    CablePoints = CableShape~>points
    N = CableShape~>nPoints

   BlockList = nil

  when( N > 2
    for( I 1 N-2

      XCord =   truncate(car(nth(I CableShape~>points))/3.12)*3.12
      YCord =   truncate(cadr(nth(I CableShape~>points))/3.12)*3.12

      BufBlock = dbCreateRect(CV BlockLPP list(list(XCord YCord) list((XCord + PathWidth*3.12) (YCord + PathWidth*3.12)))  )
      CBLSetCableProp(BufBlock "BufBlock")
      BlockList = cons(BufBlock BlockList)
     )
   )

  
BlockList
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
defun( CBLGetPathWidth ( @key (CableShape car(geGetSelectedSet())) )
  let( (
        DataWidth 
        PathWidth
        WidthFound channelBuf
       )
      DataWidth = 1
      when( stringp(CableShape~>ChannelWidth) DataWidth = evalstring(CableShape~>ChannelWidth)) 
      when( numberp(CableShape~>ChannelWidth) DataWidth = CableShape~>ChannelWidth) 
      DataWidth = DataWidth - 1
      channelBuf

   
;   WidthFound = nil
    PathWidth = 2

; if( DataWidth > 12
;  then
;   while( PathWidth > 1 && WidthFound != t
;
;      if( DataWidth <= (PathWidth-1)*12 && DataWidth >= (PathWidth-2)*12
;        then
;          WidthFound = t
;        else
;          PathWidth = PathWidth - 1
;        )
;
;     )
;  else
;   PathWidth = 2
;   )

       when( DataWidth >  0  && DataWidth < 12
          PathWidth = 2 
       )
       when( DataWidth >= 12 && DataWidth < 24
          PathWidth = 3     
       )
       when( DataWidth >= 24 && DataWidth < 36
          PathWidth = 4     
       )
       when( DataWidth >= 36 && DataWidth < 48
          PathWidth = 5     
       )
       when( DataWidth >= 48 && DataWidth < 60
          PathWidth = 6     
       )

    when( channelBuf == "TS"
       when( DataWidth >  0  && DataWidth < 12
          PathWidth = 2 
       )
       when( DataWidth >= 12 && DataWidth < 24
          PathWidth = 3     
       )
       when( DataWidth >= 24 && DataWidth < 36
          PathWidth = 4     
       )
       when( DataWidth >= 36 && DataWidth < 48
          PathWidth = 5     
       )
       when( DataWidth >= 48 && DataWidth < 60
          PathWidth = 6     
       )
     )
  PathWidth

  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
defun( CAComputeCableLength ( CableShape @key (CV (UIGetCellView)) )
  let( (
       CableLength
       I N
       StartPt EndPt
       )
  N = CableShape~>nPoints
  CableLength = 0
  for( I 0 N-2

    StartPt = nth(I CableShape~>points)
    EndPt   = nth(I+1 CableShape~>points)

   when( cadr(StartPt) == cadr(EndPt)
      CableLength = CableLength + abs(difference(car(StartPt) car(EndPt)))
      )

   when( car(StartPt) == car(EndPt)
      CableLength = CableLength + abs(difference(cadr(StartPt) cadr(EndPt)))
      )
  )
  CableLength
 )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun SetCablePattern (template @key (paths nil))
  (let (pat)
    paths = (if paths==nil (geGetSelSet) paths)
    (foreach obj paths
      (cond (obj->objType=="path"
                  pat = (if template template (GetProp obj "ChannelTemplate" "Template_A"))
                  (CreateCableProps ?paths (list obj))
                  (dbReplaceProp obj "ChannelTemplate" "string" pat)
                  )
            )
      )
    t
  )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun CreateCableProps (@key (paths nil))
  (let (
        template   width  prop_mbuf  prop_template 
        mbuf       hops   prop_hops  prop_hopdist  
        sbuf       start  prop_width 
        rbuf       end    hopdist  
       )
    paths = (if paths==nil (geGetSelSet) paths)
    props = (FindCableProps)
    when( nth(0 props) prop_mbuf     = nth(0 props) )
    when( nth(1 props) prop_hops     = nth(1 props) )
    when( nth(2 props) prop_width    = nth(2 props) )
    when( nth(3 props) prop_template = nth(3 props) )
    when( nth(4 props) prop_hopdist  = nth(4 props) )
    (foreach obj paths
             (cond (obj->objType=="path"
                       ; copy properties
                       template = (GetProp   obj   "ChannelTemplate"     prop_template )
                       mbuf     = (GetProp   obj   "ChannelBuf"          prop_mbuf     )
                       sbuf     = (GetProp   obj   "ChannelSbufOrient"  "H"            )
                       rbuf     = (GetProp   obj   "ChannelRbufOrient"  "H"            )
                       hops     = (GetProp   obj   "ChannelHops"         prop_hops     )
                       width    = (GetProp   obj   "ChannelWidth"        prop_width    )
                       start    = (GetProp   obj   "InputDistance_um"    0.0           )
                       end      = (GetProp   obj   "OutputDistance_um"   0.0           )
                       hopdist  = (GetProp   obj   "HopDistance_um"      prop_hopdist  )

                       ; delete properties
                       (dbDeletePropByName   obj   "ChannelTemplate"    )
                       (dbDeletePropByName   obj   "ChannelBuf"         )
                       (dbDeletePropByName   obj   "ChannelSbufOrient"  )
                       (dbDeletePropByName   obj   "ChannelRbufOrient"  )
                       (dbDeletePropByName   obj   "ChannelHops"        )
                       (dbDeletePropByName   obj   "ChannelWidth"       )
                       (dbDeletePropByName   obj   "InputDistance_um"   )
                       (dbDeletePropByName   obj   "OutputDistance_um"  )
                       (dbDeletePropByName   obj   "HopDistance_um"     )

                       ; set new properties
                       (dbReplaceProp        obj   "ChannelTemplate"    "string"  template )
                       (dbReplaceProp        obj   "ChannelBuf"         "string"  mbuf     )
                       (dbReplaceProp        obj   "ChannelSbufOrient"  "string"  sbuf     )
                       (dbReplaceProp        obj   "ChannelRbufOrient"  "string"  rbuf     )
                       (dbReplaceProp        obj   "ChannelHops"        "fixnum"  hops     )
                       (dbReplaceProp        obj   "ChannelWidth"       "fixnum"  width    )
                       (dbReplaceProp        obj   "InputDistance_um"   "flonum"  start    )
                       (dbReplaceProp        obj   "OutputDistance_um"  "flonum"  end      )
                       (dbReplaceProp        obj   "HopDistance_um"     "flonum"  hopdist  )

                       ))
             )
    )
  t
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun FindCableProps (@key (CV (UIGetCellView)) )
 (let ( props 
        parts1  parts2  parts3  
        bufType bufHops bufWidth buftemplate hopDist 
        )
   
    if( CV~>libName == "chip.rrc.util.cable"
     then
      parts1   =  parseString(CV~>cellName   "."  )
      parts2   =  parseString(nth(4 parts1)  "-"  )
      parts3   =  parseString(nth(1 parts2)  "_L" )
      bufHops  =   readstring(nth(0 parts3) )
      bufWidth =   readstring(nth(1 parts3) )
      case( nth(0 parts2) 
        ( "CABLE_MBUFS"     || "CABLE"     bufType = "DENSE" buftemplate = "Template_BUF_R90"    hopDist = 290.0 )
        ( "NYB_CABLE_MBUFS" || "NYB_CABLE" bufType = "NYB"   buftemplate = "Template_NYBBUF_R90" hopDist = 290.0 )
        ( "TS_CABLE_MBUFS"  || "TS_CABLE"  bufType = "TS"    buftemplate = "Template_TSBUF_R90"  hopDist = 230.0 )
       )
     props = list(bufType bufHops bufWidth buftemplate hopDist )
     else
      props = nil
      )
props
 )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun CreateCableBoundary (@key (CableShape car(geGetSelectedSet())) (CV (UIGetCellView)) (CreatePRObject nil))
  (let ( I CurrDir CurrSeg PathWidth NotStart NotEnd 
         PRList PRShape MergePRShape
        )

    PRList = CBLCreateBufBlockage(?CableShape CableShape ?BlockLPP list("prBoundary" "drawing"))
    PathWidth = CBLGetPathWidth(?CableShape CableShape)

    for( I 1 CableShape~>nPoints-1
       CurrSeg = CBLCurrSegment(CableShape I)
       CurrDir = CBLCurrDir(CableShape I)
       NotStart = 1
       NotEnd = 1
       when( I == 1 NotStart = 0 )
       when( I == CableShape~>nPoints-1 NotEnd = 0 )
       PRShape = CreateCableSegmentBoundary(CV PathWidth nth(I-1 CableShape~>points) nth(I CableShape~>points) CurrSeg CurrDir NotStart NotEnd)
       PRList = cons(PRShape PRList)
     )

    MergePRShape = car(leMergeShapes(remove(nil PRList)))

   when( CreatePRObject
    if( MergePRShape~>points
     then
       dbCreatePRBoundary(CV MergePRShape~>points)
     else
       dbCreatePRBoundary(CV BBoxToPoints(MergePRShape~>bBox))
    )
   )

MergePRShape
     
 )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun CreateCableSegmentBoundary (CV PathWidth StartPt EndPt Segment Dir NotStart NotEnd)
 (let ( StartXCord StartYCord EndXCord EndYCord 
        MetalbBox PRbBox M4Block M2Block M6Block M3Block M5Block M7Block PRShape
        ) 

        StartXCord =  truncate((car(StartPt)*1000)/(PowerGridPitch*2000))*PowerGridPitch*2
        StartYCord =  truncate((cadr(StartPt)*1000)/(PowerGridPitch*2000))*PowerGridPitch*2
        EndXCord =   truncate((car(EndPt)*1000)/(PowerGridPitch*2000))*PowerGridPitch*2
        EndYCord =   truncate((cadr(EndPt)*1000)/(PowerGridPitch*2000))*PowerGridPitch*2

   when( Segment == "V"
      if( Dir == "P"
       then
         when(NotEnd == 0 EndYCord = cadr(EndPt)) 
         when(NotStart == 0 StartYCord =  cadr(StartPt))
         MetalbBox = list(StartXCord+PowerGridPitch/4:StartYCord+PowerGridPitch/4 EndXCord-PowerGridPitch/4+PathWidth*PowerGridPitch*2:EndYCord-PowerGridPitch/4+(PathWidth*PowerGridPitch*2)*NotEnd)
         PRbBox = list(StartXCord:StartYCord+(PathWidth*PowerGridPitch*2)*NotStart EndXCord+PathWidth*PowerGridPitch*2:EndYCord)
;         M2Block = dbCreateRect(CV list("M2" "boundary") MetalbBox)
;         SetBusProp(M2Block)
;         M4Block = dbCreateRect(CV list("M4" "boundary") MetalbBox)
;         SetBusProp(M4Block)
         M6Block = dbCreateRect(CV list("M6" "boundary") MetalbBox)
         SetBusProp(M6Block)
         PRShape = dbCreateRect(CV list("prBoundary" "drawing") PRbBox)
         SetBusProp(PRShape)
       else
         when(NotEnd == 0 EndYCord = cadr(EndPt)) 
         when(NotStart == 0 StartYCord =  cadr(StartPt))
         MetalbBox = list(StartXCord+PowerGridPitch/4:StartYCord-PowerGridPitch/4+(PathWidth*PowerGridPitch*2)*NotStart EndXCord-PowerGridPitch/4+PathWidth*PowerGridPitch*2:EndYCord+PowerGridPitch/4)
         PRbBox = list(StartXCord:StartYCord EndXCord+PathWidth*PowerGridPitch*2:EndYCord+(PathWidth*PowerGridPitch*2)*NotEnd)
;         M2Block = dbCreateRect(CV list("M2" "boundary") MetalbBox)
;         SetBusProp(M2Block)
;         M4Block = dbCreateRect(CV list("M4" "boundary") MetalbBox)
;         SetBusProp(M4Block)
         M6Block = dbCreateRect(CV list("M6" "boundary") MetalbBox)
         SetBusProp(M6Block)
         PRShape = dbCreateRect(CV list("prBoundary" "drawing") PRbBox)
         SetBusProp(PRShape)
       )
     )
   when( Segment == "H"
      if( Dir == "P"
       then
         when(NotEnd == 0 EndXCord = car(EndPt)) 
         when(NotStart == 0 StartXCord = car(StartPt))
         MetalbBox = list(StartXCord+PowerGridPitch/4:StartYCord+PowerGridPitch/4 EndXCord-PowerGridPitch/4+(PathWidth*PowerGridPitch*2)*NotEnd:EndYCord-PowerGridPitch/4+PathWidth*PowerGridPitch*2)
         PRbBox = list(StartXCord+(PathWidth*PowerGridPitch*2)*NotStart:StartYCord EndXCord:EndYCord+PathWidth*PowerGridPitch*2)
;         M3Block = dbCreateRect(CV list("M3" "boundary") MetalbBox)
;         SetBusProp(M3Block)
;         M5Block = dbCreateRect(CV list("M5" "boundary") MetalbBox)
;         SetBusProp(M5Block)
         M7Block = dbCreateRect(CV list("M7" "boundary") MetalbBox)
         SetBusProp(M7Block)
         PRShape = dbCreateRect(CV list("prBoundary" "drawing") PRbBox)
         M3Block = dbCreateRect(CV list("M3" "boundary") MetalbBox)
         SetBusProp(M3Block)
         M5Block = dbCreateRect(CV list("M5" "boundary") MetalbBox)
         SetBusProp(M5Block)         SetBusProp(PRShape)
       else
         when(NotEnd == 0 EndXCord = car(EndPt)) 
         when(NotStart == 0 StartXCord =  car(StartPt))
         MetalbBox = list(StartXCord-PowerGridPitch/4+(PathWidth*PowerGridPitch*2)*NotStart:StartYCord+PowerGridPitch/4 EndXCord+PowerGridPitch/4:EndYCord-PowerGridPitch/4+PathWidth*PowerGridPitch*2)
         PRbBox = list(StartXCord:StartYCord EndXCord+(PathWidth*PowerGridPitch*2)*NotEnd:EndYCord+PathWidth*PowerGridPitch*2)
;         M3Block = dbCreateRect(CV list("M3" "boundary") MetalbBox)
;         SetBusProp(M3Block)
;         M5Block = dbCreateRect(CV list("M5" "boundary") MetalbBox)
;         SetBusProp(M5Block)
         M7Block = dbCreateRect(CV list("M7" "boundary") MetalbBox)
         SetBusProp(M7Block)
         PRShape = dbCreateRect(CV list("prBoundary" "drawing") PRbBox)
         SetBusProp(PRShape)
       )
     )

 PRShape
 )   
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun CreateCableBufObstruction (@key (CV (UIGetCellView)))
 (let ( BufInst OBTemp OBShapes )

   OBShapes = nil

   foreach( BufInst wcv()~>instances
      when( BufInst~>libName == "lib.buffer.smr"
      OBTemp = dbCreateRect(CV list("M8" "boundary") list(PGRound(caar(BufInst~>bBox)):PGRound(cadar(BufInst~>bBox)) PGRound(caadr(BufInst~>bBox)):PGRound(cadadr(BufInst~>bBox))))
      dbSet(OBTemp BufInst~>Segment "Segment")
      when( OBTemp  OBShapes = cons(OBTemp OBShapes) )
    )
   )
  leMergeShapes(OBShapes)
 )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun PGRound ( CoOrd @key (Factor 1) )
   round(round(CoOrd*1000)/(PowerGridPitch*Factor*1000))*PowerGridPitch*Factor
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun PGTruncate ( CoOrd @key (Factor 1) )
   truncate(round(CoOrd*1000)/(PowerGridPitch*Factor*1000))*PowerGridPitch*Factor
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun CreateSyncCablePowerPins ( CV layer @key (instprefix "mbuf") )
  let( ( inst instname xy x1 y1 figg figv netg netv label pinop )

    label = t

    instname = strcat(instprefix, "[0][0]") 

    inst = dbFindAnyInstByName(CV instname)

    pinop = CablePowerPinFigs(CV inst)


      ; GND pin
      if( dbFindNetByName( CV GNDNetName )
        then
          netg = dbFindNetByName( CV GNDNetName )
        else
          netg = dbMakeNet( CV GNDNetName )
       )

      figg = car(pinop)
        dbReplaceProp( figg "PinType" "string" "PowerGrid" )
        dbReplaceProp( figg "PinType" "string" "Power")
        dbCreatePin( netg figg )
        when( label MidPinsCreateLabel(CV figg) )


      ; Vdd pin
      if( dbFindNetByName( CV  VddNetName)
        then
          netv = dbFindNetByName( CV VddNetName )
        else
          netv = dbMakeNet( CV  VddNetName)
       )

      figv = cadr(pinop)
        dbReplaceProp( figv "PinType" "string" "PowerGrid" )
        dbReplaceProp( figv "PinType" "string" "Power")
        dbCreatePin( netv figv )
        when( label MidPinsCreateLabel(CV figv) )

t
 )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun CablePowerPinFigs ( CellView inst )
  let( ( xy x1 y1 figg figv ) 

      xy = inst~>xy
      x1 = car( xy )
      y1 = cadr( xy )

   case( inst~>orient
     ( "MXR90"
        figg = dbCreateRect( CellView list(layer "gnd")
                                      list(x1+3.03:y1+1.535 x1+3.08:y1+1.585) )
        figv = dbCreateRect( CellView list(layer "vdd")
                                      list(x1+1.6:y1+1.535 x1+1.65:y1+1.585) )
     )
     ( "R90"
        figg = dbCreateRect( CellView list(layer "gnd")
                                      list(x1-3.03:y1+1.535 x1-3.08:y1+1.585) )
        figv = dbCreateRect( CellView list(layer "vdd")
                                      list(x1-1.6:y1+1.535 x1-1.65:y1+1.585) )
      )
     ( "R270"
        figg = dbCreateRect( CellView list(layer "gnd")
                                      list(x1+3.03:y1-1.535 x1+3.08:y1-1.585) )
        figv = dbCreateRect( CellView list(layer "vdd")
                                      list(x1+1.6:y1-1.535 x1+1.65:y1-1.585) )
     )
    ) 

   list(figg figv)
 )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun FindCableLength ( @key (CableShape car(geGetSelectedSet())) )
 let( ( sum I cpt npt ) 

  sum = 0

  for( I 0 CableShape~>nPoints-2
      cpt = nth(I CableShape~>points)
      npt = nth(I+1 CableShape~>points)
      sum = sum + abs(car(cpt)-car(npt)) + abs(cadr(cpt)-cadr(npt))

    )
sum
 )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun CreateCableCell ( libName @key (CableShape car(geGetSelectedSet())) )
  (let ( Hops W cellName cellfound subtype cv newcv )

   cellfound = nil
   subtype = 1000
      Hops      = CableShape~>BufferHops ; evalstring(CableShape~>BufferHops)
      W         = CableShape~>ChannelWidth

    while( !cellfound 

        sprintf(cellName "%s.CABLE-L%d_%d-R.%d" libName Hops W subtype)

        cv = nrOpenCellViewReadableByName(cellName "cable")
        if( cv
         then
           dbClose(cv)
           subtype = subtype + 1 
         else
           newcv = nrOpenCellViewWritableByName(cellName "cable")
           dbCopyShape(CableShape newcv)
           dbSave(newcv)
           dbClose(newcv)
           
           cellfound = t
        )
     )

list(cellName subtype)
 )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun CreateCableSpec ( libName Hops W subtype )
  let( ( source cell module define shoutdir skoutdir root root1 
         libparts inf shinfl skinfl skoutfl outf strparts ) 

    root = "/nfs/site/disks/wdisk.83/pankala1/rrc_1/spec"
    root1 = "/nfs/site/disks/wdisk.83/pankala1/rrc_1/spec"
    source = strcat(root1 "/experiment/cable/CABLE\\(3,10\\)/1000.cast")

    libparts = parseString(libName ".")
    shoutdir = root
    skoutdir = root
    for( I 0 length(libparts)-1
      shoutdir = strcat(shoutdir "/" nth(I libparts))
      skoutdir = strcat(skoutdir "/" nth(I libparts))
     )

    ;evalstring(Hops)

    when( stringp(Hops)
       println("helo")
       Hops = evalstring(Hops)
     )
    when( stringp(W)
       W = evalstring(W)
     )

    sprintf(shoutdir "%s/CABLE\\(%d,%d\\)" shoutdir Hops W)
    sprintf(skoutdir "%s/CABLE(%d,%d)" skoutdir Hops W)
    

    sh(strcat("mkdir -p " shoutdir))
    sprintf(shinfl "%s/%d.cast" shoutdir subtype)
    sprintf(skinfl "%s/%d.cast" skoutdir subtype)
    sprintf(shoutfl "%s/%d_temp.cast" shoutdir subtype)
    sprintf(skoutfl "%s/%d_temp.cast" skoutdir subtype)
    

    inf = infile(skinfl)
 
    when( !inf 
       println("jaffa")
       sh(strcat("cp " source " " shinfl))
       sh(strcat("chmod +w " shinfl))
       inf = infile(skinfl)      
     )

    outf = outfile(skoutfl)

    sprintf(cell "%s.CABLE(%d,%d)" libName Hops W)
    module = strcat("module " cell ";\n")
    sprintf(define "define \"%d\"() <+ standard.process.standard_layout_cell <: %s { \n" subtype cell)

    while( gets(str inf)
      strparts=parseString(str)
      when( car(strparts) == "module"
          str = module
        )
      when( car(strparts) == "define"
          str = define
        )
       fprintf(outf "%s" str)
     )
 close(inf)
 close(outf)

 sh(strcat("mv " shoutfl " " shinfl))

    println(module)
    println(define)
    println(skoutdir)
    println(skinfl)
    

  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ReplaceCableShapes ( @key (CV (UIGetCellView)) (CableShapes (geGetSelectedSet)) )
  let( ( cbl cellop instname I )
     I = 0
     foreach( cbl CableShapes
        cellop = CreateCableCell("experiment.cable.ring" ?CableShape cbl)
        sprintf(instname "cable[%d]" I)
        dbCreateInstByMasterName(CV "experiment.cable.ring" car(cellop) "cable" instname list(0 0) "R0")
        CreateCableSpec("experiment.cable.ring" cbl~>BufferHops cbl~>ChannelWidth cadr(cellop))
        I = I+1
      )
t
 )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun InverseTransform ( Transform )
  (let ( newTrans )

   origin = nth(0 Transform)
   rotate = nth(1 Transform)
   
   case( rotate
      ( "R0"
         newOrigin = list(car(origin)*-1.0 cadr(origin)*-1.0)
         newRotate = "R0"
       )
      ( "R90"
         newOrigin = list(car(origin)*-1.0 cadr(origin))
         newRotate = "R270"
       )
      ( "R180"
         newOrigin = list(car(origin) cadr(origin))
         newRotate = "R180"
       )
      ( "R270"
         newOrigin = list(car(origin) cadr(origin)*-1.0)
         newRotate = "R90"
       )
      ( "MX"
         newOrigin = list(car(origin)*-1.0 cadr(origin))
         newRotate = "MX"
       )
      ( "MXR90"
         newOrigin = list(car(origin)*-1.0 cadr(origin)*-1.0)
         newRotate = "MXR90"
       )
      ( "MY"
         newOrigin = list(car(origin) cadr(origin)*-1.0)
         newRotate = "MY"
       )
      ( "MYR90"
         newOrigin = list(car(origin) cadr(origin))
         newRotate = "MYR90"
       )
      ( t
        newOrigin = origin
        newRotate = rotate
       ) 
     )

  list(newOrigin newRotate nth(2 Transform))
 )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun DrawHierTopLevelShape ( objPath @key (CV (UIGetCellView)) (layer nil) )
  (let ( tempPath  TopTransform botObj botInst topBBox topPts topObj objLayer )
  when( listp(objPath)
   tempPath = objPath
   TopTransform = dbGetHierPathTransform(objPath)

   while( listp(cadr(tempPath))
      tempPath = cadr(tempPath)
    )
   botInst = car(tempPath)
   botObj = cadr(tempPath)
   if( layer 
    then
      objLayer = layer
    else
      objLayer = botObj~>lpp
     )

   case( botObj~>objType
      ( "rect"
        topBBox = dbTransformBBox(botObj~>bBox TopTransform)
        topObj = dbCreateRect(CV objLayer topBBox) 
        dbCopyProp(botObj topObj)
      )
      ( "polygon"
        topPts = TransformPoints(botObj~>points TopTransform)
        topObj = dbCreatePolygon(CV objLayer topPts)
        dbCopyProp(botObj topObj)
      )
      ( "path"
        topPts = TransformPoints(botObj~>points TopTransform)
        topObj = dbCreatePath(CV objLayer topPts botObj~>width botObj~>pathStyle)
        dbCopyProp(botObj topObj)
      )
    )
   list(topObj botInst)
   )
 )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun PushHierTopLevelShape ( objPath topObj botCV @key (CV (UIGetCellView)) )
  (let (tempPath currInst topGuide  botObj botInst)
   when( listp(objPath)
    
    tempPath = objPath

    case( topObj~>objType
       ( "rect"    topGuide = topObj~>bBox  )
       ( "polygon" || "path" topGuide = topObj~>points )
       )    

    while( listp(cadr(tempPath))
       currInst = car(tempPath)       
       topGuide = TransformPoints(topGuide InverseTransform(currInst~>transform))
       tempPath = cadr(tempPath)
     )

       botInst = car(tempPath)
       topGuide = TransformPoints(topGuide InverseTransform(botInst~>transform))

   case( topObj~>objType
      ( "rect"
        botObj = dbCreateRect(botCV topObj~>lpp topGuide) 
        dbCopyProp(topObj botObj)
      )
      ( "polygon"
        botObj = dbCreatePolygon(botCV topObj~>lpp topGuide)
        dbCopyProp(topObj botObj)
      )
      ( "path"
        botObj = dbCreatePath(botCV topObj~>lpp topGuide topObj~>width topObj~>pathStyle)
        dbCopyProp(topObj botObj)
      )
    )
   botObj
  )
 )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun TransformPoints (points Transform)
  (let ( I newlist ) 

    for( I 0 length(points)-1
      newlist = append(newlist list(dbTransformPoint(nth(I points) Transform)))
     ) 
newlist
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun FindAllCableShapes ( @key (CV (UIGetCellView)) )
  let( ( rawCables shp currShp currInst opFile opFileName aO autoObs
         cableInsts cableShapes cableOrigins currOP )

    rawCables = dbGetTrueOverlaps(CV CV~>bBox list("M8" "bus") 32)
    sprintf(opFileName "%s.cablecells" CV~>cellName)
    opFile = outfile(opFileName)

    foreach( autoObs setof( aO CV~>shapes aO~>AutoCableShape=="TRUE" ) dbDeleteObject(autoObs) )    

    foreach( shp rawCables 
      if( listp(shp) 
       then
         currOP = DrawHierTopLevelShape(shp ?CV CV ?layer list("y9" "drawing"))
         currShp = car(currOP)
         currShp~>AutoCableShape = "TRUE"
         currInst = cadr(currOP)
         if( currShp~>ChannelWidth
           then
              cableInsts = cons(currInst cableInsts)
              cableShapes = cons(currShp cableShapes)
              cableOrigins = cons(shp cableOrigins)
           else
              dbDeleteObject(currShp)
             )           
       else
         when( shp~>ChannelWidth
              cableInsts = cons(CV cableInsts)
              cableShapes = cons(shp cableShapes)
              cableOrigins = cons(shp cableOrigins)
             )         
       )
     )

    foreach( inst cableInsts fprintf(opFile "%s\n" inst~>cellName) )
    close(opFile)

list(cableShapes cableOrigins cableInsts)
 )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun GenerateCableOverlaps ( @key (CableShapes (geGetSelectedSet)) )
  (let ( cbl currPr allPr pr tempPr obs allObs )

   foreach( cbl CableShapes
      currPr = CreateCableBoundary(?CableShape cbl)
      allPr = cons(currPr allPr)
     )

   foreach( pr allPr
        tempPr = remove(pr allPr)
        obs = dbLayerAnd(wcv() list("OBS" "bus") list(pr) tempPr)
        allObs = append(allObs obs)
      )

   leMergeShapes(allObs)
   allPr

 )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun RefreshCableBlockages ( cableShapeInstList @key (CV (UIGetCellView)) )
  (let ( I cableShapes cableInsts cableOrigins 
         remPr currShp currInst allPr obs obj autoObs aO currCV )

  cableOrigins = nth(1 cableShapeInstList)
  cableShapes = nth(0 cableShapeInstList)
  cableInsts = nth(2 cableShapeInstList)

  foreach( autoObs setof( aO CV~>shapes aO~>AutoBufBlock=="TRUE" ) dbDeleteObject(autoObs) )    

  for( I 0 length(cableShapes)-1
    println(I)
    allPr = append(allPr list(CreateCableBoundary(?CableShape nth(I cableShapes))))
    )

  for( I 0 length(cableShapes)-1
    currShp = nth(I cableShapes)
    currInst = nth(I cableInsts)
    currOrigin = nth(I cableOrigins)
    currPr = nth(I allPr)
    when( listp(currOrigin)
       obs = dbLayerAnd(CV list("OBS" "bus") list(currPr) remove(currPr allPr))
       currCV = nrOpenCellViewWritableByName(currInst~>cellName "cable")
      if( currCV
       then
         foreach( autoObs setof( aO currCV~>shapes aO~>AutoBufBlock=="TRUE" ) dbDeleteObject(autoObs) )
         foreach( obj obs obj~>AutoBufBlock = "TRUE"  PushHierTopLevelShape(currOrigin obj currCV) )
         foreach( obj obs dbDeleteObject(obj) )
         dbSave(currCV)     
         dbClose(currCV)
       else
         printf("Unable to open %s. Pls. run the auto check-out function !\n" currInst~>cellName)
        )
      )
    )

  (DeleteBusWires)
  allPr
 )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun P4AllCableSubcells ( @key (CV (UIGetCellView)) (CMD "ADD") (CustomFile nil) (CustomView "cable") )
  (let ( ipFileName ipFile cellName currCV ) 

        (if ( and
          ( equal ( ConfigFileGetValue TheCDSConfigTable "P4CONFIG" ) "" )
          ( or
            ( equal ( ConfigFileGetValue TheCDSConfigTable "P4USER" ) "" )
            ( equal ( ConfigFileGetValue TheCDSConfigTable "P4CLIENT" ) "" ) ) )
        (error "Must specify P4CONFIG or P4USER and P4CLIENT in envirnoment or cds.config" ) )

   if( CustomFile
     then
      ipFileName = CustomFile
     else
      sprintf(ipFileName "%s.cablecells" CV~>cellName)
     )

;   when( (CMD == "EDIT" || CMD == "ADD") && !CustomFile ; !ipFile
;      FindAllCableShapes(?CV CV)
;    )  

      ipFile = infile(ipFileName)

   while( gets(cellName ipFile)
      cellName = car(parseString(cellName "\n"))
      println(cellName)
      currCV = nrOpenCellViewReadableByName(cellName CustomView)
      case( CMD
          ( "ADD"
            ( CDSP4Add
                 ( ConfigFileGetValue TheCDSConfigTable "P4USER" )
                 ( ConfigFileGetValue TheCDSConfigTable "P4PASSWD" )
                 ( ConfigFileGetValue TheCDSConfigTable "P4CLIENT" )
                 ( ConfigFileGetValue TheCDSConfigTable "P4CONFIG" )
                   currCV 
                 ( ConfigFileGetValue TheCDSConfigTable "TEMP" ) )
          )
          ( "EDIT"
            ( CDSP4Edit
                 ( ConfigFileGetValue TheCDSConfigTable "P4USER" )
                 ( ConfigFileGetValue TheCDSConfigTable "P4PASSWD" )
                 ( ConfigFileGetValue TheCDSConfigTable "P4CLIENT" )
                 ( ConfigFileGetValue TheCDSConfigTable "P4CONFIG" )
                   currCV 
                 ( ConfigFileGetValue TheCDSConfigTable "TEMP" ) )
          )
          ( "REVERT"
            ( CDSP4Revert
                 ( ConfigFileGetValue TheCDSConfigTable "P4USER" )
                 ( ConfigFileGetValue TheCDSConfigTable "P4PASSWD" )
                 ( ConfigFileGetValue TheCDSConfigTable "P4CLIENT" )
                 ( ConfigFileGetValue TheCDSConfigTable "P4CONFIG" )
                   currCV 
                 ( ConfigFileGetValue TheCDSConfigTable "TEMP" ) )
          )
        )
      dbClose(currCV)
     )
 close(ipFile)
 t
 )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mergeshp ()
  let( (shp s )
   shp = setof(s wcv()~>shapes car(s~>lpp)=="M1")
   leMergeShapes(shp)
 )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun AutoCreateCable ( Cell @key (Orient "SYNC") )
  (let ( ipFile ipFileName cellName cbl shp cellname currCV  )

   sprintf(ipFileName "%s.cablecells" Cell)    
   ipFile = infile(ipFileName)

   while( gets(cellname ipFile)
      cellname = car(parseString(cellname "\n"))
      currCV = nrOpenCellViewWritableByName(cellname "cable")
      println(currCV~>cellName)

      cbl = car(setof(shp currCV~>shapes shp~>BufferHops))

      nrSyncToNetlist(currCV)
      PlaceSyncCableBuffers(?CableShape cbl ?CV currCV)

      dbSave(currCV)
      dbClose(currCV)
     )

close(ipFile)
t 
 )
)
