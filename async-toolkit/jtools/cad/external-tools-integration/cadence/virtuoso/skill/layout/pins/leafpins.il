/**
 * INTEL TOP SECRET
 * Copyright 2002-2012 Intel Corporation. All Rights Reserved.
 * $Id: //depot/sw/main/cad/external-tools-integration/cadence/virtuoso/skill/layout/pins/leafpins.il#0 $
 */

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
defvar( LeafPinE1of2Q  list(10  2 22) )
defvar( LeafPinE1of2H  list(22  2 46) )
defvar( LeafPinE1of2F  list(22 10 82) )

defvar( LeafPinE1ofNQ  list(10  2  6 18 22  2  6 18 22) )
defvar( LeafPinE1ofNH  list(22  2 14 34 46  2 14 34 46) )
defvar( LeafPinE1ofNF  list(46 4 10 16 22 36 70 72 82 4 10 16 22 36 70 72 82) )

defvar( LeafPinNodeQ  list(10) )
defvar( LeafPinNodeH  list(22) )
defvar( LeafPinNodeF  list(46) )

defvar( LeafPinStrutQ  list(12) )
defvar( LeafPinStrutH  list(26) )
defvar( LeafPinStrutF  list(50) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
defun( DrawLeafPins ( @key (CellVue (UIGetCellView)) (power nil))
 let( (
      ( ExecCmd
        (cond (
          ( and ( boundp `ExecCmd ) ( stringp ExecCmd ) )
            ExecCmd )
            (
             "" ) ) )
       )

  let( (PinFile LineText PinType
        StringList I ScannedPins
        DrawnPins CurrPinList PinOp ScanOP
        CellHeight LeftPinSlots RightPinSlots

        ( PinsFile 
          ( sprintf
            nil 
            "%s/autopins/%s.pins.il"
            ( ConfigFileGetValue TheCDSConfigTable "TEMP" )
            ( getq CellVue cellName ) ) )

        ( PinsCommand
          ( sprintf 
            nil
            "%s %s/cast2skill --cast-path=%s --cell=%s --output-dir=%s --cadence-name --root-only  --max-heap-size=%dM --64"
            ExecCmd
            ( PackageGetBinRoot )
            ( ConfigFileGetValue TheCDSConfigTable "CAST_PATH" )
            ( getq CellVue cellName )
            ( ConfigFileGetValue TheCDSConfigTable "TEMP" )
            ( nrGetMaxHeapSize )
             ) )

         )

    when( GetPrbound(CellVue) == nil
          error("Cellview doesnot contain a PRBoundary !")
        )

    when( power
       (DrawLeafM3Power ?cv CellVue)
       )

    ScannedPins = ScanFromPinTemplate(?CellVue CellVue)

  when(  !ScannedPins
   CellHeight    = cadadr(GetPrbound(CellVue)~>bBox)  
   LeftPinSlots  = LeafPinCreatePGSlotList(CellHeight)
   RightPinSlots = LeafPinCreatePGSlotList(CellHeight)

   printf("%s\n" PinsCommand )              
   shell(PinsCommand )
   println(PinsFile)

   PinFile = infile(PinsFile)

   ScanOP = ScanLeafPins(CellVue)
   DrawnPins = nth(0 ScanOP)

   LeftPinSlots = append(nth(1 ScanOP) LeftPinSlots)
   RightPinSlots = append(nth(2 ScanOP) RightPinSlots)

   while( gets(LineText PinFile) != nil

     StringList = parseString(LineText " ") 

     PinType = nth(1 StringList)

     case( PinType

         ( "PinPlaceLeft" || "PinPlaceLeftRightPin"
           CurrPinList = list(evalstring(nth(2 StringList)))
           PinOp = LeafDrawPins(CurrPinList DrawnPins LeftPinSlots "LeftNode" ?CellVue CellVue)
           LeftPinSlots = nth(0 PinOp)
           DrawnPins = nth(1 PinOp)
          )

         ( "PinPlaceRight" 
           CurrPinList = list(evalstring(nth(2 StringList)))
           PinOp = LeafDrawPins(CurrPinList DrawnPins RightPinSlots "RightNode" ?CellVue CellVue)
           RightPinSlots = nth(0 PinOp)
           DrawnPins = nth(1 PinOp)
          )

         ( "PinPlaceLeft1ofN" || "PinPlaceLeftRight1ofN"
           CurrPinList = CreatePinList(StringList)
           PinOp = LeafDrawPins(CurrPinList DrawnPins LeftPinSlots "Left" ?CellVue CellVue)
           LeftPinSlots = nth(0 PinOp)
           DrawnPins = nth(1 PinOp)
          )

         ( "PinPlaceRight1ofN" 
           CurrPinList = CreatePinList(StringList)
           PinOp = LeafDrawPins(CurrPinList DrawnPins RightPinSlots "Right" ?CellVue CellVue)
           RightPinSlots = nth(0 PinOp)
           DrawnPins = nth(1 PinOp)
          )

         ( "PinPlaceHorizontalStrut" 
           CurrPinList = CreatePinList(StringList)
           PinOp = LeafDrawStruts(CurrPinList DrawnPins LeftPinSlots RightPinSlots "Strut" ?CellVue CellVue)
           RightPinSlots = nth(1 PinOp)
           LeftPinSlots = nth(0 PinOp)
           DrawnPins = nth(2 PinOp)
          )

         ( "PinPlaceInPlace"
           CurrPinList = list(evalstring(nth(2 StringList)))
           println(CurrPinList)
           PinOp = LeafDrawPins(CurrPinList DrawnPins LeftPinSlots "LeftNode" ?CellVue CellVue)
           LeftPinSlots = nth(0 PinOp)
           DrawnPins = nth(1 PinOp)
          )

        )
      )

      close(PinFile)
    )
   t
  )
 )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
defun( CreatePinList (StringList)
 let( ( I Eval
        PinList
       )

     PinList = nil     

     for( I 2 length(StringList)-2
       when( nth(I StringList) != "(" && nth(I StringList) != ")" && nth(I StringList) != "list"
         Eval = evalstring(nth(I StringList))
         when( ! integerp(Eval)
            PinList = cons(Eval PinList)
           )
        )
      )    

  reverse(PinList)
 )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
defun( CreatePinList (StringList)
 let( ( I Eval
        PinList
       )

     PinList = nil     

     for( I 2 length(StringList)-2
       when( nth(I StringList) != "(" && nth(I StringList) != ")" && nth(I StringList) != "list"
         Eval = evalstring(nth(I StringList))
         when( ! integerp(Eval)
            PinList = cons(Eval PinList)
           )
        )
      )    

  reverse(PinList)
 )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
defun( ScanLeafPins (CellVue)
  let( (
        PinNameList LeftPinSlotList RightPinSlotList
        PinShape PinCenter
        )

   foreach( PinShape CellVue~>shapes
      when( PinShape~>pin~>net~>name && PinShape~>pin~>net~>name != "GND" && PinShape~>pin~>net~>name != "Vdd" && car(PinShape~>lpp) == "M3"
          PinNameList = cons(PinShape~>pin~>net~>name PinNameList)
          PinCenter = cadar(PinShape~>bBox) + (cadadr(PinShape~>bBox)-cadar(PinShape~>bBox))/2
        when( caar(PinShape~>bBox) <= caar(GetPrbound(CellVue)~>bBox) + DefaultWiringPitch
          LeftPinSlotList = cons(round((PinCenter - DefaultWiringPitch/2)/DefaultWiringPitch) LeftPinSlotList)
          )
        when( caadr(PinShape~>bBox) >= caadr(GetPrbound(CellVue)~>bBox) - DefaultWiringPitch
          RightPinSlotList = cons(round((PinCenter - DefaultWiringPitch/2)/DefaultWiringPitch) RightPinSlotList)
          )
         )
        )

  list(PinNameList LeftPinSlotList RightPinSlotList)
 )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
defun( ScanToPinTemplate (@key (CellVue (UIGetCellView)))
  let( ( StrNoSub PinTemp I
         TemplateCellVue
         PinShape PinCenter PinDrawn LibName CellName
        )

    LibName  = CellVue~>libName
    CellName = CellVue~>cellName

    StrNoSub = parseString(CellName ".")
    PinTemp = nth(0 StrNoSub)

    for( I 1 length(StrNoSub)-1
     when( I < length(StrNoSub)-1
      PinTemp = strcat(PinTemp "." nth(I StrNoSub))
        )
     when( I == length(StrNoSub)-1
      PinTemp = strcat(PinTemp ".pintemplate")
       )
      )

    TemplateCellVue = nrOpenCellViewWritableByName(PinTemp "layout")
    when( GetPrbound(TemplateCellVue)
        dbDeleteObject(GetPrbound(TemplateCellVue))
     )
    foreach( PinShape TemplateCellVue~>shapes
     when( PinShape
       dbDeleteObject(PinShape)
      )
    )

    if( TemplateCellVue
     then
       if( GetPrbound(CellVue)~>points != nil
        then
          dbCreatePRBoundary(TemplateCellVue GetPrbound(CellVue)~>points)
        else 
          error("Unable to find PR Boundary !")
         )
      foreach( PinShape CellVue~>shapes
        when( PinShape~>pin~>net~>name && PinShape~>pin~>net~>name != "GND" && PinShape~>pin~>net~>name != "Vdd" && car(PinShape~>lpp) == "M3"
            PinCenter = cadar(PinShape~>bBox) + (cadadr(PinShape~>bBox)-cadar(PinShape~>bBox))/2
            PinSlot = round((PinCenter - DefaultWiringPitch/2)/DefaultWiringPitch)
          when( caar(PinShape~>bBox) <= caar(GetPrbound(CellVue)~>bBox) + DefaultWiringPitch && caadr(PinShape~>bBox) >= caadr(GetPrbound(CellVue)~>bBox) - DefaultWiringPitch && !PinDrawn
               LeafPinPlaceHorizontalStrut(PinShape~>pin~>net~>name PinSlot ?CellVue TemplateCellVue)
               PinDrawn = t
            )
          when( caar(PinShape~>bBox) <= caar(GetPrbound(CellVue)~>bBox) + DefaultWiringPitch && !PinDrawn
               LeafPinPlaceLeft(PinShape~>pin~>net~>name PinSlot ?CellVue TemplateCellVue)
               PinDrawn = t
            )
          when( caadr(PinShape~>bBox) >= caadr(GetPrbound(CellVue)~>bBox) - DefaultWiringPitch && !PinDrawn
               LeafPinPlaceRight(PinShape~>pin~>net~>name PinSlot ?CellVue TemplateCellVue)
               PinDrawn = t
            )
          PinDrawn = nil
           )
          )
      else
       println("Pin Template %s is locked !" PinTemp)
       )

     dbSave(TemplateCellVue)
     dbPurge(TemplateCellVue)

     printf("Scanned Pins to \"%s\"\n" PinTemp)

 )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
defun( ScanFromPinTemplate (@key (CellVue (UIGetCellView)))
 let( ( StrNoSub PinTemp I
        TemplateCellVue ScannedPins
        PinShape PinCenter PinDrawn LibName CellName
        )

    (DeletePinsByType "LeafCell")

    LibName = CellVue~>libName
    CellName = CellVue~>cellName
    ScannedPins = nil
    StrNoSub = parseString(CellName ".")
    PinTemp = nth(0 StrNoSub)

    for( I 1 length(StrNoSub)-1
     when( I < length(StrNoSub)-1
      PinTemp = strcat(PinTemp "." nth(I StrNoSub))
        )
     when( I == length(StrNoSub)-1
      PinTemp = strcat(PinTemp ".pintemplate")
       )
      )

    TemplateCellVue = nrOpenCellViewReadable(LibName PinTemp "layout")

    foreach( PinShape TemplateCellVue~>shapes
      when( PinShape~>pin~>net~>name && PinShape~>pin~>net~>name != "GND" && PinShape~>pin~>net~>name != "Vdd" && car(PinShape~>lpp) == "M3"
          PinCenter = cadar(PinShape~>bBox) + (cadadr(PinShape~>bBox)-cadar(PinShape~>bBox))/2
          PinSlot = round((PinCenter - DefaultWiringPitch/2)/DefaultWiringPitch)
        when( caar(PinShape~>bBox) <= caar(GetPrbound(TemplateCellVue)~>bBox) + DefaultWiringPitch && caadr(PinShape~>bBox) >= caadr(GetPrbound(TemplateCellVue)~>bBox) - DefaultWiringPitch && !PinDrawn
             LeafPinPlaceHorizontalStrut(PinShape~>pin~>net~>name PinSlot ?CellVue CellVue)
             PinDrawn = t
          )
        when( caar(PinShape~>bBox) <= caar(GetPrbound(TemplateCellVue)~>bBox) + DefaultWiringPitch && !PinDrawn
             LeafPinPlaceLeft(PinShape~>pin~>net~>name PinSlot ?CellVue CellVue)
             PinDrawn = t
          )
        when( caadr(PinShape~>bBox) >= caadr(GetPrbound(TemplateCellVue)~>bBox) - DefaultWiringPitch && !PinDrawn
             LeafPinPlaceRight(PinShape~>pin~>net~>name PinSlot ?CellVue CellVue)
             PinDrawn = t
          )
        PinDrawn = nil
         )
        ScannedPins = t
     printf("Scanned Pins from %s\n" PinTemp)
        )

     ScannedPins
 )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
defun( LeafDrawPins (PinList DrawnPins PinSlots Edge @key (CellVue (UIGetCellView)))
 let( ( 
         TrackScope CellHeight
         PinSlot PinHop
         NewPinSlots
         NewDrawnPins
         OutPutList
         PinSeed PinSeedStr
       )

   NewPinSlots = PinSlots
   NewDrawnPins = DrawnPins
   PinHop = 2

   CellHeight = cadadr(GetPrbound(CellVue)~>bBox)  

   when( power
     (DelLeafM3Power ?cv CellVue )
     (DrawLeafM3Power ?cv CellVue)
    )

  case( round(CellHeight*1000)
    ( 3120
     sprintf(PinSeedStr "Q")
     )
    ( 6240
     sprintf(PinSeedStr "H")
     )
    ( 12480
     sprintf(PinSeedStr "F")
     )
    ( t
     sprintf(PinSeedStr "H")
     )
    )

   when( Edge == "Left" || Edge == "Right"
      if( length(PinList) <= 3
       then
        PinSeedStr = strcat("LeafPinE1of2" PinSeedStr)
       else
        PinSeedStr = strcat("LeafPinE1ofN" PinSeedStr)
       )
     )

   when( Edge == "LeftNode" || Edge == "RightNode" 
        PinSeedStr = strcat("LeafPinNode" PinSeedStr)
     )

  PinSeed = evalstring(PinSeedStr)

   for( I 0 length(PinList)-1
       when( member(nth(I PinList) DrawnPins) == nil
        case( Edge 
          ( "Left" || "Right"
           TrackScope = list(round(nth(I PinSeed)/12)*12 ((round(nth(I PinSeed)/12)+1)*12)-1)
          )
          ( "LeftNode" || "RightNode"
           TrackScope = list(((round(nth(I PinSeed)/12)-1)*12)+1 ((round(nth(I PinSeed)/12)+1)*12)-1)
          )
           )
          PinSlot = LeafPinFindPinSlot(NewPinSlots nth(I PinSeed) TrackScope PinHop) 
      when( PinSlot != nil
          NewPinSlots = cons(PinSlot NewPinSlots)
          NewDrawnPins = cons(nth(I PinList) NewDrawnPins)
       case( Edge
           ( "Left" || "LeftNode" 
             LeafPinPlaceLeft(nth(I PinList) PinSlot  ?CellVue CellVue)
            )
           ( "Right" || "RightNode" 
             LeafPinPlaceRight(nth(I PinList) PinSlot ?CellVue CellVue )
            )
           )
          )
       when( PinSlot == nil
           printf("*ERROR* Unable to place \"%s\" pin" nth(I PinList))
          )
         )
       )       

  list(NewPinSlots NewDrawnPins)
 )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
defun( LeafDrawStruts (PinList DrawnPins LeftPinSlots RightPinSlots Edge @key (CellVue (UIGetCellView)))
 let( ( 
         TrackScope CellHeight
         PinSlot PinHop
         NewPinSlots NewLeftPinSlots NewRightPinSlots
         NewDrawnPins
         OutPutList
         PinSeed PinSeedStr
       )

   NewPinSlots = append(LeftPinSlots RightPinSlots)
   NewLeftPinSlots = LeftPinSlots
   NewRightPinSlots = RightPinSlots

   NewDrawnPins = DrawnPins
   PinHop = 2

   CellHeight = cadadr(GetPrbound(CellVue)~>bBox)  

  case( round(CellHeight*1000)
    ( 3120
     sprintf(PinSeedStr "Q")
     )
    ( 6240
     sprintf(PinSeedStr "H")
     )
    ( 12480
     sprintf(PinSeedStr "F")
     )
    ( t
     sprintf(PinSeedStr "H")
     )
    )


  PinSeedStr = strcat("LeafPinStrut" PinSeedStr)
  PinSeed = evalstring(PinSeedStr)

   for( I 0 length(PinList)-1
       when( member(nth(I PinList) DrawnPins) == nil

          TrackScope = list(((round(nth(I PinSeed)/12)-1)*12)+1 ((round(nth(I PinSeed)/12)+1)*12)-1)
          PinSlot = LeafPinFindPinSlot(NewPinSlots nth(I PinSeed) TrackScope PinHop)  

      when( PinSlot != nil
          NewPinSlots = cons(PinSlot NewPinSlots)
          NewLeftPinSlots = cons(PinSlot NewLeftPinSlots)
          NewRightPinSlots = cons(PinSlot NewRightPinSlots)
          NewDrawnPins = cons(nth(I PinList) NewDrawnPins)
          LeafPinPlaceHorizontalStrut(nth(I PinList) PinSlot ?CellVue CellVue)
          )
      when( PinSlot == nil
           printf("*ERROR* Unable to place \"%s\" pin" nth(I PinList))
          )

         )
       )       

  list(NewLeftPinSlots NewRightPinSlots NewDrawnPins)
 ) 
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
defun( LeafPinPlace (CurrPinList ExclPinList PinList PinSlots PinSeed PinHop)
  let( (
         TrackScope
         PinSlot
         NewPinSlots
         NewPinList
         OutPutList
        )
 
   NewPinList = CurrPinList

   for( I 0 length(PinList)-1

      TrackScope = list(round(nth(I PinSeed)/12)*12 ((round(nth(I PinSeed)/12)+1)*12)-1)

      PinSlot = LeafPinFindPinSlot(PinSlots nth(I PinSeed) TrackScope PinHop) 

     when( !member(nth(I PinList) ExclPinList) && !member(nth(I PinList) CurrPinList)
            LeafPinPlaceLeft(nth(I PinList) PinSlot ?CellVue CellVue)
            NewPinSlots = cons(PinSlot NewPinSlots)
            NewPinList = cons(nth(I PinList) NewPinSlots)
      )
    )    

  OutPutList = list(NewPinList NewPinSlots)
 )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
defun( LeafPinFindPinSlot (PinSlotList SuggTrack TrackScope TrackHop)
 let( ( I 
       AvailTracks
       UpperBound
       LowerBound
       PinSlot
       PinSlotFound
       AboveCurrTrack
       BelowCurrTrack
       TempHop SearchCount
      )
    UpperBound = cadr(TrackScope)
    LowerBound =  car(TrackScope)

    TempHop = TrackHop
    SearchCount = 0
;;; Searches with given TrackHop. 
;;; Decrements 2 tracks in the next search but ends with TrackHop = 1 
;;; (eventually tries to insert in nearest available odd track)
 
  
  while( !PinSlotFound && TempHop != 0 && SearchCount < 24

     AvailTracks = round((UpperBound - LowerBound + 1)/TempHop)

     for( I 0 AvailTracks

       BelowCurrTrack = SuggTrack - I*TempHop
       AboveCurrTrack = SuggTrack + I*TempHop

       when( !PinSlotFound && greaterp(BelowCurrTrack LowerBound) && !member(BelowCurrTrack PinSlotList)
              PinSlot = BelowCurrTrack
              PinSlotFound = t
           )

       when( !PinSlotFound && lessp(AboveCurrTrack UpperBound) && !member(AboveCurrTrack PinSlotList)
              PinSlot = AboveCurrTrack
              PinSlotFound = t
           )
         )

    if( evenp(TempHop)
      then
        if( TempHop == 2
           then
             TempHop = 1
           else
             TempHop = TempHop - 2
          )
       else
         TemnpHop = 0
       )
     SearchCount = SearchCount + 1
   )
 PinSlot
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
defun( LeafPinCreatePGSlotList (CellHeight)
   let( ( I
          PGSlotList
          PGTracks
         )
  PGTracks = round(CellHeight/1.56)
  for( I 0 PGTracks-1
       PGSlotList = cons(I*12 PGSlotList)
       PGSlotList = cons((12*(I+1))-1 PGSlotList)
    )
  PGSlotList
 )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
defun( LeafPinPlaceHorizontalStrut (PinName PitchIndex @key (CellVue (UIGetCellView)))
  let( ( 
         CellLeftEdge
         CellRightEdge
         CellBottomEdge
         PinRect
         PinFig
         PinNet
         PinPitchY
       )

   CellLeftEdge   =  caar(GetPrbound(CellVue)~>bBox)
   CellRightEdge  = caadr(GetPrbound(CellVue)~>bBox)
   CellBottomEdge = cadar(GetPrbound(CellVue)~>bBox)
   PinPitchY = (PitchIndex+0.5)*DefaultWirePitch*MicronsPerMeter
   PinRect = list( CellLeftEdge+LineEndSpace/2:PinPitchY-DefaultWireWidth*MicronsPerMeter/2 CellRightEdge-LineEndSpace/2:PinPitchY+DefaultWireWidth*MicronsPerMeter/2 )
   PinFig = dbCreateRect(CellVue PinLPP PinRect)
   CoverPinLayerWithDrawing(CellVue PinFig)
   LeafPinsCreatePin(CellVue PinFig PinName)

   PinFig
 )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
defun( LeafPinPlaceLeft (PinName PitchIndex @key (CellVue (UIGetCellView)))
  let( ( 
         CellLeftEdge
         CellRightEdge
         CellBottomEdge
         PinRect
         PinFig
         PinNet
         PinPitchY
         LeafPinLength
       )

   CellLeftEdge   =  caar(GetPrbound(CellVue)~>bBox)
   CellRightEdge  = caadr(GetPrbound(CellVue)~>bBox)
   CellBottomEdge = cadar(GetPrbound(CellVue)~>bBox)

   if( (CellRightEdge - CellLeftEdge) >= 2.56
    then
      LeafPinLength = PinLength
    else
      LeafPinLength = ((CellRightEdge - CellLeftEdge)-0.16)/(2*MicronsPerMeter)
   )

   PinPitchY = (PitchIndex+0.5)*DefaultWirePitch*MicronsPerMeter
   PinRect = list( CellLeftEdge+LineEndSpace/2:PinPitchY-DefaultWireWidth*MicronsPerMeter/2 CellLeftEdge+LeafPinLength*MicronsPerMeter:PinPitchY+DefaultWireWidth*MicronsPerMeter/2 )
   PinFig = dbCreateRect(CellVue PinLPP PinRect)
   CoverPinLayerWithDrawing(CellVue PinFig)
   LeafPinsCreatePin(CellVue PinFig PinName)

   PinFig
 )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
defun( LeafPinPlaceRight (PinName PitchIndex @key (CellVue (UIGetCellView)))
  let( ( 
         CellLeftEdge
         CellRightEdge
         CellBottomEdge
         PinRect
         PinFig
         PinNet
         PinPitchY LeafPinLength
       )

   CellLeftEdge   =  caar(GetPrbound(CellVue)~>bBox)
   CellRightEdge  = caadr(GetPrbound(CellVue)~>bBox)
   CellBottomEdge = cadar(GetPrbound(CellVue)~>bBox)

   if( (CellRightEdge - CellLeftEdge) >= 2.56
    then
      LeafPinLength = PinLength
    else
      LeafPinLength = ((CellRightEdge - CellLeftEdge)-0.16)/(2*MicronsPerMeter)
   )

   PinPitchY = (PitchIndex+0.5)*DefaultWirePitch*MicronsPerMeter
   PinRect = list( CellRightEdge-LeafPinLength*MicronsPerMeter:PinPitchY-DefaultWireWidth*MicronsPerMeter/2 CellRightEdge-LineEndSpace/2:PinPitchY+DefaultWireWidth*MicronsPerMeter/2 )
   PinFig = dbCreateRect(CellVue PinLPP PinRect)
   CoverPinLayerWithDrawing(CellVue PinFig)
   LeafPinsCreatePin(CellVue PinFig PinName)

   PinFig
 )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
defun( LeafPinPlaceLeft1ofN (EnableNet DataRailNets EnablePitchIndex DataPinPitch @key (CellVue (UIGetCellView)))
  let( ( 
         I
         CellLeftEdge
         CellRightEdge
         CellBottomEdge
         PinRect
         PinFig
         PinNet
         N
         PinPitchY
         DataPinBaseIndex
         OutPins
       )
  
   CellLeftEdge   =  caar(GetPrbound(CellVue)~>bBox)
   CellRightEdge  = caadr(GetPrbound(CellVue)~>bBox)
   CellBottomEdge = cadar(GetPrbound(CellVue)~>bBox)
   N = length(DataRailNets)
   DataPinBaseIndex = EnablePitchIndex - round(N/2.0)*DataPinPitch

 for( I 0 N-1
     if( I < round(N/2.0)
         then
            PinPitchY = (DataPinBaseIndex+I*DataPinPitch+0.5)*DefaultWirePitch*MicronsPerMeter
         else
            PinPitchY = (DataPinBaseIndex+(I+1)*DataPinPitch+0.5)*DefaultWirePitch*MicronsPerMeter
        )
     PinRect = list( CellLeftEdge+LineEndSpace/2:PinPitchY-DefaultWireWidth*MicronsPerMeter/2 CellLeftEdge+PinLength*MicronsPerMeter:PinPitchY+DefaultWireWidth*MicronsPerMeter/2 )
     PinFig = dbCreateRect(CellVue PinLPP PinRect)
     CoverPinLayerWithDrawing(CellVue PinFig)
     LeafPinsCreatePin(CellVue PinFig nth(I DataRailNets))
     OutPins = cons(PinFig OutPins)
    )

    PinPitchY = (EnablePitchIndex+0.5)*DefaultWirePitch*MicronsPerMeter
    PinRect = list( CellLeftEdge+LineEndSpace/2:PinPitchY-DefaultWireWidth*MicronsPerMeter/2 CellLeftEdge+PinLength*MicronsPerMeter:PinPitchY+DefaultWireWidth*MicronsPerMeter/2 )
    PinFig = dbCreateRect(CellVue PinLPP PinRect)
    CoverPinLayerWithDrawing(CellVue PinFig)
    LeafPinsCreatePin(CellVue PinFig EnableNet)
    OutPins = cons(PinFig OutPins)

    OutPins
 )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
defun( LeafPinPlaceRight1ofN (EnableNet DataRailNets EnablePitchIndex DataPinPitch @key (CellVue (UIGetCellView)))
  let( ( 
         I
         CellLeftEdge
         CellRightEdge
         CellBottomEdge
         PinRect
         PinFig
         PinNet
         N
         PinPitchY
         DataPinBaseIndex
         OutPins
       )

   CellLeftEdge   =  caar(GetPrbound(CellVue)~>bBox)
   CellRightEdge  = caadr(GetPrbound(CellVue)~>bBox)
   CellBottomEdge = cadar(GetPrbound(CellVue)~>bBox)
   N = length(DataRailNets)
   DataPinBaseIndex = EnablePitchIndex - round(N/2.0)*DataPinPitch

 for( I 0 N-1
     if( I < round(N/2.0)
         then
            PinPitchY = (DataPinBaseIndex+I*DataPinPitch+0.5)*DefaultWirePitch*MicronsPerMeter
         else
            PinPitchY = (DataPinBaseIndex+(I+1)*DataPinPitch+0.5)*DefaultWirePitch*MicronsPerMeter
        )
     PinRect = list( CellRightEdge-PinLength*MicronsPerMeter:PinPitchY-DefaultWireWidth*MicronsPerMeter/2 CellRightEdge-LineEndSpace/2:PinPitchY+DefaultWireWidth*MicronsPerMeter/2 )
     PinFig = dbCreateRect(CellVue PinLPP PinRect)
     CoverPinLayerWithDrawing(CellVue PinFig)
     LeafPinsCreatePin(CellVue PinFig nth(I DataRailNets))
     OutPins = cons(PinFig OutPins)
    )

    PinPitchY = (EnablePitchIndex+0.5)*DefaultWirePitch*MicronsPerMeter
    PinRect = list( CellRightEdge-PinLength*MicronsPerMeter:PinPitchY-DefaultWireWidth*MicronsPerMeter/2 CellRightEdge-LineEndSpace/2:PinPitchY+DefaultWireWidth*MicronsPerMeter/2 )
    PinFig = dbCreateRect(CellVue PinLPP PinRect)
    CoverPinLayerWithDrawing(CellVue PinFig)
    LeafPinsCreatePin(CellVue PinFig EnableNet)
    OutPins = cons(PinFig OutPins)

    OutPins
 )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
defun( LeafPinPlaceLeftRight1ofN (EnableNet DataRailNets EnablePitchIndex DataPinPitch @key (CellVue (UIGetCellView)))
  let( ( 
         I
         CellLeftEdge
         CellRightEdge
         CellBottomEdge
         PinRect
         PinFig
         PinNet
         N
         PinPitchY
         DataPinBaseIndex
         OutPins
       )

   CellLeftEdge   =  caar(GetPrbound(CellVue)~>bBox)
   CellRightEdge  = caadr(GetPrbound(CellVue)~>bBox)
   CellBottomEdge = cadar(GetPrbound(CellVue)~>bBox)
   N = length(DataRailNets)
   DataPinBaseIndex = EnablePitchIndex - round(N/2.0)*DataPinPitch

 for( I 0 N-1
     if( I < round(N/2.0)
         then
            PinPitchY = (DataPinBaseIndex+I*DataPinPitch+0.5)*DefaultWirePitch*MicronsPerMeter
         else
            PinPitchY = (DataPinBaseIndex+(I+1)*DataPinPitch+0.5)*DefaultWirePitch*MicronsPerMeter
        )
     PinRect = list( CellLeftEdge+LineEndSpace/2:PinPitchY-DefaultWireWidth*MicronsPerMeter/2 CellLeftEdge+PinLength*MicronsPerMeter:PinPitchY+DefaultWireWidth*MicronsPerMeter/2 )
     PinFig = dbCreateRect(CellVue PinLPP PinRect)
     CoverPinLayerWithDrawing(CellVue PinFig)
     LeafPinsCreatePin(CellVue PinFig nth(I DataRailNets))
     OutPins = cons(PinFig OutPins)
     printf("%s is a bi-directional pin and is being placed on the left edge" nth(I DataRailNets))
    )

    PinPitchY = (EnablePitchIndex+0.5)*DefaultWirePitch*MicronsPerMeter
    PinRect = list( CellLeftEdge+LineEndSpace/2:PinPitchY-DefaultWireWidth*MicronsPerMeter/2 CellLeftEdge+PinLength*MicronsPerMeter:PinPitchY+DefaultWireWidth*MicronsPerMeter/2 )
    PinFig = dbCreateRect(CellVue PinLPP PinRect)
    CoverPinLayerWithDrawing(CellVue PinFig)
    LeafPinsCreatePin(CellVue PinFig EnableNet)
    OutPins = cons(PinFig OutPins)
    printf("%s is a bi-directional pin and is being placed on the left edge" EnableNet)
    

    OutPins
 )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
defun( LeafPinPlaceLeftRight (PinName PitchIndex @key (CellVue (UIGetCellView)))
  let( ( 
         CellLeftEdge
         CellRightEdge
         CellBottomEdge
         PinRect
         PinFig
         PinNet
         PinPitchY
       )

   CellLeftEdge   =  caar(GetPrbound(CellVue)~>bBox)
   CellRightEdge  = caadr(GetPrbound(CellVue)~>bBox)
   CellBottomEdge = cadar(GetPrbound(CellVue)~>bBox)
   PinPitchY = (PitchIndex+0.5)*DefaultWirePitch*MicronsPerMeter
   PinRect = list( CellLeftEdge+LineEndSpace/2:PinPitchY-DefaultWireWidth*MicronsPerMeter/2 CellLeftEdge+PinLength*MicronsPerMeter:PinPitchY+DefaultWireWidth*MicronsPerMeter/2 )
   PinFig = dbCreateRect(CellVue PinLPP PinRect)
   CoverPinLayerWithDrawing(CellVue PinFig)
   LeafPinsCreatePin(CellVue PinFig PinName)
   printf("\"%s\" is a bi-directional pin and is being placed on the left edge" PinName)

   PinFig
 )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
defun( LeafPinsCreatePin (CellVue PinShape PinName)
  let( ( PinNet Pin PinLabel )
     PinNet = dbMakeNet(CellVue PinName)
     Pin = dbCreatePin(PinNet PinShape)
     dbSetq( Pin ( list "top" "bottom" "left" "right" ) accessDir )
     dbSet(PinShape "LeafCell" "PinType")
     PinLabel = MidPinsCreateLabel(CellVue PinShape)
     PinLabel~>parent = PinShape
  )
t
)
