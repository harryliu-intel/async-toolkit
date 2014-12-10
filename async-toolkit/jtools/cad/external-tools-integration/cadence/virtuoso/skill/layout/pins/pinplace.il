; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

; Function: PinPlace
; Parameter: CellView (CVId)
;            PrePlacement (boolean)
; Return: CellView (CVId)
;
; Place pin for the placer to run.
;
;
;

defun( LockAllPins ( CellView )
; this is to lock leaf pins so that placer can not move them.
  let((termianl pin)
    foreach( terminal CellView~>terminals
      foreach( pin terminal~>pins
        pin~>status="locked"
      )
    )
  )
)

defun( DeleteAllPins ( CellView )
  let((termianl pin)
    foreach( terminal CellView~>terminals
      foreach( pin terminal~>pins
        if( pin~>fig then dbDeleteObject( pin~>fig ))
        if( pin then dbDeleteObject( pin ))
      )
    )
  )
)

defun( UpdatePrBoundary (CellView bBox)
  let((x0 y0 x1 y1)
    x0=leftEdge(bBox)
    y0=bottomEdge(bBox)
    x1=rightEdge(bBox)
    y1=topEdge(bBox)
    if( CellView~>prBoundary dbDeleteObject(CellView~>prBoundary))
    dbCreatePRBoundary(CellView list(x0:y0 x1:y0 x1:y1 x0:y1))  
  )
)

defun( AddPinsToCellView ( CellView PrePlacement 
                           @key (power nil)
                         )
  TargetViewName = CellView ~> viewName

  (let (      
        ( Boundary ( PinUtilFindPRBoundShape BoundaryLPP CellView ) )
        ( HorizontalLPP Metal3LPP )
        ( PinLPP ( CellInfoGetPinLayer DirectiveTable ) )
        )
   println("here0")
  (when PrePlacement 
    ;tweak boundary so as not to mess with placement
    ( dbSetq Boundary 
             ( BBoxExpandHorizontal
               ( PlacementRegionsGetCellRegionDataBBox
                 CellRegionData )
               ( times UserUnitsPerMeter WirePitch ) )
             bBox ) 
  
  )
   UpdatePrBoundary(CellView Boundary->bBox)
   DeleteAllPins( CellView )
   DrawLeafPins( ?CellVue CellView ?power power)
   LockAllPins( CellView )
   dbSave( CellView )

  CellView
  )
)

(defun PinPlace (NetName 
		 BBox
		 @key ( delete ( not PinPlaceCreateRectInfo ) )
                 ( LPP LPP )
                 )
  ( AutoPinQueuePin
    PinPlacePinTable 
    NetName
    BBox
    LPP
    ?CreateRectInfo ( not delete) 
    ?InPlace nil
    )
  )

                                        ; doesnt handle instances !
(defun PinPlaceMetaString ( OutFile
			    PinInfo )

  ( foreach RectInfo ( AutoPinGetRectInfoListFromPinInfo PinInfo )
            ( let (
                   ( Rect ( AutoPinGetRectFromRectInfo RectInfo ) )
                   ( LPP  ( AutoPinGetLPPFromRectInfo RectInfo ) )   
                   )
              (if ( not ( equal ( AutoPinGetObjTypeFromRectInfo RectInfo ) "inst" ) )
                  ( fprintf OutFile
                            "( PinPlace \"%s\" ( list %f:%f %f:%f )\n
                   ?LPP ( list \"%s\" \"%s\" ) )\n" 
                            ( AutoPinGetNetNameFromPinInfo PinInfo )
                            ( caar Rect )
                            ( cadar Rect )
                            ( caadr Rect )
                            ( cadadr Rect )
                            ( car LPP )
                            ( cadr LPP ) ) ) ) ) )

(defun PinPlaceLeft ( NetName 
		      PitchIndex
		      @key ( delete ( not PinPlaceCreateRectInfo ) )		         
                      ( LPP PinLPP )
                      ( PinLength PinLength )		   
                      ( HOffset BoundaryPinHorizontalSpacing )
                      ( VOffset   BoundaryPinVerticalSpacing )
                      @rest Rest )
  (let (
        ( WirePitch ( times UserUnitsPerMeter DefaultWirePitch ) )
        ( PinWidth (if ( car Rest )
                       ( times UserUnitsPerMeter ( car Rest ) )
                     ( times UserUnitsPerMeter DefaultWireWidth ) ) )
        ( WireSpacing (if ( cadr Rest )
                          ( times UserUnitsPerMeter ( cadr Rest ) )
                        ( times UserUnitsPerMeter DefaultWireSpacing ) ) ) )
 
    ( AutoPinQueueLeftPin 
      PinPlacePinTable
      PitchIndex
      NetName
      LPP
      PinWidth
      PinLength
      WireSpacing
      WirePitch
      BoundaryPolygonEdges
      HOffset
      VOffset   
      ManufacturingGrid
      FunctionCacheTable      
      ?CreateRectInfo ( not delete )
      )	) )

(defun PinPlaceRight ( NetName 
		       PitchIndex
		       @key ( delete ( not PinPlaceCreateRectInfo ) )
                       ( LPP PinLPP )
                       ( PinLength PinLength )
                       ( HOffset BoundaryPinHorizontalSpacing )
                       ( VOffset   BoundaryPinVerticalSpacing )
                       @rest Rest )

  (let (
        ( WirePitch ( times UserUnitsPerMeter DefaultWirePitch ) )
        ( PinWidth (if ( car Rest )
                       ( times UserUnitsPerMeter ( car Rest ) )
                     ( times UserUnitsPerMeter DefaultWireWidth ) ) )
        ( WireSpacing (if ( cadr Rest )
                          ( times UserUnitsPerMeter ( cadr Rest ) )
                        ( times UserUnitsPerMeter DefaultWireSpacing ) ) ) )
  
    ( AutoPinQueueRightPin 
      PinPlacePinTable
      PitchIndex
      NetName
      LPP
      PinWidth
      PinLength
      WireSpacing
      WirePitch
      BoundaryPolygonEdges
      HOffset
      VOffset   
      ManufacturingGrid
      FunctionCacheTable
      ?CreateRectInfo ( not delete )
      ) ) )

(defun PinPlacePowerGrid (NetName 			 
			  GridOffset 
			  GridSpacing 
			  @key ( delete ( not PinPlaceCreateRectInfo ) )
                          ( HLPP HorizontalLPP )
                          ( PinLength PinLength )
                          ( HOffset 0.0 )
                          ( VOffset BoundaryPinVerticalSpacing )
                          @rest Rest
			  ) 
  (let (
        ( WirePitch ( times UserUnitsPerMeter DefaultWirePitch ) )
        ( PinWidth (if ( car Rest )
                       ( times UserUnitsPerMeter ( car Rest ) )
                     ( times UserUnitsPerMeter DefaultWireWidth ) ) )
        ( WireSpacing (if ( cadr Rest )
                          ( times UserUnitsPerMeter ( cadr Rest ) )
                        ( times UserUnitsPerMeter DefaultWireSpacing ) ) ) ) 

    (cond (
           ( equal `inplace PowerGrid )
           ( PinPlaceInPlace 
             NetName
             ?delete delete
             ?LPP HLPP
             ?MakePin t ) )
          (
           t
           ( AutoPinQueuePowerGrid 
             PinPlacePinTable		       
             NetName
             GridOffset
             GridSpacing
             HLPP
             PinWidth
             PinLength
             WireSpacing
             WirePitch
             BoundaryPolygonEdges
             HOffset
             VOffset   
             ManufacturingGrid
             FunctionCacheTable
             ReservedPitchTable
             ?CreateRectInfo ( and ( not delete ) PowerGrid )
             ) ) ) ) )

(defun PinPlaceInPlace ( NetName 
			 @key ( delete ( not PinPlaceCreateRectInfo ) )
                         ( LPP nil )			    
                         ( MakePin t )
                         )
  ( AutoPinQueueInPlacePin
    PinPlacePinTable
    NetName
    LPP
    BoundaryPolygonEdges
    ?CreateRectInfo ( and ( not delete ) InPlace )
    ?MakePin MakePin
    )
  )


(defun PinPlaceParseParams ( Param
                             Default
                             UserUnitsPerMeter
                             N )
  ( mapcar
    (lambda ( Spacing )
      ( times UserUnitsPerMeter Spacing ) )
    (cond (
           ( and Param ( listp Param ) )
           Param )
          (
           ( ListFillList 
             (lambda ( N ) (cond ( Param ) ( Default ) ) )
             N ) ) ) ) )

(defun PinPlaceLeft1ofN ( EnableNetName 
			  DataRailNets 
			  EnablePinPitchIndex
			  PitchesBetweenPins 
			  @key ( delete ( not PinPlaceCreateRectInfo ) )
                          ( PinOffsets nil )
                          ( LPP PinLPP )
                          ( PinLength PinLength )
                          ( HOffset BoundaryPinHorizontalSpacing )
                          ( VOffset BoundaryPinVerticalSpacing )
                          @rest Rest
                          ) 
  (let (
        ( WirePitch ( times UserUnitsPerMeter DefaultWirePitch ) )
        ( PinWidths
          ( PinPlaceParseParams
            ( car Rest )
            DefaultWireWidth
            UserUnitsPerMeter
            ( plus 1 ( length DataRailNets ) ) ) )
        ( WireSpacings
          ( PinPlaceParseParams
            ( cadr Rest )
            DefaultWireSpacing
            UserUnitsPerMeter
            ( plus 1 ( length DataRailNets ) ) ) )
        )

    ( E1ofNQueueLeftE1ofNPins 
      PinPlacePinTable
      EnableNetName
      DataRailNets
      EnablePinPitchIndex
      PitchesBetweenPins
      PinOffsets
      LPP
      PinWidths
      PinLength
      WireSpacings
      WirePitch
      BoundaryPolygonEdges
      HOffset
      VOffset   
      ManufacturingGrid
      FunctionCacheTable
      ReservedPitchTable
      ?CreateRectInfo ( not delete )
      ) ) )

(defun PinPlaceRight1ofN (EnableNetName 
			  DataRailNets 
			  EnablePinPitchIndex
			  PitchesBetweenPins 
			  @key ( delete ( not PinPlaceCreateRectInfo ) )
                          ( LPP PinLPP )
                          ( PinOffsets nil )
                          ( PinLength PinLength )
                          ( HOffset BoundaryPinHorizontalSpacing )
                          ( VOffset BoundaryPinVerticalSpacing )
                          @rest Rest
                          )
  (let (
        ( WirePitch ( times UserUnitsPerMeter DefaultWirePitch ) )
        ( PinWidths
          ( PinPlaceParseParams
            ( car Rest )
            DefaultWireWidth
            UserUnitsPerMeter
            ( plus 1 ( length DataRailNets ) ) ) )
        ( WireSpacings
          ( PinPlaceParseParams
            ( cadr Rest )
            DefaultWireSpacing
            UserUnitsPerMeter
            ( plus 1 ( length DataRailNets ) ) ) )
        )

    ( E1ofNQueueRightE1ofNPins 
      PinPlacePinTable
      EnableNetName
      DataRailNets
      EnablePinPitchIndex
      PitchesBetweenPins
      PinOffsets
      LPP
      PinWidths
      PinLength
      WireSpacings
      WirePitch
      BoundaryPolygonEdges
      HOffset
      VOffset   
      ManufacturingGrid
      FunctionCacheTable
      ReservedPitchTable
      ?CreateRectInfo ( not delete )
      ) ) )

(defun PinPlaceLeftRight1ofN ( EnableNetName 
                               DataRailNets 
                               EnablePinPitchIndex
                               PitchesBetweenPins 
                               @key ( delete ( not PinPlaceCreateRectInfo ) )
                               ( PinOffsets nil )
                               ( LPP PinLPP )
                               ( PinLength PinLength )
                               ( HOffset BoundaryPinHorizontalSpacing )
                               ( VOffset BoundaryPinVerticalSpacing )
                               @rest Rest
                             ) 
  (let (
        ( WirePitch ( times UserUnitsPerMeter DefaultWirePitch ) )
        ( PinWidths
          ( PinPlaceParseParams
            ( car Rest )
            DefaultWireWidth
            UserUnitsPerMeter
            ( plus 1 ( length DataRailNets ) ) ) )
        ( WireSpacings
          ( PinPlaceParseParams
            ( cadr Rest )
            DefaultWireSpacing
            UserUnitsPerMeter
            ( plus 1 ( length DataRailNets ) ) ) )
        )

    ( E1ofNQueueLeftRightE1ofNPins 
      PinPlacePinTable
      EnableNetName
      DataRailNets
      EnablePinPitchIndex
      PitchesBetweenPins
      PinOffsets
      LPP
      PinWidths
      PinLength
      WireSpacings
      WirePitch
      BoundaryPolygonEdges
      HOffset
      VOffset   
      ManufacturingGrid
      FunctionCacheTable
      ReservedPitchTable
      ?CreateRectInfo ( not delete )
      ) ) )

(defun PinPlaceHorizontalStrut ( NetName 
				 PitchIndex
				 @key ( delete ( not PinPlaceCreateRectInfo ) )
                                 ( LPP HorizontalLPP )
                                 ( PinLength PinLength )
                                 ( HOffset BoundaryPinHorizontalSpacing )
                                 ( VOffset   BoundaryPinVerticalSpacing )
                                 @rest Rest
                                 )
  (let (
        ( WirePitch ( times UserUnitsPerMeter DefaultWirePitch ) )
        ( PinWidth (if ( car Rest )
                       ( times UserUnitsPerMeter ( car Rest ) )
                     ( times UserUnitsPerMeter DefaultWireWidth ) ) )
        ( WireSpacing (if ( cadr Rest )
                          ( times UserUnitsPerMeter ( cadr Rest ) )
                        ( times UserUnitsPerMeter DefaultWireSpacing ) ) ) )
    ( AutoPinQueueHorizontalStrut
      PinPlacePinTable
      PitchIndex					 
      NetName
      PinLPP
      PinWidth
      PinLength
      WireSpacing
      WirePitch
      BoundaryPolygonEdges
      HOffset
      VOffset 
      ManufacturingGrid
      FunctionCacheTable
      ReservedPitchTable
      ?CreateRectInfo ( not delete )
      ) ) )

(defun PinPlaceLeftRightPin (NetName 
			     PitchIndex
			     @key ( delete ( not PinPlaceCreateRectInfo ) )
                             ( LPP PinLPP )
                             ( PinLength PinLength )
                             ( HOffset BoundaryPinHorizontalSpacing )
                             ( VOffset BoundaryPinVerticalSpacing )
                             @rest Rest
                             ) 
  (let (
        ( WirePitch ( times UserUnitsPerMeter DefaultWirePitch ) )
        ( PinWidth (if ( car Rest )
                       ( times UserUnitsPerMeter ( car Rest ) )
                     ( times UserUnitsPerMeter DefaultWireWidth ) ) )
        ( WireSpacing (if ( cadr Rest )
                          ( times UserUnitsPerMeter ( cadr Rest ) )
                        ( times UserUnitsPerMeter DefaultWireSpacing ) ) ) )
    
    ( AutoPinQueueLeftRightPin
      PinPlacePinTable
      PitchIndex
      NetName		   
      LPP
      PinWidth
      PinLength
      WireSpacing
      WirePitch
      BoundaryPolygonEdges
      HOffset
      VOffset
      ManufacturingGrid
      FunctionCacheTable
      ReservedPitchTable
      ?CreateRectInfo ( not delete )
      ) ) )

(defun PinPlaceDrawUsingPDKInfo ( TargetCellView
                                  @key
                                  ( PinLength PinLength )
                                  ( PinLPP PinLPP )
                                  ( HorizontalLPP Metal3LPP )
                                  ( PinLength PinLength )
                                  ( BoundaryPinHorizontalSpacing
                                    BoundaryPinHorizontalSpacing )
                                  ( BoundaryPinVerticalSpacing
                                    BoundaryPinVerticalSpacing )
                                  ( ConnectivityViewName "netlist" )
                                  ( PinsDirectory nil )
                                  ( PinsFile "pins.il" )
                                  ( UserPinsFile "userpins.il" )
                                  ( DeleteOld t )
                                  ( CreateLog t )
                                  ( PowerGrid t )
                                  ( InPlace t )
                                  ( DeleteExistingPins nil )
                                  ( AddOnly nil )
                                )
  if( PowerGrid
      then
        DrawLeafM3Power()
      else
        DelLeafM3Power()
   )

  ( PinPlaceDraw
    TargetCellView
    UserUnitsPerMeter
    BoundaryLPP
    PinLength
    PinLPP
    HorizontalLPP 
    FoldableCellLibCellPairRegExs
    BoundaryPinHorizontalSpacing
    BoundaryPinVerticalSpacing
    PhysicalManufacturingGrid
    MetalLayerRegEx
    ViaLayerRegEx
    ?PinsDirectory PinsDirectory
    ?ConnectivityViewName ConnectivityViewName
    ?PinsFile PinsFile
    ?UserPinsFile UserPinsFile
    ?DeleteOld DeleteOld
    ?CreateLog CreateLog
    ?PowerGrid t
    ?InPlace InPlace
    ?DeleteExistingPins DeleteExistingPins
    ?AddOnly AddOnly
 ) )

(defun PinPlaceDraw ( 		  
		     TargetCellView		   	  		  
		     UserUnitsPerMeter
                     PRBoundaryLPP
                     PinLength
                     PinLPP
                     HorizontalLPP
                     FoldableCellLibCellPairRegExs
                     BoundaryPinHorizontalSpacing
                     BoundaryPinVerticalSpacing
                     ManufacturingGrid
                     MetalLayerRegEx
                     ViaLayerRegEx
                     @key
                     ( ConnectivityViewName "netlist" )
                     ( PinsDirectory nil )	
                     ( PinsFile "pins.il" )
                     ( UserPinsFile "userpins.il" )
                     ( DeleteOld t )
                     ( CreateLog t )
                     ( PowerGrid t )
                     ( InPlace t )
                     ( DeleteExistingPins nil )
                     ( AddOnly nil )
		    )
                                        ;adjust units
  ( setq PinLength 
         ( times PinLength UserUnitsPerMeter ) )
  ( setq BoundaryPinHorizontalSpacing
         ( times BoundaryPinHorizontalSpacing UserUnitsPerMeter ) )
  ( setq BoundaryPinVerticalSpacing
         ( times BoundaryPinVerticalSpacing UserUnitsPerMeter ) )
  ( setq ManufacturingGrid
         ( times ManufacturingGrid UserUnitsPerMeter ) )

                                        ; get polygon edges
  (let (
        ( PinsLogFile ( sprintf nil "%s.log" PinsFile ) )
        ( BoundaryPolygonEdges
          ( PinUtilGetBoundaryPolygonEdgesFromCellView
            PRBoundaryLPP
            TargetCellView ) ) )
                                        ; get pins directory
    (let (
          ( PinsDirectory 
            (if PinsDirectory PinsDirectory 
              ( NameGetCellPathFromCellName ( getq CellView cellName ) ) ) ) )
      (cond (
             ( not ( boundp `FunctionCacheTable ) )	 	 	 
             (defvar FunctionCacheTable ( CacheCreate ) )
             )
            (
             ( not ( tablep FunctionCacheTable ) )
             (defvar FunctionCacheTable ( CacheCreate ) ) ) )
                                        ;create tables
      ( setq ReservedPitchTable makeTable('TheReservedPitchTable) )
      ( setq PinPlacePinTable makeTable('ThePinPlacePinTable) ) 
      
      ( setq PinsFile     ( PathGetPathForFile PinsDirectory PinsFile ) )
      ( setq PinsLogFile  ( PathGetPathForFile PinsDirectory PinsLogFile ) )
      ( setq UserPinsFile ( PathGetPathForFile PinsDirectory UserPinsFile ) )
      
                                        ; Get Existing Pins that this program wrote last time 
                                        ; Dont create PinRectInfo so if this object is not in the pins or userpins files, the pins
                                        ; are deleted when updated
             ( setq PinPlaceCreateRectInfo nil ) 
             (cond (
                    ( and ( isReadable PinsLogFile ) DeleteOld )
                    ( load PinsLogFile ) ) )
                                        ;Add to queue the following pins in pins and userpins files, overwriting old entries
                                        ; create PinRectInfo so the pins are created properly
             ( setq PinPlaceCreateRectInfo t ) 
             (cond (
                    ( isReadable PinsFile )
                    ( load   PinsFile ) )
                   (
                    (error "No pins file found at %s\n" PinsFile ) ) )
             (cond (
                    ( isReadable UserPinsFile )
                    ( printf "Using userpins.il file %s\n" UserPinsFile )
                    ( load UserPinsFile ) ) )
      
                                        ;delete old pins
             (if DeleteExistingPins
                 ( PinUtilDeleteExistingPins TargetCellView ) )
                                        ;update pins
             ( AutoPinUpdateQueuedPins
               PinPlacePinTable
               TargetCellView
               ConnectivityViewName
               FoldableCellLibCellPairRegExs
               MetalLayerRegEx
               ViaLayerRegEx
               ?AddOnly AddOnly
               ) 
                                        ;center labels
             ( PinUtilCenterChildLabels TargetCellView BoundaryPolygonEdges )
                    
                                        ;write log
             (when ( isWritable PinsLogFile )
               (let (
                     ( PinsLogFilePort ( outfile PinsLogFile "w") )
                     )
                 (cond (
                        ( and ( isFile PinsFile ) CreateLog )	
                        ( FileUtilAppendFile PinsLogFilePort PinsFile )
                        ( cond ( 
                                ( isFile UserPinsFile )
                                ( FileUtilAppendFile PinsLogFilePort UserPinsFile ) ) ) ) )
                 ( close PinsLogFilePort ) ) ) ) ) )
    
(defun PinPlacePinScan ( TargetCellView 
                         ViaLayerRegEx
                         Pitched
                         BoundaryPolygonEdges
                         WirePitch
                         NetNamesToIgnore
                         @key 
                         ( UserPinsFile "userpins.il" )	  		   
                         ( FileMode "w" )		     
                         ( PinsDirectory nil )
                         )
                                        ; get pins directory
  (let (
        ( PinsDirectory 
          (if PinsDirectory PinsDirectory 
            ( NameGetCellPathFromCellName ( getq TargetCellView cellName ) ) ) )
        ( WriteFunc (cond ( 
                           Pitched
                           `PinPlaceMetaStringPitched )
                          (
                           `PinPlaceMetaString ) ) ) )
    ( setq UserPinsFile ( PathGetPathForFile PinsDirectory UserPinsFile ) )
    
    (let (
          ( OutFile ( outfile UserPinsFile FileMode ) ) )
      ( foreach 
        Fig
        ( PinUtilGetExistingPinFigs TargetCellView )
        (cond (
               ( exists NetName NetNamesToIgnore
                        ( equal NetName 
                                ( getq ( getq ( getq Fig pin ) net ) name ) ) )
               nil )
              (
               Pitched
               (let (
                     ( LeftOffset
                       ( difference
                         ( caar ( getq Fig bBox ) )
                         ( caar
                           ( PolygonGetLeftMostEdge
                             BoundaryPolygonEdges
                             ) ) ) )
                     ( RightOffset
                              ( difference
                                ( caadr
                                  ( PolygonGetRightMostEdge
                                    BoundaryPolygonEdges
                                    ) )
                                ( caadr ( getq Fig bBox ) ) ) ) )
                        ( fprintf 
                          OutFile
                          (cond (
                                 ( lessp LeftOffset RightOffset )
                                 "( PinPlaceLeft \"%s\" %d ?HOffset %f )\n" )
                                (
                                 ( greaterp LeftOffset RightOffset )
                                 "( PinPlaceRight \"%s\" %d ?HOffset %f )\n" )
                                (
                                 "( PinPlaceHorizontalStrut \"%s\" %d ?HOffset %f )\n" ) )
                                 
                          ( getq ( getq ( getq Fig pin ) net ) name )
                          ( car 
                            ( PinUtilGetHorizontalPitchWidthPairForRect 
                              ( getq Fig bBox )
                              BoundaryPolygonEdges
                              WirePitch
                              0.0 ) )
                            ( min 
                              LeftOffset RightOffset ) )
                        ) )
                     (
                      ( PinPlaceMetaString
                        OutFile
                        ( AutoPinGetPinInfoFromPinFigForceRect 
                          Fig 
                          ViaLayerRegEx ) ) ) ) )
                     
             ( close OutFile )

             ) ) )
