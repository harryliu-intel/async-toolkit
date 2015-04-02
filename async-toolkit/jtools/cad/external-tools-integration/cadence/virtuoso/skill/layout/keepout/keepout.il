(defun KeepOutCreateAbstractLayerStruct ( LPP 
                                         @key
                                         ( Purpose "boundary" ) )
   (let (
         ( Layer ( car LPP ) )
         ( Number nil ) )
     ( sscanf Layer MetalLayerFormat Number )
     ( list Layer
            ( list "hilite" ( sprintf nil "drawing%d" Number ) )
            ( list Layer Purpose )
            ( sprintf nil "abstract_keepout%d" Number )
            ( sprintf nil "metal%d_in_window" Number )
            ( sprintf nil "used_conductor%d" Number )
            ( list Layer "dummy" )
            ) ) )

(defun KeepOutCreateRoutingLayerStruct ( LPP 
                                         @key
                                         ( Purpose "boundary" ) )
  (let (
        ( Layer ( car LPP ) )
        ( Number nil ) )
    ( sscanf Layer MetalLayerFormat Number )
    ( list Layer
           ( list "hilite" ( sprintf nil "drawing%d" Number ) )
           ( list Layer Purpose )
           ( sprintf nil "routing_keepout%d" Number )
           ( sprintf nil "metal%d_in_window" Number )
           ( sprintf nil "used_conductor%d" Number )
           ( list Layer "dummy" )
           ) ) )

(defun KeepOutCreateDrawingToPinMapping ( PinLayers )
  ( mapcar 
    (lambda ( PinLayer ) ( list ( list PinLayer "drawing" )
                                ( list PinLayer "pin" ) ) )
    PinLayers ) )

(defun KeepOutDuplicateShapeOnPins ( CellView OldToNewLPPPairs NetNamesToAvoid )
  ( foreach OldToNewLPPPair OldToNewLPPPairs
            (let (
                  ( OldPinLPP ( car OldToNewLPPPair ) )
                  ( NewPinLPP ( cadr OldToNewLPPPair ) ) )
              ( mapcar 
                (lambda ( Rect )
                  ( dbCreateRect
                    CellView
                    NewPinLPP
                    Rect ) )
                ( PinUtilGetPinRectsForCellView 
                  CellView
                  (lambda ( NetName ) ( not ( exists NetNameToAvoid NetNamesToAvoid 
                                                     ( equal NetName NetNameToAvoid ) ) ) )
                  (lambda ( LPP ) ( equal ( car LPP ) ( car OldPinLPP ) ) ) ) ) ) ) )

(defun KeepOutChangeLayerOnPins ( CellView OldToNewLPPPairs NetNamesToAvoid )
  ( foreach OldToNewLPPPair OldToNewLPPPairs
            (let (
                  ( OldPinLPP ( car OldToNewLPPPair ) )
                  ( NewPinLPP ( cadr OldToNewLPPPair ) ) )
              ( mapcar 
                (lambda ( PinFig )
                  ( dbSetq PinFig NewPinLPP lpp ) )
                ( PinUtilGetPinFigsForCellView 
                  CellView
                  (lambda ( NetName ) ( not ( exists NetNameToAvoid NetNamesToAvoid 
                                                     ( equal NetName NetNameToAvoid ) ) ) )
                  (lambda ( LPP ) ( equal LPP OldPinLPP ) ) ) ) ) ) )





(defun KeepOutGetPinLayerFromLayerStruct ( LayerStruct )
  ( nth 0 LayerStruct ) )

(defun KeepOutGetPitchLPPFromLayerStruct ( LayerStruct )
  ( nth 1 LayerStruct ) )

(defun KeepOutGetKeepOutLPPFromLayerStruct ( LayerStruct )
  ( nth 2 LayerStruct ) )

(defun KeepOutGetAssuraLayerFromLayerStruct ( LayerStruct )
  ( nth 3 LayerStruct ) )

(defun KeepOutGetAssuraWindowMetalLayerFromLayerStruct ( LayerStruct )
  ( nth 4 LayerStruct ) )

(defun KeepOutGetConductorAssuraLayerFromLayerStruct ( LayerStruct )
  ( nth 5 LayerStruct ) )

(defun KeepOutGetConductorLPPFromLayerStruct ( LayerStruct )
  ( nth 6 LayerStruct ) )

(defun KeepOutCreateKeepOutView ( CellView
                                  TargetLibName
                                  TargetCellName
                                  TargetViewName
                                  KeepOutRuleFile
                                  WorkingDir
                                  LayerStructs 
                                  PinNetNamesToIgnoreLPP )
  ;copy the Target View to the new view
  (let (
        ( TempLibName 
          ( LibCreateTempLibraryFromCellView CellView "KEEPOUT" WorkingDir ) )
        ( PinLayers 
          ( mapcar (lambda ( LayerStruct ) 
                     ( KeepOutGetPinLayerFromLayerStruct 
                       LayerStruct ) )
                   LayerStructs ) )
        ( AssuraLayerMappings 
          ( ListNonDestructiveMapCan
            (lambda ( LayerStruct ) 
              ( list
                ( list ( KeepOutGetAssuraLayerFromLayerStruct
                         LayerStruct )
                       ( KeepOutGetKeepOutLPPFromLayerStruct 
                         LayerStruct ) )
                ( list ( KeepOutGetAssuraWindowMetalLayerFromLayerStruct
                         LayerStruct )
                       ( list 
                         ( KeepOutGetPinLayerFromLayerStruct LayerStruct )
                         "drawing" ) )
                ( list ( KeepOutGetConductorAssuraLayerFromLayerStruct
                         LayerStruct )
                       ( list 
                         ( KeepOutGetPinLayerFromLayerStruct LayerStruct )
                         "drawing" ) )
                ) )
            LayerStructs ) )
        ( PreKeepOutCellView ( betterCopyCellView
                               CellView
                               TempLibName 
                               TargetCellName
                               TargetViewName
                               nil
                               nil
                               t ) )
        )
                                        ;draw pin layer on drawing layer
;    ( KeepOutChangeLayerOnPins
 ;     PreKeepOutCellView 
  ;    ( KeepOutCreateDrawingToPinMapping PinLayers )
   ;   PinNetNamesToIgnoreLPP )

    ( dbSave PreKeepOutCellView )

    (let (
          ( ErrorStr 
            ( AssuraRunAssuraLayerProcessor 
              PreKeepOutCellView 
              TargetLibName
              TargetCellName
              TargetViewName
              KeepOutRuleFile
              WorkingDir
              AssuraLayerMappings
              nil ) ) )
      (if ErrorStr 
          ErrorStr
        ( dbOpenCellViewByType
          TargetLibName
          TargetCellName
          TargetViewName
          nil
          "r" ) ) ) ) )
      
(defun KeepOutCreateAbstractView ( CellView
                                   TargetLibName
                                   TargetCellName
                                   TargetViewName
                                   KeepOutRuleFile
                                   WorkingDir
                                   PRBoundaryLPP
                                   LayerStructs
                                   PinNetNamesToIgnoreLPP )
  (let (
        ( TempLibName ( LibCreateTempLibraryFromCellView 
                        CellView "ABSTRACT" WorkingDir ) ) )
    (let (
          ( PinLayers  ( mapcar ( lambda ( LayerStruct ) 
                                  ( KeepOutGetPinLayerFromLayerStruct 
                                    LayerStruct ) )
                                LayerStructs ) )
          ( KeepOutViewRet ( KeepOutCreateKeepOutView
                             CellView
                             TempLibName
                             TargetCellName
                             TargetViewName
                             KeepOutRuleFile
                             WorkingDir
                             LayerStructs
                             PinNetNamesToIgnoreLPP ) ) )
                                        ;run assura
      (let ( 
            ( Ret
              (cond (
                     ( stringp KeepOutViewRet )
                     KeepOutViewRet )
                    (
                     (let (
                           ( AbstractCellView ( betterCopyCellView
                                                CellView
                                                TargetLibName
                                                TargetCellName
                                                TargetViewName
                                                nil
                                                nil
                                                t
                                                ) ) )
                                        ;first delete all shapes
                       ( foreach Shape ( getq AbstractCellView shapes )
                                 ( dbDeleteObject Shape ) )
                                        ; move the pins and boundary to the abstract view
                       ( AssuraCopyShapesFromCellView 
                         CellView 
                         AbstractCellView
                         (lambda ( Shape ) 
                           ( or ( equal ( getq Shape lpp ) PRBoundaryLPP )
                                ( and ( getq Shape pin )
                                      ( exists 
                                        PinLayer
                                        PinLayers
                                        ( equal 
                                          ( car ( getq Shape lpp ) )
                                          PinLayer ) )
                                      ) ) ) )
                       ;copy the keepout from the assura made view
                       ( AssuraCopyShapesFromCellView 
                         KeepOutViewRet AbstractCellView nil )
                       ;change all instantiations to abstract view if possible
                       ( SwapInstanceMasterViews 
                         AbstractCellView "abstract" "maskLayout" )
                                        ;delete non abstract shapes
                       ( foreach 
                         Instance 
                         ( getq AbstractCellView instances )
                         (if ( not ( equal 
                                     ( getq Instance viewName )
                                     "abstract" ) )
                             ( dbDeleteObject Instance ) ) )
                       ( dbSave AbstractCellView )
                       AbstractCellView  ) ) ) ) )
        ( ddDeleteObj ( ddGetObj TempLibName ) )
        Ret ) ) ) )

(defun KeepOutCreateRoutingView ( CellView
                                  TargetLibName
                                  TargetCellName
                                  TargetViewName
                                  KeepOutRuleFile
                                  WorkingDir
                                  LayerStructs
                                  PinNetNamesToIgnoreLPP )

  (let (
        ( TempLibName ( LibCreateTempLibraryFromCellView 
                        CellView 
                        "ROUTING"
                        WorkingDir ) ) )
    (let (
          ( KeepOutViewRet ( KeepOutCreateKeepOutView
                             CellView
                             TempLibName
                             TargetCellName
                             TargetViewName
                             KeepOutRuleFile
                             WorkingDir
                             LayerStructs
                             PinNetNamesToIgnoreLPP ) ) )
                                        ;run assura
      (let ( 
            ( Ret
              (cond (
                     ( stringp KeepOutViewRet )
                     KeepOutViewRet )
                    (
                     (let (
                           ( RoutingCellView ( betterCopyCellView
                                               CellView
                                               TargetLibName 
                                               TargetCellName
                                               TargetViewName
                                               nil
                                               nil
                                               t
                                               ) ) )

                       ;first delete existing keepouts shapes
                       ( foreach 
                         Shape 
                         ( setof 
                           Shape
                           ( getq RoutingCellView shapes )
                           ( exists 
                             LayerStruct 
                             LayerStructs
                             ( or
                               ( equal ( getq Shape lpp )
                                       ( KeepOutGetKeepOutLPPFromLayerStruct
                                         LayerStruct ) )
                               ( equal ( getq Shape lpp )
                                       ( KeepOutGetConductorLPPFromLayerStruct
                                         LayerStruct ) ) ) ) )
                         ( dbDeleteObject Shape ) )

                       ;then copy the keepout from the assura made view
                       ( AssuraCopyShapesFromCellView 
                         KeepOutViewRet
                         RoutingCellView
                         nil )
                       ( SwapInstanceMasterViews 
                         RoutingCellView
                         "abstract"
                         "maskLayout" )
                       ( dbSave RoutingCellView )
                       RoutingCellView  ) ) ) ) )

        ( ddDeleteObj ( ddGetObj TempLibName ) )

        Ret ) ) ) )
  
(defun KeepOutDrawKeepOut ( CellView
                            DirectiveTable
                            HorizontalLayerStructs
                            VerticalLayerStructs
                            BoundaryLPP
                            UserUnitsPerMeter
                            DirectiveUnitsPerMeter
                            NetNamesToAvoid
                            KeepoutPattern ;nil implies use directives
                            DrawKeepout
                            DrawPinPitches
                            PinKeepInWidth
                            @key 
                            ( Area nil ) )
                            
  (let (
        ( BoundaryPolygonEdges 
          ( PinUtilGetBoundaryPolygonEdgesFromCellView 
            BoundaryLPP 
            CellView ) ) )
    ( foreach 
      X
      ( append 
        ( mapcar (lambda ( X )
                   ( list X 
                          ( list 
                            `AutoPinCalculateNumVerticalWirePitches
                            `PinUtilGetVerticalPitchWidthPairForRect
                            `KeepOutGetVerticalRect ) ) )
                 VerticalLayerStructs  )
        ( mapcar (lambda ( X )
                   ( list X 
                          ( list 
                            `AutoPinCalculateNumHorizontalWirePitches
                            `PinUtilGetHorizontalPitchWidthPairForRect
                            `KeepOutGetHorizontalRect ) ) )
                 HorizontalLayerStructs  ) )
      (let (
            ( LayerStruct ( car X ) )
            ( Funcs ( cadr X ) ) )
        (let (
              ( MetalLPP
                ( list 
                  ( KeepOutGetPinLayerFromLayerStruct LayerStruct ) "drawing" ) ) )
          (let (
                ( PinRects
                  ( PinUtilGetPinRectsForCellView 
                    CellView
                    (lambda ( NetName ) ( not ( exists NetNameToAvoid NetNamesToAvoid 
                                                       ( equal NetName NetNameToAvoid ) ) ) )

                    (lambda ( LPP )
                      ( equal ( car LPP ) ( KeepOutGetPinLayerFromLayerStruct LayerStruct ) ) ) ) )
                ( KeepoutPattern
                  (if ( null KeepoutPattern )
                      ( KeepOutParseKeepOutDirectiveString 
                        ( CellInfoGetLayerKeepOutPattern 
                          DirectiveTable
                          MetalLPP
                          ) )
                    KeepoutPattern ) )
                ( PowerGridPattern 
                  ( PowerGridCreatePowerGridPatternFromDirectives 
                    DirectiveTable MetalLPP ) )
                ( WirePitch 
                  ( times UserUnitsPerMeter 
                          ( CellInfoGetLayerWirePitchInMeters
                            DirectiveTable
                            MetalLPP
                            DirectiveUnitsPerMeter
                            ) ) )
                ( PowerGridWireWidth
                  ( times UserUnitsPerMeter 
                          ( CellInfoGetLayerPowerGridWireWidthInMeters
                            DirectiveTable
                            MetalLPP
                            DirectiveUnitsPerMeter
                            ) ) )
                ( WireWidth 
                  ( times UserUnitsPerMeter 
                          ( CellInfoGetLayerWireWidthInMeters
                            DirectiveTable
                            MetalLPP
                            DirectiveUnitsPerMeter
                            ) ) )
                ( WireSpacing 
                  ( times UserUnitsPerMeter 
                          ( CellInfoGetLayerWireSpacingInMeters
                            DirectiveTable
                            MetalLPP
                            DirectiveUnitsPerMeter
                            ) ) )
                        
                )
            (let (
                  ( PitchOffset 
                    ( KeepoutGetPitchOffset
                      PowerGridWireWidth
                      WireSpacing
                      WirePitch ) ) )
              ( println PitchOffset )
              ( DrawKeepOutForLayer
                LayerStruct
                BoundaryPolygonEdges 
                PowerGridPattern
                KeepoutPattern      
                ( nth 0 Funcs )
                ( nth 1 Funcs )
                ( nth 2 Funcs )
                PinRects
                WirePitch 
                WireWidth
                PitchOffset
                DrawKeepout
                DrawPinPitches               
                PinKeepInWidth ) ) ) ) ) ) ) )
          
(defun KeepoutGetPitchOffset ( PowerGridWireWidth
                               WireSpacing
                               WirePitch )
  (let (
        ( HalfPitchOffset ( times 0.5 
                                  ( plus PowerGridWireWidth
                                         WireSpacing ) ) ) )
    ( difference 
      0.0
      ( difference
        ( times 
          ( ceiling
            ( quotient HalfPitchOffset
                       WirePitch ) )
          WirePitch )
        HalfPitchOffset ) ) ) )

(defun DrawKeepOutForLayer ( LayerStruct
                             BoundaryPolygonEdges 
                             PowerGridPattern
                             KeepoutPattern                          
                             NumPitchesFunc
                             GetPinPitchWidthPairFunc
                             RectFunc
                             PinRects
                             WirePitch 
                             WireWidth
                             PitchOffset
                             DrawKeepout
                             DrawPinPitches               
                             PinKeepInWidth 
                             )
         
  (let (
        ( NumPitches ( apply NumPitchesFunc 
                             ( list 
                               BoundaryPolygonEdges
                               WirePitch
                               PitchOffset
                               ) ) )
        )
    (let (
          ( FirstPitch 0 )
          ( LastPitch NumPitches ) )
      (let (
            ( KeepOutPitches 
              ( KeepOutGetKeepOutPitches
                KeepoutPattern
                PowerGridPattern
                FirstPitch
                LastPitch ) ) )
        (let (
              ( PinPitchWidthPairs
                ( mapcar
                  (lambda ( Rect )
                    ( apply GetPinPitchWidthPairFunc 
                            ( list Rect BoundaryPolygonEdges WirePitch PitchOffset ) ) )
                  PinRects ) ) )
          
          (when DrawPinPitches
            ( mapcar
              (lambda ( Rect )
                ( dbCreateRect 
                  CellView
                  ( KeepOutGetPitchLPPFromLayerStruct LayerStruct )
                  Rect ) )
              ( mapcar 
                (lambda ( PitchWidthPair ) 
                  ( apply RectFunc ( list 
                                     PitchWidthPair
                                     WirePitch
                                     WirePitch
                                     PitchOffset
                                     BoundaryPolygonEdges ) ) )
                PinPitchWidthPairs ) ) )
          
          (when DrawKeepout 
            ( mapcar
              (lambda ( Rect )
                ( dbCreateRect 
                  CellView
                  ( KeepOutGetKeepOutLPPFromLayerStruct LayerStruct )
                  Rect ) )
              ( mapcar 
                (lambda ( Pitch ) 
                  ( apply RectFunc ( list ( list Pitch 1 )
                                          WirePitch 
                                          WireWidth
                                          PitchOffset
                                          BoundaryPolygonEdges ) ) )
                ( KeepOutSubtractOutPitchWidthPairsFromPitches 
                  FirstPitch
                  LastPitch
                  KeepOutPitches
                  ( mapcar 
                    (lambda ( PitchWidthPair )
                      ( KeepOutChangeWidthOfPitchWidthPair 
                        PitchWidthPair PinKeepInWidth ) )
                    PinPitchWidthPairs ) ) ) ) ) ) ) ) ) )
                
  
  
(defun KeepOutSubtractOutPitchWidthPairsFromPitches ( Min
                                                      Max
                                                      Pitches
                                                      PitchWidthPairs )
  
  (let (
        ( Size ( plus ( difference Max Min ) 1 ) ) )
    (let (
          ( KeepOutVector ( makeVector Size t ) ) )
      ( foreach Pitch Pitches
                ( setarray KeepOutVector ( difference Pitch Min ) nil ) )      
      ( foreach PitchWidthPair PitchWidthPairs
                ( for I 
                      ( car PitchWidthPair ) 
                      ( min ( difference Size 1 )
                            ( plus ( car PitchWidthPair ) ( difference ( cadr PitchWidthPair ) 1 ) ) )
                      ( setarray KeepOutVector ( difference I Min ) t ) ) )
      ( KeepOutGetKeepOutPitches
        ( vectorToList KeepOutVector )
        ( list nil )
        Min
        Max ) ) ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Getting the pitch/width pairs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun KeepOutChangeWidthOfPitchWidthPair ( PitchWidthPair WidthModifier )
  (let (
        ( OriginalPitch ( car PitchWidthPair ) )
        ( OriginalWidth ( cadr PitchWidthPair ) ) )
    (let (
          ( Width (if ( equal WidthModifier 0 ) 0 ( plus OriginalWidth ( difference WidthModifier 1 ) ) ) ) )
      ( list ( max 0 ( difference OriginalPitch ( quotient ( difference Width OriginalWidth ) 2 ) ) )
             Width ) ) ) )
   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Creating and Drawing the Rectangles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun KeepOutGetHorizontalRect ( PitchWidthPair
                                  WirePitch
                                  WireWidth
                                  WirePitchOffset
                                  BoundaryPolygonEdges )
  (let (
        ( YRange ( PinUtilGetYRangeForHorizontalPitchWidthPair 
                   PitchWidthPair
                   WirePitch
                   WireWidth
                   WirePitchOffset
                   BoundaryPolygonEdges ) ) )
    (let (
          ( XRange ( PolygonGetHorizontalLeftAndRightBoundaryFromBottomAndTop
                     YRange
                     BoundaryPolygonEdges ) ) )
      ( RectMakeFromRanges
        XRange
        YRange ) ) ) )


(defun KeepOutGetVerticalRect ( PitchWidthPair
                                WirePitch
                                WireWidth
                                WirePitchOffset
                                BoundaryPolygonEdges )
  (let (
        ( XRange ( PinUtilGetXRangeForVerticalPitchWidthPair 
                   PitchWidthPair
                   WirePitch
                   WireWidth
                   WirePitchOffset
                   BoundaryPolygonEdges ) ) )
    (let (
          ( YRange ( PolygonGetVerticalTopAndBottomBoundaryFromLeftAndRight 
                     XRange
                     BoundaryPolygonEdges ) ) )
      ( RectMakeFromRanges
        XRange
        YRange ) ) ) )


(defun KeepOutParseKeepOutDirectiveString ( DirectiveString )
  (let (
        ( Len ( strlen DirectiveString ) )
        ( Ret nil ) )
    ( for
      CurrIndex
      1
      Len
      (let (
            ( CurrChar ( substring DirectiveString CurrIndex 1 ) ) )
        (cond
         (
          ( equal CurrChar "0" )
          ( setq Ret ( tconc Ret nil ) ) )
         (
          ( equal CurrChar "1" )
          ( setq Ret ( tconc Ret t ) ) ) ) ) )
    ( car Ret ) ) )



(defun KeepOutGetKeepOutPitches ( Pattern PowerGridPattern FirstPitch LastPitch )
  (let (
        ( CurrPattern nil )
        ( CurrPowerGridPattern nil )
        ( CurrPitch FirstPitch )
        ( Ret nil ) )
    (while ( lessp CurrPitch LastPitch )
      (let ()
        (unless CurrPattern
          ( setq CurrPattern Pattern ) )
        (unless CurrPowerGridPattern
          ( setq CurrPowerGridPattern PowerGridPattern ) )
        
        (let (
              ( PowerInCurrPitch ( car CurrPowerGridPattern ) ) )
          (unless PowerInCurrPitch
            (let (
                  ( KeepoutInPitch ( not ( car CurrPattern ) ) ) )
              (when KeepoutInPitch
                ( setq Ret ( tconc Ret CurrPitch ) ) )
              ( setq CurrPattern ( cdr CurrPattern ) ) ) )
          ( setq CurrPitch ( plus CurrPitch 1 ) )
          ( setq CurrPowerGridPattern ( cdr CurrPowerGridPattern ) ) ) ) )
    ( car Ret ) ) )



; check if the cell has keepouts
(defun HasKeepout (view)
 (let ((keepout nil))
   (foreach shape view->shapes
     (when (cadr shape->lpp)=="boundary"
       keepout = t
     )
   )
   keepout
 )
)

; delete keepout
(defun DeleteKeepout (@key (CV nil))
  (cond (CV==nil CV = (geGetEditCellView)))
  (foreach shape CV->shapes
           (cond ((isMetalKeepout shape->layerName shape->purpose)
                  (dbDeleteObject shape))
                 ((and shape->layerName=="prBoundary" shape->purpose=="block")
                  (dbDeleteObject shape)
                  )
                 )
           )
  (foreach obj CV->blockages (dbDeleteObject obj))
  t
  )

; delete worthless nanoroute shape
(defun DeletePrboundBy (@key (CV nil))
    (cond (CV==nil CV = (geGetEditCellView)))
    (foreach shape CV->shapes
             (cond (shape->layerName=="prBoundary" &&  shape->purpose=="boundary"
                 (dbDeleteObject shape))
                   )
             )
  t
  )
