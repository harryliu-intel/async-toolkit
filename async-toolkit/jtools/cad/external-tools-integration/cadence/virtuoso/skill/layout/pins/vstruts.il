; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

defun( VStruts ( CellView )
  (VStrutsDrawStruts t t "All" ?CellView CellView)
  (VStrutsDropM12Vias t t "All" ?CellView CellView)
  (VStrutsDropM23Vias t t ?CellView CellView)

  dbSave( CellView )
  CellView
)

; anything below this line is old code and to be deleted.

(defun VStrutsSetProp (obj)
  (when obj
    (dbReplaceProp obj "VStrutObject" "boolean" t)
  ) 
)

(defun VStrutsIsObject (obj)
 (let (isobj)
  isobj = nil
  (when obj
    (when (dbGetPropByName obj "VStrutObject")->value=="TRUE"
      isobj=t 
    )
  )
  isobj
 )
)

(defun VStrutsMakeRectFromVStrutInfo ( VStrutInfo Pitch NumPitches WireWidth WireSpacing WirePitch WirePitchOffset )
  (let (
        ( Left ( difference
                 ( plus WirePitchOffset 
                        ( times WirePitch Pitch ) )
                 ( times 0.5 WireWidth ) ) )
        ( Right ( plus WirePitchOffset
                       ( times WirePitch 
                               ( difference ( plus Pitch NumPitches ) 1 ) )
                       ( times 0.5 WireWidth ) ) ) )
    ( list
      ( list
        Left
        ( car VStrutInfo ) )
      ( list
        Right
        ( cadr VStrutInfo ) ) ) ) )
            

(defun VStrutsGetVStrutInfoTableForPitch ( PitchTable Pitch )
  ( arrayref PitchTable Pitch ) )

(defun VStrutsAddVStrutInfo ( PitchTable
                              Pitch 
                              VStrutInfo
                              IsUsed )
  (let ( 
        ( VStrutInfoTable 
          (cond (
                 ( arrayref PitchTable Pitch ) )
                ( 
                 ( makeTable `vstrut nil ) ) ) ) )
    ( setarray PitchTable Pitch VStrutInfoTable )
    ( setarray VStrutInfoTable
               VStrutInfo
               IsUsed ) ) )

(defun VStrutRemoveVStrutInfo ( PitchTable
                                Pitch 
                                VStrutInfo )
  (let ( 
        ( VStrutInfoTable ( arrayref PitchTable Pitch ) ) )
    ( remove VStrutInfo VStrutInfoTable ) ) )

(defun VStrutsGetDistanceToVStrut ( VStrutInfo
                                    YRange )
  (if ( RangeDoIntersect
        YRange
        VStrutInfo 
        1e-9 )
      ( difference 
        0
        ( RangeGetSize
          ( RangeIntersection
            YRange
            VStrutInfo ) ) )
    ( RangeGetDistance VStrutInfo YRange ) ) )

(defun VStrutsGetPitchForColumnData ( ColumnData 
                                      WirePitch
                                      WirePitchOffset
                                      Left )
  (let (
        ( Junction
          ( PlacementRegionGetColumnNPJunction 
            ColumnData ) ) )
    (cond (
           Left
           ( difference 
             ( round 
               ( quotient
                 ( difference 
                   Junction
                   WirePitch WirePitchOffset )
                 WirePitch ) )
             1 ) )
          (
           ( difference
             ( round
               ( quotient
                 ( difference
                   ( plus
                     Junction
                     WirePitch )
                   WirePitchOffset )
                 WirePitch ) )
             0
             ) ) ) ) )


(defun VStrutsGetPitchForContactRects (
                                       ContactRects
                                       WirePitch
                                       WireSpacing
                                       WirePitchOffset
                                       Justification )
  (let (
        ( MinPitch
          ( difference
            ( ceiling
              ( quotient 
                ( plus 
                  ( ListFindMinimum
                    ContactRects
                    (lambda ( Rect )
                      ( RectGetLeft Rect ) ) )
                  ( difference
                    WireSpacing
                    WirePitchOffset
                    1e-9 ) )
                WirePitch ) )
            2 ) )
        ( MaxPitch
          ( floor ( quotient
                    ( difference
                      ( ListFindMaximum
                        ContactRects
                        (lambda ( Rect )
                          ( RectGetRight Rect ) ) )
                      WirePitchOffset
                      WireSpacing
                      -1e-9 )
                    WirePitch ) ) ) )
    (let (
          ( Range 
            ( ListFillList (lambda ( N ) ( plus MinPitch N ) )
                           ( difference MaxPitch MinPitch -1 ) ) ) )
      ( ListFindMaximumElement
        Range
        (lambda ( Pitch )
          ( ListFindSum
            ContactRects
            (lambda ( ContactRect )
              ( RangeGetSize
                (cond (
                       ( RangeIntersection
                         ( list
                           ( plus WirePitchOffset
                                  ( times WirePitch Pitch ) )
                           ( plus WirePitchOffset
                                  ( times WirePitch ( plus Pitch 2 ) ) ) )
                         ( RectGetXRange ContactRect ) ) )
                      (
                       ( list 0 0 ) ) ) ) ) ) ) ) ) ) )

(defun VStrutsUpdateVStrutsForPitch ( PitchTable
                                      Pitch
                                      YRange
                                      IsUsed )
  (when YRange
    (let (
          ( VStrutInfoTable
            ( arrayref PitchTable Pitch ) ) )
      (let ( 
            ( VStrutInfoToUse
              (when ( tablep VStrutInfoTable )
                ( ListFindMinimumElement
                  ( mapcar `car ( tableToList VStrutInfoTable ) )
                  (lambda ( VStrutInfo )
                    ( VStrutsGetDistanceToVStrut VStrutInfo YRange ) ) ) ) ) )
        ( VStrutsMergeVStrutInfos 
          PitchTable
          Pitch
          VStrutInfoToUse
          YRange
          IsUsed ) ) ) ) )

(defun VStrutsMergeVStrutInfos ( PitchTable
                                 Pitch
                                 Range1
                                 Range2
                                 IsUsed )
  ( VStrutRemoveVStrutInfo
    PitchTable
    Pitch 
    Range1 )
  ( VStrutRemoveVStrutInfo
    PitchTable
    Pitch 
    Range2 )
  ( VStrutsAddVStrutInfo 
    PitchTable
    Pitch
    ( RangeUnion 
      Range1
      Range2 )
    IsUsed
    ) )
         

(defun VStrutsInitializePitchTablePitchTableFromHorizontalRects ( PitchTable
                                                                  HorizontalRects
                                                                  WirePitch
                                                                  WirePitchOffset )
  ( foreach HorizontalRect HorizontalRects
            (let (
                  ( MinPitch
                    ( ceiling ( quotient ( difference 
                                           ( RectGetLeft HorizontalRect ) WirePitchOffset )
                                         WirePitch ) ) )
                  ( MaxPitch 
                    ( floor ( quotient ( difference 
                                         ( RectGetRight HorizontalRect ) WirePitchOffset )
                                       WirePitch ) ) ) )
              ( for Pitch MinPitch MaxPitch                    
                    ( VStrutsUpdateVStrutsForPitch
                      PitchTable
                      Pitch 
                      ( RectGetYRange HorizontalRect )
                      nil ) ) ) ) )


(defun VStrutsGetFinalVStrutRectsFromPitchTable ( PitchTable 
                                                  WireWidth
                                                  WireSpacing
                                                  WirePitch
                                                  WirePitchOffset )
  (let (
        ( VStrutRectInfos nil ) )
    ( foreach CurrPitch PitchTable
              (let (
                    ( CurrVStrutInfoTable ( arrayref PitchTable CurrPitch ) ) )
                ( foreach CurrVStrutInfo CurrVStrutInfoTable
                          (let (
                                ( IsUsed ( arrayref CurrVStrutInfoTable CurrVStrutInfo ) ) )
                            (if IsUsed
                                ( setq VStrutRectInfos 
                                       ( cons 
                                         ( VStrutsMakeRectFromVStrutInfo
                                           CurrVStrutInfo
                                           CurrPitch
                                           2
                                           WireWidth
                                           WireSpacing
                                           WirePitch
                                           WirePitchOffset
                                           )
                                         VStrutRectInfos ) ) ) ) ) ) )
    VStrutRectInfos ) )

(defun VStrutsGetContactRectsForInstances ( Instances
                                            NetName
                                            LPP )
  ( PinUtilGetPinRectsForInstances
    Instances
    (lambda ( SomeNetName ) ( equal SomeNetName NetName ) )
    (lambda ( SomeLPP ) ( equal ( car SomeLPP ) ( car LPP ) ) ) ) )


(defun VStrutsGetBoundRectForContactRects ( ContactRects )
  ( ListApplyFuncToListAndAccumulateResult
    ContactRects
    (lambda ( Rect BoundRect )
      ( RectGetBoundRect 
        Rect
        BoundRect ) )
    nil
    nil ) )

(defun VStrutsDrawVStrutsOnCellView ( CellView
                                      CellRegionData
                                      NetName
                                      Instances
                                      WireWidth
                                      WireSpacing
                                      WirePitch 
                                      WirePitchOffset
                                      ComponentContactLPP
                                      VStrutLPP
                                      HStrutLPP
                                      Extension
                                      IsNRegion
                                      )
  (let (
        ( PitchTable ( makeTable `pitch nil ) ) )
    (let (
          ( HorizontalRects
            ( PinUtilGetPinRectsForCellView
              CellView
              (lambda ( SomeNetName ) ( equal SomeNetName NetName ) )
              (lambda ( SomeLPP ) ( equal ( car SomeLPP ) ( car HStrutLPP ) ) ) ) ) )

      ( VStrutsInitializePitchTablePitchTableFromHorizontalRects
        PitchTable
        HorizontalRects
        WirePitch
        WirePitchOffset )

      (if CellRegionData
          ( foreach 
            ColumnData
            ( PlacementRegionsGetCellRegionDataColumnList CellRegionData )
            (let (
                  ( ContactRects
                    ( VStrutsGetContactRectsForInstances
                      ( setof Instance Instances
                              ( RectIsRectInRectClose
                                ( getq Instance bBox )
                              ( PlacementRegionGetColumnBBox ColumnData )
                              1e-9 ) )
                      NetName
                      ComponentContactLPP
                      ) ) )
              (when ContactRects
                ( VStrutsUpdateVStrutsForPitch 
                  PitchTable
                  ( VStrutsGetPitchForColumnData
                    ColumnData
                    WirePitch 
                    WirePitchOffset
                    ( equal ( not IsNRegion )
                            ( PlacementRegionGetNPRegionParity ColumnData ) )
                    )
                  ( RectGetYRange
                    ( RectShrinkInY
                      ( VStrutsGetBoundRectForContactRects
                        ContactRects )
                      -Extension
                    )
                  )
                  t )
                ) ) )
        (let (
              RectFig
              ( ContactRects
                ( VStrutsGetContactRectsForInstances
                  Instances
                  NetName
                  ComponentContactLPP ) ) )
          (when ContactRects
            ( VStrutsUpdateVStrutsForPitch 
              PitchTable
              ( VStrutsGetPitchForContactRects
                ContactRects
                WirePitch 
                WireSpacing
                WirePitchOffset )
              ( RectGetYRange
                ( RectShrinkInY
                  ( VStrutsGetBoundRectForContactRects ContactRects )
                  ( quotient WireSpacing -4.0 )
                  ) )
              t ) ) ) )

      ( foreach VerticalRect
                ( VStrutsGetFinalVStrutRectsFromPitchTable
                  PitchTable 
                  WireWidth
                  WireSpacing
                  WirePitch
                  WirePitchOffset )
                RectFig = ( dbCreateRect 
                       CellView
                       VStrutLPP
                       VerticalRect )
                ( VStrutsSetProp RectFig ) 
                ( dbCreatePin 
                  ( dbMakeNet CellView NetName )
                  RectFig  ) ) ) ) )








(defun VStrutsGetExtendedConnectToRectsToFromRects ( CellView
                                                     ConnectToRects
                                                     ConnectFromRects
                                                     ConnectToLPP 
                                                     )
  (mapcar 
   (lambda ( ConnectToRect )
     (let (
           ( Intersect 
             ( VStrutsGetExtendedConnectToRectToFromRects 
               ConnectToRect
               ConnectFromRects
               ConnectToLPP ) ) )
       Intersect ) )
   ConnectToRects ) )


(defun VStrutsGetExtendedConnectToRectToFromRects ( ConnectToRect
                                                    ConnectFromRects
                                                    ConnectToLPP )
  (let ( 
        RectFig
        ( ConnectFromRect
          ( ListFindMinimumElement
            ConnectFromRects
            (lambda ( ConnectFromRect )
              ( RectGetDistanceFromRectToRect
                ConnectToRect
                ConnectFromRect ) ) ) ) )
    (when ConnectFromRect
      (let (
            ( Rect
              (  VStrutsGetFootInTheDoorExtendedConnectToRectToFromRect
                 ConnectToRect
                 ConnectFromRect
                 .005 ) ) )
        (when ( not ( RectIsRectInRectClose
                      Rect
                      ConnectToRect
                      1e-9 ) )
          RectFig = ( dbCreateRect
            CellView
            ConnectToLPP
            Rect ) 
          ( VStrutsSetProp RectFig )
         )
        Rect ) ) ) )

(defun VStrutsGetFootInTheDoorExtendedConnectToRectToFromRect ( ConnectToRect
                                                                ConnectFromRect
                                                                Distance
                                                                )
  ( RectGetBoundRect
    ConnectToRect
    ( BBoxCanonicalize
      (cond (
             ;left
             ( leqp 
               ( RectGetRight ConnectToRect )
               ( RectGetLeft ConnectFromRect )
               )
             ( list
               ( list
                 ( RectGetLeft ConnectToRect )
                 ( RectGetBottom ConnectToRect ) )
               ( list
                 ( plus ( RectGetLeft ConnectFromRect ) Distance )
                 ( RectGetTop ConnectToRect ) ) ) )
            (
             ;right
             ( geqp
               ( RectGetLeft ConnectToRect )
               ( RectGetRight ConnectFromRect )
               )
             ( list
               ( list
                 ( difference ( RectGetRight ConnectFromRect ) Distance )
                 ( RectGetBottom ConnectToRect ) )
               ( list
                 ( RectGetRight ConnectToRect )
                 ( RectGetTop ConnectToRect ) ) ) )
            (
             ;overlaps
             ConnectToRect
             ) ) ) ) )

(defun VStrutsGetGetMinimalExtendedConnectToRectToFromRect (
                                                            ConnectToRect
                                                            ConnectFromRect
                                                            ViaCutWidth
                                                            ViaMinExtension
                                                            ViaXExtension
                                                            ViaYExtension
                                                            )
  ( println ( list ConnectToRect ConnectFromRect ) )

  (let (
        ( CenterY
          ( cadr ( RectGetCenter ConnectToRect ) ) )
        ( HalfHeight
          ( plus 
            ( times 0.5 ViaCutWidth ) 
            ViaYExtension ) ) )
    (let (
          ( Bottom
            ( difference CenterY HalfHeight ) )
          ( Top
            ( plus CenterY HalfHeight ) )
          )
      ( BBoxCanonicalize
        (cond (
               ( leqp 
                 ( RectGetRight ConnectFromRect )
                 ( RectGetRight ConnectToRect ) )
               ( list
                 ( list
                   ( difference
                     ( RectGetRight ConnectFromRect )
                     ( plus
                       ( plus ViaMinExtension ViaXExtension )
                         ViaCutWidth ) )
                     Bottom
                     )
                   ( list
                     ( plus 
                       ( RectGetRight ConnectFromRect )
                       ( difference
                         ViaXExtension ViaMinExtension ) )
                     Top
                     ) ) )
                (
                 ( geqp 
                   ( RectGetLeft ConnectFromRect )
                   ( RectGetLeft ConnectToRect ) )
                 ( list
                   ( list
                     ( difference
                       ( RectGetLeft ConnectFromRect )
                       ( difference
                         ViaXExtension ViaMinExtension ) )
                     Bottom
                     )
                   ( list
                     ( plus
                       ( RectGetLeft ConnectFromRect )
                       ( plus
                         ( plus ViaMinExtension ViaXExtension )
                         ViaCutWidth ) )
                     Top
                     ) ) ) 
                 ( t ConnectToRect )
            ) ) ) ) )

  
(defun VStrutsTestRect ( CellView 
                         Rect
                         ContactRect
                         Spacing
                         LPP )
  (let (
        ( Rect
          ( RectShrinkInX
            ( RectShrinkInY
              Rect 
              -Spacing+.005 )
            -Spacing+.005 ) ) )
    (let (
          ( Ret 
            (if ( not 
                  ( exists
                    Path
                    ( dbProduceOverlap 
                      CellView
                      Rect
                      ( list 1 4 )
                      LPP )
                    ( not 
                      ( RectIsRectInRectClose
                        ( TransformGetBBoxFromPath
                          Path )
                        ( RectGetBoundRect 
                          Rect
                          ContactRect )
                        1e-9
                        ) ) ) )
                t ) ) )
      Ret
 ) ) )
        
        

      
(defun VStrutsDrawContactsAtIntersectionOfRects ( CellView
                                                  ConnectToLPP
                                                  ConnectFromLPP
                                                  ConnectToSpacing
                                                  ConnectToRects
                                                  ConnectFromRects
                                                  ViaLibraryName
                                                  ViaMasterNameFormat	  
                                                  ViaViewName
                                                  ViaCutWidth
                                                  ViaMinExtension
                                                  ViaExtension
                                                  ViaXSpacing
                                                  ViaYSpacing
                                                  ConnectAll
                                                  DrawRect
                                                  ExtendTo
                                                  )
  (let (
        ( GetInstanceInfoFunc 
          (lambda ( RectIntersection )
            ( VStrutsGetViaInstanceInfoForRect
              RectIntersection
              ViaLibraryName
              ViaMasterNameFormat	  
              ViaViewName
              ConnectToLPP
              ConnectFromLPP
              ViaCutWidth
              ViaMinExtension
              ViaExtension
              ViaXSpacing
              ViaYSpacing ) ) )
        ( Func (if ConnectAll `foreach `exists ) ) )
    ( eval 
      ( ExpressionReplaceSymbolsWithValues
        `( foreach
           ConnectToRect
           ConnectToRects                      
           ( Func
             ConnectFromRect
             ConnectFromRects
             (let (
                   ( RectIntersection
                     ( RectGetIntersection
                       ConnectToRect
                       ConnectFromRect ) )
                   ( RectM1BL ( list 
                              ( RectGetLeft ConnectFromRect ) 
                              ( RectGetBottom ConnectToRect ) ) )
                   ( RectM1UR ( list 
                              ( RectGetRight ConnectFromRect ) 
                              ( RectGetTop ConnectToRect ) ) )
                  )
                  
;(setq RectIntersection ( RectGetIntersection ConnectToRect ConnectFromRect ) )
;(when (equal ConnectToLPP Metal1LPP)
;  (setq RectIntersection (list RectM1BL RectM1UR) )
;)
               (if RectIntersection 
                   (let (
                         ( InstanceInfo 
                           ( apply 
                             GetInstanceInfoFunc
                             ( list RectIntersection ) ) ) )
;                             ( list ( list RectBL RectUR ) ) ) ) )
                     
                     (when ( and DrawRect InstanceInfo )
                       ( dbCreateRect
                         CellView
                         ConnectToLPP
                         RectIntersection ) )
;                         ( list RectBL RectUR ) ) )

                     ;try extending the ToRect to FromRect horizontally
                     (when ( and
                             ( null InstanceInfo )
                             )
                       (let (
                             ( NewRect
                               ( VStrutsGetGetMinimalExtendedConnectToRectToFromRect
                                 ConnectToRect
                                 ConnectFromRect
                                 ViaCutWidth
                                 ViaMinExtension
                                 ViaExtension
                                 ViaExtension
                                 ) ) )
                         (when ( VStrutsTestRect
                                 CellView
                                 NewRect
                                 ConnectToRect
                                 ConnectToSpacing
                                 ConnectToLPP )
                           ( setq
                             InstanceInfo 
                             ( apply 
                               GetInstanceInfoFunc
                               ( list 
                                 NewRect
                                 ) ) ) )

                         (when ( null InstanceInfo )
                           ( setq 
                             NewRect
                             ( VStrutsGetGetMinimalExtendedConnectToRectToFromRect
                               ConnectToRect
                               ConnectFromRect
                               ViaCutWidth
                               ViaMinExtension
                               ViaExtension
                               ViaMinExtension
                               ) )
                           (when ( VStrutsTestRect
                                   CellView
                                   NewRect
                                   ConnectToRect
                                   ConnectToSpacing
                                   ConnectToLPP )
                             ( setq
                               InstanceInfo 
                               ( apply 
                                 GetInstanceInfoFunc
                                 ( list 
                                   NewRect
                                   ) ) ) ) )

                              
                         (when ( null InstanceInfo )
                           ( setq 
                             NewRect
                             ( VStrutsGetGetMinimalExtendedConnectToRectToFromRect
                               ConnectToRect
                               ConnectFromRect
                               ViaCutWidth
                               ViaMinExtension
                               ViaMinExtension
                               ViaExtension

                               ) )
                           (when ( VStrutsTestRect
                                   CellView
                                   NewRect
                                   ConnectToRect
                                   ConnectToSpacing
                                   ConnectToLPP )
                             ( setq
                               InstanceInfo 
                               ( apply 
                                 GetInstanceInfoFunc
                                 ( list 
                                   NewRect
                                   ) ) ) ) )
                                
                         (when InstanceInfo
                           ( dbCreateRect
                             CellView
                             ConnectToLPP
                             NewRect ) ) ) )

                         ;try using the whole connecttorect
                     (when ( null InstanceInfo )
                       ( setq InstanceInfo
                              ( apply 
                                GetInstanceInfoFunc
                                ( list ConnectToRect ) ) )
                       (when InstanceInfo
                         ( dbCreateRect
                           CellView
                           ConnectFromLPP
                           ConnectToRect )
                         ( dbCreateRect
                           CellView
                           ConnectToLPP
                           ConnectToRect ) )
                       )

                                        ;try adding on some extension
                     (when ( null InstanceInfo )
                       (let (
                             ( NewRect 
                               ( VStrutsGetCrappyHackRect
                                 ConnectFromRect
                                 ConnectToRect
                                 ViaLibraryName
                                 ViaMasterNameFormat	  
                                 ViaViewName
                                 ConnectToLPP
                                 ConnectFromLPP
                                 ViaCutWidth
                                 ViaMinExtension
                                 ViaExtension
                                 ViaXSpacing
                                 ViaYSpacing ) ) )
                         ( setq InstanceInfo
                                ( apply 
                                  GetInstanceInfoFunc
                                  ( list NewRect ) ) )
                         ( dbCreateRect
                           CellView
                           ConnectToLPP
                           NewRect )
                         ( dbCreateRect
                           CellView
                           ConnectFromLPP
                           NewRect ) ) )
                     
                     ( VStrutsDrawViaInstanceInfo
                       CellView
                       InstanceInfo )
                     
                     ) ) ) ) )
        ( list `Func ) ) ) ) )
 
                                                   
(defun VStrutsDrawViaInstanceInfo ( CellView InstanceInfo )
  (when InstanceInfo 
    ( AutoPinCreateInstance 
      CellView 
      InstanceInfo
      nil ) ) )


(defun VStrutsGetViaInstanceInfoForRect ( Rect
                                          ViaLibraryName
                                          ViaMasterNameFormat	  
                                          ViaViewName
                                          ConnectToLPP
                                          ConnectFromLPP
                                          ViaCutWidth
                                          ViaMinExtension
                                          ViaExtension
                                          ViaXSpacing
                                          ViaYSpacing )
  (when Rect
    (let (
          ( ColumnRowPair 
            ( ListFindMaximumElement
              ( setof
                ColumnRowPair
                ( list ( list 1 1 ) ( list 1 2 )
                       ( list 2 1 ) ( list 2 2 ) )
                ( exists 
                  WidthHeightPair 
                  ( ListNonDestructiveMapCan
                    (lambda ( WidthHeightPair )
                      ( list
                        ( list ( plus ( car WidthHeightPair ) ( times 2.0 ViaExtension ) )
                               ( plus ( cadr WidthHeightPair ) ( times 2.0 ViaMinExtension ) ) )
                        ( list ( plus ( car WidthHeightPair ) ( times 2.0 ViaMinExtension ) )
                               ( plus ( cadr WidthHeightPair ) ( times 2.0 ViaExtension ) ) ) ) )
                    ( list 
                      ( list 
                      ( plus ( times ( car ColumnRowPair ) ViaCutWidth )
                             ( times ( difference ( car ColumnRowPair ) 1 ) ViaXSpacing ) )
                      ( plus ( times ( cadr ColumnRowPair ) ViaCutWidth )
                             ( times ( difference ( cadr ColumnRowPair ) 1 ) ViaYSpacing ) ) ) )
                  )
                  ( and 
                    ( lessp ( car WidthHeightPair )
                            ( plus ( RectGetWidth Rect ) 1e-9 ) )
                    ( lessp ( cadr WidthHeightPair )
                            ( plus ( RectGetHeight Rect ) 1e-9  ) ) ) ) )
              (lambda ( ColumnRowPair ) ( plus ( times 1.1 ( car ColumnRowPair ) )
                                               ( times 1.0 ( cadr ColumnRowPair ) ) ) ) ) ) )
      ( println ColumnRowPair )
      (if ColumnRowPair
          ( AutoPinMakeViaInstanceInfo 
            ViaLibraryName
            ViaMasterNameFormat
            nil
            ViaViewName
            ConnectToLPP
            ConnectFromLPP            
            ( RectGetCenter Rect )
            ( plus ViaXSpacing ViaCutWidth )
            ( plus ViaYSpacing ViaCutWidth )
            ?Row ( cadr ColumnRowPair )
            ?Column ( car ColumnRowPair ) ) ) ) ) )


                                        ;in the case the interesection is not big enough for a via, do the crappy thing
                                        ;and make a via justified to outsied edge of ConnectToRect and rect on both layers
                                        ;big enough to satisfy via extension
(defun VStrutsGetCrappyHackRect ( ConnectFromRect
                                  ConnectToRect
                                  ViaLibraryName
                                  ViaMasterNameFormat	  
                                  ViaViewName
                                  ConnectToLPP
                                  ConnectFromLPP
                                  ViaCutWidth
                                  ViaMinExtension
                                  ViaExtension
                                  ViaXSpacing
                                  ViaYSpacing )
  (let (
        ( YOffset ( plus ViaExtension  ( times 0.5 ViaCutWidth ) ) )
        ( XOffset ( plus ( times 2.0 ViaMinExtension ) ( times 0.5 ViaCutWidth ) ) )
        ( YCenter ( RangeGetMidpoint ( RectGetYRange ConnectToRect ) ) ) )
    (let (
          ( FillRect  (cond (
                             ( lessp ( caar ConnectToRect ) ( caar ConnectFromRect ) )
                             ( list 
                               ( list ( caar ConnectToRect ) 
                                      ( difference YCenter YOffset ) )
                               ( list ( caadr ConnectToRect )
                                      ( plus YCenter YOffset ) ) ) )
                            (
                             t
                             ( list 
                               ( list ( caar ConnectToRect )
                                      ( difference YCenter YOffset ) )
                               ( list ( caadr ConnectToRect ) 
                                      ( plus YCenter YOffset ) ) ) ) ) ) )
      FillRect ) ) )





  
(defun VStrutsDrawStruts ( DoGND DoVdd Extent @key (CellView (UIGetCellView)))
  (let ( Instances Gates Chains ConnectFromRects ChainPins GatePins Regions)
     Instances= (if ( equal Extent "All" )
                  ( getq CellView instances )
                ( setof Selected ( geGetSelectedSet )
                        ( equal ( getq Selected objType ) "inst" ) ) )
      ( foreach NetName ( append (if DoGND ( list GNDNetName ) )
                                 (if DoVdd ( list VddNetName ) ) )
                 Regions=( PlacementRegionsScrapeCellRegionDataFromCellView
                   CellView
                   Instances
                   NChainLibCellPairRegExs
                   PChainLibCellPairRegExs
                   GateLibCellPairRegExs
                   NPlugLibCellPairRegExs
                   PPlugLibCellPairRegExs
                   ( times UserUnitsPerMeter EvilGatePRegionOffset )
                   ( times UserUnitsPerMeter MinWellSpacing )
                   CellView->prBoundary
                   0.0
                   ( times UserUnitsPerMeter DefaultWiringSpacing/2)
                  )
                (when !(car Regions) 
                  (printf "Can't find gates to place strut for %s.\n" NetName)
                )

                ( VStrutsDrawVStrutsOnCellView 
                  CellView
                  Regions
                  NetName 
                  Instances
                  ( times UserUnitsPerMeter DefaultWireWidth )
                  ( times UserUnitsPerMeter DefaultWireSpacing )
                  ( times UserUnitsPerMeter DefaultWirePitch )
                  ( times UserUnitsPerMeter M2StrutOffset )
                  Metal1LPP
                  Metal2LPP
                  Metal3LPP
                  ( times UserUnitsPerMeter ContactExtension )
                  ( equal NetName GNDNetName )
                  ) 
      ) 
      ;;;;;;;;;;;;;;;;;;;
      ;;; TSMC28 specific tweaks
      ;;;;;;;;;;;;;;;;;;;
      ;to meet drc, we have to shorten the GND struts away from the prbound,
      ;extend the Vdd struts to extend to the end of the via,
      ;and shrink their width for fat wire spacing
      ( foreach shape CellView->shapes
         ( when (VStrutsIsObject shape) && shape->lpp==Metal2LPP
           shape->bBox = ( RectShrinkInX shape->bBox 0.015 )
           ( when shape->net->name==GNDNetName
              shape->bBox = ( RectShrinkInY shape->bBox 0.01 )
           )
           ( when shape->net->name==VddNetName
              shape->bBox = ( RectShrinkInY shape->bBox -0.035 )
           )
         )
      )
      ;;;;;;;;;;;;;;;;;;;
      ;;;;;;;;;;;;;;;;;;;
  ) 
)



(defun VStrutsDropM12Vias ( DoGND DoVdd Extent @key (CellView (UIGetCellView) ))
  (let ( Instances Gates Chains ConnectFromRects ChainPins GatePins ThisNet)
     Instances= (if ( equal Extent "All" )
                  ( getq CellView instances )
                ( setof Selected ( geGetSelectedSet )
                        ( equal ( getq Selected objType ) "inst" ) ) )
      WellPlugLibCellPairRegExs=list( list( "^stack$" "stack\\.[NP]PLUG\\.0"))
      Gates= ( cadr ( NameFilterInstances
                                 Instances
                                 GateLibCellPairRegExs ) )
      Chains= ( cadr ( NameFilterInstances
                                 Instances
                                 ChainLibCellPairRegExs ) )
      WellPlugs= ( cadr ( NameFilterInstances
                                 Instances
                                 WellPlugLibCellPairRegExs ) )

      ( foreach NetName ( append (if DoGND ( list GNDNetName ) )
                                 (if DoVdd ( list VddNetName ) ) )
        ThisNet=dbMakeNet( CellView NetName )
        ConnectFromRects= ( PinUtilGetPinRectsForCellView 
                          CellView
                          (lambda ( SomeNetName ) ( equal SomeNetName NetName ) )
                          (lambda ( LPP ) ( equal ( car LPP ) ( car Metal2LPP ) ) ) )

        ChainPins= (VStrutsGetExtendedConnectToRectsToFromRects
                              CellView
                              ( PinUtilGetPinRectsForInstances
                                Chains
                                (lambda ( SomeNetName ) ( equal SomeNetName NetName ) )
                                (lambda ( LPP ) ( equal ( car LPP ) ( car Metal1LPP ) ) ) )
                              ConnectFromRects
                              Metal1LPP
                              )
        GatePins= (PinUtilGetPinRectsForInstances
                                Gates
                                (lambda ( SomeNetName ) ( equal SomeNetName NetName ) )
                                (lambda ( LPP ) ( equal ( car LPP ) ( car Metal1LPP ) ) ) )
        WellPlugPins= (PinUtilGetPinRectsForInstances
                                WellPlugs
                                (lambda ( SomeNetName ) ( equal SomeNetName NetName ) )
                                (lambda ( LPP ) ( equal ( car LPP ) ( car Metal1LPP ) ) ) )

        (foreach Stripe ConnectFromRects
          (foreach Pin (append (append GatePins ChainPins ) WellPlugPins )
            (when (RectGetIntersection Stripe Pin)
              Via=(dbCreateVia
                     CellView
                     ( techFindViaDefByName TechLib LeafM12PwrVia )
                     ( list (car (RectGetCenter Stripe)) (cadr (RectGetCenter Pin)) )
                     "R0")
              (VStrutsSetProp Via)
              Via->net=ThisNet
             )
           )
         )

       )
 
))


(defun VStrutsDropM23Vias ( DoGND DoVdd @key (CellView (UIGetCellView)))
  (let ( 
         ConnectFromRects ConnectToRects Intersection Via xCoordVia yCoordVia ThisNet
        )

      ( foreach NetName ( append (if DoGND ( list GNDNetName ) )
                                 (if DoVdd ( list VddNetName ) ) )
         ThisNet=dbMakeNet( CellView NetName )
         ConnectFromRects = ( PinUtilGetPinRectsForCellView 
                          CellView
                          (lambda ( SomeNetName ) ( equal SomeNetName NetName ) )
                          (lambda ( LPP ) ( equal ( car LPP ) ( car Metal2LPP ) ) ) )
         ConnectToRects = ( PinUtilGetPinRectsForCellView 
                          CellView
                          (lambda ( SomeNetName ) ( equal SomeNetName NetName ) )
                          (lambda ( LPP ) ( equal ( car LPP ) ( car Metal3LPP ) ) ) )

         (foreach Strut ConnectFromRects
           (foreach Stripe ConnectToRects
             Intersection=(RectGetIntersection Strut Stripe)
             (when Intersection 

               (if (cadr (car Intersection)) < 2*DefaultWiringPitch ||
                   (cadr (car Intersection)) > (cadr (cadr (GetPrbound CellView)->bBox))-2*DefaultWiringPitch then
                 Via=(dbCreateVia
                        CellView
                        ( techFindViaDefByName TechLib LeafM23EdgePwrVia )			
                        (RectGetCenter Intersection)
                        "R0")
                 (VStrutsSetProp Via)
                 Via->net=ThisNet
               else 
                  Via=(dbCreateVia
                         CellView
                         ( techFindViaDefByName TechLib LeafM23PwrVia )
                         (RectGetCenter Intersection)
                         "R0")
                  (VStrutsSetProp Via)
                  Via->net=ThisNet
               )
             )
           )
         )
      )

      ;;;;;;;;;;;;;;;;;;;
      ;;; TSMC28 specific tweaks
      ;;;;;;;;;;;;;;;;;;;
      ;to meet drc, we have to fix the placement of the vias to be space = 0.08
      (foreach item CellView->vias
        (when (item->viaHeader->viaDefName==LeafM23EdgePwrVia || item->viaHeader->viaDefName==LeafM23PwrVia) && 
              (VStrutsIsObject item)		
		yCoordVia=(cadr item -> origin)
		xCoordVia=(car item -> origin)
                ;except we don't have to at the edges because those struts were already shortened
	if(yCoordVia<2*DefaultWiringPitch then
		    item->origin = (list xCoordVia (yCoordVia - 0.005) ))
	if(yCoordVia>(cadr (cadr (GetPrbound CellView)->bBox))-2*DefaultWiringPitch then
		    item->origin = (list xCoordVia (yCoordVia + 0.005) ))
                (unless yCoordVia<2*DefaultWiringPitch || 
                        yCoordVia>(cadr (cadr (GetPrbound CellView)->bBox))-2*DefaultWiringPitch
		  (if (yCoordVia-(floor yCoordVia/PowerGridPitch)*PowerGridPitch) < PowerGridPitch/2 then 
		    item->origin = (list xCoordVia (yCoordVia + 0.005) ) 
		  else
		    item->origin = (list xCoordVia (yCoordVia - 0.005) )  
	          )
                )
  	 )
       ) 
      ;;;;;;;;;;;;;;;;;;;
      ;;;;;;;;;;;;;;;;;;;

  )
)


(defun VStrutsDeleteObjects ( @key (CellView (UIGetCellView) ))
  (let ( Shapes)
    (foreach shape CellView->shapes
      (when (VStrutsIsObject shape)
        (dbDeleteObject shape)
      )
    )
    (foreach shape CellView->vias
      (when (VStrutsIsObject shape)
        (dbDeleteObject shape)
      )
    )
  )
)


(defun ConnectLeafPower ( @key (cv (UIGetCellView)) )
  (VStrutsDrawStruts t t "All")
  (VStrutsDropM12Vias t t "All")
  (VStrutsDropM23Vias t t)
)

