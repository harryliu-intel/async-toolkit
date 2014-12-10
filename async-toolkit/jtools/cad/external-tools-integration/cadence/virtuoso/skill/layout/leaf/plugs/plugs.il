; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

/*DOC
Code for drawing well plugs
The basic procedure is:
<ol>
<li>Use <a href=$src/hw/layout/nv90/pdk/Fulcrum/plugs/plugs.assura.rules>assura rules</a> to flood fill plug mosiacs everyhwere that is inside well but far aenoguh from diff/poly/m1/etc..
<li>Inline mosiacs into strips of plugs on left/right boundary
<li>Sort plugs by distance from edge(we want them to be close to components for density reasons).
<li>Add the best plugs (according to the sorting) until they cover the whole column.
<li>Throw away plugs that redundantly cover part of the column.
</ol>

<h3> Data Structures </h3>
<dl>
<dt> PlugInfo
<dd> (bbox (lib cell view))
</dl>
*/

(defun PlugsForceConnectPlug ( Plug CellView NetName TerminalName )
  "Deletes existing connections/nets on TerminalName (e.g created by virtuoso XL) and connects TerminalName to NetName"
  (let (
        ( ExistingInstTerm 
          ( car ( exists 
                  InstTerm
                  ( getq Plug conns )
                  ( equal 
                    ( getq ( getq InstTerm term ) name )
                    TerminalName ) ) ) ) )
    (when ( and 
            ExistingInstTerm 
            ( not 
              ( equal
                ( getq ( getq ExistingInstTerm net ) name )
                NetName ) ) )
      ( dbDeleteObject ( getq ExistingInstTerm net ) )
      ( dbDeleteObject ExistingInstTerm ) )
    ( dbCreateConnByName
      ( dbMakeNet CellView NetName )
      Plug
      TerminalName ) ) )



(defun PlugsClean ( Plugs 
                    MinWellSpacing )
  "Greedily deletes Plugs that are closer thatn MinWellSpacing"
  (while 
      (let (
            ( PlugToRemove 
              ( car
                ( exists
                  Plug
                  Plugs
                  ( exists
                    OtherPlug
                    Plugs
                    ( and
                      ( not
                        ( equal
                          Plug
                          OtherPlug ) )
                      ( equal ( getq Plug cellName )
                              ( getq OtherPlug cellName ) )
                      ( lessp 
                        ( RectGetDistanceFromRectToRect
                          ( getq Plug bBox )
                          ( getq OtherPlug bBox ) )
                        MinWellSpacing ) ) ) ) ) ) )
        (when PlugToRemove
          ( setq Plugs ( remove PlugToRemove Plugs ) )
          ( dbDeleteObject PlugToRemove )
          t ) )
    t ) )

(defun PlugsInlineMosaic ( Mosaic )
  "Inlines a mosaic into a list of PlugInfos.  Keeps only the left and right most two columns of the mosaics."
  (let (
        ( UX ( getq Mosaic uX ) )
        ( UY ( getq Mosaic uY ) )
        ( XY ( getq Mosaic xy ) )
        ( Instance ( car ( getq Mosaic instanceList ) ) ) )
    (let (
          ( Info
            ( list
              ( getq ( getq Instance master ) libName )
              ( getq ( getq Instance master ) cellName )
              ( getq ( getq Instance master ) viewName ) ) ) )
      ( ListNonDestructiveMapCan
        (lambda ( X ) X )
        ( ListFillList
          (lambda ( I )
            ( list
              ( list
                ( RectMakeFromCenter
                  ( PointAdd
                    XY
                    ( list ( times ( difference ( getq Mosaic columns ) 1 ) UX )
                           ( times ( difference I 1 ) UY ) ) )
                  ( RectGetWidth ( getq Instance bBox ) )
                  ( RectGetHeight ( getq Instance bBox ) ) )
                Info )
              ( list
                ( RectMakeFromCenter
                  ( PointAdd
                    XY
                    ( list 0
                           ( times ( difference I 1 ) UY ) ) )
                  ( RectGetWidth ( getq Instance bBox ) )
                  ( RectGetHeight ( getq Instance bBox ) ) )
                Info ) ) )
          ( getq Mosaic rows ) ) ) ) ) )
        
(defun PlugsDeleteUnNeededPlugs ( CellView
                                  NRegionGatePlugRects
                                  PRegionGatePlugRects
                                  NPlugLibCellPairRegExs
                                  PPlugLibCellPairRegExs
                                  CellRegionData
                                  PlugEffectiveRadius
                                  MinWellSpacing )
  
  (let ( 
        ( MosaicsToKeep nil )
        ( PlugInfosToKeep nil )
        ( NMosaics ( setof Mosaic ( getq CellView mosaics )
                           ( cadr ( NameFilterInstances
                                    ( getq Mosaic instanceList )
                                    NPlugLibCellPairRegExs ) ) ) )
        ( PMosaics ( setof Mosaic ( getq CellView mosaics )
                           ( cadr ( NameFilterInstances
                                    ( getq Mosaic instanceList )
                                    PPlugLibCellPairRegExs ) ) ) )
        ( IsFirst t )
        ( Regions ( PlacementRegionsGetRealRegions
                       CellRegionData ) )
        )

    (while Regions
      (let (
            ( RegionRect ( PlacementRegionGetRealRegionBBox
                           ( car Regions ) ) )
            ( IsP ( PlacementRegionGetRealRegionIsP
                    ( car Regions ) ) )
            ( IsLast ( null ( cdr Regions ) ) )
            )
        (let (
              ( GatePlugRects
                ( setof Rect (if IsP
                                 PRegionGatePlugRects
                               NRegionGatePlugRects )
                        ( RectIsRectInRectClose 
                          Rect
                          RegionRect
                          1e-9 ) ) )
              ( Mosaics
                ( setof Mosaic (if IsP 
                                   PMosaics              
                                 NMosaics )
                        ( and ( getq Mosaic bBox )
                              ( RectDoRectsOverlap
                                ( getq Mosaic bBox )
                                RegionRect ) ) ) ) )
          (let (
                ;inline mosaics in column into PlugInfos
                ( PlugsInfos
                  ( ListNonDestructiveMapCan
                    (lambda ( Mosaic )
                      ( PlugsInlineMosaic 
                        Mosaic
                        ) )
                    Mosaics ) ) )
            ;printf("PlugsInfo %L\n" PlugsInfos)
            ;determine which ones to keep
            ( setq PlugInfosToKeep 
                   ( append 
                     PlugInfosToKeep 
                     ( PlugsGetPlugsToKeep
                       PlugsInfos
                       GatePlugRects
                       RegionRect
                       PlugEffectiveRadius
;                       ( sqrt 
;                         ( min 2.0 
;                               ( difference
                                        ;                                 ( times PlugEffectiveRadius
;                                         PlugEffectiveRadius )
;                                 ( times ( RectGetWidth RegionRect )
;                                         ( RectGetWidth RegionRect ) ) ) ) )
                       MinWellSpacing
                       IsFirst
                       IsLast
                       ) ) )
            ;printf("PlugInfoToKeep %L\n" PlugInfosToKeep)
            
            ( setq Regions ( cdr Regions ) )
            ( setq IsFirst nil )
            ) ) ) )

    ;delete all mosaics
    ( foreach Mosaic ( getq CellView mosaics )
              ( dbDeleteObject Mosaic ) )

    ;instantiate the needed plugs from PlugInfos
    (let (
          ( Plugs
            ( mapcar
              (lambda ( PlugInfo ) 
                ( dbCreateInstByMasterName
                  CellView
                  ( nth 0 ( cadr PlugInfo ) )
                  ( nth 1 ( cadr PlugInfo ) )
                  ( nth 2 ( cadr PlugInfo ) )
                  nil
                  ( RectGetCenter
                    ( car PlugInfo ) )
                  "R0"
                  1 ) )
              PlugInfosToKeep
              ) ) )
      ;delete plug that violate minimum well spacing 
      ( PlugsClean
        Plugs
        MinWellSpacing )
      Plugs
      )
    ) )

(defun PlugInfoDistanceFromEdge ( PlugInfo
                                    Rect
                                    IsFirst 
                                    IsLast )
  (cond (
         IsFirst
         ( abs
           ( difference 
             ( RectGetRight Rect )
             ( car ( RectGetCenter ( car PlugInfo ) ) ) ) ) )
        (
         IsLast
         ( abs
           ( difference 
             ( RectGetLeft Rect )
             ( car ( RectGetCenter ( car PlugInfo ) ) ) ) ) )
        (
         t
         ( min
           ( abs
             ( difference 
               ( RectGetLeft Rect )
               ( car ( RectGetCenter ( car PlugInfo ) ) ) ) )
           ( abs
             ( difference 
               ( RectGetRight Rect )
               ( car ( RectGetCenter ( car PlugInfo ) ) ) ) ) )
           ) ) )

(defun PlugsGetPlugsToKeep ( PlugInfos
                             GatePlugRects
                             Rect
                             PlugEffectiveRadius
                             MinWellSpacing
                             IsFirst
                             IsLast
                             )
  "Greedily remove redundant plugs, and return the remaining PlugInfos."
  ;only look at plugs that are enough from edge to be legal
 
;   PlugInfos =
;          ( setof PlugInfo PlugInfos
;                  ( geqp ( RectGetDistanceFromRectToSurroundingRect 
;                           ( car PlugInfo )
;                           Rect )
;                         MinWellSpacing ) )  
    (let (
          ( RangeCompare
            (lambda ( Range1 Range2 )
            ( lessp
              ( PlugInfoDistanceFromEdge
                ( arrayref PlugTable Range1 )
                Rect IsFirst IsLast)
              ( PlugInfoDistanceFromEdge
                ( arrayref PlugTable Range2 )
                Rect IsFirst IsLast) ) ) )
          )
      (let (
            ( RangeToCover ( RectGetYRange Rect ) )
            ( PlugTable ( makeTable `bla nil ) )
            ( GatePlugRanges 
              ( mapcar 
                (lambda ( GatePlugRect )
                  ( RangeIntersection
                    ( RangeExpandRange
                      ( RectGetYRange GatePlugRect )
                      PlugEffectiveRadius )
                    ( RectGetYRange Rect ) ) )
                GatePlugRects ) ) )
        ;create a map from Range->PlugInfo
        ;keep only the plugs closest to edge 
        ( foreach
          PlugInfo
          PlugInfos
          (let (
                ( Range 
                  ( RangeIntersection
                    ( RangeExpandRange
                      ( RectGetYRange ( car PlugInfo ) )
                      PlugEffectiveRadius )
                    RangeToCover ) ) )
            (let (
                  ( OtherPlugInfo
                    ( arrayref PlugTable Range ) ) )
              ( setarray 
                PlugTable
                Range
                (if ( and 
                      OtherPlugInfo
                      ( lessp
                        ( PlugInfoDistanceFromEdge
                          OtherPlugInfo Rect IsFirst IsLast ) 
                        ( PlugInfoDistanceFromEdge
                          PlugInfo Rect IsFirst IsLast ) ) )
                    OtherPlugInfo 
                  PlugInfo ) ) ) ) )


        (let (
              ( NeededRanges nil )
              ( Ranges ( sort 
                         ( mapcar 
                           `car 
                           ( tableToList PlugTable ) )
                         RangeCompare ) ) )
          ;add ranges until we cover the RangeToCover
          (while ( and Ranges
                       ( RangeGetAllRangesNotCovered
                         RangeToCover
                         ( append GatePlugRanges NeededRanges )
                         ) )
            ( setq NeededRanges ( cons ( car Ranges ) NeededRanges ) )
            ( setq Ranges ( cdr Ranges ) ) )
      ;greedily throw away ranges that are redundant   
      (let (
            ( RangesToKeep 
              ( ListFindBestNonRedundantElementsPreSorted
                NeededRanges
                (lambda ( Range OtherRanges )
                  ( RangeIsRangeContainedInUnionOfRangesClose
                    Range
                    ( append 
                      GatePlugRanges
                      OtherRanges )
                    1e-9 ) ) ) ) )
        ;map back to PlugInfos
        (let (
              ( PlugInfosToKeep
                ( mapcar
                  (lambda ( Range )
                    ( arrayref PlugTable Range ) )
                  RangesToKeep ) ) )
          PlugInfosToKeep
          ) ) ) ) ) ) 



(defun PlugsAlignToContacts ( Plugs 
                              Contacts
                              CellView
                              CellRegionData
                              ImplantLPPs
                              MinWellSpacing )
  "attempt to move plugs next to a power contact"
  ( foreach
    Column
    ( PlacementRegionsGetCellRegionDataColumnList
      CellRegionData )
    (let (
          ( Plugs ( setof Plug Plugs
                          ( RectIsRectInRectClose
                            ( getq Plug bBox )
                            ( PlacementRegionGetColumnBBox Column )
                            1e-9 ) ) )
          ( Contacts ( setof Contact Contacts
                             ( RectIsRectInRectClose
                               Contact
                               ( PlacementRegionGetColumnBBox Column )
                               1e-9 ) ) ) )
      ( foreach
        Plug
        Plugs
        (let (
              ( XY ( getq Plug xy ) )
              ( Contact
                ( ListFindMinimumElement
                  ( setof 
                    Contact
                    Contacts
                    ( equal
                      ( lessp 
                        ( car ( RectGetCenter Contact ) )
                        ( PlacementRegionGetColumnNPJunction
                          Column ) )
                      ( lessp 
                        ( car ( RectGetCenter ( getq Plug bBox ) ) )
                        ( PlacementRegionGetColumnNPJunction
                          Column ) ) ) )
                  (lambda ( Contact )
                    ( abs 
                      ( difference
                        ( cadr ( RectGetCenter Contact ) )
                        ( cadr ( RectGetCenter ( getq Plug bBox ) ) ) ) ) ) ) ) )
          (when Contact
            ( dbSetq 
              Plug 
              ( list 
                ( car XY )
                ( cadr ( RectGetCenter Contact ) ) )
              xy )
            (when 
                ( or 
                  ( lessp 
                    ( RectGetDistanceFromRectToSurroundingRect
                      ( getq Plug bBox )
                      ( PlacementRegionGetColumnBBox Column ) )
                    MinWellSpacing )
                  ( exists
                    LPP ImplantLPPs
                    ( exists
                      Overlap
                      ( dbGetTrueOverlaps
                        CellView
                        ( getq Plug bBox )
                        LPP
                        0:32 
                        )
                      (if ( listp Overlap )
                          ( equal ( car Overlap ) Plug )
                        ( equal Overlap Plug )
                        ) ) ) )
              ( dbSetq 
                Plug
                XY
                xy ) ) ) ) ) ) ) )





;IMPORTANT: here, nplug means the plug that goes in the n region
(defun PlugsDrawPlugsOnCellView ( CellView
                                  CellRegionData
                                  TempDir
                                  RuleFile
                                  GNDNetName
                                  VddNetName
                                  NRegionGatePlugRects
                                  PRegionGatePlugRects
                                  PlugLibName
                                  M1_SUB
                                  M1_NWELL
                                  PlugViewName
                                  PlugLPP
                                  NImplantLPP
                                  PImplantLPP
                                  PlugEffectiveRadius
                                  MinWellSpacing
                                  @key
                                  ( DrawPlugs t )
                                  ( DrawImplant nil )
                                  ( LeaveMess nil )
                                  )
  (let (
        ( TechLibFile ( techGetTechFileName CellView ) )
        ( TempLibName
          ( LibCreateTempLibraryFromCellView CellView "PLUGS" TempDir ) )
        ( ErrorStr nil ) )
    ( setq
      ErrorStr
      ( AssuraRunAssuraLayerProcessor
        CellView
        TempLibName
        ( getq CellView cellName )
        ( getq CellView viewName )
        RuleFile
        TempDir
        ( append
          (when DrawPlugs
            ( list 
              ( list "nimp_add" NImplantLPP )
              ( list "pimp_add" PImplantLPP )
              ( list "M1_NWELL_fill" PlugLPP )
              ( list "M1_SUB_fill" PlugLPP )
              ) )
          (when DrawImplant
            ( list 
              ( list "nimp_add" NImplantLPP )
              ( list "pimp_add" PImplantLPP ) ) ) )
        (when DrawPlugs
          ( list
            ( list
              ( list
                PlugLibName
                M1_NWELL
                PlugViewName )
              "M1_NWELL_fill" )
            ( list
              ( list
                PlugLibName
                M1_SUB
                PlugViewName )
              "M1_SUB_fill" ) ) )
        ?LeaveMess LeaveMess
        ) )
    (unless ErrorStr
      (let (
            ( CellWithGrid 
              ( dbOpenCellViewByType
                TempLibName
                ( getq CellView cellName )
                ( getq CellView viewName )
                nil
                "a" ) ) )
        (if CellWithGrid
            ;remove redundant plugs
            (let (
                  ( Plugs
                    ( PlugsDeleteUnNeededPlugs
                      CellWithGrid
                      NRegionGatePlugRects
                      PRegionGatePlugRects
                      ( list 
                        ( list PlugLibName
                               M1_SUB ) )
                      ( list
                        ( list PlugLibName
                               M1_NWELL ) )
                      CellRegionData
                      PlugEffectiveRadius
                      MinWellSpacing ) )
                  )
              
                
              ;Copy the plugs to new cellview
printf( "plugs %L \n" Plugs)
              (when DrawPlugs
                ( foreach
                  Plug
                  Plugs
                  ( dbCopyFig Plug CellView ) ) )
              
              ;or the implant
              (when DrawImplant
                ( AssuraCopyShapesFromCellView
                  CellWithGrid
                  CellView
                  (lambda (Shape)
                    ( or
                      ( equal ( getq Shape lpp ) NImplantLPP )
                      ( equal ( getq Shape lpp ) PImplantLPP ) ) ) ) ) )
          ( setq 
            ErrorStr
            ( sprintf
              nil
              "%L %L %L does not exist."
              TempLibName
              ( getq CellView cellName )
              ( getq CellView viewName ) ) ) ) ) )
    (unless LeaveMess ( ddDeleteObj ( ddGetObj TempLibName ) ) )
    ( techBindTechFile CellView TechLibFile )
    ErrorStr ) )
