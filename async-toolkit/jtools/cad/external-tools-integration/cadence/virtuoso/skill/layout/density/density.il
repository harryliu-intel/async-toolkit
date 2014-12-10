; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

/*DOC
<h2> Description </h2
Determines transistor counts, density factor for leaf cells.<br>
Determines area usage for mid level cells based on prBoundary layer.<br>

<h2> References </h2>
Used by <tool density>.
Uses <skill AssuraGetAreas> for mid level cell layer operations.

<h2> Output format </h2>
<dl>
<dt> leaf cells
<dd> <pre> [cellname] density: %g area: %g transistor_width: %g count(count,cdl_transistors,layout_transistors) gate: %d %d %d chain: %d %d %d flat: %d %d %d.</pre>
Where:
<dl>
<dt> density 
<dd> (total transistor gate area/prBoundary area)
<dt> area
<dd> total transistor gate area
<dt> transistor width
<dd> The total of all transistor gate widths
<dt> count
<dd> transistor counts.  cdl_transistors is the number of transistors from the schematic that were implemented.  layout_transistors is the total number of transistors drawn by gates/stacks.  The counts are then broken up by whether they were found in gates, chains, or flat nmos/pmos.
</dl>
<dt> mid-level cells 
<dd> <pre>[cellname] cover '%s leaf: %g merge: %g bound: %g subcell: %g box: %g'.</pre>
<dl>
<dt> leaf
<dd> The leaf cell bounding box area.
<dt>  merge 
<dd> The area of the bloated-merged-shrunk leaf cell boundaries.  Bloating is by one bitpitch.
<dt> bound
<dd> The area of the top level bounding box.
<dt> subcell
<dd> The area of the subcell bounding boxes.
<dt> box 
<dd> The area of the top level bounding box.
</dl>
</dl>
*/

(defun DensityCalculateTotalTransistorCount ( CellView 
                                              GateLibCellPairRegExs
                                              ChainLibCellPairRegExs
                                              TransistorLibCellPairRegExs )
  "(cdl gate transistor count, actual gate transistor count) \
   (cdl chain transistor count, actual chain transistor count) \
   (cdl flat transistor count, actual flat transistor count)"
  (let (
        ( GateFilter ( NameFilterInstances
                       ( getq CellView instances )
                       GateLibCellPairRegExs ) ) )
    (let (
          ( Gates ( cadr GateFilter ) )
          ( TransistorFilter 
            ( NameFilterInstances 
              ( car GateFilter )
              TransistorLibCellPairRegExs ) ) )
      (let (
            ( Chains
              ( cadr
                ( NameFilterInstances 
                  ( car TransistorFilter )
                  ChainLibCellPairRegExs ) ) )
            ( Transistors ( cadr TransistorFilter ) ) )
        (let (
              ( InstanceMap
                ( NameMakeInstanceMapFromInstances 
                  Chains
                  ChainLibCellPairRegExs ) ) )
        ( list
          ( list
            ( length Gates )
            ( ListFindSum
              Gates
              (lambda ( Gate )
                ( DensityGetUnfoldedComponentCount
                  Gate
                  TransistorLibCellPairRegExs ) ) )
            ( ListFindSum
              Gates
              (lambda ( Gate )
                ( DensityGetTotalComponentCount
                  Gate
                  TransistorLibCellPairRegExs ) ) ) )
          ( list
            ( length 
              ( NameGetCanonicalInstanceNames
                InstanceMap ) )
            ( ListFindSum
              ( NameGetCanonicalInstanceNames
                InstanceMap )
              (lambda ( CanonicalName )
                ( DensityGetUnfoldedComponentCount 
                  ( car ( NameGetInstancesForCanonicalName 
                          InstanceMap
                          CanonicalName ) )
                  TransistorLibCellPairRegExs ) ) )
            ( ListFindSum
              Chains
              (lambda ( Chain )
                ( DensityGetTotalComponentCount
                  Chain
                  TransistorLibCellPairRegExs ) ) ) )
          ( list ( length Transistors ) 0 ( length Transistors ) ) ) ) ) ) ) )



(defun DensityCalculateTotalTransistorWidth
  ( CellView
    ComponentLibCellPairRegExs
    TransistorLibCellPairRegExs
    DefaultTransistorLength
    BaseTransistorWidth
    UserUnitsPerMeter
    PolyLPP
    DiffLPP
    )

  (let (
        ( ComponentFilter ( NameFilterInstances
                            ( getq CellView instances )
                            ComponentLibCellPairRegExs ) ) )
    (let (
          ( Components ( cadr ComponentFilter ) )
          ( TransistorFilter ( NameFilterInstances 
                               ( car ComponentFilter )
                               TransistorLibCellPairRegExs ) ) )
      (let (
            ( Transistors ( cadr TransistorFilter ) ) )
        (let (
              ( PolyShapes ( setof Shape ( getq CellView shapes )
                                   ( equal ( getq Shape lpp ) PolyLPP ) ) )
              ( DiffShapes ( setof Shape ( getq CellView shapes )
                                   ( equal ( getq Shape lpp ) DiffLPP ) ) ) )
          (let (
                ( Easy
                  ( plus
                    ( ListFindSum
                      Components
                      (lambda ( Component )
                        ( DensityGetTotalComponentWidth
                          Component
                          DefaultTransistorLength
                          BaseTransistorWidth
                          TransistorLibCellPairRegExs
                          ) )
                      )
                    ( ListFindSum
                      Transistors
                      (lambda ( Transistor )
                        ( DensityGetTotalTransistorWidth
                          Transistor
                          BaseTransistorWidth
                          UserUnitsPerMeter ) ) ) ) )
                ( Hard
                  ( DensityGetTotalDiffPolyArea
                    DiffShapes
                    PolyShapes
                    BaseTransistorWidth ) ) )
            (when ( geqp Hard 0.0 )
              ( plus Easy Hard ) ) ) ) ) ) ) )

(defun DensityCalculateTotalTransistorArea
  ( CellView
    ComponentLibCellPairRegExs
    TransistorLibCellPairRegExs
    DefaultTransistorLength
    BaseTransistorWidth
    UserUnitsPerMeter
    PolyLPP
    DiffLPP
    )

  (let (
        ( ComponentFilter ( NameFilterInstances
                            ( getq CellView instances )
                            ComponentLibCellPairRegExs ) ) )
    (let (
          ( Components ( cadr ComponentFilter ) )
          ( TransistorFilter ( NameFilterInstances 
                               ( car ComponentFilter )
                               TransistorLibCellPairRegExs ) ) )
      (let (
            ( Transistors ( cadr TransistorFilter ) ) )
        (let (
              ( PolyShapes ( setof Shape ( getq CellView shapes )
                                   ( equal ( getq Shape lpp ) PolyLPP ) ) )
              ( DiffShapes ( setof Shape ( getq CellView shapes )
                                   ( equal ( getq Shape lpp ) DiffLPP ) ) ) )
          (let (
                ( Easy
                  ( plus
                    ( ListFindSum
                      Components
                      (lambda ( Component )
                        ( DensityGetTotalComponentArea 
                          Component
                          DefaultTransistorLength
                          BaseTransistorWidth
                          TransistorLibCellPairRegExs
                          ) )
                      )
                    ( ListFindSum
                      Transistors
                      (lambda ( Transistor )
                        ( DensityGetTotalTransistorArea 
                          Transistor
                          BaseTransistorWidth
                          UserUnitsPerMeter ) ) ) ) )
                ( Hard
                  ( DensityGetTotalDiffPolyArea 
                    DiffShapes
                    PolyShapes
                    BaseTransistorWidth ) ) )
            (when ( geqp Hard 0.0 )
              ( plus Easy Hard ) ) ) ) ) ) ) )



(defun DensityGetUnfoldedComponentCount ( Component 
                                          TransistorLibCellPairRegExs )
  (let (
        ( N 
          ( PropGetPropValueForPropName ( getq ( getq Component master ) prop ) "ncount" ) )
        ( P
          ( PropGetPropValueForPropName ( getq ( getq Component master ) prop ) "pcount" ) ) )
    (if ( and N P ) ( plus N P ) -1e9 ) ) )


(defun DensityGetTotalComponentCount ( Component TransistorLibCellPairRegExs )
  ( length
    ( cadr 
      ( NameFilterInstances
        ( getq ( getq Component master ) instances )
        TransistorLibCellPairRegExs ) ) ) )

(defun DensityGetTotalComponentArea ( Component
                                      DefaultTransistorLength
                                      BaseTransistorWidth
                                      TransistorLibCellPairRegExs )
  ( ListFindSum
    ( cadr
      ( NameFilterInstances
        ( getq ( getq Component master ) instances )
        TransistorLibCellPairRegExs ) )
    (lambda ( Transistor )
      ( DensityGetTotalTransistorArea 
        Transistor 
        BaseTransistorWidth
        UserUnitsPerMeter ) ) ) )

(defun DensityGetTotalComponentWidth ( Component
                                       DefaultTransistorLength
                                       BaseTransistorWidth
                                       TransistorLibCellPairRegExs )
  ( ListFindSum
    ( cadr
      ( NameFilterInstances
        ( getq ( getq Component master ) instances )
        TransistorLibCellPairRegExs ) )
    (lambda ( Transistor )
      ( DensityGetTotalTransistorWidth
        Transistor 
        BaseTransistorWidth
        UserUnitsPerMeter ) ) ) )


(defun DensityGetTotalTransistorArea ( Transistor 
                                       BaseTransistorWidth
                                       UserUnitsPerMeter )
  ( times
    ( DensityGetTotalTransistorWidth 
      Transistor 
      BaseTransistorWidth
      UserUnitsPerMeter )
    ( DensityGetTotalTransistorLength
      Transistor 
      UserUnitsPerMeter ) ) )

(defun DensityGetTotalTransistorLength ( Transistor 
                                         UserUnitsPerMeter )
  (let (
        ( Length
          ( StringUtilParseLengthString
            ( PCellGetParameterValue Transistor "l" ) ) ) )
    (cond (
           ( greaterp Length 1e-3 )
           ( quotient Length UserUnitsPerMeter ) )
          (
           Length ) ) ) )

(defun DensityGetTotalTransistorWidth ( Transistor 
                                        BaseTransistorWidth
                                        UserUnitsPerMeter )
  (let (
        ( Width
          ( StringUtilParseLengthString
            ( PCellGetParameterValue Transistor "w" ) ) ) )
    ( plus 
      (cond (
             ( greaterp Width 1e-3 )
             ( quotient Width UserUnitsPerMeter ) )
            (
             Width ) )        
      BaseTransistorWidth ) ) )


(defun DensityGetTotalDiffPolyArea ( DiffShapes PolyShapes  BaseTransistorWidth )
  (let (
        ( Ret 0.0 )
        ( TotalArea 0.0 ) )
    ( foreach DiffShape DiffShapes
              (let (
                    ( PolyShape ( car ( exists PolyShape PolyShapes 
                                               ( RectDoRectsOverlap 
                                                 ( getq DiffShape bBox )
                                                 ( getq PolyShape bBox ) ) ) ) ) )
                (when PolyShape
                  ( println ( list DiffShape PolyShape ) )
                  ( setq TotalArea ( plus TotalArea -1e9
                                          /*( DensityGetTotalDiffPolyAreaOne
                                              DiffShape
                                              PolyShape
                                              BaseTransistorWidth )*/
                                          ) ) ) ) )
    TotalArea 
    ) )

(defun DensityGetTotalDiffPolyAreaOne ( DiffShape
                                        PolyShape
                                        BaseTransistorWidth )
  (cond (
         ( RangeIsRangeInRangeProperClose
           ( RectGetXRange ( getq DiffShape bBox ) )
           ( RectGetXRange ( getq PolyShape bBox ) ) )
         ( times
                                        ;width
           ( plus BaseTransistorWidth
                  ( RectGetHeight
                    ( getq DiffShape bBox ) ) )
                                        ;length
           (cond (
                  ( equal ( getq PolyShape objType ) "rect" )
                  ( RectGetHeight ( getq PolyShape bBox ) ) )
                 (
                  ( equal ( getq PolyShape objType ) "path" )
                  ( getq PolyShape width ) )
                 (
                  ( equal ( getq PolyShape objType ) "polygon" )
                  ( PolygonGetShortestEdge
                    ( getq PolyShape points ) ) ) ) ) )
        (
         ( RangeIsRangeInRangeProperClose
           ( RectGetYRange ( getq DiffShape bBox ) )
           ( RectGetYRange ( getq PolyShape bBox ) ) )
         ( times
           ( plus BaseTransistorWidth 
                  ( RectGetHeight
                    ( getq DiffShape bBox ) ) )
           (cond (
                  ( equal ( getq PolyShape objType ) "rect" )
                  ( RectGetWidth ( getq PolyShape bBox ) ) )
                 (
                  ( equal ( getq PolyShape objType ) "path" )
                  ( getq PolyShape width ) )
                 (
                  ( equal ( getq PolyShape objType ) "polygon" )
                  ( PolygonGetShortestEdge
                    ( getq PolyShape points ) ) ) ) ) )
        (
         0.0 ) ) )

(defun DensityFactor ( CellView
                       ComponentLibCellPairRegExs
                       TransistorLibCellPairRegExs
                       UserUnitsPerMeter
                       DefaultTransistorLength
                       BaseTransistorWidth
                       BoundaryLPP
                       PolyLPP
                       DiffLPP )
  (let (
        ( TransistorArea
          ( DensityCalculateTotalTransistorArea 
            CellView
            ComponentLibCellPairRegExs
            TransistorLibCellPairRegExs
            DefaultTransistorLength
            BaseTransistorWidth
            UserUnitsPerMeter
            PolyLPP
            DiffLPP ) ) )
    (cond (
           ( not ( numberp TransistorArea ) )
           nil )
          (
           ( leqp TransistorArea 0.0 )
           t )
          (
           t
           ( quotient 
             ( RectGetArea
               ( PinUtilFindPRBoundBBox
                 BoundaryLPP
                 CellView ) )
             ( times
               UserUnitsPerMeter
               UserUnitsPerMeter
               TransistorArea ) ) ) ) ) )

(defun DensityCoverGetCoverOld ( CellView
                                     LPP 
                                     LeafCellPredicate  )
  ( list
    ( RectGetAreaOfUnion
      ( MidLevelGetRects
        CellView 
        LPP
        LeafCellPredicate 
        ) )
    ( RectGetArea ( getq CellView bBox ) )
    ( RectGetArea ( getq CellView bBox ) )
    ) )

(defun DensityCoverGetCover ( CellView
                              LPP 
                              LeafCellPredicate )
  ( AssuraGetAreas
    CellView
    LPP
    LeafCellPredicate
    ) )

(defun DensityFactorVerboseUsingPDKInfo ( CellView 
                                          BaseTransistorWidth )

  (let (
        ( Area ( RectGetArea 
                 ( getq CellView bBox ) ) ) )
    (cond (
           ( cadr ( NameFilterInstances
                    ( getq CellView instances )
                    ( append
                      ( append
                        GateLibCellPairRegExs
                        ChainLibCellPairRegExs )
                      TransistorLibCellPairs ) ) )
           (let (
                 ( Density
                   ( DensityFactor
                     CellView
                     ( append
                       GateLibCellPairRegExs
                       ChainLibCellPairRegExs )
                     TransistorLibCellPairs
                     UserUnitsPerMeter
                     DefaultTransistorLength
                     BaseTransistorWidth
                     BoundaryLPP
                     PolyLPP
                     DiffLPP ) )
                 ( TWidth
                   ( times
                     UserUnitsPerMeter
                     ( DensityCalculateTotalTransistorWidth
                       CellView
                       ( append
                         GateLibCellPairRegExs
                         ChainLibCellPairRegExs )
                       TransistorLibCellPairs
                       DefaultTransistorLength
                       BaseTransistorWidth
                       UserUnitsPerMeter

                     PolyLPP
                     DiffLPP ) ) )
                 ( Count 
                   ( DensityCalculateTotalTransistorCount 
                     CellView 
                     GateLibCellPairRegExs
                     ChainLibCellPairRegExs
                     TransistorLibCellPairs ) )
                 )
             (cond (
                    ( numberp Density )
                    ( sprintf
                      nil
                      "%s density: %L area: %L transistor_width: %L count(count,cdl_transistors,layout_transistors) gate: %d %L %d chain: %d %d %d flat: %d %d %d"
                      ( getq CellView cellName )
                      Density
                      Area
                      TWidth
                      ( round ( car ( car Count ) ) )
                      (cond ( ( geqp ( cadr ( car Count ) ) 0 )
                              ( cadr ( car Count ) ) 0 )
                            ( "?" ) )
                      ( round ( caddr ( car Count ) ) )
                      ( round ( car ( cadr Count ) ) )
                      ( round ( cadr ( cadr Count ) ) )
                      ( round ( caddr ( cadr Count ) ) )
                      ( round ( car ( caddr Count ) ) )
                      ( round ( cadr ( caddr Count ) ) )
                      ( round ( caddr ( caddr Count ) ) )

                      ) )
                   (
                    ( equal Density nil )
                    ( sprintf nil "%s has flattened transistors...not supported" 
                              ( getq CellView cellName ) )
                    )
                   (
                    t
                    ( sprintf nil "%s has no transistors" 
                              ( getq CellView cellName ) )
                    ) ) ) )
          (
           t
           (let (
                 ( Cover
                   ( DensityCoverGetCover
                     CellView
                     BoundaryLPP
                     (lambda ( CellView )
                       ( null 
                         ( car ( NameFilterInstances
                                 ( getq CellView instances )
                                 LeafCellInstanceLibCellPairRegExs ) ) ) )

                     ) ) )
             ( sprintf nil "%s cover leaf: %L merge: %L bound: %L subcell: %L box: %L"
                       ( getq CellView cellName )
                       ( car Cover )
                       ( cadr Cover )
                       ( caddr Cover )
                       ( cadddr Cover )
                       Area
                       ) ) ) ) ) )
