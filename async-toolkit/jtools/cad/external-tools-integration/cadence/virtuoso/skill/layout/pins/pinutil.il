; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

(defun PinUtilGetValidLayer ( LPP ViaLayerRegEx )
  ( cadr 
    ( PinUtilGetValidLayers LPP ViaLayerRegEx ) ) )

(defun PinUtilGetValidLayers ( LPP ViaLayerRegEx )
  (cond (
         ( rexMatchp ViaLayerRegEx ( car LPP ) )
         (let (
               ( Digit ( rexSubstitute "\\1" ) ) )
           (if ( equal Digit "" )
               ( list
                 ( list "POLYG" ( cadr LPP ) )
                 ( list "METAL1" ( cadr LPP ) )
                 )
             ( list 
               ( list ( strcat "METAL"
                               ( sprintf nil "%d" ( difference ( atoi Digit ) 1 ) ) )
                      ( cadr LPP ) )
               ( list ( strcat "METAL" Digit ) ( cadr LPP ) )
               ) ) ) )
        (
         t
         ( list LPP LPP ) ) ) )

(defun PinUtilGetLayoutCellView ( CellName Permission )
  (let (
        ( LibCellPair ( NameParseCellName CellName ) ) )
    ( dbOpenCellViewByType
      ( car LibCellPair )
      ( cadr LibCellPair )
      "layout"
      "maskLayout"
      Permission ) ) )

(defun PinUtilGetNetListCellView ( CellName Permission )
  (let (
        ( LibCellPair ( NameParseCellName CellName ) ) )
    ( dbOpenCellViewByType
      ( car LibCellPair )
      ( cadr LibCellPair )
      "netlist"
      nil
      Permission ) ) )

(defun PinUtilFindPRBoundBBox ( PRBoundaryLPP TargetCellView )
  ( getq ( PinUtilFindPRBoundShape PRBoundaryLPP TargetCellView ) bBox )
)

(defun PinUtilFindPRBoundShape ( PRBoundaryLPP TargetCellView )  
  ( car 
    ( sort 
      ( setof Shape ( getq TargetCellView shapes ) ( equal ( getq Shape lpp ) PRBoundaryLPP ) )
      ( lambda ( Shape1 Shape2 ) ( lessp ( difference ( RectGetArea ( getq Shape2 bBox ) )
                                                      ( RectGetArea ( getq Shape1 bBox ) ) ) 0  ) ) ) ) )

(defun PinUtilGetBoundaryPolygonEdgesFromCellView ( PRBoundaryLPP TargetCellView )
  (cond (
         ( PinUtilGetBoundaryPolygonEdgesFromShape
           ( PinUtilFindPRBoundShape
             PRBoundaryLPP
             TargetCellView ) ) )
        (
         t
         ( dbComputeBBox TargetCellView )
         ( BBoxGetPolygonEdges ( getq TargetCellView bBox ) ) ) ) )



(defun PinUtilGetBoundaryPolygonEdgesFromShape ( PRBoundaryShape )
  (cond ( 
         ;polygon
          ( getq  PRBoundaryShape points )			    
          ( PolygonGetEdges ( getq PRBoundaryShape points ) ) )
         (
          ( getq PRBoundaryShape bBox )
          ;bbox
          ( BBoxGetPolygonEdges ( getq PRBoundaryShape bBox ) ) ) ) )
                 

(defun PinUtilIsShapeInPlaceForNet ( Shape Net )
  ( exists InstTerm ( getq Net allInstTerms )
           ( exists Shape ( getq ( getq InstTerm inst ) shapes )
                    (let (
                          ( BBox1 ( dbTransformBBox ( getq Shape bBox ) ( getq ( getq InstTerm inst ) transform ) ) )
                          ( BBox2 ( getq Shape bBox ) ) )                              
                      ( BBoxClose BBox1 BBox2 1e-10 ) ) ) ) )



(defun PinUtilGetAllShapesOnLPP ( CellView TheLPP )
  ( getq ( car ( exists LPP ( getq CellView lpps )
                        ( and ( equal ( getq LPP layerName ) ( car TheLPP ) )
                              ( equal ( getq LPP purpose ) ( cadr TheLPP ) ) ) ) )
         shapes ) )

;;;;;;;;;;;;;;;;;;;;;;;;
;; pitchwidthpair <-> rect

(defun PinUtilGetHorizontalPitchWidthPairForRect ( Rect 
                                                   BoundaryPolygonEdges
                                                   WirePitch
                                                   WirePitchOffset )
  (let (
        ( PinHeight ( RectGetHeight Rect ) )
        ( YPosition ( cadar Rect ) )
        ( XPosition ( caar Rect ) ) )
    (let (
          ( BoundaryBottomEdge 
            (cond (
                   ( PolygonGetBottomMostEdgeAtX BoundaryPolygonEdges XPosition ) )
                  (
                   ( PolygonGetBottomMostEdge BoundaryPolygonEdges ) ) ) ) )
      (let (
            ( BoundaryBottom ( float ( LineGetSegmentMaxY BoundaryBottomEdge ) ) ) )
        (let (
              ( Pitch ( floor ( quotient ( difference YPosition BoundaryBottom WirePitchOffset )
                                         WirePitch ) ) )
              ( Width ( ceiling ( quotient PinHeight WirePitch ) ) ) )
          ( list Pitch Width ) ) ) ) ) )

(defun PinUtilGetVerticalPitchWidthPairForRect ( Rect 
                                                 BoundaryPolygonEdges
                                                 WirePitch
                                                 WirePitchOffset )
  
  (let (
        ( PinWidth ( RectGetWidth Rect ) )
        ( YPosition ( cadar Rect ) )
        ( XPosition ( caar Rect ) ) )
    (let (
          ( BoundaryLeftEdge 
            (cond (
                   ( PolygonGetLeftMostEdgeAtY BoundaryPolygonEdges YPosition ) )
                  (
                   ( PolygonGetLeftMostEdge BoundaryPolygonEdges ) ) ) ) )
      (let (
            ( BoundaryLeft ( float ( LineGetSegmentMaxX BoundaryLeftEdge ) ) ) )
        (let (
              ( Pitch ( floor ( quotient ( difference XPosition BoundaryLeft WirePitchOffset )
                                         WirePitch ) ) )
              ( Width ( ceiling ( quotient PinWidth WirePitch ) ) ) )
          ( list Pitch Width ) ) ) ) ) )



(defun PinUtilGetYRangeForHorizontalPitch ( Pitch
                                            NumPitches
                                            ActualWireWidth
                                            WirePitch
                                            WirePitchOffset
                                            BoundaryPolygonEdges )
  (let (
        ( BoundaryBottom
          ( LineGetSegmentMaxY
            ( PolygonGetBottomMostEdge BoundaryPolygonEdges ) ) ) )
    (let (
          ( PinVerticalMidpoint 
            ( plus BoundaryBottom
               WirePitchOffset
               ( times 0.5 ( plus ( times WirePitch Pitch )
                                  ( times WirePitch ( plus Pitch NumPitches ) ) ) ) ) ) )
      (let (
            ( PinBottom ( difference PinVerticalMidpoint ( times 0.5 ActualWireWidth ) ) )
            ( PinTop ( plus PinVerticalMidpoint ( times 0.5 ActualWireWidth ) ) ) )
        ( list PinBottom PinTop ) ) ) ) )



(defun PinUtilGetXRangeForVerticalPitch ( Pitch
                                          NumPitches
                                          ActualWireWidth
                                          WirePitch
                                          WirePitchOffset
                                          BoundaryPolygonEdges )
  (let (
        ( BoundaryLeft
          ( LineGetSegmentMaxX  
            ( PolygonGetLeftMostEdge BoundaryPolygonEdges ) ) ) )
    (let (
          ( PinHorizontalMidpoint 
            ( plus BoundaryLeft
                   WirePitchOffset
                   ( times 0.5 ( plus ( times WirePitch Pitch )
                                      ( times WirePitch ( plus Pitch NumPitches ) ) ) ) ) ) )
      (let (
            ( PinLeft ( difference PinHorizontalMidpoint ( times 0.5 ActualWireWidth ) ) )
            ( PinRight ( plus PinHorizontalMidpoint ( times 0.5 ActualWireWidth ) ) ) )
        ( list PinLeft PinRight ) ) ) ) )



;note - this treats wirewidth from a directive pov, ie.e
;width = wirewidth + ( width in pitches - 1 ) * WirePitch
(defun PinUtilGetYRangeForHorizontalPitchWidthPair ( PitchWidthPair
                                                     WirePitch
                                                     WireWidth
                                                     WirePitchOffset
                                                     BoundaryPolygonEdges )
  (  PinUtilGetYRangeForHorizontalPitch 
     ( car PitchWidthPair )
     ( cadr PitchWidthPair )
     ( plus ( times ( difference ( cadr PitchWidthPair ) 1 ) WirePitch ) WireWidth )
     WirePitch
     WirePitchOffset
     BoundaryPolygonEdges ) )

(defun PinUtilGetXRangeForVerticalPitchWidthPair ( PitchWidthPair
                                                   WirePitch
                                                   WireWidth
                                                   WirePitchOffset
                                                   BoundaryPolygonEdges )
  (  PinUtilGetXRangeForVerticalPitch 
     ( car PitchWidthPair )
     ( cadr PitchWidthPair )
     ( plus ( times ( difference  ( cadr PitchWidthPair ) 1 ) WirePitch ) WireWidth )     
     WirePitch
     WirePitchOffset
     BoundaryPolygonEdges ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun PinUtilGetPinRectsForCellView ( CellView
				       NetNamePredicate
				       LPPPredicate )
  ( mapcar
    (lambda ( Fig )
	( getq Fig bBox ) )
  ( PinUtilGetPinFigsForCellView	
    CellView
    NetNamePredicate
    LPPPredicate ) ) )

(defun PinUtilGetPinFigsForCellView ( CellView
                                      NetNamePredicate
                                      LPPPredicate )
  ( setof Shape ( getq CellView shapes )
         ( and ( getq Shape pin )
               ( apply LPPPredicate ( list ( getq Shape lpp ) ) )
               ( apply NetNamePredicate ( list ( getq ( getq ( getq Shape pin ) net ) name ) ) ) ) ) )

(defun PinUtilGetPinRectsForInstances ( Instances
                                        NetNamePredicate
                                        LPPPredicate )
  ( ListNonDestructiveMapCan
    ( lambda ( Instance )
      ( PinUtilGetPinRectsForInstance
        Instance
        NetNamePredicate
        LPPPredicate ) )
    Instances ) )

(defun PinUtilGetPinRectsForInstance ( Instance
                                       NetNamePredicate
                                       LPPPredicate
                                       @key ( TerminalP nil )
                                       )
  ( mapcar 
    (lambda ( Shape )
      (let (
            ( RealShape
              ( AutoPinGetSmallestBoundingShape
                ( getq Instance master )
                Shape
                ?LPPPredicate LPPPredicate ) ) )
        (when RealShape
          ( dbTransformBBox 
            ( getq
              RealShape
              bBox )
            ( getq Instance transform ) ) ) ) )
    ( PinUtilGetPinFigsForInstance 
      Instance
      NetNamePredicate
      LPPPredicate
      ?TerminalP TerminalP ) ) )
                                      

(defun PinUtilGetPinFigsForInstance ( Instance
                                      NetNamePredicate
                                      LPPPredicate
                                      @key ( TerminalP nil )
                                      )
  ( setof 
    Fig 
    ( mapcar 
      (lambda ( Pin )
        ( getq Pin fig ) )
      ( ListNonDestructiveMapCan
        (lambda ( InstTerm )
          ( getq ( getq InstTerm term ) pins ) )
        ( setof InstTerm ( getq Instance conns )
                ( and
                  ( or ( null TerminalP )
                       ( apply 
                         TerminalP
                         ( list ( getq ( getq InstTerm term ) name ) ) ) )
                  ( apply 
                    NetNamePredicate 
                    ( list ( getq ( getq InstTerm net ) name ) ) ) ) ) ) )
    ( and Fig ( apply LPPPredicate ( list ( getq Fig lpp ) ) ) ) ) )


(defun PinUtilGetPCellParams ( Instance )
  ( getq ( car ( exists Property ( getq ( getq ( getq Gate master ) superMaster ) prop ) ( equal ( getq Property name ) "parameters" ) ) ) "value" ) )

(defun PinUtilIsLabelForNetName ( Shape NetName )
  ( and ( equal ( getq Shape objType ) "label" )
	( equal ( getq Shape theLabel ) NetName ) ) )

;;;;;;;;;;;;;;;;

(defun PinUtilGetConnectedPinInstancePairs_1 ( ConnectivityCellView LayoutCellView LayoutInstanceMap NetName )
  ( ListNonDestructiveMapCan 
    ( lambda ( TerminalInstancePair )   
      ( mapcar ( lambda ( Pin ) 
                 ( list Pin ( cadr TerminalInstancePair ) ) )
               ( getq ( dbFindTermByName ( getq  ( cadr TerminalInstancePair ) master )
                                         ( car TerminalInstancePair ) ) pins ) ) )
           ( PinUtilGetConnectedTerminalInstancePairs ConnectivityCellView LayoutCellView LayoutInstanceMap NetName ) ) )

(defun PinUtilGetConnectedTerminalInstancePairs ( ConnectivityCellView LayoutCellView LayoutInstanceMap NetName )
  (let (
        ( Net ( dbFindNetByName ConnectivityCellView NetName ) ) )
    ( ListNonDestructiveMapCan 
      ( lambda ( InstTerm )
        ( mapcar ( lambda ( Instance ) ( list ( getq ( getq InstTerm term ) name ) Instance ) )
                 ( NameGetInstancesForCanonicalName
                   LayoutInstanceMap             
                   ( getq ( getq InstTerm inst ) name ) ) ) )
      ( getq Net allInstTerms ) ) ) )

;;;;;;;;;;;;;;;

(defun PinUtilGetConnectedPinInstancePairs_2 ( ConnectivityCellView LayoutCellView LayoutInstanceMap NetName )
  ( ListNonDestructiveMapCan 
    ( lambda ( InstTerm )   
      ( mapcar ( lambda ( Pin ) 
                 ( list Pin ( getq InstTerm inst ) ) )
               ( getq ( getq InstTerm term ) pins ) ) )
           ( PinUtilGetConnectedInstTerms ConnectivityCellView LayoutCellView LayoutInstanceMap NetName ) ) )

(defun PinUtilGetConnectedInstances ( ConnectivityCellView LayoutCellView LayoutInstanceMap NetName )
  (let (
        ( Net ( dbFindNetByName ConnectivityCellView NetName ) ) )
    ( ListNonDestructiveMapCan 
      ( lambda ( InstTerm )
        ( NameGetInstancesForCanonicalName
          LayoutInstanceMap             
          ( getq ( getq InstTerm inst ) name ) ) )
      ( getq Net allInstTerms ) ) ) )

(defun PinUtilGetConnectedInstTerms ( ConnectivityCellView LayoutCellView LayoutInstanceMap NetName ) 
  ( ListNonDestructiveMapCan 
    ( lambda ( Instance )
      ( setof InstTerm ( getq Instance conns ) 
              ( equal ( getq ( getq InstTerm net ) name ) NetName ) ) )
    ( PinUtilGetConnectedInstances ConnectivityCellView LayoutCellView LayoutInstanceMap NetName ) ) )

;;;;;;;;;;;;;;;;


;centers pin on the correct handle if rod, centers to center of bbox of shape if no rod
(defun PinUtilCenterChildLabels ( TargetCellViews BoundaryPolygonEdges )
  ( foreach Fig ( PinUtilGetExistingFigs TargetCellView )
	    ( foreach Label ( setof Shape ( getq Fig children )
				    ( equal ( getq Shape objType ) "label" ) )
                      ( dbSetq Label 
                               ( BBoxGetCenter ( getq Fig bBox ) )
                               xy )
                      ( dbSetq Label  
                               ( PinUtilGetJustification
                                 BoundaryPolygonEdges
                                 ( getq Fig bBox )
                                 Label  ) 
                                 justify ) ) ) )

(defun PinUtilGetJustification ( BoundaryPolygonEdges 
                                 ParentBBox
                                 Label  )
  ( dbSetq Label "centerCenter" justify ) 

  (let (
	( BoundaryLeft 
	   ( LineGetSegmentMaxX 
	     ( PolygonGetLeftMostEdge BoundaryPolygonEdges ) ) )
	 ( BoundaryRight 
	   ( LineGetSegmentMinX  
	     ( PolygonGetRightMostEdge BoundaryPolygonEdges ) ) )   
	 ( BoundaryBottom 
	   ( LineGetSegmentMaxY 
	     ( PolygonGetBottomMostEdge BoundaryPolygonEdges ) ) )
	 ( BoundaryTop
	   ( LineGetSegmentMinY 
	     ( PolygonGetTopMostEdge BoundaryPolygonEdges ) ) )
	 ( BBox ( getq Label bBox ) )
	 )
 
      ( cond (
					;too far left
	      ( leqp ( caar BBox ) BoundaryLeft )	   	    
	      "leftCenter"
	      )
	     (
					;too far right
	      ( geqp ( caadr BBox ) BoundaryRight )	   	    
	      "rightCenter" 
	      )
	     (
	      "centerCenter"
	      )
	     ) ) ) 



(defun PinUtilGetExistingPinFigs ( TargetCellView )
  ( append ( PinUtilGetExistingPinInstances TargetCellView )
           ( PinUtilGetExistingPinShapes TargetCellView ) ) )

(defun PinUtilGetExistingFigs ( TargetCellView )
  ( append ( getq TargetCellView instances )
           ( getq TargetCellView shapes ) ) )

(defun PinUtilGetExistingPinInstances ( TargetCellView ) 
  ( setof Instance ( getq TargetCellView instances ) ( getq Instance pin  ) ) )

(defun PinUtilGetExistingPinShapes ( TargetCellView )
  "returns a list of pininfos for each existing pin in cellview "  
    ( setof Shape ( getq TargetCellView shapes ) ( getq Shape pin ) ) )

;deletes all pin shapes and symbolic pins
(defun PinUtilDeleteExistingPins ( TargetCellView )
  ( foreach Pin ( PinUtilGetExistingPinFigs TargetCellView ) 
            ( printf "Deleting pin for net: %s" ( getq ( getq Pin net ) name ) )
	    ( dbDeleteObject Pin ) ) )

(defun PinUtilFixPinAccess ( TargetCellView )
  ( foreach PinFig ( PinUtilGetExistingPinFigs TargetCellView ) 
            ( dbSetq ( getq PinFig pin ) (list "left" "right" "top" "bottom" ) accessDir ) ) )

(defun PinUtilGetPinsForNetName ( TargetCellView NetName )
  ( let (Pins) 
  ( foreach Pin ( PinUtilGetExistingPinFigs TargetCellView )
            ( when ( equal Pin->net->name NetName ) 
              ( setq Pins ( cons Pin Pins ) ) ) )
  Pins ) )

(defun PinUtilDeleteM2PowerPins ( TargetCellView )
  ( let (Pins)
  ( setq Pins ( PinUtilGetPinsForNetName TargetCellView "GND" ) )
  ( setq Pins ( append Pins ( PinUtilGetPinsForNetName TargetCellView "Vdd" ) ) )
  ( foreach Pin Pins 
            ( when ( equal Pin->lpp Metal2LPP )
              ( dbDeleteObject Pin->pin ) ) ) ) )


; delete pins and labels
(defun DeletePins (@key (CV (geGetEditCellView)))
  (foreach obj CV->shapes
           (cond ((or obj->pin obj->objType=="label")
                  (dbDeleteObject obj))
                 )
           )
  t
  )

(defun RecursiveDeletePins (@key (CV (geGetEditCellView)))
  (let (uniqueInstances cell)
    (DeletePins ?CV CV)
    uniqueInstances=nil
    (foreach inst CV->instances
             (if !(member inst->cellName uniqueInstances->cellName ) then
                 uniqueInstances= (cons inst uniqueInstances )
                 )
             )
    (foreach inst uniqueInstances
             cell = (nrOpenCellViewWritable inst->libName inst->cellName inst->viewName)
             (when cell 
               (RecursiveDeletePins ?cv cell)
               )
             )
    )
  )

(defun DeletePinsByType (type @key (CV (geGetEditCellView)))
  (foreach obj CV->shapes
           (when obj->pin
             (when (dbGetPropByName obj "PinType")->value == type
                   (dbDeleteObject obj)
                   )
             )
           )
  t
  )

(defun DeletePinsExcludeType (type @key (CV (geGetEditCellView)))
  (let (to_delete)
    to_delete = (makeTable "to_delete" nil)
    (foreach obj CV->shapes
             (when obj->pin
               (when (dbGetPropByName obj "PinType")->value != type
                     to_delete[obj] = t
                     )
               )
             )
    (foreach x to_delete (dbDeleteObject x))
    )
  t
  )

(defun DeletePinsByPurpose (purpose @key (CV (geGetEditCellView)))
  (foreach obj CV->shapes
           (when obj->pin
             (when (cadr obj->lpp) == purpose
                   (dbDeleteObject obj)
                   )
             )
           )
  t
  )

(defun PinUtilDeletePinsByType (view type)
  (let (obj)
    (foreach obj view->shapes
     (when ( obj->pin )
       (when ( dbGetPropByName obj "PinType" )->value == type
         (dbDeleteObject obj)
       )
     )
    )
  )
  t )

defun( PinUtilDeleteAllSubPins (view)
  let( (obj inst subCellView)
    (foreach obj view->shapes
             (cond ((or obj->pin obj->objType=="label")
                    (dbDeleteObject obj))
                   )
             )
    foreach( inst view~>instances
       subCellView=nrOpenCellViewWritable( inst~>libName inst~>cellName inst~>viewName )
       if( subCellView then PinUtilDeleteAllSubPins( subCellView )
       else printf("error open cell for write: %s %s %s" view~>libName view~>cellName view~>viewName ))
    )
    dbSave(view)
  )
)

; Rename pin, label, shape, and connected instance terms
(defun PinUtilRenamePin (cv fig newname)
 (let (oldnet oldname newnet newpin)
   oldnet = fig->net
   oldname = oldnet->name
   (when oldname && newname && oldname!=newname

         ; rename pins
         (printf "Renaming pin %s to %s\n" oldname newname)
         newnet = (dbMakeNet cv newname)
         newpin = (dbCreatePin newnet fig)
         newpin->term->direction = "inputOutput"

         ; rename labels and shape connectivity
         (foreach shape cv->shapes
                  (when shape->objType=="label" && shape->theLabel==oldname
                        shape->theLabel = newname        
                        )
                  (when shape->net==oldnet
                    shape->net=nil
                    shape->net=newnet
                    )
                  )

         ; rename instTerms
         (foreach term oldnet->instTerms
                  term->net=nil
                  term->net=newnet
                  )
         )
   )
 )

(defun PinUtilOrientLabels (cv)
 (let (labels)
  (foreach shape cv->shapes
    (when shape->objType=="label"
      (when (IsInList shape->lpp HorizontalMetalLPPs)
        shape->orient="R0"
      )
      (when (IsInList shape->lpp VerticalMetalLPPs)
        shape->orient="R90"
      )
    )
  )
 )
)
