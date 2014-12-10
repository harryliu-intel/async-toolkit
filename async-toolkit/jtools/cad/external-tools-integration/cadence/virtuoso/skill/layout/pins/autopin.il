                                        ; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
                                        ; $Id$
                                        ; $DateTime$
                                        ; $Author$



                                        ;Label Info
(defun AutoPinMakeLabelInfo ( LPP )
  ( list LPP )
  )

(defun AutoPinGetLPPFromLabelInfo ( LabelInfo ) 
  ( car LabelInfo ) )

                                        ; Rect Info
(defun AutoPinMakePinRectInfo ( LayerPurposePair BBox LabelInfo MakePin ObjType )
  ( list LayerPurposePair BBox LabelInfo MakePin ObjType ) )

(defun AutoPinGetLPPFromRectInfo ( RectInfo )
  ( car RectInfo ) )

(defun AutoPinGetRectFromRectInfo ( RectInfo ) 
  ( cadr RectInfo ) )

(defun AutoPinGetLabelInfoFromRectInfo ( RectInfo ) 
  ( caddr RectInfo ) )

(defun AutoPinGetMakePinFromRectInfo ( RectInfo ) 
  ( nth 3 RectInfo ) )

(defun AutoPinGetObjTypeFromRectInfo ( RectInfo ) 
  ( nth 4 RectInfo ) )

                                        ;Pin Info
(defun AutoPinMakePinInfo ( NetName PinName RectInfoList InstanceInfoList @key (InPlace t) )
  ( list NetName PinName RectInfoList InstanceInfoList InPlace ) )

(defun AutoPinGetNetNameFromPinInfo ( PinInfo )
  ( car PinInfo ) )

(defun AutoPinGetPinNameFromPinInfo ( PinInfo )
  ( cadr PinInfo ) )

(defun AutoPinGetRectInfoListFromPinInfo ( PinInfo )
  ( caddr PinInfo ) )

(defun AutoPinGetInstanceInfoListFromPinInfo ( PinInfo )
  ( cadddr PinInfo ) )

(defun AutoPinGetInPlaceInfoFromPinInfo ( PinInfo )
  ( car ( cddddr PinInfo ) )
  )

                                        ;InPlace Pin Info

(defun AutoPinMakeInPlaceInfo ( LayerPurposePair BoundaryPolygonEdges MakePin )
  ( list LayerPurposePair BoundaryPolygonEdges MakePin ) )

(defun AutoPinMakeInPlacePinInfo ( NetName PinName InPlaceInfo )
  ( list NetName PinName nil nil InPlaceInfo  ) )

(defun AutoPinIsInPlacePinInfo ( PinInfo )
  ( not ( equal ( AutoPinGetInPlaceInfoFromPinInfo PinInfo ) t ) ) )

(defun AutoPinGetLPPFromInPlacePinInfo ( InPlacePinInfo )
  ( car  ( AutoPinGetInPlaceInfoFromPinInfo InPlacePinInfo ) ) )

(defun AutoPinGetBoundaryPolygonEdgesFromInPlacePinInfo ( InPlacePinInfo )
  ( cadr ( AutoPinGetInPlaceInfoFromPinInfo InPlacePinInfo ) ) )

(defun AutoPinGetMakePinFromInPlacePinInfo ( InPlacePinInfo )
  ( caddr ( AutoPinGetInPlaceInfoFromPinInfo InPlacePinInfo ) ) )


                                        ;reserved pitches
(defun AutoPinIsPitchReserved ( ReservedPitchTable Pitch )
  ( neq ( arrayref ReservedPitchTable Pitch ) `unbound )
  )

                                        ; PinTable
(defun AutoPinQueuePinInfo ( PinTable PinInfo )
                                        ;( printf "%s\n" ( AutoPinGetNetNameFromPinInfo PinInfo ) )
  ( setarray PinTable ( AutoPinGetPinNameFromPinInfo PinInfo ) PinInfo ) )

                                        ; Instance Info
(defun AutoPinMakeInstanceInfo ( LibraryName MasterName ViewName Suffix Location AlignObj LabelInfo MakePin PCellParams )
  ( list ( list LibraryName MasterName ViewName Suffix ) ( list Location AlignObj LabelInfo MakePin PCellParams  ) ) )

(defun AutoPinGetLibraryNameFromInstanceInfo ( InstanceInfo )
  ( car ( car InstanceInfo ) ) )

(defun AutoPinGetMasterNameFromInstanceInfo ( InstanceInfo )
  ( cadr ( car InstanceInfo ) ) )

(defun AutoPinGetViewNameFromInstanceInfo ( InstanceInfo )
  ( caddr ( car InstanceInfo ) ) )

(defun AutoPinGetSuffixFromInstanceInfo ( InstanceInfo )
  ( cadddr ( car InstanceInfo ) ) )


(defun AutoPinGetLocationFromInstanceInfo ( InstanceInfo )
  ( car ( cadr InstanceInfo ) ) )

(defun AutoPinGetAlignObjFromInstanceInfo ( InstanceInfo )
  ( cadr ( cadr InstanceInfo ) ) )

(defun AutoPinGetLabelInfoFromInstanceInfo ( InstanceInfo )
  ( caddr ( cadr InstanceInfo ) ) )

(defun AutoPinGetMakePinFromInstanceInfo ( InstanceInfo )
  ( cadddr ( cadr InstanceInfo ) ) )

(defun AutoPinGetPCellParamsFromInstanceInfo ( InstanceInfo )
  ( car ( cddddr ( cadr InstanceInfo ) ) ) )



                                        ;Functions
(defun AutoPinGetName ( PinName Suffix RectCount )
  ( sprintf nil "%s.%s%d" PinName Suffix RectCount )
  )


(defun AutoPinIsPinInNet (PinName NetName)
  ( let (
	 ( AppendedNetName ( strcat NetName ".") )
	 )
    ( equal ( substring PinName 1 ( strlen AppendedNetName) ) AppendedNetName )
    )
  )



(defun AutoPinGetExistingPinTable ( TargetCellView ) 
  (let (
        ( AllPins ( PinUtilGetExistingPinFigs TargetCellView ) )
        ( ExistingPinTable ( makeTable `ExistingPinTable nil ) )
        )   
    ( foreach Pin AllPins 
              (let (
                    ( NetName ( getq ( getq Pin net ) name ) ) )
                (let (
                      ( PinsInNet ( arrayref ExistingPinTable NetName ) )            
                      )        
                  ( setarray ExistingPinTable NetName ( cons Pin PinsInNet ) ) ) ) )
    ExistingPinTable ) )


(defun AutoPinGetCellHeight ( BoundaryPolygonEdges XPosition)
  (let (      
        ( BoundaryBottomEdge ( PolygonGetBottomMostEdgeAtX BoundaryPolygonEdges XPosition ) ) 
        ( BoundaryTopEdge    ( PolygonGetTopMostEdgeAtX BoundaryPolygonEdges XPosition ) ) 
        )
    (let (
          ( BoundaryBottom ( float ( LineGetSegmentMaxY BoundaryBottomEdge ) ) ) 
	  ( BoundaryTop    ( float ( LineGetSegmentMinY BoundaryTopEdge ) ) ) 	  
          )
          
      ( MathRoundToNearest ( difference BoundaryTop BoundaryBottom ) 1e-4 ) ) ) )

(defun AutoPinGetCellWidth ( BoundaryPolygonEdges YPosition )
  (let (      
        ( BoundaryLeftEdge ( PolygonGetLeftMostEdgeAtY BoundaryPolygonEdges YPosition ) ) 
        ( BoundaryRightEdge ( PolygonGetRightMostEdgeAtY BoundaryPolygonEdges YPosition ) ) 
        )
    (let (
          ( BoundaryLeft ( float ( LineGetSegmentMaxX BoundaryLeftEdge ) ) ) 
	  ( BoundaryRight ( float ( LineGetSegmentMinX BoundaryRightEdge ) ) ) 	  
          )
      ( MathRoundToNearest ( difference BoundaryRight BoundaryLeft ) 1e-4 ) ) ) )

                                        ;Instances
(defun AutoPinUpdateInstance ( Instance Location )  
  ( dbSetq Instance Location xy )
  )

(defun AutoPinCreateInstance ( TargetCellView 
                               InstanceInfo
                               InstanceName
                               ) 
  (cond (
         PCellParams 
          ( dbCreateParamInstByMasterName 
            TargetCellView
            ( AutoPinGetLibraryNameFromInstanceInfo InstanceInfo )
            ( AutoPinGetMasterNameFromInstanceInfo InstanceInfo )
            ( AutoPinGetViewNameFromInstanceInfo InstanceInfo )		    
            InstanceName
            ( AutoPinGetLocationFromInstanceInfo InstanceInfo )
            "R0"
            1
            ( AutoPinGetPCellParamsFromInstanceInfo InstanceInfo ) ) )
        (
         t
         ( dbCreateInstByMasterName
           TargetCellView
           ( AutoPinGetLibraryNameFromInstanceInfo InstanceInfo )
           ( AutoPinGetMasterNameFromInstanceInfo InstanceInfo )
           ( AutoPinGetViewNameFromInstanceInfo InstanceInfo )
           InstanceName
           ( AutoPinGetLocationFromInstanceInfo InstanceInfo )
           "R0"
           1
           ) ) ) )



(defun AutoPinCreateLabel ( TargetCellView LabelInfo NetName Fig )
  (let (
        ( Rect ( getq Fig bBox ) )
        )
    (when LabelInfo
      (let (
            ( Text
              ( dbCreateLabel
                TargetCellView			    
                ( AutoPinGetLPPFromLabelInfo LabelInfo ) 			       
                ( FigGetValidPoint Fig )
                NetName
                "centerCenter" 
                "R0" 
                "stick" 
                ( min ( BBoxGetHeight Rect ) LeafPinLabelHeight )
                )
              )
            )
        ( dbSetq Text Fig parent ) ) ) ) )

(defun AutoPinUpdateLabel ( Label LabelInfo )
  ( dbSetq Label ( AutoPinGetLPPFromLabelInfo LabelInfo ) lpp ) )

(defun AutoPinCreateConnectivity ( TargetCellView NetName Fig )
  (let (
        ( Net ( dbMakeNet TargetCellView NetName ) ) )
    (let (
          ( Term (cond (
                        ( getq Net term )
                        )
                       (
                        ( dbCreateTerm Net NetName "inputOutput" ) ) ) ) )
      (let ( 
            ( Pin ( dbCreatePin Net Fig ) ) )
        ( dbSetq Pin (list "left" "right" "top" "bottom" ) accessDir ) ) ) ) )


                                        ; rod Rects
(defun AutoPinMoveShape ( Shape BBox )
  (let (
	( OldBBox ( getq Shape bBox ) ) )
                                        ;move it ( so labels move )
    ( dbMoveFig Shape nil ( list ( list ( difference ( caar BBox ) ( caar OldBBox ) )
					( difference ( cadar BBox ) ( cadar OldBBox ) )
                                        )
				 "R0"
                                 ) ) ) )

(defun AutoPinUpdateRodShape ( RodObject LPP BBox ) 
  (let (
        ( Shape ( getq RodObject dbId ) ) )
    ( dbSetq Shape BBox bBox )          
    ( AutoPinMoveShape Shape BBox )  
    ;reset lpp
    ( dbSetq Shape LPP lpp ) 
    ) )

(defun AutoPinCreateRodShape ( TargetCellView PinName NetName LayerPP Rect )
  "Draws a rect and creates a pin that is strongly connected to other pins in the terminal"
                                        ; draw the shape unless there's already a shape with the same name       
  (while ( not ( rodIsFigNameUnused PinName TargetCellView ) )
    ( setq PinName ( sprintf nil "%s.0" PinName ) ) )  
  ( rodCreateRect
    ?name PinName
    ?layer LayerPP
    ?bBox Rect
    ?cvId TargetCellView		      
    ) )

(defun AutoPinGetViaMasterName ( LibraryName MasterNameFormat Layer1 Layer2 )
  "guesses the name of the cellview master for a via connecting LPP1 to LPP2"
  ( let (
	 ( TestStrings ( list "METAL" "met" "M" ) )
	 ( MasterName nil )
	 ( MetalNum1 nil )
	 ( MetalNum2 nil )
	 ( Found nil )
         )
    (while TestStrings
      ( sscanf Layer1 ( strcat ( car TestStrings ) "%d" ) MetalNum1 )
      ( sscanf Layer2 ( strcat ( car TestStrings ) "%d" ) MetalNum2 )
      (when ( and MetalNum1 MetalNum2 )
        (cond (		       		     
               ( ddGetObj LibraryName ( sprintf MasterName MasterNameFormat MetalNum1 MetalNum2 ) ) )
              (
               ( ddGetObj LibraryName ( sprintf MasterName MasterNameFormat MetalNum2 MetalNum1 ) ) ) ) )
      ( setq TestStrings ( cdr TestStrings ) )
      )
    MasterName
    )	      
  )

(defun AutoPinMakeViaInstanceInfo ( LibraryName
				    MasterNameFormat
                                    Suffix
				    ViewName				     				    
				    VerticalLayerPP
				    HorizontalLayerPP
				    Location
				    Width 
				    Height
				    @key
				    ( Row 1 )
				    ( Column 1 )
                                    )				
  ( let (
	 ( MasterName ( AutoPinGetViaMasterName LibraryName MasterNameFormat ( car HorizontalLayerPP) ( car VerticalLayerPP ) ) ) 
         )    
    ( AutoPinMakeInstanceInfo 
      LibraryName       
      MasterName
      ViewName 
      Suffix
      Location
      nil
      nil
      nil
      ( list ( list "xPitch" "float" Width ) 
	     ( list "yPitch" "float" Height ) 
	     ( list "row" "int" Row ) 
	     ( list "column" "int" Column )    
             )
      )
    )
  )

(defun AutoPinUpdateInstanceInfoList ( TargetCellView NetName PinName InstanceInfoList PinInstancesInNet AddOnly )
  ( setq ExistingInstanceList nil )
  ( foreach PinInstance PinInstancesInNet           
            ( cond (                                   
                    ( AutoPinIsPinInNet ( getq PinInstance name ) PinName )                  
                    ( setq ExistingInstanceList ( cons PinInstance ExistingInstanceList ) ) ) ) )
  
  (let (
        ( CurrInstanceNum 0 )
        )
    ( foreach
      InstanceInfo
      InstanceInfoList
      (let (
            ( InstanceName 
              ( AutoPinGetName
                PinName
                ( AutoPinGetSuffixFromInstanceInfo InstanceInfo )
                CurrInstanceNum ) ) ) 
        (let ( 
              ( Instance
                ( dbFindAnyInstByName TargetCellView InstanceName ) ) )

          (cond (
                ;if in list, it updates it and removes it from list 	  
                 ( and 
                   Instance				     
                   ( not AddOnly ) )
                 ( AutoPinUpdateInstance
                   Instance 
                   ( AutoPinGetLocationFromInstanceInfo InstanceInfo ) )
                 ( setq ExistingInstanceList 
                        ( remove Instance ExistingInstanceList ) )
                 )
                ;if not in list, creates it
                ( 
                 ( not Instance )
                  (let (
                        ( Instance
                          ( AutoPinCreateInstance 
                            TargetCellView 
                            InstanceInfo
                            InstanceName ) ) )
                    ( AutoPinCreateLabel
                      TargetCellView 
                      ( AutoPinGetLabelInfoFromInstanceInfo InstanceInfo )
                      NetName
                      Instance )
                    (when ( AutoPinGetMakePinFromInstanceInfo InstanceInfo )
                      ( AutoPinCreateConnectivity
                        TargetCellView
                        NetName 
                        Instance )
                      ) ) ) )
      ( setq CurrInstanceNum ( plus CurrInstanceNum 1 ) ) ) ) ) ) )

 
(defun AutoPinUpdateRectInfoList ( TargetCellView 
                                   NetName 
                                   PinName
                                   RectInfoList
                                   PinShapesInNet
                                   AddOnly
                                   IsInPlace
                                   )

                                        ;looks for all rodObjects with name PinName.* and puts them in a list
  (let (
        ( ExistingPinTable
          ( makeTable `foo nil ) ) )
    ( foreach Pin PinShapesInNet
              (let (
                    ( RodObject ( rodGetObj Pin ) )
                    )
                (when ( and 
                        RodObject
                        ( AutoPinIsPinInNet ( getq RodObject name ) PinName ) )
                  ( setarray ExistingPinTable ( getq RodObject name ) RodObject ) ) ) )
    (let (
          ( CurrRectNum 0 )
          )
      ( foreach
        RectInfo
        RectInfoList
        (let (
              ( PinName ( AutoPinGetName PinName "" CurrRectNum ) )
              )               
          (let ( 
                ( RodObject ( rodGetObj PinName TargetCellView ) )
                )	         
            (cond (  
                  ;if in list, it update it and remove it from list
                   ( and RodObject
                         ( not AddOnly ) )
                   ( AutoPinUpdateRodShape
                     RodObject                      
                     ( AutoPinGetLPPFromRectInfo RectInfo )
                     ( AutoPinGetRectFromRectInfo RectInfo )
                     )
                   (let (
                         ( Label ( car ( exists Label ( getq ( getq RodObject dbId ) children )
                                                ( PinUtilIsLabelForNetName Label NetName ) ) ) ) )
                     (if ( AutoPinGetLabelInfoFromRectInfo RectInfo )
                         (if ( null Label )
                             ( AutoPinCreateLabel
                               TargetCellView 
                               ( AutoPinGetLabelInfoFromRectInfo RectInfo ) 
                               NetName 
                               ( getq RodObject dbId ) )
                           ( AutoPinUpdateLabel
                             Label
                             ( AutoPinGetLabelInfoFromRectInfo RectInfo ) ) )
                       (if Label
                           ( dbDeleteObject Label ) ) ) )
                   (let (
                         ( Pin ( getq ( getq RodObject dbId ) pin ) ) )
                     (if Pin
                         (if ( null ( AutoPinGetMakePinFromRectInfo RectInfo ) )
                             ( dbDeleteObject Pin ) ) ) )
                   ( remove PinName ExistingPinTable ) )
                  ;if not in list, creates it
                  ( 
                   ( not RodObject )
                    (let (
                          ( RodRect
                            ( AutoPinCreateRodShape
                              TargetCellView 
                              PinName
                              NetName                                
                              ( AutoPinGetLPPFromRectInfo RectInfo )
                              ( AutoPinGetRectFromRectInfo RectInfo ) ) ) )
                     
                      ( AutoPinCreateLabel
                        TargetCellView 
                        ( AutoPinGetLabelInfoFromRectInfo RectInfo )
                        NetName
                        ( getq RodRect dbId ) )
                      (when ( AutoPinGetMakePinFromRectInfo RectInfo )
                       (if IsInPlace then
                         (dbReplaceProp (getq RodRect dbId) "PinType" "string" "InPlace")
                        else
                         (dbReplaceProp (getq RodRect dbId) "PinType" "string" "UserPin")
                       )
                       ( AutoPinCreateConnectivity
                          TargetCellView
                          NetName 
                          ( getq RodRect dbId ) )
                        ) ) ) )
            ( setq CurrRectNum ( plus CurrRectNum 1 ) ) ) ) ) )

    ;deletes all remaining corresponding rodObjects in list
    (when ( not AddOnly )
      ( foreach ExistingPin
                ( mapcar `cadr ( tableToList ExistingPinTable ) )
                ( dbDeleteObject ( getq ExistingPin dbId ) ) )
      ) ) )

(defun AutoPinUpdatePinInfo ( PinInfo 
                              TargetCellView
                              ConnectivityCellView
                              LayoutInstanceMap
                              ExistingPinTable
                              DrawFigs
                              AddOnly
                              MetalLayerRegEx
                              ViaLayerRegEx
                              LogPort )
  (cond (
         ( and ( AutoPinIsInPlacePinInfo PinInfo )
               ( AutoPinGetInPlaceInfoFromPinInfo PinInfo ) )
         ( setq PinInfo ( AutoPinMakePinInfoFromInPlacePinInfo 
                          ( AutoPinGetNetNameFromPinInfo PinInfo )
                          ( AutoPinGetPinNameFromPinInfo PinInfo )
                          TargetCellView 
                          ConnectivityCellView     
                          LayoutInstanceMap
                          ( AutoPinGetBoundaryPolygonEdgesFromInPlacePinInfo PinInfo )
                          MetalLayerRegEx
                          ViaLayerRegEx
                          ?MakeLabel t
                          ?MakePin ( AutoPinGetMakePinFromInPlacePinInfo PinInfo )
                          ?LogPort LogPort ) ) ) )
  ( AutoPinUpdatePinInfoSimple PinInfo TargetCellView ExistingPinTable DrawFigs AddOnly) )

(defun AutoPinUpdatePinInfoSimple ( PinInfo 
                                    TargetCellView
                                    ExistingPinTable
                                    DrawFigs
                                    AddOnly )
  (if DrawFigs
      (let (
            ( PinName ( AutoPinGetPinNameFromPinInfo PinInfo ) ) 
            ( NetName ( AutoPinGetNetNameFromPinInfo PinInfo ) ) 
            ( RectInfoList ( AutoPinGetRectInfoListFromPinInfo PinInfo ) )
            ( IsInPlace ( AutoPinGetInPlaceInfoFromPinInfo PinInfo ) )
            ( InstanceInfoList ( AutoPinGetInstanceInfoListFromPinInfo PinInfo ) )        
            )			         
        ( AutoPinUpdateRectInfoList
          TargetCellView
          NetName
          PinName    
          RectInfoList         
          ( setof Fig ( arrayref ExistingPinTable NetName ) ( equal ( getq Fig "objType" ) "rect" ) )
          AddOnly
          IsInPlace
          )
        ( AutoPinUpdateInstanceInfoList
          TargetCellView
          NetName
          PinName
          InstanceInfoList
          ( setof Fig ( arrayref ExistingPinTable NetName ) ( equal ( getq Fig "objType" ) "inst" ) )
          AddOnly
 ) ) ) )


(defun AutoPinUpdateQueuedPins ( PinTable 
                                 TargetCellView 
                                 ConnectivityViewName
                                 FoldableCellLibCellRegExPairs
                                 MetalLayerRegEx
                                 ViaLayerRegEx
                                 @key
                                 ( DrawFigs t )
                                 ( AddOnly nil )
                                 ( LogPort poport )
                                 )
  (let (
        ( ExistingPinTable ( AutoPinGetExistingPinTable TargetCellView ) )
        ( ConnectivityCellView ( dbOpenCellViewByType
				 ( getq TargetCellView libName )
				 ( getq TargetCellView cellName )
                                 ConnectivityViewName
				 nil
				 "r" ) ) )
    (when ConnectivityCellView
      (let (
            ( LayoutInstanceMap ( NameMakeInstanceMapFromCellView
                                  TargetCellView 
                                  FoldableCellLibCellRegExPairs ) ) )
        ( foreach 
          PinName
          PinTable      
          (let (
                ( CurrPinInfo ( arrayref PinTable PinName ) )
                )
            ( AutoPinUpdatePinInfo 
              CurrPinInfo
              TargetCellView
              ConnectivityCellView
              LayoutInstanceMap
              ExistingPinTable
              DrawFigs
              AddOnly
              MetalLayerRegEx
              ViaLayerRegEx
              LogPort ) ) ) ) ) ) )
      
(defun AutoPinUpdateQueuedPinsSimple ( PinTable 
                                       TargetCellView 
                                       @key
                                       ( DrawFigs t )
                                       ( AddOnly nil )
                                       )
  (let (
        ( ExistingPinTable ( AutoPinGetExistingPinTable TargetCellView ) ) )
    ( foreach 
      PinName
      PinTable      
      (let (
            ( CurrPinInfo ( arrayref PinTable PinName ) ) 
            )
        ( AutoPinUpdatePinInfoSimple 
          CurrPinInfo
          TargetCellView
          ExistingPinTable
          DrawFigs
          AddOnly
          ) ) ) ) )
  

(defun AutoPinGetHorizontalPinBottomAndTop ( PitchIndex 
					     WidthInPitches
                                             WireWidth
                                             WireSpacing
					     WirePitch
                                             BoundaryPolygonEdges 
                                             BoundaryPinSpacing
                                             XPosition
                                             ManufacturingGrid )
  (let (
        ( BoundaryBottomEdge ( PolygonGetBottomMostEdgeAtX BoundaryPolygonEdges XPosition ) ) )
    (let (
          ( BoundaryBottom ( float ( LineGetSegmentMaxY BoundaryBottomEdge ) ) )          
          )
      (let (
            ( PinBottom
              ( MathRoundToNearest
                ( plus
                  BoundaryBottom
                  BoundaryPinSpacing
                  ( times PitchIndex WirePitch )
                  ( times 0.5 WireSpacing ) )
                ManufacturingGrid
                ) ) )
        ( list
          PinBottom
          ( plus PinBottom WireWidth ) ) ) ) ) )

                          

(defun AutoPinMakeLeftPinRectInfo ( PitchIndex
                                    LayerPurposePair
                                    PinWidth
                                    PinLength
                                    WireSpacing
				    WirePitch
                                    BoundaryPolygonEdges
                                    BoundaryPinHorizontalSpacing
                                    BoundaryPinVerticalSpacing
                                    ManufacturingGrid
                                    @key ( MakeLabel t )
				    )
  (let (
        ( BoundaryLeft 
          ( LineGetSegmentMaxX 
            ( PolygonGetLeftMostEdge BoundaryPolygonEdges ) ) ) )
    (let (
          ( PinBottomTop ( AutoPinGetHorizontalPinBottomAndTop
                           PitchIndex 
			   1
                           PinWidth
                           WireSpacing
			   WirePitch
                           BoundaryPolygonEdges
                           BoundaryPinVerticalSpacing
                           BoundaryLeft
                           ManufacturingGrid
                           ) ) )
      (let (
            ( PinLeft ( plus BoundaryLeft BoundaryPinHorizontalSpacing ) )
            ( PinBottom ( car PinBottomTop ) )
            ( PinTop ( cadr PinBottomTop ) ) )    
        (let (
              ( PinRight ( plus PinLeft PinLength ) )
	      ( LabelInfo ( cond (
				  MakeLabel
				  ( AutoPinMakeLabelInfo 				  
				    ( list ( car LayerPurposePair ) "drawing" )
				    )
				  )
				 (
				  nil 
				  )
				 )
                          )
              )	
          (let (
                ( Rect ( list
                         ( list PinLeft PinBottom )
                         ( list PinRight PinTop ) 
                         ) ) )
            ( AutoPinMakePinRectInfo 
              LayerPurposePair
              Rect             
              LabelInfo
              t    
              "rect"
              ) ) ) ) ) ) )

(defun AutoPinMakeRightPinRectInfo ( PitchIndex
                                     LayerPurposePair
                                     PinWidth
                                     PinLength
                                     WireSpacing
				     WirePitch
                                     BoundaryPolygonEdges
                                     BoundaryPinHorizontalSpacing
                                     BoundaryPinVerticalSpacing	 
                                     ManufacturingGrid
                                     @key ( MakeLabel t )
                                     )
  (let (
        ( BoundaryRight 
          ( LineGetSegmentMinX  
            ( PolygonGetRightMostEdge BoundaryPolygonEdges ) ) ) )
    (let (
          ( PinBottomTop ( AutoPinGetHorizontalPinBottomAndTop
                           PitchIndex
			   1
                           PinWidth
                           WireSpacing
			   WirePitch
                           BoundaryPolygonEdges
                           BoundaryPinVerticalSpacing
                           BoundaryRight
                           ManufacturingGrid
                           ) ) )
      (let (
            ( PinRight ( difference BoundaryRight BoundaryPinHorizontalSpacing ) )
            ( PinBottom ( car PinBottomTop ) )
            ( PinTop ( cadr PinBottomTop ) ) )
        (let (
              ( PinLeft ( difference PinRight PinLength ) )
	      ( LabelInfo ( cond (
				  MakeLabel
				  ( AutoPinMakeLabelInfo 				
				    ( list ( car LayerPurposePair ) "drawing" )
				    )
				  )
				 (
				  nil 
				  )
				 )
                          )
              )	
          (let (
                ( Rect ( list
                         ( list PinLeft PinBottom )
                         ( list PinRight PinTop ) 
                         ) ) )
            ( AutoPinMakePinRectInfo 
              LayerPurposePair
              Rect
              LabelInfo
              t             
              "rect"
              ) ) ) ) ) ) )


(defun AutoPinMakeHorizontalStrutRectInfo (PitchIndex
					   LayerPurposePair
					   PinWidth
					   PinLength
					   WireSpacing
					   WirePitch
					   BoundaryPolygonEdges
					   BoundaryPinHorizontalSpacing
					   BoundaryPinVerticalSpacing
                                           ManufacturingGrid
                                           ReservedPitchTable
                                           @key
                                           ( MakeLabel t )
					   )
  "returns a RectInfo representing a horizontal strut"
                                        ;assume a horizontal strut can be drawn from left to right edge
                                        ;reserve pitches	  
  (let (
                                        ;width + spacing <= pitch * n
                                        ;n = ceiling ( w+s / p )
        ( WidthInPitches ( ceiling ( quotient ( plus PinWidth WireSpacing ) WirePitch ) ) )
        ( BoundaryLeft 
          ( LineGetSegmentMaxX 
            ( PolygonGetLeftMostEdge BoundaryPolygonEdges ) ) )
        ( BoundaryRight 
          ( LineGetSegmentMinX  
            ( PolygonGetRightMostEdge BoundaryPolygonEdges ) ) )      
	)
    (if ReservedPitchTable 
        ( for I PitchIndex ( plus PitchIndex ( difference WidthInPitches 1 ) ) 
              ( setarray ReservedPitchTable I t )						    
              ) )
    (let (	  
          ( PinBottomTop ( AutoPinGetHorizontalPinBottomAndTop
                           PitchIndex 
                           WidthInPitches
                           PinWidth
                           WireSpacing
			   WirePitch
                           BoundaryPolygonEdges
                           BoundaryPinVerticalSpacing
                           ( caar
                             ( PolygonGetBottomMostEdge
                               BoundaryPolygonEdges ) )
                           ManufacturingGrid
                           
                           ) ) )
      (let (
            ( LeftAndRight ( PolygonGetHorizontalLeftAndRightBoundaryFromBottomAndTop
                             PinBottomTop
                             BoundaryPolygonEdges ) ) )
        (let (
              ( BoundaryLeft ( car LeftAndRight ) )
              ( BoundaryRight ( cadr LeftAndRight ) ) )
          (let (
                ( PinLeft ( plus BoundaryLeft BoundaryPinHorizontalSpacing ) )
                ( PinRight ( difference BoundaryRight BoundaryPinHorizontalSpacing ) )
                ( PinBottom ( car PinBottomTop ) )
                ( PinTop ( cadr PinBottomTop ) ) 
                )      
            (let (
                  ( LabelInfo ( cond (
                                      MakeLabel
                                      ( AutoPinMakeLabelInfo 			
                                        ( list ( car LayerPurposePair ) "drawing" ) ) )
                                     ( nil ) ) )
                  ( Rect  ( list
                            ( list PinLeft PinBottom )
                            ( list PinRight PinTop ) ) ) )			
              ( AutoPinMakePinRectInfo 
                LayerPurposePair
                Rect
                LabelInfo
                t       
                "rect"
                ) ) ) ) ) ) ) )


(defun AutoPinCalculateNumHorizontalWirePitches ( BoundaryPolygonEdges
                                                  WirePitch
                                                  WirePitchOffset )
  ( floor 
    ( quotient 
      ( difference
        ( plus 
          1e-9
          ( PolygonGetHeight
            BoundaryPolygonEdges  ) )
        WirePitchOffset )
      WirePitch ) ) )

(defun AutoPinCalculateNumVerticalWirePitches ( BoundaryPolygonEdges
                                                WirePitch
                                                WirePitchOffset
                                                )
  ( floor 
    ( quotient 
      ( difference
        ( plus
          1e-9 
          ( PolygonGetWidth
            BoundaryPolygonEdges ) )
        WirePitchOffset
        )
      WirePitch ) ) )

(defun AutoPinMakePowerGridRectAndInstanceInfoLists (  	
						     NetName
						     GridOffset
						     GridSpacing
						     LayerPurposePair
						     PinWidth
						     PinLength
						     WireSpacing
						     WirePitch
						     BoundaryPolygonEdges
						     BoundaryPinHorizontalSpacing
						     BoundaryPinVerticalSpacing
                                                     ManufacturingGrid
						     ReservedPitchTable
						     )
  "returns a list of RectInfo's for the power grid"   

  (let (	 
        ( CurrentPitch GridOffset) 
        ( RectInfoList nil )
        ( InstanceInfoList nil )
        ( WidthInPitches ( ceiling ( quotient ( plus PinWidth WireSpacing ) WirePitch ) ) )
        ( NumPitchesInCell ( AutoPinCalculateNumHorizontalWirePitches 
                             BoundaryPolygonEdges
                             WirePitch
                             0.0 ) )
        ( MakeLabel t ) )
                                        ; horizontal struts
    ( while ( leqp ( plus CurrentPitch WidthInPitches ) NumPitchesInCell )
      if( CurrentPitch==-1 then    
        ( setq RectInfoList ( cons ( AutoPinMakeHorizontalStrutRectInfo
                                   -2
                                   LayerPurposePair
                                   PinWidth/2
                                   PinLength
                                   WireSpacing
                                   WirePitch/8
                                   BoundaryPolygonEdges
                                   BoundaryPinHorizontalSpacing
                                   BoundaryPinVerticalSpacing 
                                   ManufacturingGrid
                                   ReservedPitchTable
                                   ?MakeLabel MakeLabel
                                   )
                                 RectInfoList ) )
        ( setq RectInfoList ( cons ( AutoPinMakeHorizontalStrutRectInfo
                                   NumPitchesInCell*8-8
                                   LayerPurposePair
                                   PinWidth/2
                                   PinLength
                                   WireSpacing
                                   WirePitch/8
                                   BoundaryPolygonEdges
                                   BoundaryPinHorizontalSpacing
                                   BoundaryPinVerticalSpacing 
                                   ManufacturingGrid
                                   ReservedPitchTable
                                   ?MakeLabel nil
                                   )
                                 RectInfoList ) )
      else
        ( setq RectInfoList ( cons ( AutoPinMakeHorizontalStrutRectInfo
                                   CurrentPitch
                                   LayerPurposePair
                                   PinWidth
                                   PinLength
                                   WireSpacing
                                   WirePitch
                                   BoundaryPolygonEdges
                                   BoundaryPinHorizontalSpacing
                                   BoundaryPinVerticalSpacing 
                                   ManufacturingGrid
                                   ReservedPitchTable
                                   ?MakeLabel MakeLabel
                                   )
                                 RectInfoList ) )
      )
      ( setq MakeLabel nil )
      ( setq CurrentPitch ( plus CurrentPitch GridSpacing ) ) )
    ( list RectInfoList InstanceInfoList ) ) )

(defun AutoPinQueuePin ( PinTable 
			 NetName
			 BBox
			 LayerPurposePair
			 @key
                         ( CreateRectInfo t )
                         ( MakeLabel t )
                         ( InPlace t )
                         )

  (let (
        ( LabelInfo
          (if MakeLabel
              ( AutoPinMakeLabelInfo 			  
                ( list ( car LayerPurposePair ) "drawing" ) ) ) ) )
    ( AutoPinQueuePinInfo
      PinTable    
      ( AutoPinMakePinInfo           
        NetName
        ( EscapeString NetName )
        (if CreateRectInfo 
            ( list ( AutoPinMakePinRectInfo 
                     LayerPurposePair
                     BBox
                     LabelInfo
                     t 
                     "rect" ) ) )
        nil
        ?InPlace InPlace          
        ) ) ) )


(defun AutoPinQueueLeftPin ( PinTable
                             PitchIndex
                             NetName
                             LayerPurposePair
                             PinWidth
                             PinLength
                             WireSpacing
			     WirePitch
                             BoundaryPolygonEdges
                             BoundaryPinHorizontalSpacing
                             BoundaryPinVerticalSpacing 
                             ManufacturingGrid
			     FunctionCacheTable
			     @key ( CreateRectInfo t )
                                  ( Strut nil )
                             )

  ( AutoPinQueuePinInfo
    PinTable  
    ( AutoPinMakePinInfo 
      NetName
      ( EscapeString NetName )     
      (if CreateRectInfo
          ( list
            (if Strut
                ( CacheGetValue
                  FunctionCacheTable
                  `AutoPinMakeHorizontalStrutRectInfo
                  PitchIndex
                  LayerPurposePair
                  PinWidth
                  PinLength
                  WireSpacing
                  WirePitch
                  BoundaryPolygonEdges
                  BoundaryPinHorizontalSpacing
                  BoundaryPinVerticalSpacing
                  ManufacturingGrid
                  nil
                  )

              ( CacheGetValue
                FunctionCacheTable
                `AutoPinMakeLeftPinRectInfo
                PitchIndex
                LayerPurposePair
                PinWidth
                PinLength
                WireSpacing
                WirePitch
                BoundaryPolygonEdges
                BoundaryPinHorizontalSpacing
                BoundaryPinVerticalSpacing
                ManufacturingGrid
                 ) ) ) )
      nil ) ) )

    
(defun AutoPinQueueRightPin ( PinTable
                              PitchIndex
                              NetName
			      LayerPurposePair			  
			      PinWidth
			      PinLength
			      WireSpacing
			      WirePitch
			      BoundaryPolygonEdges			    
			      BoundaryPinHorizontalSpacing 
			      BoundaryPinVerticalSpacing 
                              ManufacturingGrid
			      FunctionCacheTable
			      @key
                              ( CreateRectInfo t )
                              ( Strut nil )
                              )
  ( AutoPinQueuePinInfo
    PinTable  
    ( AutoPinMakePinInfo      
      NetName
      ( EscapeString NetName )
      (if CreateRectInfo
          ( list
            (if Strut
                ( CacheGetValue
                  FunctionCacheTable
                  `AutoPinMakeHorizontalStrutRectInfo
                  PitchIndex
                  LayerPurposePair
                  PinWidth
                  PinLength
                  WireSpacing
                  WirePitch
                  BoundaryPolygonEdges
                  BoundaryPinHorizontalSpacing
                  BoundaryPinVerticalSpacing
                  ManufacturingGrid
                  nil
                  )

              ( CacheGetValue
                FunctionCacheTable
                `AutoPinMakeRightPinRectInfo
                PitchIndex
                LayerPurposePair
                PinWidth
                PinLength
                WireSpacing
                WirePitch
                BoundaryPolygonEdges
                BoundaryPinHorizontalSpacing
                BoundaryPinVerticalSpacing
                ManufacturingGrid
                ) )  
            ) )
      nil ) ) )


(defun AutoPinQueuePowerGrid ( PinTable
			       NetName
			       GridOffset
			       GridSpacing
			       LayerPurposePair
			       PinWidth
			       PinLength
			       WireSpacing			       
			       WirePitch
			       BoundaryPolygonEdges
			       BoundaryPinHorizontalSpacing
			       BoundaryPinVerticalSpacing
                               ManufacturingGrid
			       FunctionCacheTable
			       ReservedPitchTable
			       @key
                               ( CreateRectInfo t )
                               )
  "enqueues a power grid pinInfo in the PinTable"
  (let (
	( RectAndInstanceInfoLists 
          (if CreateRectInfo
              ( CacheGetValue
                FunctionCacheTable
                `AutoPinMakePowerGridRectAndInstanceInfoLists
                NetName
                GridOffset
                GridSpacing
                LayerPurposePair
                PinWidth
                PinLength
                WireSpacing
                WirePitch
                BoundaryPolygonEdges
                BoundaryPinHorizontalSpacing
                BoundaryPinVerticalSpacing
                ManufacturingGrid
                ReservedPitchTable
                ) ) ) )
;    ( AutoPinQueuePinInfo
;      PinTable  
;      ( AutoPinMakePinInfo      
;	NetName
;	( EscapeString NetName )
;	( car RectAndInstanceInfoLists )
;        ( cadr RectAndInstanceInfoLists )
;	) ) 
t
) )

(defun AutoPinQueueHorizontalStrut ( PinTable
				     PitchIndex
				     NetName
				     LayerPurposePair
				     PinWidth
				     PinLength
				     WireSpacing
				     WirePitch
				     BoundaryPolygonEdges
				     BoundaryPinHorizontalSpacing
				     BoundaryPinVerticalSpacing
                                     ManufacturingGrid
				     FunctionCacheTable
				     ReservedPitchTable
				     @key
                                     ( CreateRectInfo t )
                                     )
  "enqueues a horizontal strut pin of a given width"
  ( AutoPinQueuePinInfo
    PinTable  
    ( AutoPinMakePinInfo      
      NetName
      ( EscapeString NetName )
      (if CreateRectInfo
          ( list
            ( CacheGetValue
              FunctionCacheTable
              `AutoPinMakeHorizontalStrutRectInfo
              PitchIndex
              LayerPurposePair
              PinWidth
              PinLength
              WireSpacing
              WirePitch
              BoundaryPolygonEdges
              BoundaryPinHorizontalSpacing
              BoundaryPinVerticalSpacing
              ManufacturingGrid
              ReservedPitchTable
              ) 
            ) )
      nil ) ) )

(defun AutoPinQueueLeftRightPin ( PinTable
				  PitchIndex
				  NetName
				  LayerPurposePair
				  PinWidth
				  PinLength
				  WireSpacing
				  WirePitch
				  BoundaryPolygonEdges
				  BoundaryPinHorizontalSpacing
				  BoundaryPinVerticalSpacing 
                                  ManufacturingGrid
				  FunctionCacheTable
				  ReservedPitchTable
				  @key
                                  ( CreateRectInfo t ) 
                                  ( Strut nil )
                                  )
  "enqueues a pin on the left and right connected to the same net"
 
  ( AutoPinQueuePinInfo
    PinTable  
    ( AutoPinMakePinInfo      
      NetName
      ( EscapeString NetName )
      ( cond ( CreateRectInfo
               (if Strut
                   ( list 
                     ( CacheGetValue
                       FunctionCacheTable
                       `AutoPinMakeHorizontalStrutRectInfo
                       PitchIndex
                       LayerPurposePair
                       PinWidth
                       PinLength
                       WireSpacing
                       WirePitch
                       BoundaryPolygonEdges
                       BoundaryPinHorizontalSpacing
                       BoundaryPinVerticalSpacing
                       ManufacturingGrid
                       nil
                       ) )
                 ( list 
                   ( CacheGetValue
                     FunctionCacheTable
                     `AutoPinMakeLeftPinRectInfo
                     PitchIndex           
                     LayerPurposePair
                     PinWidth
                     PinLength
                     WireSpacing
                     WirePitch
                     BoundaryPolygonEdges
                     BoundaryPinHorizontalSpacing
                     BoundaryPinVerticalSpacing 
                     ManufacturingGrid
                     ?MakeLabel t
                     )
                       
                   ( CacheGetValue
                     FunctionCacheTable
                     `AutoPinMakeRightPinRectInfo
                     PitchIndex           
                     LayerPurposePair
                     PinWidth
                     PinLength
                     WireSpacing
                     WirePitch
                     BoundaryPolygonEdges
                     BoundaryPinHorizontalSpacing
                     BoundaryPinVerticalSpacing 
                     ManufacturingGrid
                     ?MakeLabel nil
                     )		
                   )
                 ) 	     
               ) 
	     ( nil )
             )	       
      nil
      ) 
    )
                                        ;reserve pitch
  ( setarray ReservedPitchTable PitchIndex t )	
  )

(defun AutoPinQueueInPlacePin ( PinTable 
				NetName 			
				LayerPurposePair
				BoundaryPolygonEdges
				@key ( CreateRectInfo t )
                                ( MakePin t )
                                )   
  ( AutoPinQueuePinInfo 
    PinTable    
    ( AutoPinMakeInPlacePinInfo 
      NetName
      ( EscapeString NetName )
      (if CreateRectInfo 
          ( AutoPinMakeInPlaceInfo
            LayerPurposePair
            BoundaryPolygonEdges
            MakePin ) ) ) ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; InPlace 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun AutoPinLayerValue ( LPP MetalRegEx )
  (cond (
         ( rexMatchp MetalRegEx ( car LPP ) )
         ( difference 0 ( atoi ( rexSubstitute "\\1" ) ) ) )
        (
         t
         0 ) ) )
   
(defun AutoPinPreferredLayerTest ( LPP1 LPP2 MetalRegEx )
  ( difference ( AutoPinLayerValue LPP1 MetalRegEx )
               ( AutoPinLayerValue LPP2 MetalRegEx ) ) )

(defun AutoPinBBoxValue ( BBox BoundaryPolygonEdges )
                                        ;lower is better
  ( AutoPinGetMinDistanceFromBoundaryPolygonEdges 
    BoundaryPolygonEdges 
    ( BBoxGetPolygonEdges BBox ) ) )

(defun AutoPinPreferredBBoxTest ( BBox1 BBox2 BoundaryPolygonEdges )
  ( difference ( AutoPinBBoxValue BBox1 BoundaryPolygonEdges )
               ( AutoPinBBoxValue BBox2 BoundaryPolygonEdges ) ) )

(defun AutoPinTypeValue ( Type )
  (cond
        (
         ( equal Type "label" )
         5
         )
        (
         ( equal Type "donut" )
         4
         )
        (
         ( equal Type "polygon" )
         3
         )
        (
         ( equal Type "path" )
         2
         )
        (
         ( equal Type "inst" )
         1
         )
        (
         ( equal Type "rect" )
         0
         ) ) )

(defun AutoPinPreferredTypeTest ( Type1 Type2 )
  if( Type1=="label" || Type2=="label" then -1
  else  
  ( difference ( AutoPinTypeValue Type1 )
               ( AutoPinTypeValue Type2 ) ) ) )

(defun AutoPinMakePinInfoFromInPlacePinInfo ( NetName
                                              PinName
                                              TargetCellView 
                                              ConnectivityCellView
                                              LayoutInstanceMap
                                              BoundaryPolygonEdges
                                              MetalLayerRegEx
                                              ViaLayerRegEx
                                              @key
                                              ( MakeLabel t )
                                              ( MakePin t )
                                              ( LogPort poport )
                                              )	 
  (let (  
        ( PinInstancePairs ( PinUtilGetConnectedPinInstancePairs_2
                             ConnectivityCellView                                
                             TargetCellView
                             LayoutInstanceMap
                             NetName ) ) )
                                        ; get a list of all pin-instance pairs
    (let (
          ( PinInfos ( mapcar 
                       (lambda ( PinInstancePair )
                         ( AutoPinGetPinInfoFromPinFigForceRect 
                           ( getq ( car PinInstancePair ) fig )
                           ViaLayerRegEx
                           ?ParentInstance ( cadr PinInstancePair )
                           ?MakePin MakePin ) )
                       PinInstancePairs ) ) )
      (let (
            ( ClosestPinToBoundaryPinInfo
              ( ListFindBestElement
                PinInfos
                (lambda ( PinInfo1 PinInfo2 )
                  (let (
                        ( PinRectInfo1 ( car ( AutoPinGetRectInfoListFromPinInfo PinInfo1 ) ) ) 
                        ( PinRectInfo2 ( car ( AutoPinGetRectInfoListFromPinInfo PinInfo2 ) ) ) )
                    (let (
                          ( LayerPreference 
                            ( AutoPinPreferredLayerTest
                              ( AutoPinGetLPPFromRectInfo PinRectInfo1 )
                              ( AutoPinGetLPPFromRectInfo PinRectInfo2 )
                              MetalLayerRegEx
                              ) ) )
                      (if ( lessp LayerPreference 0 )
                          t
                        (if ( greaterp LayerPreference 0 )
                            nil
                          (let (
                                ( TypePreference ( AutoPinPreferredTypeTest 
                                                   ( AutoPinGetObjTypeFromRectInfo PinRectInfo1 )
                                                   ( AutoPinGetObjTypeFromRectInfo PinRectInfo1 ) ) ) )
                            (if ( lessp TypePreference 0 )
                                t
                              (if ( greaterp TypePreference 0 )
                                  nil
                                (let (
                                      ( BBoxPreference 
                                        ( AutoPinPreferredBBoxTest 
                                          ( AutoPinGetRectFromRectInfo PinRectInfo1 )
                                          ( AutoPinGetRectFromRectInfo PinRectInfo2 )
                                          BoundaryPolygonEdges ) ) )
                                  (if ( lessp BBoxPreference 0 )
                                      t
                                    nil ) ) ) ) ) ) ) ) ) )
                nil
                ) ) )
        (if ( null ClosestPinToBoundaryPinInfo )                             
            ( fprintf LogPort "WARNING: No underlying pin for net : %s\n" NetName )
                                        ;use net name given, not underlying net name
          ( fprintf LogPort "made pin for %s on layer %s %s\n" 
                    NetName 
                    ( car ( AutoPinGetLPPFromRectInfo ( car ( AutoPinGetRectInfoListFromPinInfo ClosestPinToBoundaryPinInfo ) ) ) )
                    ( cadr ( AutoPinGetLPPFromRectInfo ( car ( AutoPinGetRectInfoListFromPinInfo ClosestPinToBoundaryPinInfo ) ) ) ) ) )
        
        ( AutoPinMakePinInfo
          NetName
          PinName
          ( AutoPinGetRectInfoListFromPinInfo ClosestPinToBoundaryPinInfo )
          ( AutoPinGetInstanceInfoListFromPinInfo ClosestPinToBoundaryPinInfo ) ) ) ) ) )

                                        ;only left or right edges count
(defun AutoPinGetMinDistanceFromBoundaryPolygonEdges ( BoundaryPolygonEdges PolygonEdges ) 
  ( let (
         ( BoundaryLeftEdge  ( PolygonGetLeftMostEdge  BoundaryPolygonEdges ) )
         ( BoundaryRightEdge ( PolygonGetRightMostEdge BoundaryPolygonEdges ) )
         ( LeftEdge  ( PolygonGetLeftMostEdge  PolygonEdges ) )
         ( RightEdge ( PolygonGetRightMostEdge PolygonEdges ) )
         )
    ( let (
           ( DistLeft  ( AutoPinGetMinDistanceBetweenEdges BoundaryLeftEdge LeftEdge ) )
           ( DistRight ( AutoPinGetMinDistanceBetweenEdges BoundaryRightEdge RightEdge ) )
           )
      ( min DistLeft DistRight )
      )
    )
  )   

                                        ;TODO - make a better algorithm, 
(defun AutoPinGetMinDistanceBetweenEdges ( Edge1 Edge2 )
  ( let ( 
	 ( Point11 ( car  Edge1 ) )
	 ( Point12 ( cadr Edge1 ) )
	 ( Point21 ( car  Edge2 ) )
	 ( Point22 ( cadr Edge2 ) )
         )
    ( let (
	   ( Segment11 ( list Point11 ( LineGetClosestPointOnSegmentToPoint Point11 Edge2 ) ) )
	   ( Segment12 ( list Point12 ( LineGetClosestPointOnSegmentToPoint Point12 Edge2 ) ) )
	   ( Segment21 ( list Point21 ( LineGetClosestPointOnSegmentToPoint Point21 Edge1 ) ) )
	   ( Segment22 ( list Point22 ( LineGetClosestPointOnSegmentToPoint Point22 Edge1 ) ) )		     	
	   )    
      ( min ( LineGetSegmentLength Segment11 )
	    ( LineGetSegmentLength Segment12 )
            ( LineGetSegmentLength Segment21 )
	    ( LineGetSegmentLength Segment22 )  
            )  
      )
    )
  )

(defun AutoPinGetSmallestBoundingShape ( TargetCellView 
                                         Shape 
                                         @key
                                         ( LPPPredicate (lambda ( x ) t ) ) ) 
  (let (
        ( Rect ( getq Shape bBox ) ) )
    (if ( not ( BBoxIsPoint Rect ) )
        Shape
      (let (
            ( MinArea 1000000000 )
            ( MinShape nil ) )
        ( foreach Shape ( getq TargetCellView shapes ) 
                  (when ( apply LPPPredicate ( list ( getq Shape lpp ) ) )
                    (let (       
                          ( ShapeRect ( getq Shape bBox ) ) )
                      (if ( RectIsRectInRect Rect ShapeRect )                                             
                          (let (
                                ( ShapeArea ( RectGetArea ShapeRect ) ) )                                     
                            (cond (
                                   ( and ( not ( eqv ShapeArea 0.0 ) ) ( lessp ShapeArea MinArea ) )
                                   ( setq MinArea ShapeArea )
                                   ( setq MinShape Shape ) ) ) ) ) ) )       )
        MinShape ) ) ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Existing Pin Info
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                                        ;HACK
(defun AutoPinGetExistingPinInfosForceRect ( TargetCellView ViaLayerRegEx )
  "returns a list of pininfos for each existing pin in cellview "
  ( mapcar (lambda ( PinFig ) ( AutoPinGetPinInfoFromPinFigForceRect PinFig ViaLayerRegEx ) )   
           ( PinUtilGetExistingPinFigs TargetCellView ) ) )

(defun AutoPinGetPinInfoFromPinFigForceRect ( Fig 
                                              ViaLayerRegEx
                                              @key
                                              ( ParentInstance nil )                                            
                                              ( MakePin t ) )

  (cond (
                                        ;instance pins
         ( equal ( getq Fig objType ) "inst" )
         ( AutoPinGetPinInfoFromPinInstanceForceRect 
           Fig 
           ViaLayerRegEx
           ?ParentInstance ParentInstance
           ?MakePin MakePin ) 
         )
        (        
                                        ;shape pins
         ( AutoPinGetPinInfoFromPinShape 
           Fig
           ViaLayerRegEx
           ?ParentInstance ParentInstance
           ?MakePin MakePin ) ) ) )

(defun AutoPinGetPinInfoFromPinShape ( Shape  
                                       ViaLayerRegEx
                                       @key
                                       ( ParentInstance nil )
                                       ( MakePin t )                                   
                                       )

  (let (
        ;use the smallest bounding shape in the parent instance
        ( InPlaceShape
          (if ParentInstance
              ( AutoPinGetSmallestBoundingShape
                ( getq ParentInstance master ) Shape )
            Shape ) ) )
    (let (
          ( BBox
            (if ParentInstance
                ( dbTransformBBox
                  ( FigGetRectInsideFig InPlaceShape )
                  ( getq ParentInstance transform ) )
              ( FigGetRectInsideFig InPlaceShape ) ) )
          ( LayerPurposePair ( getq InPlaceShape lpp ) ) )

      ( AutoPinMakePinInfo 		      
        ( getq ( getq Shape net ) name )
        ( EscapeString ( getq ( getq Shape net ) name ) )
        ( list 
          ( AutoPinMakePinRectInfo 
            ( PinUtilGetValidLayer LayerPurposePair ViaLayerRegEx )
            BBox
            ( AutoPinMakeLabelInfo 
              ( list
                ( car 
                  ( PinUtilGetValidLayer LayerPurposePair ViaLayerRegEx ) )
                "drawing" ) )
            MakePin
            ( getq InPlaceShape objType )
            ) )
        nil ) ) ) )

(defun AutoPinGetPinInfoFromPinInstanceForceRect ( Instance  
                                                   ViaLayerRegEx
                                                   @key 
                                                   ( ParentInstance nil )
                                                   ( MakePin t )
                                                   )
  (let ( 
                                        ;get the first pin shape in the instance
	( PinShape ( car ( exists Shape ( getq ( getq Instance master ) shapes ) ( getq Shape pin ) ) ) ) )
    (let (
                                        ; get the bounding shape in the instance
          ( InPlaceShape ( AutoPinGetSmallestBoundingShape ( getq Instance master ) PinShape ) ) )     
      (let (
            ( NetName ( getq ( getq ( getq Instance pin ) net ) name ) )
            ( LayerPurposePair ( getq InPlaceShape lpp ) )
                                        ;transform into parent instance coordinates
            ( BBox ( dbTransformBBox ( FigGetRectInsideFig InPlaceShape ) ( getq Instance transform ) ) ) )       
        (let (
                                        ;transform into cellview coordinates
              ( BBox (if ParentInstance
                         ( dbTransformBBox BBox ( getq ParentInstance transform ) )
                       BBox ) ) )          
          ( AutoPinMakePinInfo 		      
            NetName
            ( EscapeString NetName )         
            ( list ( AutoPinMakePinRectInfo
                     ( PinUtilGetValidLayer LayerPurposePair ViaLayerRegEx )
                     BBox
                     ( AutoPinMakeLabelInfo 
                       ( list
                         ( car 
                           ( PinUtilGetValidLayer LayerPurposePair ViaLayerRegEx ) )
                         "drawing" ) )
                     MakePin
                     "inst"
                     ) )
            nil ) ) ) ) ) )
