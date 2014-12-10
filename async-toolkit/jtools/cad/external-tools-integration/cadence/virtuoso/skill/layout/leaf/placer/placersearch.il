; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

; Function: PlacerSearch
; Parameter: none
; Return: PlacerSearchStatus = nil We do not yet have a placed view
;         RPlacerSearchStatus = 1 We have a placed view, but trying to do better
;         PlacerSearchStatus = t Search is done 
;
; Main program of running placer. 
;
;

defun(  PlacerSearch ( )


CurrentPlacedOnlyViewName= (if OptionKeepPlacerViewHistory 
  sprintf( nil "%s_%d" PlacedOnlyViewName CurrentStepNumber ) PlacedOnlyViewName )
CurrentPlacedScratchViewName= (if OptionKeepPlacerViewHistory 
  sprintf( nil "%s_%d" PlacedScratchViewName CurrentStepNumber ) PlacedScratchViewName ) 
CurrentPreRouteScratchViewName= (if OptionKeepPlacerViewHistory 
  sprintf( nil "%s_%d" PreRouteScratchViewName CurrentStepNumber ) PreRouteScratchViewName )

CurrentPlacedViewName= sprintf( nil "%s_%d" PlacedViewName CurrentStepNumber )
CurrentPreRouteViewName= sprintf( nil "%s_%d" PreRouteViewName CurrentStepNumber )
CurrentStepNumber= CurrentStepNumber + 1


;NWidth = nth( arrayref(NWidthSearchTable "trial") caddr(NWidthData)) 
;PWidth = nth( arrayref(PWidthSearchTable "trial") caddr(PWidthData)) 
;if((PWidth > 2.8) then PWidth = 2.8)
;if((NWidth > 2.8) then NWidth = 2.8)

CreateSchematicView()
ScratchView = CopyView( GenFromSourceViewName ScratchViewName )
errset( initnp( ScratchView ) t)
;errset( WellPlugsOff( ScratchView ) t)
errset( FoldGates( ScratchView ) t)
errset( TurnOffGateContact( ScratchView ) t)
errset( FoldAndChain( ScratchView ) t)
errset( GuessPlacementRegions( ScratchView ) t)
errset( PlacementRegions( ScratchView ) t)
errset( AddPinsToCellView( ScratchView nil ?power nil) t)

RunPlacer( ScratchViewName CurrentPlacedOnlyViewName )

ScratchView = CopyView( CurrentPlacedOnlyViewName ScratchViewName )
errset( CompactSuperstacks( ScratchView ) t)
errset( InlineSuperstacks( ScratchView t ) t)
errset( TurnOffGateContact( ScratchView ) t)
;SnapGatesStacksToPitch( ScratchView )

; Well plugs are to satisfy DRC rule of a wellplug every 30um

errset( DeCompact( ScratchView t ) t)
;errset( AddPlugs( ScratchView ) t)
;errset( DeCompact( ScratchView t ) t)
;errset( AddPlugs( ScratchView ) t)
;errset( DeCompact( ScratchView t ) t)

errset( Squish( ScratchView ) t)
SnapGatesStacksToPitch( ScratchView )
errset( RegionWell( ScratchView ) t)
errset( AddPlugs( ScratchView ) t)
FixMustConnectTerm( ScratchView )
errset( AddPinsToCellView( ScratchView nil ?power t) t)
errset( CopyView( ScratchViewName CurrentPreRouteScratchViewName ) t)
;AddBlkCell( ScratchView )
errset( RegionWell( ScratchView ) t)
errset( DrawImplant( ScratchView ) t)
;errset( ConnectWellPlugs( ScratchView ) t)
errset( VStruts( ScratchView ) t)
errset( CurrentPlacedScratchView = CopyView( ScratchViewName CurrentPlacedScratchViewName ) t)

printf("Start Assura Checking for view: %s\n" CurrentPlacedScratchViewName)
errset( PlacerCheck( CurrentPlacedScratchViewName (if ( not PlugsInRoutingStage ) ( list "LATCH_UP" ) ) ) t)

errset( PlacerSearchStatus = PlacerSearchDone( CurrentPlacedScratchViewName CurrentPlacedViewName CurrentPreRouteScratchViewName CurrentPreRouteViewName ) t)

; PlecerSearchStatus = nil ---> We do not yet have a placed view
; PlecerSearchStatus = 1   ---> We have a placed view, but trying to do better
; PlecerSearchStatus = t   ---> Search is done 
errset( printf( "NWidth= %f PWidth= %f\n" NWidth PWidth) t)

PlacerSearchStatus
)
