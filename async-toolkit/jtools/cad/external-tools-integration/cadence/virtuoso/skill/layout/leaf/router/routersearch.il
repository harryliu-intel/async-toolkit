; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

; Function: RouterSearch
; Parameter: none
; Return: RouterSearchStatus = nil We do not yet have a placed view
;         RouterSearchStatus = 1 We have a placed view, but trying to do better
;         RouterSearchStatus = t Search is done 
;
; Main program of running router. 
;
;

defun(  RouterSearch ( )

CurrentPreKeepOutRoutedScratchViewName= (if OptionKeepRouterViewHistory 
   ( sprintf nil "%s_%d" PreKeepOutRoutedScratchViewName CurrentStepNumber ) 
   PreKeepOutRoutedScratchViewName ) 
CurrentPreKeepOutRoutedViewName= (if OptionKeepRouterViewHistory 
   ( sprintf nil "%s_%d" PreKeepOutRoutedViewName CurrentStepNumber ) PreKeepOutRoutedViewName )
CurrentRoutedScratchViewName= (if OptionKeepRouterViewHistory 
   ( sprintf nil "%s_%d" RoutedScratchViewName CurrentStepNumber ) RoutedScratchViewName )

CurrentRoutedViewName= ( sprintf nil "%s_%d" RoutedViewName CurrentStepNumber ) 
CurrentStepNumber= ( plus CurrentStepNumber 1 )


errset( RouterSearchInit( ) t)
errset( ScratchView = CopyView( CurrentPreRouteViewName ScratchViewName ) t)
errset( RouterWidth( ScratchView ) t)
errset( CreateSuperStacks( ScratchView ) t)
errset( SpaceStacks( ScratchView ColumnStackSpaceThreshold ColumnStackSpaceMinSpacing ColumnStackSpaceMaxShift ) t)
errset( InlineSuperstacks( ScratchView t ) t)

;errset( (if PlugsInRoutingStage WellPlugsOn( ScratchView ) ) t)
;errset( (if PlugsInRoutingStage AddPlugs( ScratchView ) ) t)
errset( RegionWell( ScratchView ) t)
errset( DrawImplant( ScratchView ) t)
errset( AddPinsToCellView( ScratchView nil ?power t) t)
;errset( ConnectWellPlugs( ScratchView ) t)
errset( VStruts( ScratchView ) t)
;errset( if( AutoPinTemplateFound==0 SnapFatPins( ScratchView )))
;errset( PreroutePowerVia( ScratchView ))
;errset( AddPrboundBoundary( ScratchView -0.24 ))
;errset( SetGLCParams( ScratchView "TRUE"))

CurrentPreKeepOutRoutedScratchView = CopyView( ScratchViewName CurrentPreKeepOutRoutedScratchViewName )
CurrentRoutedScratchView = CopyView( ScratchViewName CurrentRoutedScratchViewName )
RoutingGood = Router( CurrentRoutedScratchViewName )

;errset( DeletePrboundBoundary( CurrentRoutedScratchView ))
;errset( fixNegativeExtension( CurrentRoutedScratchView ))
;errset( SetGLCParams( CurrentRoutedScratchView "FALSE"))

(if RoutingGood then
  errset( RoutingGood = RouterCheck( CurrentRoutedScratchView )
    t)
)


;(if RoutingGood then

;  DrawDirectiveKeepout( CurrentRoutedScratchView )
;  errset( fixFatPolyContact( CurrentRoutedScratchView ))
;  errset( SwapVias( CurrentRoutedScratchView ))
;  errset( DelExtraPolyContact( CurrentRoutedScratchView ) t)
;  CurrentRoutedScratchView = nrOpenCellViewWritable( LibName CellName CurrentRoutedScratchViewName )
;  InvalidLPPs = list( "" )
;  RemoveInvalidLayers( CurrentRoutedScratchView )
;  errset( CheckPolygons( CurrentRoutedScratchView ) t)
;  LVSNodes( CurrentRoutedScratchView )
;)

RouterSearchStatus= RouterSearchDone( CurrentRoutedScratchViewName CurrentRoutedViewName CurrentPreKeepOutRoutedScratchViewName CurrentPreKeepOutRoutedViewName )
;LicenseReturnLicense( "300" )

RouterSearchStatus
)
