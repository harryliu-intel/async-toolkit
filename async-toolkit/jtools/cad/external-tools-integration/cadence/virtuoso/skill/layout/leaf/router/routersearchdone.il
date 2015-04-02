; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

; Function: RouterSearchDone
; Parameter: CurrentRoutedScratchViewName (string)
;            CurrentRoutedViewName ( string )
;            CurrentPreKeepOutRoutedScratchViewName ( string )
;            CurrentPreKeepOutRoutedViewName ( string )
;            
; Return:  RoutingComplete (boolean)
;
; Check if routing is done

defun( RouterSearchDone ( CurrentRoutedScratchViewName
                          CurrentRoutedViewName
                          CurrentPreKeepOutRoutedScratchViewName
                          CurrentPreKeepOutRoutedViewName)
            
    ;do the search step
    ( SearchFindSmallestStep 
      CurrentSearchTable 
      (lambda ( Trial ) RoutingGood )
      nil )

    (cond (
           RoutingGood 
           ( printf "Routing Passed\n" )  
           CurrentRoutedScratchView=dbOpenCellViewByType(
                 LibName
                 CellName
                 CurrentRoutedScratchViewName
                 "maskLayout"
                 "a" )
           ; I realize that this is the wrong place to do these
           ; post-processing things, but it's the best place I could find.
;           PinUtilDeleteM2PowerPins( CurrentRoutedScratchView )
           (let (NWList) 
             (foreach shape CurrentRoutedScratchView->shapes
               (when (car shape->lpp)=="PW" 
                 (dbDeleteObject shape)
               )
               (when (car shape->lpp)=="NW"
                 (if !NWList then NWList=(list shape)
                 else NWList=(cons shape NWList) )
               )
             )
             (leMergeShapes NWList)
            )

           dbSave(CurrentRoutedScratchView)
             
           ( dbSave
             ( betterCopyCellView
               CurrentRoutedScratchView
               LibName
               CellName
               CurrentRoutedViewName
               nil
               nil
               t ) )
           ( dbSave
             ( betterCopyCellView
               ( dbOpenCellViewByType
                 LibName
                 CellName
                 CurrentPreKeepOutRoutedScratchViewName
                 "maskLayout"
                 "a" )
               LibName
               CellName
               CurrentPreKeepOutRoutedViewName
               nil
               nil
               t ) )
           (let (
                 ( CurrentRoutedViewArea
                   ( RectGetArea 
                       ( dbOpenCellViewByType
                         LibName
                         CellName
                         CurrentRoutedViewName
                         "maskLayout"
                         "a" ) -> prBoundary -> bBox
                    ) ) ) 
             (when
                 ( lessp 
                   CurrentRoutedViewArea 
                   BestRoutedViewArea )
               (defvar BestRoutedViewName CurrentRoutedViewName )
               (defvar BestRoutedViewArea CurrentRoutedViewArea )
               (defvar BestPreKeepOutRoutedViewName CurrentPreKeepOutRoutedViewName ) ) ) )
          (
           ( printf "Routing Failed:\n" ) ) )
        
    ;initial search
    (when ( and RoutingGood
                ( not InitialRoutingSearchDone ) )
      (defvar InitialRoutingSearchPassed t ) )
    (if ( arrayref RouterGapWidthInitialSearchTable "done" )
        (defvar InitialRoutingSearchDone t ) )

    (let (
          ( RoutingComplete 
            ( and InitialRoutingSearchDone
                  ;if initial search, then we're done if
                  ; there was no success (failed run)
                  ; detailed routersearch is set off
                  ; we finished with no router width
                  ;if detailed search done if all gap searches are done
                  (if ( equal CurrentSearchTable RouterGapWidthInitialSearchTable )
                      ( or ( not InitialRoutingSearchPassed )
                           (or ( not OptionDetailedRouterSearch )
                               ( equal ( arrayref CurrentSearchTable "trial" ) 0 ) ) )
                    ( forall RouterGapWidthSearchTable RouterGapWidthSearchTableList
                             ( arrayref RouterGapWidthSearchTable "done" ) ) ) ) ) )

      (cond (
             ( and InitialRoutingSearchDone OptionDetailedRouterSearch )                      
             (cond (
                    ( or RoutingGood ( arrayref CurrentSearchTable "done" ) )
                    ( setq CurrentSearchTable 
                           (cond (
                                  ( ListFindMaximumElement
                                    ( remove CurrentSearchTable 
                                             ( setof RouterGapWidthSearchTable RouterGapWidthSearchTableList
                                                     ( not ( arrayref RouterGapWidthSearchTable "done" ) ) ) )
                                    (lambda ( RouterGapWidthSearchTable )
                                      ( arrayref RouterGapWidthSearchTable "max" ) ) ) )
                                 ( 
                                  CurrentSearchTable ) ) ) ) ) ) 
      )

      if( RoutingComplete then
        printf( "Routing Search Completed\n" )
      else
        printf( "Routing Search Not Complete\n" ) 
      )

      ( printf "Route View: %s of %L\n"
               ( car PreRouteViewNames )
               PreRouteViewNames )
      ;print current search tables
      (cond (
             RouterGapWidthSearchTableList
             ( mapcar 
               (lambda ( RouterGapWidthSearchTable )
                 ( println ( tableToList RouterGapWidthSearchTable ) ) )
               RouterGapWidthSearchTableList ) )
            ( println ( tableToList CurrentSearchTable ) ) )
      
      ;choose next view to chug on
      (defvar PreRouteViewNames
        (if RoutingComplete
            (let ()
              ;reset
              (defvar InitialRoutingSearchDone nil )
              (defvar InitialRoutingSearchPassed nil )
              (defvar RouterGapWidthInitialSearchTable nil )
              ( exists SomePreRouteViewName ( cdr PreRouteViewNames )
                       ( lessp 
                         ( RectGetArea 
                           ( PinUtilFindPRBoundBBox
                             BoundaryLPP
                             ( dbOpenCellViewByType
                               LibName
                               CellName
                               SomePreRouteViewName
                               "maskLayout"
                               "a" )
                             ) )
                         BestRoutedViewArea ) ) )
              PreRouteViewNames ) )

      (defvar RPLRet t )
      (cond (
             ;if more views to try, try them             
             ( car PreRouteViewNames )
             (defvar RPLRet 1 ) )
            (
             ;else we're done. If there's any routed view, turn it into THE routed view
             ( and BestRoutedViewName
                   BestPreKeepOutRoutedViewName )
             (cond (
                    ( and BestPreKeepOutRoutedViewName
                          BestRoutedViewName
                          ( ddGetObj LibName CellName BestPreKeepOutRoutedViewName )
                          ( ddGetObj LibName CellName BestRoutedViewName )
                          )
                    (if ( not ( equal BestPreKeepOutRoutedViewName PreKeepOutRoutedViewName ) )
                        ( dbSave
                          ( betterCopyCellView
                            ( dbOpenCellViewByType
                              LibName
                              CellName
                              BestPreKeepOutRoutedViewName
                              "maskLayout"
                              "a" )
                            LibName
                            CellName
                            PreKeepOutRoutedViewName
                            nil
                            nil
                            t ) ) )
                    (if ( not ( equal BestRoutedViewName RoutedViewName ) )
                        ( dbSave
                          ( betterCopyCellView
                            ( dbOpenCellViewByType
                              LibName
                              CellName
                              BestRoutedViewName
                              "maskLayout"
                              "a" )
                            LibName
                            CellName
                            RoutedViewName
                            nil
                            nil
                            t ) ) )

                    (let (
                          ( CellView 
                            ( betterCopyCellView
                              ( dbOpenCellViewByType
                                LibName
                                CellName
                                RoutedViewName
                                "maskLayout"
                                "a" )
                              LibName
                              CellName
                              ScratchViewName
                              nil
                              nil
                              t ) ) )

                      ( dbReopen CellView "a" )

                      ( KeepOutDrawKeepOut 
                        CellView      
                        DirectiveTable
                        ( mapcar 
                          (lambda ( LPP )
                            ( KeepOutCreateRoutingLayerStruct LPP ) )
                          ( list Metal3LPP ) )
                        ( mapcar 
                          (lambda ( LPP )
                            ( KeepOutCreateRoutingLayerStruct LPP ) )
                          nil )                       
                        BoundaryLPP 
                        UserUnitsPerMeter
                        DirectiveUnitsPerMeter
                        ( list GNDNetName VddNetName )
                        ( list t nil )
                        t
                        t
                        1 )

                      ( KeepOutCreateAbstractView 
                        CellView
                        LibName
                        CellName
                        AbstractViewName
                        KeepOutDRCRuleFile
                        WorkingDir
                        BoundaryLPP
                        ( mapcar 
                          (lambda ( LPP )
                            ( KeepOutCreateAbstractLayerStruct LPP ) )
                          ( list Metal3LPP ) )
                        ( list GNDNetName VddNetName ) )

                    ( FileUtilAppendStringToFileAndClose 
                      LogFileName 
                      ( sprintf 
                        nil
                        "%s routed density factor: %f / %f\n" 
                        CellName 
                        ( quotient ( RectGetArea 
                                     ( PinUtilFindPRBoundBBox
                                       BoundaryLPP
                                       CellView
                                       ) )
                                   TotalTransistorArea )
                        ( CellInfoLookup DirectiveTable "density_factor" )
                        ) ) ) ) ) ) ) ) 
RPLRet
)

