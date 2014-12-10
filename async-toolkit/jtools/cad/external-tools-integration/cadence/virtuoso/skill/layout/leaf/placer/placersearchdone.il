; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

; Function: PlacerSearchDone
; Parameter: CurrentPlacedScratchViewName (string)
;            CurrentPlacedViewName (string)
;            CurrentPreRouteScratchViewName (string)
;            CurrentPreRouteViewName (string)

; Return: RPLRet = nil We do not yet have a placed view
;         RPLRet = 1 We have a placed view, but trying to do better
;         RPLRet = t Search is done 
;
; Check if everything is done with placer search.
;
;
;
defun(  PlacerSearchDone ( CurrentPlacedScratchViewName 
                          CurrentPlacedViewName  
                          CurrentPreRouteScratchViewName
                          CurrentPreRouteViewName
                         )

  ;step 
  ( SearchFindSmallestStep
    CurrentSearchTable 
    ( lambda ( Trial ) PlacementGood )
    nil )

  (let (
        ( PlacementComplete 
            ( forall SearchTable 
                     ( list ColumnSearchTable NWidthSearchTable PWidthSearchTable )
                     ( arrayref SearchTable "done" ) ) ) )
  if( PlacementGood then 
       PlacedView = dbOpenCellViewByType( 
                   LibName
                   CellName
                   CurrentPlacedScratchViewName
                   "maskLayout"
                   "a" ) 
       PlacedAreaBoundBox = PinUtilFindPRBoundBBox( 
                       BoundaryLPP
                       PlacedView ) 
       if((PlacedAreaBoundBox == nil ) then
         PlacementGood = nil
         PlacementComplete = t      ; Placer exist abnormally, so do not search any more.         
         PlacedArea = -1
         printf( "Placer exit abnormally.]n")
       ) 
  )
  (cond (
         PlacementGood 
         (let (
               ( PlacedView 
                 ( dbOpenCellViewByType 
                   LibName
                   CellName
                   CurrentPlacedScratchViewName
                   "maskLayout"
                   "a" ) )
               ( PreRouteView
                 ( dbOpenCellViewByType 
                   LibName
                   CellName
                   CurrentPreRouteScratchViewName 
                   "maskLayout"
                   "a" ) ) )
             ( dbSave
               ( dbCopyCellView
                 PlacedView
                 LibName
                 CellName
                 CurrentPlacedViewName
                 nil
                 nil
                 t ) )
             ( dbSave
               ( dbCopyCellView
                 PreRouteView
                 LibName
                 CellName
                 CurrentPreRouteViewName
                 nil
                 nil
                 t ) )
           PlacedAreaBoundBox = PinUtilFindPRBoundBBox( 
                       BoundaryLPP
                       PlacedView ) 
           if((PlacedAreaBoundBox != nil) then PlacedArea = RectGetArea( PlacedAreaBoundBox )
               printf( "Placement Passed\n" )
               printf( "PlacedArea %f BestPlacedViewArea %f\n" PlacedArea BestPlacedViewArea)
               printf( "%s: Column %d NWidth %f PWidth %f\n " CurrentPlacedViewName
                  ( arrayref ColumnSearchTable "trial" )
                  nth( arrayref(NWidthSearchTable "trial") caddr(NWidthData))
                  nth( arrayref(PWidthSearchTable "trial") caddr(PWidthData))
               )        
             else ; double check just in case. 
               printf( "Placer exit abnormally.]n")
               PlacedArea = -1
               PlacementComplete = t ; Placer exist abnormally, so do not search any more.
           )
           (when ( PlacedArea > 0 && 
                   PlacedArea < BestPlacedViewArea )
             (defvar BestPlacedViewArea PlacedArea )
             (defvar BestPlacedViewName CurrentPlacedViewName )
             (defvar BestPreRouteViewName CurrentPreRouteViewName )
           ) 
         ) )
        (
         ( printf "Placement Failed\n" ) ) )

  (cond (
         ( or PlacementGood ( arrayref CurrentSearchTable "done" ) )
        ;if placement is good, switch to another search
        ;switch to the side that has worst width ratios
         ( setq CurrentSearchTable
                (cond (
                       ( not ( arrayref ColumnSearchTable "done" ) )
                       ColumnSearchTable )
                      (
                       ( ListFindMaximumElement
                         ( setof SearchTable 
                                 ( list NWidthSearchTable PWidthSearchTable )
                                 ( not ( arrayref SearchTable "done" ) ) )
                         (lambda ( SearchTable )
                           ( NPSearchGetMaxWidthMeanWidthRatioFromWidthData
                             (if ( equal SearchTable NWidthSearchTable )
                                 NWidthData PWidthData )
                             (if ( equal SearchTable NWidthSearchTable )
                                 NWidth PWidth ) ) )
                           ) )
                      (
                       CurrentSearchTable ) ) ) ) )
  (defvar RPLRet nil )
  (cond (
         ;If we have a placed view, but we can do better
         ;reset everything and try the next column count
         ( and PlacementComplete 
               ( ddGetObj LibName CellName BestPlacedViewName )
               (let (
                     ( PossibleArea
                       ( times
                         YPitch
                         ( plus ColumnCount 1 )
                         ( plus
                           ( NPSearchGetMinFitWidthFromWidthData
                             NWidthData  )
                           ( NPSearchGetMinFitWidthFromWidthData
                             PWidthData  ) ) ) ) )
                 ( printf "%f %f" PossibleArea BestPlacedViewArea )
                 ( lessp
                   PossibleArea
                   BestPlacedViewArea ) ) )
         (defvar PlacementComplate nil )
         (defvar NMinimumTrialMaximumTriple nil )
         (defvar PMinimumTrialMaximumTriple nil )
         (defvar NWidthSearchTable ( makeTable `foo ) )
         (defvar PWidthSearchTable ( makeTable `bar ) )
         (defvar NWidthData nil )
         (defvar PWidthData nil )
         (defvar CellRegionData nil )
         (defvar ColumnCount ( plus ColumnCount 1 ) )
         ( BinarySearchInitTable
           ColumnSearchTable
           ColumnCount
           ColumnCount
           ColumnCount )
         (defvar CurrentSearchTable ColumnSearchTable )
         (defvar RPLRet 1 ) 
         ( printf "Search for %d Columns Completed. Trying %d Columns...\n" ColumnCount-1  ColumnCount ) 

        )
        (
         ;Really done
         PlacementComplete 
         ( printf "Completed Search\n" ) 
         ( setq CurrentSearchTable nil )
         (when ( and BestPlacedViewName
                     BestPreRouteViewName
                     ( ddGetObj LibName CellName BestPlacedViewName )
                     ( ddGetObj LibName CellName BestPreRouteViewName ) )
           (when ( not ( equal BestPlacedViewName PlacedViewName ) )
             ( dbSave 
               ( dbCopyCellView
                 ( dbOpenCellViewByType 
                   LibName
                   CellName
                   BestPlacedViewName
                   "maskLayout"
                   "a" )
                 LibName
                 CellName
                 PlacedViewName
                 nil
                 nil
                 t ) ) )
           (when ( not ( equal BestPreRouteViewName PreRouteViewName ) )
             ( dbSave
               ( dbCopyCellView
                 ( dbOpenCellViewByType 
                   LibName
                   CellName
                   BestPreRouteViewName
                   "maskLayout"
                   "a" )
                 LibName
                 CellName
                 PreRouteViewName
                 nil
                 nil
                 t ) ) )
           ( FileUtilAppendStringToFileAndClose 
             LogFileName 
             ( sprintf 
               nil
               "%s placed density factor: %f / %f\n" 
               CellName 
               ( quotient ( RectGetArea 
                            (cond ( PinUtilFindPRBoundBBox(
                              BoundaryLPP
                              ( dbOpenCellViewByType 
                                LibName
                                CellName
                                PlacedViewName
                                "maskLayout"
                                "a" ) ) ) list(0:0 0:0) ))
                          TotalTransistorArea )
               ( CellInfoLookup DirectiveTable "density_factor" ) || 10.00
               ) ) )
         (defvar RPLRet t ) )
        (
         t
         (defvar RPLRet 1 )
         ( printf "Search Not Complete\n" ) ) )
  
  (when CurrentSearchTable
    ( println ( tableToList ColumnSearchTable ) )
    ( println ( tableToList NWidthSearchTable ) )
    ( println ( tableToList PWidthSearchTable ) ) ) ) 

RPLRet
)
  
