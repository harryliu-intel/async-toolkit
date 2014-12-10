; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

/*DOC
Most of the time, we want genereated layout to be as compact as possible.  
However, there are cases when we want to 'decompact' the layout to allow for 
more routing bandwidth.<br>
In particular, the results of placement/superstack compaction results in unused area in the top and bottom of each column.  This module has code to spread out superstacks in the vertical direction.
<br>
<dl>
  <dt> InstanceTable
  <dd> Maps a YRange to a list of instances in that range.
  <dt>
  <dd>
</dl>
*/


defun( DeCompact ( CellView Recursive )
  
  ( DeCompactVerticalKeepOrderFromCellRegionData 
    CellView
    CellRegionData
    `DeCompactVerticalKeepOrderMindfulOfPlugs
    ( list
      (lambda ( Instance )
        ( and 
          ( equal ( getq Instance libName ) WellPlugLibrary )
          ( or
            ( equal ( getq Instance cellName ) NWellPlugCellName )
            ( equal ( getq Instance cellName ) PWellPlugCellName ) ) ) )
      ( times UserUnitsPerMeter MinWellSpacing )
      ( times UserUnitsPerMeter ManufacturingGrid )
      Recursive ) )

  ( dbSave CellView )
  CellView
 )

(defun DeCompactVerticalKeepOrderYRanges ( YRanges
                                           EnclosingYRange                        
                                           Recursive
                                           )
  (let (
        ( Table ( makeTable `bla nil ) )
            ( Offset 0.0 )
            ( WhiteSpace 
              ( difference
                ( RangeGetSize EnclosingYRange )
                (if YRanges ( RangeGetCover YRanges ) 0.0  ) ) )
            ( SortedRanges 
              ( sort ( copy YRanges )
                     (lambda ( a b ) ( lessp ( car a ) ( car b ) ) ) ) ) )
    (cond (
           Recursive
           (let (
                 ( Spacing
                   (cond (
                          ( lessp WhiteSpace 0.0 )
                          ( quotient WhiteSpace
                                     ( max
                                       1
                                       ( difference ( length YRanges ) 1 ) )  ) )
                         (
                          ( quotient WhiteSpace
                                     ( plus ( length YRanges ) 1 ) )  ) ) ) )
             (let (
                   ( Bottom 
                     (cond (
                            ( lessp WhiteSpace 0.0 )
                            ( car EnclosingYRange ) )
                           (
                            ( plus
                              ( car EnclosingYRange )
                              Spacing ) ) ) ) )
               ( foreach Range SortedRanges
                         (let (
                               ( Size ( RangeGetSize Range ) ) )
                           ( setarray Table Range 
                                      ( list Bottom ( plus Bottom Size )  ) )
                           ( setq Bottom ( plus Bottom 
                                                Spacing
                                                Size ) ) ) ) ) ) )
          (
           t
           (let (
                 ( Regions 
                   ( plus 
                     1
                     ( ceiling 
                       ( quotient 
                         ( RangeGetSize EnclosingYRange )
                         40.0 ) ) ) ) )
             (let (
                   ( Spacing 
                     ( quotient WhiteSpace ( difference Regions 1 ) ) )
                   ( RegionSpacing 
                     ( quotient ( RangeGetSize EnclosingYRange ) Regions ) )
                   ( RegionBoundary 
                     ( quotient ( RangeGetSize EnclosingYRange ) Regions ) )
                   ( Below t )
                   ( Bottom ( car EnclosingYRange ) ) )
               ( foreach 
                 Range SortedRanges
                 (let (
                       ( Size ( RangeGetSize Range ) ) )
                   (when ( and 
                           Below
                           ( greaterp ( car Range ) RegionBoundary ) )
                     ( setq RegionBoundary
                            ( plus RegionBoundary RegionSpacing) )
                     ( setq Bottom
                            ( plus Bottom Spacing ) ) )
                   ( setq Below
                          ( leqp ( car Range ) RegionBoundary ) )
                   ( setarray Table Range 
                              ( list Bottom ( plus Bottom Size ) ) )
                   ( setq Bottom
                          ( plus Bottom 
                                 Size ) ) ) ) ) ) ) )
    ( mapcar 
      (lambda ( Range )
        ( arrayref Table Range ) )
      YRanges )
    ) )


(defun DeCompactGetInstanceTable ( Instances )
  ( FigGetClumpTable
    Instances
    ?Clumpify (lambda ( C ) ( RectGetYRange ( getq C bBox ) ) )
    ?ClumpUnion `RangeUnion
    ?ClumpPredicate `RangeIntersection ) )

(defun DeCompactVerticalKeepOrderDefault ( Instances
                                           BoundingRange
                                           Grid
                                           Recursive )
  (let (
        ( InstanceTable
          ( DeCompactGetInstanceTable 
            Instances ) ) )
      (let (
            ( UnionRanges
              ( mapcar `car ( tableToList InstanceTable ) ) ) )
        ( println ( tableToList InstanceTable ) )

        ( DeCompactVerticalKeepOrder 
          InstanceTable
          UnionRanges
          BoundingRange
          Grid
          Recursive ) ) ) )

  
(defun DeCompactVerticalKeepOrderMindfulOfPlugs ( Instances
                                                  BoundingRange
                                                  PlugPredicate
                                                  MinWellSpacing
                                                  Grid
                                                  Recursive )
  (let (
        ( InstanceTable
          ( DeCompactGetInstanceTable 
            Instances ) ) )
      (let (
            ( UnionRanges
              ( mapcar `car ( tableToList InstanceTable ) ) ) )
        (let (            
              ( BottomRange 
                ( ListFindMinimumElement
                  UnionRanges
                  (lambda ( Range ) ( car Range ) ) ) )
              ( TopRange 
                ( ListFindMaximumElement
                  UnionRanges
                  (lambda ( Range ) ( cadr Range ) ) ) ) )
          (let (
                ( BoundingRangeToDecompact
                  ( list
                    (if ( exists 
                          Instance 
                          ( arrayref InstanceTable BottomRange ) 
                          ( and 
                            ( apply PlugPredicate ( list Instance ) )
                            ( lessp 
                              ( difference 
                                ( RectGetBottom ( getq Instance bBox ) )
                                ( car BottomRange ) )
                              MinWellSpacing ) ) )
                        ( cadr BottomRange ) ( car BoundingRange ) )
                    (if ( exists 
                          Instance
                          ( arrayref InstanceTable TopRange ) 
                          ( and
                            ( apply PlugPredicate ( list Instance ) )
                            ( lessp 
                              ( difference 
                                ( cadr TopRange )
                                ( RectGetTop ( getq Instance bBox ) ) )
                              MinWellSpacing ) ) )
                        ( car TopRange ) ( cadr BoundingRange ) ) ) ) )
            (let (
                  ( RangesToDecompact
                    ( setof Range UnionRanges
                            ( RangeIsRangeInRangeClose 
                              Range
                              BoundingRangeToDecompact
                              1e-9 ) ) ) )
              ( DeCompactVerticalKeepOrder
                InstanceTable
                RangesToDecompact
                BoundingRangeToDecompact
                Grid
                Recursive )
              (let (
                    ( Plugs 
                      ( setof 
                        Instance 
                        Instances
                        ( apply PlugPredicate ( list Instance ) ) ) ) )
                ( PlugsClean
                  Plugs 
                  MinWellSpacing )

              ) ) ) ) ) ) )

(defun DeCompactVerticalKeepOrder ( InstanceTable
                                    RangesToDecompact
                                    BoundingRangeToDecompact
                                    Grid
                                    Recursive )

  (let (
        ( NewRanges          
          ( DeCompactVerticalKeepOrderYRanges
            RangesToDecompact
            BoundingRangeToDecompact
            Recursive ) ) )

    ( mapcar 
      (lambda ( OldRange NewRange )
        (let (
              ( Offset 
                ( difference 
                  ( MathRoundToNearest ( car NewRange ) Grid )
                  ( car OldRange ) ) ) )
          ( foreach Instance ( arrayref InstanceTable OldRange )
                    ( dbSetq Instance 
                             ( list ( car ( getq Instance xy ) )
                                    ( plus ( cadr ( getq Instance xy ) )
                                           Offset ) )
                             xy ) ) ) )
      RangesToDecompact
      NewRanges ) ) )

(defun DeCompactVerticalKeepOrderFromCellRegionData ( CellView
                                                      CellRegionData
                                                      CompactFunc
                                                      CompactFuncParams )

  ( foreach 
    ColumnData
    ( PlacementRegionsGetCellRegionDataColumnList CellRegionData )
    (let (
          ( BoundingRect
            ( PlacementRegionGetColumnBBox ColumnData ) ) )
      (let (
            ( Instances
              ( setof Instance ( getq CellView instances )
                      ( RangeIsRangeInRangeClose
                        ( RectGetXRange ( getq Instance bBox ) )
                        ( RectGetXRange BoundingRect )
                        1e-9 ) ) ) )
        ( apply CompactFunc
                ( cons Instances
                       ( cons ( RectGetYRange BoundingRect )
                              CompactFuncParams ) ) ) ) ) )
  nil
  )
