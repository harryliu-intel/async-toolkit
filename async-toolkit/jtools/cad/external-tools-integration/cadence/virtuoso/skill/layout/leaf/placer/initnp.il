; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

; Function: initnp
; Parameter: CellView (CVId) 
; Return: CellView (CVId)
;
; Initialize NWidthData & PWidthData used for binary search.
; NwidthData are for N-type components and PWidthData are for
; P-type components. Finds the minimum NWidth and PWidth that
; will fit all the components. 
;
; Their data structure looks like:
; NWidthData=
;  (table:ComponentInfoListTable table:GateComponentInfoTable 
;     (1.92 2.4 2.88 3.36 3.84 5.28 6.24) 
;      3.84
;  )
;
; It means:
;  (table:ComponentInfoListTable table:GateComponentInfoTable 
;     (all the possible widths of all components rounded up to
;      the nearest multiple of pitch size 0.48) 
;      The smallest width that can fit all of the component
;  )
;
; There are two type of components: gate and stack. 
;  tables ComponentInfoListTable and GateComponentInfoTable 
;  contains all the possible length and width for the instances.
;  ComponentInfoListTable contains instance who is a stack component;
;  while GateComponentInfoTable contains instance who is a gate.
;  A stack component is a group of gate stacked together.
;
; A such table looks like this: 
; (   ("_r.0" 
;        (("_r.0" 1.82 5.06) 
;            ("_r.0" 1.91 4.23) 
;            ("_r.0" 2.05 3.4) 
;        )
;    )
;    ("R.3" 
;        (("R.3" 2.01 1.43))
;    ) 
; )
;
; It means:
; (   (instance1 
;        ((instance1 possible_width1 possible_height1)
;            (instance1 possible_width2 possible_height2)
;            (instance1 possible_width3 possible_height3)
;        )
;    ) There are three possible dimensions for this device.
;    (instance2
;        ((instance2 possible_width1 possible_height1))
;    ) There is just one possible dimension for this device.
;    ....
; )
;
; NWidthSearchTable and PWidthSearchTable records the search effort and looks like: 
;  (("iters" 0) ("trial" 6) ("type" "binary") ("max" 6) ("done" nil) ("min" 0))
; It means:
;  ((iteration number) (index of trial compoent) (search type) 
;   (index of the component with max width) (done?) (index of the component with min width))
;
; NWidth and PWidth will look up the actual width from NWidthSearchTable and PWidthSearchTable
;

defun( initnp ( CellView )
  
  ;get initial 
  (unless NWidthData
    (defvar NWidthData 
      ( NPSearchGetWidthData 
        ( cadr ( NameFilterInstances 
                 ( getq CellView instances )
                 GateLibCellPairRegExs ) )
        ( cadr ( NameFilterInstances 
                 ( getq CellView instances )
                 NChainLibCellPairRegExs ) )
        ( times UserUnitsPerMeter EvilGatePRegionOffset )
        ( times UserUnitsPerMeter TransistorMinimumContactableWidth )
        ( times UserUnitsPerMeter TransistorDiffusionToBBoxDistance )
        MaximumComponentWidthRatio
        ( times UserUnitsPerMeter NRegionOffsetFromColumnCenter )
        ( times UserUnitsPerMeter 
                ( CellInfoGetLayerWirePitchInMeters
                  DirectiveTable
                  Metal3LPP
                  DirectiveUnitsPerMeter ) )        
        MaximumNColumnWidth
        UserUnitsPerMeter
        t
        PreferEvenFolds
        ( times UserUnitsPerMeter ExtraGateWidth ) ) )
    ( BinarySearchInitTable
      NWidthSearchTable 
      0
      ( difference ( length ( NPSearchGetWidthVectorFromWidthData NWidthData ) ) 1 )
      ( difference ( length ( NPSearchGetWidthVectorFromWidthData NWidthData ) ) 1 ) ) 
)
  
  (unless PWidthData
    (defvar PWidthData
      ( NPSearchGetWidthData 
        ( cadr ( NameFilterInstances 
                 ( getq CellView instances )
                 GateLibCellPairRegExs ) )
        ( cadr ( NameFilterInstances 
                 ( getq CellView instances )
                 PChainLibCellPairRegExs ) )
        ( times UserUnitsPerMeter EvilGatePRegionOffset )
        ( times UserUnitsPerMeter TransistorMinimumContactableWidth )
        ( times UserUnitsPerMeter TransistorDiffusionToBBoxDistance )
        MaximumComponentWidthRatio
        ( times UserUnitsPerMeter PRegionOffsetFromColumnCenter )
        ( times UserUnitsPerMeter 
                ( CellInfoGetLayerWirePitchInMeters
                  DirectiveTable
                  Metal3LPP
                  DirectiveUnitsPerMeter ) )
        MaximumPColumnWidth
        UserUnitsPerMeter
        nil
        PreferEvenFolds
        ( times UserUnitsPerMeter ExtraGateWidth )
        ) )
    ( BinarySearchInitTable 
      PWidthSearchTable 
      0
      ( difference ( length ( NPSearchGetWidthVectorFromWidthData PWidthData ) ) 1 )
      ( difference ( length ( NPSearchGetWidthVectorFromWidthData PWidthData ) ) 1 ) ) 

)
  
  ; if we're searching width now, then make first real guess ( # of columns is final now )
  ; we have CellRegionData from the last run...all we care about is the number of columns,
  ; and the height of the regions which will change only if the current search table
  ; is the ColumnSearchTable
    (cond (
           ( and ( equal CurrentSearchTable NWidthSearchTable )
                 ( null NMinimumTrialMaximumTriple ) )
           (defvar NMinimumTrialMaximumTriple
             ( NPSearchGetMinimumTrialMaximumTriple
               ( PlacementRegionsGetTotalNRegionHeight CellRegionData )
               NWidthData
               NColumnWidthParamName
               PColumnWidthParamName
               CellView
               SuperStackLibraryName
               NSuperStackCellName
               GateSuperStackCellName
               SuperStackViewName
               NChainLibCellPairRegExs
               GateLibCellPairRegExs
               UserUnitsPerMeter
               1e-9
               ( list MaxNonPowerMergeHeight
                      MaxPowerMergeHeight
                      MaxMergeHeight )
               GNDNetName
               VddNetName
               PreferEvenFolds
               nil ) )
           ( BinarySearchInitTable 
             NWidthSearchTable 
             ( car NMinimumTrialMaximumTriple )
             ( caddr NMinimumTrialMaximumTriple )
             ( min                                 
               ( caddr NMinimumTrialMaximumTriple )
               ( plus ( cadr NMinimumTrialMaximumTriple ) 1 ) ) ) )
          (
           ( and ( equal CurrentSearchTable PWidthSearchTable )
                 ( null PMinimumTrialMaximumTriple ) )
           (defvar PMinimumTrialMaximumTriple
             ( NPSearchGetMinimumTrialMaximumTriple
               ( PlacementRegionsGetTotalPRegionHeight CellRegionData )
               PWidthData
               PColumnWidthParamName
               NColumnWidthParamName
               CellView
               SuperStackLibraryName
               PSuperStackCellName
               GateSuperStackCellName
               SuperStackViewName
               PChainLibCellPairRegExs
               GateLibCellPairRegExs
               UserUnitsPerMeter
               1e-9
               ( list MaxNonPowerMergeHeight
                      MaxPowerMergeHeight
                      MaxMergeHeight
                      )
               GNDNetName
               VddNetName
               PreferEvenFolds
               nil ) )
           ( BinarySearchInitTable
             PWidthSearchTable 
             ( car PMinimumTrialMaximumTriple )
             ( caddr PMinimumTrialMaximumTriple )
             ( min                                 
               ( caddr PMinimumTrialMaximumTriple )
               ( plus ( cadr PMinimumTrialMaximumTriple ) 1 ) ) ) ) )
    


  ;choose widths
  (defvar NWidth 
    ( NPSearchGetWidthFromIndex 
      NWidthData 
      (if ( equal CurrentSearchTable NWidthSearchTable )
          ( arrayref NWidthSearchTable "trial" )
        ( arrayref NWidthSearchTable "max" ) ) ) )
  (defvar PWidth 
    ( NPSearchGetWidthFromIndex
      PWidthData
      (if ( equal CurrentSearchTable PWidthSearchTable )
          ( arrayref PWidthSearchTable "trial" )
        ( arrayref PWidthSearchTable "max" ) ) ) )


  ( printf "NWidthData comp: %L\n" ( tableToList ( nth 0 NWidthData ) ) )
  ( printf "NWidthData gate: %L\n" ( tableToList ( nth 1 NWidthData ) ) )

  ( printf "PWidthData comp: %L\n" ( tableToList ( nth 0 PWidthData ) ) )
  ( printf "PWidthData gate: %L\n" ( tableToList ( nth 1 PWidthData ) ) )

  ( printf "NWidthSearchTable: %L\n" ( tableToList NWidthSearchTable ) )
  ( printf "PWidthSearchTable: %L\n" ( tableToList PWidthSearchTable ) )

  
  ( printf "NWidth: %f\n"  NWidth )
  ( printf "PWidth: %f\n"  PWidth )


  ( dbSave CellView )
  CellView
 )



