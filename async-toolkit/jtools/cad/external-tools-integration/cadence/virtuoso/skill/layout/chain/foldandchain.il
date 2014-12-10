; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

; Function: FoldAndChain
; Parameter: CellView (CVId)
; Return: CellView (CVId)
;
; Change the folding of the Gates to adjust the width of them
;
;
;

defun( FoldAndChain ( CellView )

  ;fold
  ( ChainFoldAllChains CellView
                       NChainLibCellPairRegExs 
                       ( difference NWidth ( times UserUnitsPerMeter NRegionOffsetFromColumnCenter ) )
                       UserUnitsPerMeter
                       ManufacturingGrid
                       PreferEvenFolds ) 
  ( ChainFoldAllChains CellView
                       PChainLibCellPairRegExs 
                       ( difference PWidth ( times UserUnitsPerMeter PRegionOffsetFromColumnCenter ) )
                       UserUnitsPerMeter

                       ManufacturingGrid
                       PreferEvenFolds )

  ;chain chains
  ( SuperStackCreateFromCellView
    CellView
    SuperStackLibraryName
    NSuperStackCellName
    SuperStackViewName
    NChainPrefix
    `SuperStackGetDefaultMergeFuncInfoLists_Chain
    ( list MaxNonPowerMergeHeight MaxPowerMergeHeight MaxMergeHeight )
    GNDNetName
    VddNetName
    NChainLibCellPairRegExs
    t
    nil )

  ( SuperStackCreateFromCellView
    CellView
    SuperStackLibraryName
    PSuperStackCellName
    SuperStackViewName
    PChainPrefix
    `SuperStackGetDefaultMergeFuncInfoLists_Chain
    ( list MaxNonPowerMergeHeight MaxPowerMergeHeight MaxMergeHeight )
    GNDNetName
    VddNetName
    PChainLibCellPairRegExs 
    t
    nil )

  ;stack gates
 ( SuperStackCreateFromCellView
    CellView
    SuperStackLibraryName
    GateSuperStackCellName
    SuperStackViewName
    GateChainPrefix
    `SuperStackGetDefaultMergeFuncInfoLists_Gate
    ( list MaxNonPowerMergeHeight 
           MaxPowerMergeHeight
           MaxMergeHeight
           GNDNetName 
           VddNetName)
    GNDNetName
    VddNetName
    GateLibCellPairRegExs
    nil
    nil )

  ( dbSave CellView )
 )

