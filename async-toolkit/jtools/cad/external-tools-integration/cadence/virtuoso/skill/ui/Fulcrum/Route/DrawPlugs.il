; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

( list "DrawPlugs" "UIDrawPlugs" )

( UICreateMenuItemCallBack
  `UIDrawPlugsCB
  (lambda ()
    ( hiDisplayForm UIDrawPlugs_Form ) ) )


(defun UIDrawPlugsGetCellRegionData ( CellView )
  ( PlacementRegionsScrapeCellRegionDataFromCellView
    CellView
    ( getq CellView instances )
    NChainLibCellPairRegExs
    PChainLibCellPairRegExs
    GateLibCellPairRegExs
    NPlugLibCellPairRegExs
    PPlugLibCellPairRegExs
    ( times UserUnitsPerMeter EvilGatePRegionOffset )
    ( times UserUnitsPerMeter MinWellSpacing )
    BoundaryLPP
    0.0
    ( times UserUnitsPerMeter MinWellSpacing )
    ) )

( UICreateAction
  `UIDrawPlugs_DrawWellCB
  (lambda ()
    (let (
          ( CellView ( UIGetCellView ) ) )
      (let (
            ( CellRegionData
              ( UIDrawPlugsGetCellRegionData CellView )
              ) )
        ( PlacementRegionsDrawCellRegionData
          CellView
          CellRegionData
          PWellLPP
          NWellLPP ) ) ) ) )

( UICreateAction
  `UIDrawPlugs_ConnectPlugsCB
  (lambda ()
    (let (
          ( CellView ( UIGetCellView ) ) )
      ( foreach NRegionPlug
                ( cadr ( NameFilterInstances
                         ( getq CellView instances )
                         ( list ( list WellPlugLibrary
                                       PWellPlugCellName ) ) ) )
                ( PlugsForceConnectPlug
                  NRegionPlug
                  CellView
                  GNDNetName
                  WellPlugTerminalName ) )

      ( foreach PRegionPlug
                ( cadr ( NameFilterInstances
                         ( getq CellView instances )
                         ( list ( list WellPlugLibrary
                                       NWellPlugCellName ) ) ) )
                ( PlugsForceConnectPlug
                  PRegionPlug
                  CellView
                  VddNetName
                  WellPlugTerminalName ) )
      ) ) )

( UICreateAction
  `UIDrawPlugs_DrawPlugsCB
  (lambda ()
    ( UIDrawPlugsAndImplant nil ) ) )

( UICreateAction
  `UIDrawPlugs_DrawImplantCB
  (lambda ()
    ( UIDrawPlugsAndImplant t ) ) )

(defun UIDrawPlugsAndImplant ( Mode )
    (let (
          ( CellView ( UIGetCellView ) ) )
      (let (
            ( WorkingDir
              ( ConfigFileGetValue TheCDSConfigTable "TEMP" )
               )
            ( PlugDRCRuleFile
              ( sprintf
                nil
                "%s/share/Fulcrum/plugs/plugs.assura.rules"
                ( ConfigFileGetValue TheCDSConfigTable "FULCRUM_PDK_ROOT" ) ) )
            ( CellRegionData
              ( UIDrawPlugsGetCellRegionData CellView )
              )
            )
        ( PlugsDrawPlugsOnCellView
          CellView
          CellRegionData
          WorkingDir
          PlugDRCRuleFile
          GNDNetName
          VddNetName
          ( DrawImplantFindPlugs
            CellView
            WellPlugLibrary
            PWellPlugCellName
            WellPlugViewName
            PImplantLPP
            ( times EvilGatePRegionOffset UserUnitsPerMeter )
            NImplantLPP
            ContactLPP )
        ( DrawImplantFindPlugs
            CellView
            WellPlugLibrary
            NWellPlugCellName
            WellPlugViewName
            NImplantLPP
            ( times EvilGatePRegionOffset UserUnitsPerMeter )
            NImplantLPP
            ContactLPP )
        WellPlugLibrary
        PWellPlugCellName
        NWellPlugCellName
        WellPlugViewName
        WellPlugLPP
        NImplantLPP
        PImplantLPP
        ( times UserUnitsPerMeter WellPlugEffectiveRadius )
        ( times UserUnitsPerMeter MinWellSpacing )
        ?LeaveMess t
        ?DrawPlugs ( not Mode )
        ?DrawImplant Mode
        ) ) ) )



( UICreateComponent
  "UIDrawPlugs_Form"
  ( UICreateConstructor
    (lambda ( Children )
      (let (
            ( Fields
              ( list
                ( hiCreateButton
                  ?name `UIDrawPlugs_Form_DrawWell
                  ?buttonText "Draw Well"
                  ?callback "( UIDrawPlugs_DrawWellCB )" )
                ( hiCreateButton
                  ?name `UIDrawPlugs_Form_DrawPlugs
                  ?buttonText "Draw Plugs"
                  ?callback "( UIDrawPlugs_DrawPlugsCB )" )
                ( hiCreateButton
                  ?name `UIDrawPlugs_Form_DrawImplant
                  ?buttonText "Draw Implied Implant"
                  ?callback "( UIDrawPlugs_DrawImplantCB )" )
                ( hiCreateButton
                  ?name `UIDrawPlugs_Form_ConnectPlugs
                  ?buttonText "ConnectPlugs"
                  ?callback "( UIDrawPlugs_ConnectPlugsCB )" )
                ) ) )
        ( hiCreateAppForm
          ?name `UIDrawPlugs_Form
          ?formTitle "Draw Plugs/Implant"
          ?fields Fields
          ) ) ) ) )


