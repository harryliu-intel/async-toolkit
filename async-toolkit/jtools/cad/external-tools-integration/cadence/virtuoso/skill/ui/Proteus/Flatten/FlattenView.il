; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id: //depot/sw/main/cad/external-tools-integration/cadence/virtuoso/skill/ui/Nano/Flatten/FlattenView.il#13 $
; $DateTime: 2009/05/18 11:19:56 $
; $Author: stevemc $

; 
; menu item Nano/Flatten
;
( list "1.Create Flatten View..." "UICreateProteusFlattenView" )

( UICreateMenuItemCallBack
  `UICreateProteusFlattenViewCB
  (lambda ()
    let((CellView powerCellName CellBaseName OBSCellName BusWireCellName Components RComponents)
      CellView = UIGetCellView()
      Components = parseString( CellView~>cellName "." )
      CellBaseName = cadr( reverse( parseString( CellView~>cellName "." ) ))
      RComponents = reverse( Components )
      if( length(RComponents) >= 3 then
        powerCellName = sprintf( nil "%s_%s" cadr(RComponents) car(RComponents)) 
        OBSCellName = strcat( CellView~>libName ".wires." CellBaseName)
        BusWireCellName = strcat( CellView~>libName ".wires." CellBaseName "_buswires")
        powerCellName = sprintf( nil "%s.wires.%s_POWER_GRID_TIEOFF" 
                                 car( NameParseCellName( CellView~>cellName )) powerCellName )
        UICreateFlattenView_Form->UICreateFlattenView_OBSCellName->value = OBSCellName
        UICreateFlattenView_Form->UICreateFlattenView_PowerGridCellName->value = powerCellName
        UICreateFlattenView_Form->UICreateFlattenView_BusWireCellName->value = BusWireCellName
      )
      UICreateFlattenView_Form->UICreateAbstract_FulcrumPDKRoot->value=
        getShellEnvVar( "FULCRUM_PDK_ROOT" )
      if(UICreateFlattenView_Form->UICreateAbstract_DfIIDir->value== "default" then
        UICreateFlattenView_Form->UICreateAbstract_DfIIDir->value=
          NameGetDFIIDirFromCellName( UIGetCellView()~>cellName)
      )
      if(UICreateFlattenView_Form->UICreateAbstract_P4Client->value== "default" then
        UICreateFlattenView_Form->UICreateAbstract_P4Client->value=
          ConfigFileGetValue( TheCDSConfigTable "P4CLIENT" )
      )
      UICreateFlattenView_Form->UICreateFlattenView_FlattenCellsList->value=
        strcat( CellView~>cellName ".flatten_cells_list")
      UICreateFlattenView_Form->UICreateFlattenView_M3PowerPins->value=t
      UICreateFlattenView_Form->UICreateFlattenView_ResolveM3MacroConflict->value=t
      UICreateFlattenView_Form->UICreateFlattenView_Class->value="cover"
      hiDisplayForm( UICreateFlattenView_Form ) 
    )
  ) 
)
