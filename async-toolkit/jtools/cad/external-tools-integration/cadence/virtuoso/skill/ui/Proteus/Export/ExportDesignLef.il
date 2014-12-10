; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id: //depot/sw/main/cad/external-tools-integration/cadence/virtuoso/skill/ui/Nano/Export/ExportDesignLef.il#3 $
; $DateTime: 2006/07/07 15:13:59 $
; $Author: tomjones $

; 
; menu item Nano/Export
;
( list "2.Export Design LEF..." "UIExportDesignLefP" )


( UICreateMenuItemCallBack
  `UIExportDesignLefPCB
  (lambda ()
    let((CellView powerCellName Components RComponents)
      CellView = UIGetCellView()
      Components = parseString( CellView~>cellName "." )
      RComponents = reverse( Components )
      if( length(RComponents)>=3 then
        powerCellName = sprintf( nil "%s_%s" cadr(RComponents) car(RComponents)) 
        powerCellName = sprintf( nil "%s.wires.%s_POWER_GRID_TIEOFF" 
                                 car( NameParseCellName( CellView~>cellName )) powerCellName )
        UIExportDesignLefP_Form->UIExportDesignLefP_PowerGridCellName->value = powerCellName
      )
      UIExportDesignLefP_Form->UIExportDesignLefP_OutFile->value =
        strcat( CellView~>cellName ".lef")
      UIExportDesignLefP_Form->UICreateAbstract_FulcrumPDKRoot->value =
        getShellEnvVar( "FULCRUM_PDK_ROOT" )
      if(UIExportDesignLefP_Form->UICreateAbstract_DfIIDir->value == "default" then
        UIExportDesignLefP_Form->UICreateAbstract_DfIIDir->value =
          ConfigFileGetValue( TheCDSConfigTable "DFII_DIR" )
      )

      UIExportDesignLefP_Form->UIExportDesignLefP_LefCellsList->value =
        strcat( CellView~>cellName ".flatten_cells_list")
      hiDisplayForm( UIExportDesignLefP_Form ) 
    )
  ) 
)
 

( UICreateAction
  'UIExportDesignLefPFormCB
  (lambda ()
    let( (errorMsg CellView libName cellName viewName LogFile)
      LogFile = "exportDesignLef.log"
      nrDeleteToolSum( LogFile )
      CellView=UIGetCellView()
      Command = sprintf( nil
                "%s/exportDesignLef --cell-list=%s --dfII-dir=%s --design=%s --fulcrum-pdk-root=%s --lefout=%s --power-grid-cell=%s --include-USE"
                PackageGetBinRoot( )
                UIExportDesignLefP_Form->UIExportDesignLefP_LefCellsList->value
                UIExportDesignLefP_Form->UICreateAbstract_DfIIDir->value
                CellView~>cellName
                UIExportDesignLefP_Form->UICreateAbstract_FulcrumPDKRoot->value
                UIExportDesignLefP_Form->UIExportDesignLefP_OutFile->value
                UIExportDesignLefP_Form->UIExportDesignLefP_PowerGridCellName->value
                )
      if( UIExportDesignLefP_Form->UIExportDesignLefP_OverWrite->value then
        Command = sprintf( nil "%s --write-lef" Command)
      )
      printf( "%s\n" Command )
      shell( Command )

      ; display results
      nrDisplayToolStatus( "Export Design LEF" 
                           sprintf( nil "Export Design LEF for %s" CellView~>cellName )
                           LogFile
      ) 
    )
  )
)


( UICreateComponent
  "UIExportDesignLefP_Form"
  ( UICreateConstructor 
    (lambda ( Children )
      (let ( Fields )
         Fields = list(
                ( hiCreateStringField
                  ?name `UICreateAbstract_FulcrumPDKRoot
                  ?prompt "Fulcrum PDK Root" 
                  ?defValue "default" )
                ( hiCreateStringField
                  ?name `UICreateAbstract_DfIIDir
                  ?prompt "DfII Dir" 
                  ?defValue "default" )
                ( hiCreateStringField
                  ?name `UIExportDesignLefP_OutFile
                  ?prompt "LEF Filename" 
                  ?defValue "lef.out" )
                ( hiCreateStringField
                  ?name `UIExportDesignLefP_LefCellsList
                  ?prompt "LEF Cells List" 
                  ?defValue "flatten_cells_list" )
                ( hiCreateStringField
                  ?name `UIExportDesignLefP_PowerGridCellName
                  ?prompt "Power Grid Cell Name" 
                  ?defValue "default" )
                ( hiCreateBooleanButton
                  ?name `UIExportDesignLefP_OverWrite
                  ?buttonText "Over Write Existing LEF File"
                  ?defValue t )
                ( hiCreateFormButton
                  ?name `UIExportDesignLefP_Help
                  ?buttonText "Tool Help"
                  ?callback "nrOpenHtml( \"Export_Design_LEF_help.html\" )" )
        )       
        ( hiCreateAppForm
          ?name `UIExportDesignLefP_Form
          ?formTitle "Export Design LEF"  
          ?fields Fields
          ?callback "( UIExportDesignLefPFormCB )"
        ) 
      ) 
    ) 
  ) 
)
