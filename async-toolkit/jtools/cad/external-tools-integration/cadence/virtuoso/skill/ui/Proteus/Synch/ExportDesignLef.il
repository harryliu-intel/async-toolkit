; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id: //depot/sw/main/cad/external-tools-integration/cadence/virtuoso/skill/ui/Nano/Export/ExportDesignLef.il#4 $
; $DateTime: 2009/08/18 11:38:11 $
; $Author: stevemc $

; 
; menu item Nano/Export
;
( list "5.Export Design LEF..." "UISyExportDesignLef" )


( UICreateMenuItemCallBack
  `UISyExportDesignLefCB
  (lambda ()
    let((CellView)
      CellView = UIGetCellView()
      UISyExportDesignLef_Form->UISyExportDesignLef_OutFile->value =
        strcat( CellView~>cellName ".lef")
      UISyExportDesignLef_Form->UICreateAbstract_FulcrumPDKRoot->value =
        getShellEnvVar( "FULCRUM_PDK_ROOT" )
      if(UISyExportDesignLef_Form->UICreateAbstract_DfIIDir->value == "default" then
        UISyExportDesignLef_Form->UICreateAbstract_DfIIDir->value =
          ConfigFileGetValue( TheCDSConfigTable "DFII_DIR" )
      )

      UISyExportDesignLef_Form->UISyExportDesignLef_LefCellsList->value =
        strcat( CellView~>cellName ".flatten_cells_list")
      hiDisplayForm( UISyExportDesignLef_Form ) 
    )
  ) 
)
 

( UICreateAction
  'UISyExportDesignLefFormCB
  (lambda ()
    let( (errorMsg CellView libName cellName viewName LogFile)
      LogFile = "exportDesignLef.log"
      nrDeleteToolSum( LogFile )
      CellView=UIGetCellView()
      Command = sprintf( nil
                "%s/exportDesignLef --cell-list=%s --dfII-dir=%s --design=%s --fulcrum-pdk-root=%s --power-grid-cell=none --lefout=%s"
                PackageGetBinRoot( )
                UISyExportDesignLef_Form->UISyExportDesignLef_LefCellsList->value
                UISyExportDesignLef_Form->UICreateAbstract_DfIIDir->value
                CellView~>cellName
                UISyExportDesignLef_Form->UICreateAbstract_FulcrumPDKRoot->value
                UISyExportDesignLef_Form->UISyExportDesignLef_OutFile->value
                )
      if( UISyExportDesignLef_Form->UISyExportDesignLef_OverWrite->value then
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
  "UISyExportDesignLef_Form"
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
                  ?name `UISyExportDesignLef_OutFile
                  ?prompt "LEF Filename" 
                  ?defValue "lef.out" )
                ( hiCreateStringField
                  ?name `UISyExportDesignLef_LefCellsList
                  ?prompt "LEF Cells List" 
                  ?defValue "flatten_cells_list" )
                ( hiCreateBooleanButton
                  ?name `UISyExportDesignLef_OverWrite
                  ?buttonText "Over Write Existing LEF File"
                  ?defValue t )
                ( hiCreateFormButton
                  ?name `UISyExportDesignLef_Help
                  ?buttonText "Tool Help"
                  ?callback "nrOpenHtml( \"Export_Design_LEF_help.html\" )" )
        )       
        ( hiCreateAppForm
          ?name `UISyExportDesignLef_Form
          ?formTitle "Export Design LEF"  
          ?fields Fields
          ?callback "( UISyExportDesignLefFormCB )"
        ) 
      ) 
    ) 
  ) 
)
