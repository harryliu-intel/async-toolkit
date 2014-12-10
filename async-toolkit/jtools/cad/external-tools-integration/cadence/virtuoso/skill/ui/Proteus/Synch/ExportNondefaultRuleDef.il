; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id: //depot/sw/main/cad/external-tools-integration/cadence/virtuoso/skill/ui/Nano/Export/ExportNondefaultRuleDef.il#2 $
; $DateTime: 2006/05/30 09:26:14 $
; $Author: tomjones $

;
; menu item Nano/Export
;
( list "7.Export Nondefault Rule DEF..." "UISyExportNondefaultRuleDef" )


( UICreateMenuItemCallBack
  `UISyExportNondefaultRuleDefCB
  (lambda ()
    UISyExportNondefaultRuleDef_Form->UISyExportNondefaultRuleDef_OutFile->value=
      strcat( UIGetCellView()~>cellName "_nondefault.def")
    hiDisplayForm( UISyExportNondefaultRuleDef_Form ) 
  ) 
)
 

( UICreateAction
  'UISyExportNondefaultRuleDefFormCB
  (lambda ()
    let( (CellView Command ToolName)
      CellView= UIGetCellView()
      Command = sprintf( nil
                "%s/cast2def --cast-path=%s --cell=%s --outfile=%s --cadence-name --max-heap-size=%s &> %s"
                PackageGetBinRoot( )
                ConfigFileGetValue( TheCDSConfigTable "CAST_PATH" )
                CellView~>cellName
                UISyExportNondefaultRuleDef_Form->UISyExportNondefaultRuleDef_OutFile->value
                UISyExportNondefaultRuleDef_Form->UISyExportNondefaultRuleDef_HeapSize->value
                UISyExportNondefaultRuleDef_Form->UISyExportNondefaultRuleDef_LogFile->value
                )
      printf( "%s\n" Command )
      shell( Command )

      ; display results
      printf( "Nondefault Rule File %s is created.\n" UISyExportNondefaultRuleDef_Form->UISyExportNondefaultRuleDef_OutFile->value)
      ToolName = "Export Nondefault Rule DEF"
      hiDisplayAppDBox(
        ?name `UISyExportNondefaultRuleDef_StatusDbox
        ?dboxBanner strcat( ToolName " Status")
        ?dboxText sprintf( nil "%s finished. View log file?" ToolName)
        ?buttonLayout 'YesNo
        ?defaultButton 1
        ?callback "nrDisplayLogFile( ToolName UISyExportNondefaultRuleDef_Form->UISyExportNondefaultRuleDef_LogFile->value )"
      )
    )
  )
)


( UICreateComponent
  "UISyExportNondefaultRuleDef_Form"
  ( UICreateConstructor 
    (lambda ( Children )
      (let ( Fields )
         Fields = list(
                ( hiCreateStringField
                  ?name `UISyExportNondefaultRuleDef_OutFile
                  ?prompt "DEF Filename" 
                  ?defValue "routing.def" )
                ( hiCreateStringField
                  ?name `UISyExportNondefaultRuleDef_LogFile
                  ?prompt "Log Filename" 
                  ?defValue "exportNondefaultRuleDEF.log" )
                ( hiCreateStringField
                  ?name `UISyExportNondefaultRuleDef_HeapSize
                  ?prompt "Max Memory Heap Size" 
                  ?defValue "1800M" )
                ( hiCreateFormButton
                  ?name `UISyExportNondefaultRuleDef_Help
                  ?buttonText "Tool Help"
                  ?callback "nrOpenHtml( \"Export_Nondefault_Rule_DEF_help.html\" )" )
                 )
        ( hiCreateAppForm
          ?name `UISyExportNondefaultRuleDef_Form
          ?formTitle "Export Nondefault Rule DEF"  
          ?fields Fields
          ?callback "( UISyExportNondefaultRuleDefFormCB )"
        ) 
      ) 
    ) 
  ) 
)
