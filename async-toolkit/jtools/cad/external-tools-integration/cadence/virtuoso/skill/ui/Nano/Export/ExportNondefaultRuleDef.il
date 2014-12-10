; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

;
; menu item Nano/Export
;
( list "4.Export Nondefault Rule DEF..." "UIExportNondefaultRuleDef" )


( UICreateMenuItemCallBack
  `UIExportNondefaultRuleDefCB
  (lambda ()
    UIExportNondefaultRuleDef_Form->UIExportNondefaultRuleDef_OutFile->value=
      strcat( UIGetCellView()~>cellName "_nondefault.def")
    hiDisplayForm( UIExportNondefaultRuleDef_Form ) 
  ) 
)
 

( UICreateAction
  'UIExportNondefaultRuleDefFormCB
  (lambda ()
    let( (CellName Command ToolName filename)
      CellName = UIGetCellView()~>cellName
      ; check for existence of file saved in cell dir
      filename = strcat( "/"  
          buildString( reverse( cddr( reverse( parseString( 
            UIGetCellView()~>fileName "/")))) "/")
          "/" CellName "_nondefault.def")
      if( isFile(filename) then
      ;create a soft link
        hiDisplayAppDBox(
          ?name 'NDdefExistsDbox
          ?dboxBanner "Notice"
          ?dboxText "A nondefault.def file is already saved in the cell directory.\n
                     Creating a symbolic link."
          ?dialogType hicQuestionDialog
          ?buttonLayout 'Close
        )
        Command = strcat( "ln -s " filename " .")
        shell(Command)
      else
      ;create a new one

      Command = sprintf( nil
                "%s/cast2def --cast-path=%s --cell=%s --outfile=%s --cadence-name --max-heap-size=%s &> %s"
                PackageGetBinRoot( )
                ConfigFileGetValue( TheCDSConfigTable "CAST_PATH" )
                CellName
                UIExportNondefaultRuleDef_Form->UIExportNondefaultRuleDef_OutFile->value
                UIExportNondefaultRuleDef_Form->UIExportNondefaultRuleDef_HeapSize->value
                UIExportNondefaultRuleDef_Form->UIExportNondefaultRuleDef_LogFile->value
                )
      printf( "%s\n" Command )
      shell( Command )

      ; display results
      printf( "Nondefault Rule File %s is created.\n" UIExportNondefaultRuleDef_Form->UIExportNondefaultRuleDef_OutFile->value)
      ToolName = "Export Nondefault Rule DEF"
      hiDisplayAppDBox(
        ?name `UIExportNondefaultRuleDef_StatusDbox
        ?dboxBanner strcat( ToolName " Status")
        ?dboxText sprintf( nil "%s finished. View log file?" ToolName)
        ?buttonLayout 'YesNo
        ?defaultButton 1
        ?callback "nrDisplayLogFile( ToolName UIExportNondefaultRuleDef_Form->UIExportNondefaultRuleDef_LogFile->value )"
      )
    )
  )
))


( UICreateComponent
  "UIExportNondefaultRuleDef_Form"
  ( UICreateConstructor 
    (lambda ( Children )
      (let ( Fields )
         Fields = list(
                ( hiCreateStringField
                  ?name `UIExportNondefaultRuleDef_OutFile
                  ?prompt "DEF Filename" 
                  ?defValue "routing.def" )
                ( hiCreateStringField
                  ?name `UIExportNondefaultRuleDef_LogFile
                  ?prompt "Log Filename" 
                  ?defValue "exportNondefaultRuleDEF.log" )
                ( hiCreateStringField
                  ?name `UIExportNondefaultRuleDef_HeapSize
                  ?prompt "Max Memory Heap Size" 
                  ?defValue "1800M" )
                ( hiCreateFormButton
                  ?name `UIExportNondefaultRuleDef_Help
                  ?buttonText "Tool Help"
                  ?callback "nrOpenHtml( \"Export_Nondefault_Rule_DEF_help.html\" )" )
                 )
        ( hiCreateAppForm
          ?name `UIExportNondefaultRuleDef_Form
          ?formTitle "Export Nondefault Rule DEF"  
          ?fields Fields
          ?callback "( UIExportNondefaultRuleDefFormCB )"
        ) 
      ) 
    ) 
  ) 
)
