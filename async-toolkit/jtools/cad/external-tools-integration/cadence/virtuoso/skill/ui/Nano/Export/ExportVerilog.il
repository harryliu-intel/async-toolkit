; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

; 
; menu item Nano/Export
;
( list "1.Export Verilog Netlist..." "UICreateVerilog" )


( UICreateMenuItemCallBack
  `UICreateVerilogCB
  (lambda ()
    UICreateVerilog_Form->UICreateVerilog_OutFile->value=
      strcat( UIGetCellView()~>cellName ".v")
    hiDisplayForm( UICreateVerilog_Form ) 
  ) 
)
 

( UICreateAction
  'UICreateVerilogFormCB
  (lambda ()
    let( (CellView Command ToolName)
      CellView= UIGetCellView()
      Command = sprintf( nil
                "%s/prs2verilog --cast-path=%s --cell=%s --outfile=%s --max-heap-size=%s --converter=netlist --skip-power-rail --routed --translate=cadence --cadence-name &> %s"
                PackageGetBinRoot( )
                ConfigFileGetValue( TheCDSConfigTable "CAST_PATH" )
                CellView~>cellName
                UICreateVerilog_Form->UICreateVerilog_OutFile->value
                UICreateVerilog_Form->UICreateVerilog_MaxHeapSize->value
                UICreateVerilog_Form->UICreateVerilog_LogFile->value
                )
      printf( "%s\n" Command )
      shell( Command )

      ; display results
      printf( "Verilog File %s is created.\n" UICreateVerilog_Form->UICreateVerilog_OutFile->value)
      ToolName = "Export Verilog Netlist"
      hiDisplayAppDBox(
        ?name `UICreateVerilog_StatusDbox
        ?dboxBanner strcat( ToolName " Status")
        ?dboxText sprintf( nil "%s finished. View log file?" ToolName)
        ?buttonLayout 'YesNo
        ?defaultButton 1
        ?callback "nrDisplayLogFile( ToolName UICreateVerilog_Form->UICreateVerilog_LogFile->value )"
      )
    )
  )
)


( UICreateComponent
  "UICreateVerilog_Form"
  ( UICreateConstructor 
    (lambda ( Children )
      (let ( Fields )
         Fields = list(
                ( hiCreateStringField
                  ?name `UICreateVerilog_OutFile
                  ?prompt "Verilog Filename" 
                  ?defValue "verilog.v" )
                ( hiCreateStringField
                  ?name `UICreateVerilog_LogFile
                  ?prompt "Log Filename" 
                  ?defValue "exportVerilogNetlist.log" )
                ( hiCreateStringField
                  ?name `UICreateVerilog_MaxHeapSize
                  ?prompt "Max Heap Size" 
                  ?defValue "1800M" )
                ( hiCreateFormButton
                  ?name `UICreateVerilog_Help
                  ?buttonText "Tool Help"
                  ?callback "nrOpenHtml( \"Export_Verilog_Netlist_help.html\" )" )
                 ) 
        ( hiCreateAppForm
          ?name `UICreateVerilog_Form
          ?formTitle "Export Verilog Netlist"  
          ?fields Fields
          ?callback "( UICreateVerilogFormCB )"
          ) 
      ) 
    ) 
  ) 
)
