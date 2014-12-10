; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

;
; menu item Nano/Nanoroute
;
( list "2.Run Nanoroute..." "UIRunNanoroute" )


( UICreateMenuItemCallBack
  `UIRunNanorouteCB
  (lambda ()
    let((cellName)
      cellName=UIGetCellView()~>cellName
      UIRunNanoroute_Form->UIRunNanoroute_CmdFile->value=strcat( cellName ".tcl" )

      hiDisplayForm( UIRunNanoroute_Form ) 
    )
  ) 
)


( UICreateComponent
  "UIRunNanoroute_Form"
  ( UICreateConstructor 
    ( lambda ( Children )
      ( let ( Field1 Field2 Field3 Field4 Field5 Field6 AllFields License )

        ; head fields
        Field1=( hiCreateRadioField
          ?name `UIRunNanoroute_Mode
          ?prompt "Nanoroute Execution Mode"
          ?choices list("Batch" "Local")
          ?defValue "Batch"
          ?callback list( 
            "UIRunNanorouteFormModeCB(\"Batch\" hiGetCurrentForm())" 
            "UIRunNanorouteFormModeCB(\"Local\" hiGetCurrentForm())" 
          )
        )
        Field2=( hiCreateStringField
          ?name `UIRunNanoroute_CmdFile
          ?prompt "Nanoroute Init Filename" 
          ?defValue "initNanoroute.tcl" 
        )

        ; specify SOC license to use
        Field2B=( hiCreateRadioField
          ?name `UIRunNanoroute_License
          ?prompt "SOC Encounter License to Use"
          ?choices list("DBS" "GPS" "Full")
          ?defValue "DBS"
        )

        ; specify grid ticket to use
        Field2C=( hiCreateRadioField
          ?name `UIRunNanoroute_Ticket
          ?prompt "Grid Ticket to Use (use nano if queue is full and job is quick.)"
          ?choices list("nano" "np")
          ?defValue "np"
        )

        ; batch fields
        Field3=( hiCreateStringField
          ?name `UIRunNanoroute_SubFile
          ?prompt "Qsub Command Filename" 
          ?defValue "sub_nanoroute" 
        )
        Field4=( hiCreateRadioField
          ?name `UIRunNanoroute_Arch
          ?prompt "Compute Host Architecture"
          ?choices list("32-bit" "64-bit")
          ?defValue "64-bit"
        )
        Field5=( hiCreateIntField
          ?name `UIRunNanoroute_Mem
          ?prompt "Compute Host Memory (MB)"
          ?range list(2000 16000)
          ?defValue 4000
        )

        ; tail fields
        Field6=( hiCreateFormButton
          ?name `UIRunNanoroute_Help
          ?buttonText "Tool Help"
          ?callback "nrOpenHtml( \"Run_Nanoroute_help.html\" )" 
        )
        AllFields = list( Field1 Field2 Field2B Field2C Field3 Field4 Field5 Field6 )

        ; global var used in UIRunNanorouteFormModeCB
        RunNanoroute_Form_BatchFields = list( Field3 Field4 Field6 )

        ( hiCreateAppForm
          ?name `UIRunNanoroute_Form
          ?formTitle "Run Nanoroute"  
          ?fields AllFields
          ?callback "UIRunNanorouteFormCB()"
        ) 
      ) 
    ) 
  ) 
)

 
( UICreateAction
  'UIRunNanorouteFormCB
  (lambda ()
    let( ( Command pout Success Msg )
      Success = t
      if( UIRunNanoroute_Form->UIRunNanoroute_Mode->value == "Batch" then
        if( UIRunNanoroute_Form->UIRunNanoroute_SubFile->value == "" then
          Msg = sprintf( nil "Qsub command file required in Batch mode." )
          nrToolStatus( "Run Nanoroute" Msg )
          Success = nil
        else
          pout = outfile( UIRunNanoroute_Form->UIRunNanoroute_SubFile->value )
          if( pout then
            fprintf( pout "#! /bin/tcsh\n" )
            fprintf( pout "fulcrum --cadence=soc.101usr2 encounter" )
            if( UIRunNanoroute_Form->UIRunNanoroute_Arch->value == "64-bit" then
              fprintf( pout " -64" )
            )
            cond( (UIRunNanoroute_Form->UIRunNanoroute_License->value == "DBS"
                          fprintf( pout " -socel" ) )
                  (UIRunNanoroute_Form->UIRunNanoroute_License->value == "GPS"
                          fprintf( pout " -socexl" ) )
                  (t      fprintf( pout " -SOCE" ) )
            )
            if( UIRunNanoroute_Form->UIRunNanoroute_CmdFile->value != "" then
              fprintf( pout " -init %s -nowin\n" UIRunNanoroute_Form->UIRunNanoroute_CmdFile->value )
            else
              Msg = sprintf( nil 
                    "ERROR: Run Nanoroute FAILED. Nanoroute command file required in Batch mode.")
              nrToolStatus( "Run Nanoroute" Msg )
              Success = nil
            )
            close(pout)
          else
            Msg = sprintf( nil "ERROR: Run Nanoroute FAILED. Unable to open qsub command file '%s'." 
                           UIRunNanoroute_Form->UIRunNanoroute_SubFile->value )
            nrToolStatus( "Run Nanoroute" Msg )
            Success = nil
          )
          Command = sprintf( nil "qsub -cwd -now n -l %s=1,mem=%dM"
                             UIRunNanoroute_Form->UIRunNanoroute_Ticket->value
                             UIRunNanoroute_Form->UIRunNanoroute_Mem->value )
          if( UIRunNanoroute_Form->UIRunNanoroute_Arch->value == "64-bit" then
            Command = strcat( Command ",a=lx24-amd64" )
          )
          Command = sprintf( nil "%s %s" 
                             Command UIRunNanoroute_Form->UIRunNanoroute_SubFile->value )
        )
      else
        Command = sprintf( nil "qrsh -now n -cwd -l a=lx24-amd64,%s=1,mem=%dM fulcrum --cadence=soc.101usr2 encounter"
                             UIRunNanoroute_Form->UIRunNanoroute_Ticket->value
                             UIRunNanoroute_Form->UIRunNanoroute_Mem->value )
        if( UIRunNanoroute_Form->UIRunNanoroute_CmdFile->value != "" then
          Command = sprintf( nil "%s -init %s" 
            Command UIRunNanoroute_Form->UIRunNanoroute_CmdFile->value )
        )
        Command = strcat(Command " &")
      )

      if(Success then
        printf( "%s\n" Command )
        shell( Command )
      )
    )
  )
)


; update form based on Mode
defun( UIRunNanorouteFormModeCB ( Mode FormID )
  if( Mode == "Local" then
    hiDeleteFields( FormID list(`UIRunNanoroute_SubFile `UIRunNanoroute_Arch ))
  )
  if( Mode == "Batch" then
    hiDeleteFields( FormID list(`UIRunNanoroute_Help) )
    hiAddFields( FormID RunNanoroute_Form_BatchFields )
    hiFormDefaults( FormID )
;    hiSetFormSize( FormID list(601 242))
    hiSetFormSize( FormID list(601 276))
  )
  cellName=UIGetCellView()~>cellName
  UIRunNanoroute_Form->UIRunNanoroute_CmdFile->value=strcat( cellName ".tcl" )
)


