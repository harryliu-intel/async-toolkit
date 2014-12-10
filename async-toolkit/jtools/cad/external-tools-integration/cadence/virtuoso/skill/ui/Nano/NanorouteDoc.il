; $Id$
; $DateTime$
; $Author$

;
; menu item Nano/
;
; Loads Nanoroute flow documentation in web browser
;
( list "Nanoroute Flow Help" "UINanoFlowHelp" )

( UICreateMenuItemCallBack
  `UINanoFlowHelpCB
  (lambda ()
    ( nrOpenHtml "Route_Upper_Level_Cells_frame.html" ) ) )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Utility Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
; load html file
;
; Assumes doc root html path is "http://internal.avlsi.com/eng/depot/sw/cad/doc/flow/AsicDesignFlow/html/"
;
defun( nrOpenHtml (Html)
  let( ( Command DocRoot Doc )
    DocRoot = "http://internal.avlsi.com/eng/depot/sw/cad/doc/flow/AsicDesignFlow/html/"
    Doc = strcat( DocRoot Html )
    Command = sprintf( nil "LD_LIBRARY_PATH= firefox %s &" Doc )
    printf( "%s\n" Command )
    shell( Command )
  )
)


;
; display tool summary file
;
; IntroStr is tool run text such that the message is '<text> finished/FAILED'
;
defun( nrDisplayToolStatus ( ToolName IntroStr LogFileName )
  let( (Lines Msg Banner SumFileName pin)
    SumFileName = substring( LogFileName 1 ( strlen( LogFileName ) - 3 ))
    SumFileName = strcat( SumFileName "sum" )
    Banner = strcat( ToolName " Status")
    pin = infile( SumFileName )
    if( pin then
      Lines = ( ReadFileReadFileLines SumFileName )
      Msg = strcat( IntroStr " finished.\n\n" )
      while( !atom( Lines )
        if( car( Lines ) then
          Msg = strcat( Msg car( Lines ) "\n")
        else
          Msg = strcat( Msg "\n" )
        )
        Lines = cdr( Lines )
      )
      Msg = strcat( Msg "\nView log file?" )
      hiDisplayAppDBox( 
        ?name `UINanorouteToolStatusDbox
        ?dboxBanner Banner
        ?dboxText Msg
        ?buttonLayout 'YesNo
        ?defaultButton 1
        ?callback "nrDisplayLogFile( ToolName LogFileName )"
      )
      close( pin )
    else
      Msg = strcat( IntroStr " FAILED.\n" )
      hiDisplayAppDBox( 
        ?name `UINanorouteToolStatusDbox
        ?dboxBanner Banner
        ?dboxText Msg
        ?buttonLayout 'Close
      )
    )
    printf( "%s" Msg )
  )
)


;
; display tool log file
;
defun( nrDisplayLogFile ( ToolName LogFileName )
  let( (Msg Str pin)
    Str = strcat( ToolName " Log File" )
    printf( "View log file %s\n" LogFileName )
    pin = infile( LogFileName )
    if( pin then
      view( LogFileName nil Str )
    else
      Msg = sprintf( nil "ERROR -- Unable to open log file %s\n" LogFileName )
      hiDisplayAppDBox( 
        ?name `UINanorouteToolLogDbox
        ?dboxBanner Str
        ?dboxText Msg
        ?buttonLayout 'Close
      )
    )
    close( pin )
  )
)
        



;
; status message box
;
defun( nrToolStatus ( Tool Msg )
  let(()
    printf( "%s\n" Msg )
    hiDisplayAppDBox(
      ?name `UINanorouteStatusMsgDbox
      ?dboxBanner strcat(Tool " Status")
      ?dboxText Msg
      ?buttonLayout 'Close
    )
  )
)


