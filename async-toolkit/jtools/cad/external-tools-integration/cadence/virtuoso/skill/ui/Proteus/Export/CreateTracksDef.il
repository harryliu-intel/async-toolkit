; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id: //depot/sw/main/cad/external-tools-integration/cadence/virtuoso/skill/ui/Nano/Export/CreateTracksDef.il#10 $
; $DateTime: 2008/01/15 10:43:46 $
; $Author: stevemc $

;
; menu item Nano/Export
;
( list "5.Create Tracks DEF..." "UICreateTracksDefP" )


( UICreateMenuItemCallBack
  `UICreateTracksDefPCB
  (lambda ()
    let(()
      UICreateTracksDefP_Form->UICreateTracksDefP_RefDef->value = 
        strcat(UIGetCellView()~>cellName ".def")
      UICreateTracksDefP_Form->UICreateTracksDefP_OutDef->value = 
        strcat(UIGetCellView()~>cellName "_tracks.def")

      hiDisplayForm( UICreateTracksDefP_Form ) 
    )
  ) 
)
 

( UICreateComponent
  "UICreateTracksDefP_Form"
  ( UICreateConstructor 
    (lambda ( Children )
      (let ( Fields )
        Fields = list(
                ( hiCreateStringField
                  ?name `UICreateTracksDefP_RefDef
                  ?prompt "Reference Design DEF Filename" 
                  ?defValue "" )
                ( hiCreateStringField
                  ?name `UICreateTracksDefP_OutDef
                  ?prompt "Output DEF Filename" 
                  ?defValue "tracks.def" )
                ( hiCreateBooleanButton
                  ?name `UICreateTracksDefP_E1of4
                  ?buttonText "Use E1of4 Style Tracks"
                  ?defValue t )

                ( hiCreateSeparatorField
                  ?name `UICreateFlattenView_sep1 )

                ( hiCreateBooleanButton
                  ?name `UICreateTracksDefP_AutoOffsets
                  ?buttonText "Use Auto-Generated Offsets"
                  ?defValue t )
                ( hiCreateFloatField
                  ?name `UICreateTracksDefP_OffsetX
                  ?prompt "   X Offset"
                  ?defValue 0.0 )
                ( hiCreateFloatField
                  ?name `UICreateTracksDefP_OffsetY
                  ?prompt "   Y Offset"
                  ?defValue 0.0 )
                ( hiCreateFloatField
                  ?name `UICreateTracksDefP_OffsetE1of4X
                  ?prompt "   E1of4 X Offset"
                  ?defValue 0.0 )
                ( hiCreateFloatField
                  ?name `UICreateTracksDefP_OffsetE1of4Y
                  ?prompt "   E1of4 Y Offset"
                  ?defValue 0.0 )

                ( hiCreateSeparatorField
                  ?name `UICreateFlattenView_sep2 )

                ( hiCreateFloatField
                  ?name `UICreateTracksDefP_TrackPitch
                  ?prompt "Base Track Pitch"
                  ?defValue DefaultWiringPitch )

                ( hiCreateFormButton
                  ?name `UICreateTracksDefP_Help
                  ?buttonText "Tool Help"
                  ?callback "nrOpenHtml( \"Create_Tracks_DEF_help.html\" )" )
        )       
        ( hiCreateAppForm
          ?name `UICreateTracksDefP_Form
          ?formTitle "Create Tracks DEF"  
          ?fields Fields
          ?callback "( UICreateTracksDefPFormCB )"
        ) 
        hiSetFormSize( UICreateTracksDefP_Form list(601 585))
      ) 
    ) 
  ) 
)


( UICreateAction
  'UICreateTracksDefPFormCB
  (lambda ()
    let( (Command)
      Command = sprintf( nil
                "%s/genTracks --refdef=%s --outfile=%s --track-pitch=%.3f --rotate"
                PackageGetBinRoot( )
                UICreateTracksDefP_Form->UICreateTracksDefP_RefDef->value
                UICreateTracksDefP_Form->UICreateTracksDefP_OutDef->value
                UICreateTracksDefP_Form->UICreateTracksDefP_TrackPitch->value
                )
      if( UICreateTracksDefP_Form->UICreateTracksDefP_AutoOffsets->value == nil then
	Command = sprintf( nil "%s --offsetx=%.3f --offsety=%.3f --offset-e1of4x=%.3f --offset-e1of4y=%.3f" 
	          Command
                  UICreateTracksDefP_Form->UICreateTracksDefP_OffsetX->value
                  UICreateTracksDefP_Form->UICreateTracksDefP_OffsetY->value
                  UICreateTracksDefP_Form->UICreateTracksDefP_OffsetE1of4X->value
                  UICreateTracksDefP_Form->UICreateTracksDefP_OffsetE1of4Y->value)
      )
      if( UICreateTracksDefP_Form->UICreateTracksDefP_E1of4->value then
        Command = sprintf( nil "%s --e1of4" Command)
      )
      printf( "%s\n" Command )
      shell( Command )
      printf( "Create Tracks DEF finished.\n" )

    )
  )
)


