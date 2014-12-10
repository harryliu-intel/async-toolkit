; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

;
; menu item Nano/Export
;
( list "5.Create Tracks DEF..." "UICreateTracksDef" )


( UICreateMenuItemCallBack
  `UICreateTracksDefCB
  (lambda ()
    let(()
      UICreateTracksDef_Form->UICreateTracksDef_RefDef->value = 
        strcat(UIGetCellView()~>cellName ".def")
      UICreateTracksDef_Form->UICreateTracksDef_OutDef->value = 
        strcat(UIGetCellView()~>cellName "_tracks.def")

      hiDisplayForm( UICreateTracksDef_Form ) 
    )
  ) 
)
 

( UICreateComponent
  "UICreateTracksDef_Form"
  ( UICreateConstructor 
    (lambda ( Children )
      (let ( Fields )
        Fields = list(
                ( hiCreateStringField
                  ?name `UICreateTracksDef_RefDef
                  ?prompt "Reference Design DEF Filename" 
                  ?defValue "" )
                ( hiCreateStringField
                  ?name `UICreateTracksDef_OutDef
                  ?prompt "Output DEF Filename" 
                  ?defValue "tracks.def" )
                ( hiCreateBooleanButton
                  ?name `UICreateTracksDef_E1of4
                  ?buttonText "Use E1of4 Style Tracks"
                  ?defValue t )

                ( hiCreateSeparatorField
                  ?name `UICreateFlattenView_sep1 )

                ( hiCreateBooleanButton
                  ?name `UICreateTracksDef_AutoOffsets
                  ?buttonText "Use Auto-Generated Offsets"
                  ?defValue t )
                ( hiCreateFloatField
                  ?name `UICreateTracksDef_OffsetX
                  ?prompt "   X Offset"
                  ?defValue 0.0 )
                ( hiCreateFloatField
                  ?name `UICreateTracksDef_OffsetY
                  ?prompt "   Y Offset"
                  ?defValue 0.0 )
                ( hiCreateFloatField
                  ?name `UICreateTracksDef_OffsetE1of4X
                  ?prompt "   E1of4 X Offset"
                  ?defValue 0.0 )
                ( hiCreateFloatField
                  ?name `UICreateTracksDef_OffsetE1of4Y
                  ?prompt "   E1of4 Y Offset"
                  ?defValue 0.0 )

                ( hiCreateSeparatorField
                  ?name `UICreateFlattenView_sep2 )

                ( hiCreateFloatField
                  ?name `UICreateTracksDef_TrackPitch
                  ?prompt "Base Track Pitch"
                  ?defValue DefaultWiringPitch )

                ( hiCreateFormButton
                  ?name `UICreateTracksDef_Help
                  ?buttonText "Tool Help"
                  ?callback "nrOpenHtml( \"Create_Tracks_DEF_help.html\" )" )
        )       
        ( hiCreateAppForm
          ?name `UICreateTracksDef_Form
          ?formTitle "Create Tracks DEF"  
          ?fields Fields
          ?callback "( UICreateTracksDefFormCB )"
        ) 
        hiSetFormSize( UICreateTracksDef_Form list(601 585))
      ) 
    ) 
  ) 
)


( UICreateAction
  'UICreateTracksDefFormCB
  (lambda ()
    let( (Command)
      Command = sprintf( nil
                "%s/genTracks --refdef=%s --outfile=%s --track-pitch=%.3f"
                PackageGetBinRoot( )
                UICreateTracksDef_Form->UICreateTracksDef_RefDef->value
                UICreateTracksDef_Form->UICreateTracksDef_OutDef->value
                UICreateTracksDef_Form->UICreateTracksDef_TrackPitch->value
                )
      if( UICreateTracksDef_Form->UICreateTracksDef_AutoOffsets->value == nil then
	Command = sprintf( nil "%s --offsetx=%.3f --offsety=%.3f --offset-e1of4x=%.3f --offset-e1of4y=%.3f" 
	          Command
                  UICreateTracksDef_Form->UICreateTracksDef_OffsetX->value
                  UICreateTracksDef_Form->UICreateTracksDef_OffsetY->value
                  UICreateTracksDef_Form->UICreateTracksDef_OffsetE1of4X->value
                  UICreateTracksDef_Form->UICreateTracksDef_OffsetE1of4Y->value)
      )
      if( UICreateTracksDef_Form->UICreateTracksDef_E1of4->value then
        Command = sprintf( nil "%s --e1of4" Command)
      )
      printf( "%s\n" Command )
      shell( Command )
      printf( "Create Tracks DEF finished.\n" )

    )
  )
)


