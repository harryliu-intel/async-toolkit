; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

( list "Draw Pins" "UIDrawPins" )


( UICreateMenuItemCallBack 
  `UIDrawPinsCB
  (lambda ()
;    UIDrawPins_Form->UIDrawPins_Form_UserPinsFile->value = 
;    ( sprintf
;      nil 
;      "%s/userpins.il"
;      ( NameGetCellPathFromCellName
;        ( getq ( UIGetCellView ) cellName ) ) )
    
;    UIDrawPins_Form->UIDrawPins_Form_ConnectivityViewName->value = ( getq ( UIGetCellView ) viewName )
    CellViewName = ( getq ( UIGetCellView ) viewName )
    ( hiDisplayForm UIDrawPins_Form ) 
  ) 
)


( UICreateAction
  'UIDrawPinsFormCB
  (lambda ()
    ( createDir ( ConfigFileGetValue TheCDSConfigTable "TEMP" ) )
    (let (
          ( CellView  ( UIGetCellView ) ) ( DrawPins t ) )
      (let (   
            ( TargetDFIIPath 
              ( NameGetCellDirFromDFIIDirAndCellName
                ( ConfigFileGetValue TheCDSConfigTable "TEMP" )
                ( getq CellView cellName ) ) ) )
        (let (
              ( PinsFile 
                ( sprintf
                  nil 
                  "%s/autopins/%s.pins.il"
                  ( ConfigFileGetValue TheCDSConfigTable "TEMP" )
                  ( getq CellView cellName ) ) )
;              ( UserPinsFile 
;                UIDrawPins_Form->UIDrawPins_Form_UserPinsFile->value )
              ( ExecCmd
                (cond (
                       ( and ( boundp `ExecCmd ) ( stringp ExecCmd ) )
                       ExecCmd )
                      (
                       "" ) ) )
              )
          
          ;check if this is a leaf cell
;          (when !(IsLeafCell CellView->cellName)
;            DrawPins = (hiDisplayAppDBox
;            ?name 'PinsErrorDialogBox
;            ?dboxText "This is not a leaf cell. You almost certainly should not be running this menu.\n Use the Nano menu or Ctrl-Alt-g to draw pins using bus scripts.\n Are you sure you want to proceed anyway?"
;            ?dboxBanner "Alert!"
;            ?dialogType     hicQuestionDialog
;            ?dialogStyle    'modal
;            ?buttonLayout   'YesNo
;            ?defaultButton 2
;            )
;          )

          (when UIDrawPins_Form->UIDrawPins_Form_DrawInplace->value
            if( (IsLeafCell CellView->cellName)
             then
              (DrawLeafPins)
             else
              (RedrawAllPins)
             )
            )

          if( UIDrawPins_Form->UIDrawPins_Form_DrawPowerGrid->value
           then 
             (DrawLeafM3Power)
           else
             (DelLeafM3Power)
            )

;          ;if forced to, run cast2skill again
;          ; to create pins file
;          (when 
;              ( or
;                ( not ( isFile PinsFile ) )
;                ( not UIDrawPins_Form->UIDrawPins_Form_NoRerun->value ) )
;
;            ( printf "%s doesn't exist...creating\n" PinsFile )
;            (let (
;                  ( PinsCommand
;                    ( sprintf 
;                      nil
;                      "%s %s/cast2skill --cast-path=%s --cell=%s --output-dir=%s --cadence-name --root-only  --max-heap-size=%dM --64"
;                      ExecCmd
;                      ( PackageGetBinRoot )
;                      ( ConfigFileGetValue TheCDSConfigTable "CAST_PATH" )
;                      ( getq CellView cellName )
;                      ( ConfigFileGetValue TheCDSConfigTable "TEMP" )
;                      ( nrGetMaxHeapSize )
;                      ) ) )
;              (when ( not 
;                      ( equal
;                        UIDrawPins_Form->UIDrawPins_Form_PinType->value 
;                        "auto" ) )
;                ( setq PinsCommand
;                       ( sprintf 
;                         nil
;                         "%s --cell-height=%g --pin-type=%s"
;                         PinsCommand
;                         ( quotient 
;                           ( RectGetHeight 
;                             ( PinUtilFindPRBoundBBox BoundaryLPP CellView ) )
;                           UserUnitsPerMeter )
;                         UIDrawPins_Form->UIDrawPins_Form_PinType->value ) ) )
;              
;              ( setq PinsFile 
;                     ( sprintf 
;                       nil
;                       "%s/autopins/%s.pins.il" 
;                       ( ConfigFileGetValue TheCDSConfigTable "TEMP" )
;                       ( getq CellView cellName ) ) )
;              ( printf "%s\n" PinsCommand )
;              ( shell PinsCommand ) ) )
;         println("start") 
;          ( PinPlaceDrawUsingPDKInfo
;            CellView
;            ?PinsFile PinsFile
;            ?PinLength UIDrawPins_Form->UIDrawPins_Form_PinLength->value
;            ?PinsDirectory ""
;            ?UserPinsFile UserPinsFile
;            ?ConnectivityViewName CellViewName
;            ?PowerGrid UIDrawPins_Form->UIDrawPins_Form_DrawPowerGrid->value
;            ?InPlace UIDrawPins_Form->UIDrawPins_Form_DrawInplace->value 
;            ?AddOnly UIDrawPins_Form->UIDrawPins_Form_AddOnly->value
;            ) 
;         println("end") 
;
;       ) 
;        if( UIDrawPins_Form->UIDrawPins_Form_PinExtension->value
;           nrInplacePins( CellView ?PinExtension UIDrawPins_Form->UIDrawPins_Form_PinExtension->value) 
;        )

       )
      ) 
    ) 
  )
)
    

;( UICreateAction
;  'UICreateUserPinsFileCB
;  (lambda ()
;    ( PinPlacePinScan 
;      ( UIGetCellView )
;      ViaLayerRegEx
;      (when (equal UIDrawPins_Form->UIDrawPins_Form_PinType->value "pitched" ) t )
;      ( PinUtilGetBoundaryPolygonEdgesFromCellView
;        BoundaryLPP
;        ( UIGetCellView ) )
;      ( times UserUnitsPerMeter DefaultWirePitch )
;      ( list GNDNetName VddNetName )
;      ?PinsDirectory ""
;      ?UserPinsFile UIDrawPins_Form->UIDrawPins_Form_UserPinsFile->value
;      ?FileMode (if ( equal UIDrawPins_Form->UIDrawPins_Form_FileMode->value "Append" ) "a" "w" ) 
;    ) 
;  ) 
;)
      

( UICreateComponent
  "UIDrawPins_Form"
  ( UICreateConstructor 
    (lambda ( Children )
      (let (
            ( Fields
              ( list
;                ( hiCreateStringField
;                  ?name `UIDrawPins_Form_UserPinsFile
;                  ?prompt "User Pins File"
;                  ?defValue "" )
;                ( hiCreateStringField
;                  ?name `UIDrawPins_Form_ConnectivityViewName
;                  ?prompt "Connectivity View Name"
;                  ?defValue "" )
;                ( hiCreateFloatField
;                  ?name `UIDrawPins_Form_PinLength
;                  ?prompt "Pin Length"
;                  ?defValue PinLength )
;                ( hiCreateBooleanButton
;                  ?name `UIDrawPins_Form_AddOnly
;                  ?buttonText "Only Add Pins"
;                  ?defValue nil )
                ( hiCreateBooleanButton
                  ?name `UIDrawPins_Form_DrawPowerGrid
                  ?buttonText "Draw Leaf Cell Power Grid"
                  ?defValue nil )
                ( hiCreateBooleanButton
                  ?name `UIDrawPins_Form_DrawInplace
                  ?buttonText "Draw Pins"
                  ?defValue t )
                ( hiCreateFormButton
                  ?name `UIDrawPins_Form_Help
                  ?buttonText "Scan Pins to --> Pin Template"
                  ?callback "ScanToPinTemplate" )
;                ( hiCreateBooleanButton
;                  ?name `UIDrawPins_Form_PinExtension
;                  ?buttonText "Regard Metal Overlapping Pin Shapes As Pins"
;                  ?defValue nil )
;                ( hiCreateBooleanButton
;                  ?name `UIDrawPins_Form_NoRerun
;                  ?buttonText "Don't Recreate Pins File"
;                  ?defValue t )
;                ( hiCreateCyclicField
;                  ?name `UIDrawPins_Form_PinType
;                  ?choices ( list "auto" "inplace" "pitched" )
;                  ?defValue "auto"
;                  ?prompt "Pin Type" )
;                ( hiCreateButton
;                  ?name `UIDrawPins_Form_CreateUserPinsFile
;                  ?buttonText "Scan Existing Pins To User Pins File"
;                  ?callback "( UICreateUserPinsFileCB )" )
;                ( hiCreateCyclicField
;                  ?name `UIDrawPins_Form_FileMode
;                  ?choices ( list "Append" "Write" )
;                  ?defValue "Append"
;                  ?prompt "User Pins File Mode" )
;                ( hiCreateFormButton
;                  ?name `UIDrawPins_Form_Help
;                  ?buttonText "Tool Help"
;                  ?callback "nrOpenHtml( \"Draw_Pins_help.html\" )" )
                ) ) )
        ( hiCreateAppForm
          ?name `UIDrawPins_Form
          ?formTitle "Draw Pins"  
          ?fields Fields
          ?callback "( UIDrawPinsFormCB )"
        ) 
      ) 
    )
  ) 
)
        
 
