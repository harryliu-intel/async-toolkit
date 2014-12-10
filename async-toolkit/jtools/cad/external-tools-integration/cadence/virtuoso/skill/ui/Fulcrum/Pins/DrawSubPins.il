; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id: //depot/sw/cad/external-tools-integration/cadence/virtuoso/main/skill/ui/Fulcrum/Pins/DrawPins.il#24 $
; $DateTime: 2004/12/10 10:42:22 $
; $Author: rliu $

( list "Draw Pins and Sub Level Pins" "UIDrawSubPins" )

( UICreateMenuItemCallBack 
  `UIDrawSubPinsCB
  (lambda ()
    
    UIDrawSubPins_Form->UIDrawSubPins_Form_ConnectivityViewName->value =
    ( getq ( UIGetCellView ) viewName )
    
    ( hiDisplayForm UIDrawSubPins_Form ) ) )

defun( UIDrawCellViewPins ( CellView pinFilename
                          @key
                          ( PinExtension t )
                          )
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
              ( UserPinsFile 
                pinFilename )
              ( ExecCmd
                (cond (
                       ( and ( boundp `ExecCmd ) ( stringp ExecCmd ) )
                       ExecCmd )
                      (
                       "" ) ) )
              )

          ;if forced to, run cast2skill again
          ; to create pins file
          (when 
              ( or
                ( not ( isFile PinsFile ) )
                ( not UIDrawSubPins_Form->UIDrawSubPins_Form_NoRerun->value ) )

            ( printf "%s doesn't exist...creating\n" PinsFile )
            (let (
                  ( PinsCommand
                    ( sprintf 
                      nil
                      "%s %s/cast2skill --cast-path=%s --cell=%s --output-dir=%s --cadence-name --root-only  --max-heap-size=%dM --64"
                      ExecCmd
                      ( PackageGetBinRoot )
                      ( ConfigFileGetValue TheCDSConfigTable "CAST_PATH" )
                      ( getq CellView cellName )
                      ( ConfigFileGetValue TheCDSConfigTable "TEMP" )
                      ( nrGetMaxHeapSize )
                      ) ) )
              (when ( not 
                      ( equal
                        UIDrawSubPins_Form->UIDrawSubPins_Form_PinType->value 
                        "auto" ) )
                ( setq PinsCommand
                       ( sprintf 
                         nil
                         "%s --cell-height=%g --pin-type=%s"
                         PinsCommand
                         ( quotient 
                           ( RectGetHeight 
                             ( PinUtilFindPRBoundBBox BoundaryLPP CellView ) )
                           UserUnitsPerMeter )
                         UIDrawSubPins_Form->UIDrawSubPins_Form_PinType->value ) ) )
              
              ( setq PinsFile 
                     ( sprintf 
                       nil
                       "%s/autopins/%s.pins.il" 
                       ( ConfigFileGetValue TheCDSConfigTable "TEMP" )
                       ( getq CellView cellName ) ) )
              ( printf "%s\n" PinsCommand )
              ( shell PinsCommand ) ) )
          
          ( PinPlaceDrawUsingPDKInfo
            CellView
            ?PinsFile PinsFile
            ?PinLength UIDrawSubPins_Form->UIDrawSubPins_Form_PinLength->value
            ?PinsDirectory ""
            ?UserPinsFile UserPinsFile
            ?ConnectivityViewName UIDrawSubPins_Form->UIDrawSubPins_Form_ConnectivityViewName->value
            ?PowerGrid UIDrawSubPins_Form->UIDrawSubPins_Form_DrawPowerGrid->value
            ?InPlace UIDrawSubPins_Form->UIDrawSubPins_Form_DrawInplace->value 
            ?AddOnly UIDrawSubPins_Form->UIDrawSubPins_Form_AddOnly->value
            ) 
            nrInplacePins( CellView ?PinExtension PinExtension) 
    ) 
  )
)

defun( UIDrawSubCellViewPins ( CellView drawnCellList
                         @key
                         ( routedDirectiveList nil )
                         ( Verbose nil )
                         ( PinExtension t)
                         )
  let(( TargetDFIIPath pinFilename instances uniqueInstances temp LibCellsToIgnore)
    LibCellsToIgnore = append( WiringCellLibCellPairRegExs
              append( TechLibCellPairRegExs
              append( GateLibCellPairRegExs StackLibCellPairRegExs ) ) )
    TargetDFIIPath=NameGetCellDirFromDFIIDirAndCellName( ConfigFileGetValue( TheCDSConfigTable "TEMP" ) CellView~>cellName ) 
    pinFilename=sprintf( nil "%s/userpins.il" NameGetCellPathFromCellName( CellView~>cellName  ) )
    if( CellView~>terminals==nil || setof( pin CellView~>terminals pin~>pins==nil ) then 
      temp=nrOpenCellViewWritable( CellView~>libName CellView~>cellName CellView~>viewName )    
      if( temp then
        CellView=temp
        printf("Drawing Pins on %s...\n" CellView~>cellName )
        if( Verbose printf("Drawing Pins on %s...\n" CellView~>cellName ))
        if( member( CellView~>cellName routedDirectiveList ) then UIDrawCellViewPins( CellView pinFilename ?PinExtension PinExtension)
        else
          instances= car( NameFilterInstances( CellView~>instances LibCellsToIgnore ))
          uniqueInstances=nil
          foreach( inst instances
            if( !member( inst~>cellName uniqueInstances~>cellName ) then 
              uniqueInstances=cons( inst uniqueInstances )
            )
          )
          instances=setof( inst uniqueInstances !member( inst~>cellName drawnCellList ))
          foreach( inst instances
            drawnCellList=UIDrawSubCellViewPins( inst~>master drawnCellList 
               ?routedDirectiveList routedDirectiveList ?Verbose Verbose ?PinExtension PinExtension)
          )
          UIDrawCellViewPins( CellView pinFilename ?PinExtension PinExtension)   
        )
        dbSave(CellView)
      else 
        printf("ERROR: can not open %s(%s) for write.\n" CellView~>cellName CellView~>libName)
      )
    )
    drawnCellList=cons( CellView~>cellName drawnCellList)
  )
)

( UICreateAction
  'UIDrawSubPinsFormCB
  (lambda ()
    ( createDir ( ConfigFileGetValue TheCDSConfigTable "TEMP" ) )
 
    let( ( CellView uniqueInstances instances instCellView )
      
      CellView= UIGetCellView( ) 
      routedDirectiveList = nrCastQuery( CellView ?RegenerateFromCast UIDrawSubPins_Form->UIDrawSubPins_Form_RegenerateFromCast->value)
      UIDrawSubCellViewPins( CellView nil 
                ?routedDirectiveList routedDirectiveList
                ?PinExtension UIDrawSubPins_Form->UIDrawSubPins_Form_PinExtension->value
                )

    )
  )
)
    
( UICreateAction
  'UICreateUserPinsFileCB
  (lambda ()
    ( PinPlacePinScan 
      ( UIGetCellView )
      ViaLayerRegEx
      (when (equal UIDrawSubPins_Form->UIDrawSubPins_Form_PinType->value "pitched" ) t )
      ( PinUtilGetBoundaryPolygonEdgesFromCellView
        BoundaryLPP
        ( UIGetCellView ) )
      ( times UserUnitsPerMeter DefaultWirePitch )
      ( list GNDNetName VddNetName )
      ?PinsDirectory ""
      ?UserPinsFile UIDrawSubPins_Form->UIDrawSubPins_Form_UserPinsFile->value
      ?FileMode (if ( equal UIDrawSubPins_Form->UIDrawSubPins_Form_FileMode->value "Append" ) "a" "w" ) ) ) )
      

( UICreateComponent
  "UIDrawSubPins_Form"
  ( UICreateConstructor 
    (lambda ( Children )
      (let (
            ( Fields
              ( list
                 ( hiCreateBooleanButton
                  ?name `UIDrawSubPins_Form_RegenerateFromCast
                  ?buttonText "Regenerate Routed Directive From Cast"
                  ?defValue t )
                ( hiCreateStringField
                  ?name `UIDrawSubPins_Form_ConnectivityViewName
                  ?prompt "Connectivity View Name"
                  ?defValue "" )
                ( hiCreateFloatField
                  ?name `UIDrawSubPins_Form_PinLength
                  ?prompt "Pin Length"
                  ?defValue PinLength )
                ( hiCreateBooleanButton
                  ?name `UIDrawSubPins_Form_AddOnly
                  ?buttonText "Only Add Pins"
                  ?defValue nil )
                ( hiCreateBooleanButton
                  ?name `UIDrawSubPins_Form_DrawPowerGrid
                  ?buttonText "Draw Power Grid"
                  ?defValue t )
                ( hiCreateBooleanButton
                  ?name `UIDrawSubPins_Form_DrawInplace
                  ?buttonText "Draw Inplace pins"
                  ?defValue t )
                ( hiCreateBooleanButton
                  ?name `UIDrawSubPins_Form_PinExtension
                  ?buttonText "Regard Metal overlapping Pin Shapes as Pins"
                  ?defValue t )
                ( hiCreateBooleanButton
                  ?name `UIDrawSubPins_Form_NoRerun
                  ?buttonText "Don't recreate pins file"
                  ?defValue nil )
                ( hiCreateCyclicField
                  ?name `UIDrawSubPins_Form_PinType
                  ?choices ( list "auto" "inplace" "pitched" )
                  ?defValue "auto"
                  ?prompt "PinType..." )
                ( hiCreateButton
                  ?name `UIDrawSubPins_Form_CreateUserPinsFile
                  ?buttonText "Scan existing pins to userpins.il file"
                  ?callback "( UICreateUserPinsFileCB )" )
                ( hiCreateCyclicField
                  ?name `UIDrawSubPins_Form_FileMode
                  ?choices ( list "Append" "Write" )
                  ?defValue "Append"
                  ?prompt "userpins.il file mode" )
                ) ) )
        ( hiCreateAppForm
          ?name `UIDrawSubPins_Form
          ?formTitle "Draw Pins"  
          ?fields Fields
          ?callback "( UIDrawSubPinsFormCB )"
          ) ) )
    ) )
        
 
