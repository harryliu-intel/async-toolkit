; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id: //depot/sw/cad/external-tools-integration/cadence/virtuoso/main/skill/ui/Fulcrum/Route/Keepout.il#15 $
; $DateTime: 2004/09/22 19:55:39 $
; $Author: khe $
( list "Create Mid-level Abstract..." "UICreateMidAbstract" )


( UICreateMenuItemCallBack 
  `UICreateMidAbstractCB
  (lambda ()
    ( hiDisplayForm UICreateMidAbstract_Form ) 
  ) 
)


( UICreateAction
  'UICreateMidAbstract_FormCB
  (lambda ()
    (let ( cellListFile CellView CellList libName cellName file line Command OptionList bloatdialog )
      
    bloatdialog=1
      ; bloatdialog == 1 means dialog returned Go Ahead Anyway
      if( bloatdialog == 1 then
          ; create list of cells to process
          cellListFile = UICreateMidAbstract_Form->UICreateMidAbstract_Form_cellListFile->value
          (cond (cellListFile=="" CellList = (list (UIGetCellView)))
                (t
                  file = (infile cellListFile)
                  (cond (file==nil (printf "ERROR: can't read %s\n" cellListFile))
                        (t
                          CellList = nil
                          (while (gets line file)
                            cellName = (StringUtilChompNewline line)
                            temp = (parseString cellName ".")
                            libName = nil
                            (for i 0 (length temp)-3
                               libName = (append libName (list (nth i temp)))
                            )
                            libName = (buildString libName ".")
                            CellView = (dbOpenCellViewByType libName cellName "layout")
                            (cond (CellView!=nil CellList = (cons CellView CellList))
                                  (t (printf "ERROR: can't open %s %s\n" libName cellName))
                            )
                          )
                          (close file)
                        )
                   )
                 )
          )

          ; create abstracts for all CellViews in CellList
          (foreach CellView CellList
            nrCreateAbstractCB( CellView 
              ?ERASE_POWERGRID UICreateMidAbstract_Form->UICreateMidAbstract_Form_ErasePowerGrid->value 
              ?PIN_CUTOUT UICreateMidAbstract_Form->UICreateMidAbstract_Form_PIN_CUTOUT->value 
              ?PIN_LENGTH UICreateMidAbstract_Form->UICreateMidAbstract_Form_PIN_LENGTH->value 
              ?M1BLOAT UICreateMidAbstract_Form->UICreateMidAbstract_Form_M1BLOAT->value 
              ?M2BLOAT UICreateMidAbstract_Form->UICreateMidAbstract_Form_M2BLOAT->value 
              ?M3BLOAT UICreateMidAbstract_Form->UICreateMidAbstract_Form_M3BLOAT->value 
              ?M4BLOAT UICreateMidAbstract_Form->UICreateMidAbstract_Form_M4BLOAT->value 
              ?M5BLOAT UICreateMidAbstract_Form->UICreateMidAbstract_Form_M5BLOAT->value 
              ?M6BLOAT UICreateMidAbstract_Form->UICreateMidAbstract_Form_M6BLOAT->value 
              ?M7BLOAT UICreateMidAbstract_Form->UICreateMidAbstract_Form_M7BLOAT->value 
              ?M8BLOAT UICreateMidAbstract_Form->UICreateMidAbstract_Form_M8BLOAT->value 
              ?useAbstractSubcells UICreateMidAbstract_Form->UICreateMidAbstract_Form_useAbstractSubcells->value
              ?keepLayoutSubcells UICreateMidAbstract_Form->UICreateMidAbstract_Form_keepLayoutSubcells->value
              ?allbloat UICreateMidAbstract_Form->UICreateMidAbstract_Form_AllBloat->value
              ?PowerNets UICreateMidAbstract_Form->UICreateMidAbstract_Form_PowerNets->value
              ?NO_HOLES_ABOVE_PINS UICreateMidAbstract_Form->UICreateMidAbstract_Form_NoHolesAbovePins->value
              ?targetViewName UICreateMidAbstract_Form->UICreateMidAbstract_Form_targetViewName->value
              ?M4PowerNets UICreateMidAbstract_Form->UICreateMidAbstract_Form_M4PowerNets->value
              ?M5PowerNets UICreateMidAbstract_Form->UICreateMidAbstract_Form_M5PowerNets->value
              ?M6PowerNets UICreateMidAbstract_Form->UICreateMidAbstract_Form_M6PowerNets->value
              ?M7PowerNets UICreateMidAbstract_Form->UICreateMidAbstract_Form_M7PowerNets->value
            )

          ; create options list (all options but cellListFile in same order as form):
          ; ErasePowerGrid useAbstractSubcells keepLayoutSubcells PowerNets targetViewName CUTOUT LENGTH M1-M8BLOAT
          OptionList = sprintf( nil 
                       "Create Mid-level Abstract options: %s %s %s %s %.2f %.2f %.2f %.2f %.2f %.2f %.2f %.2f %.2f %.2f %s %s"
                       (B2S UICreateMidAbstract_Form->UICreateMidAbstract_Form_ErasePowerGrid->value)
                       (B2S UICreateMidAbstract_Form->UICreateMidAbstract_Form_useAbstractSubcells->value)
                       (B2S UICreateMidAbstract_Form->UICreateMidAbstract_Form_keepLayoutSubcells->value)
                       UICreateMidAbstract_Form->UICreateMidAbstract_Form_targetViewName->value
                       UICreateMidAbstract_Form->UICreateMidAbstract_Form_PIN_CUTOUT->value
                       UICreateMidAbstract_Form->UICreateMidAbstract_Form_PIN_LENGTH->value
                       UICreateMidAbstract_Form->UICreateMidAbstract_Form_M1BLOAT->value 
                       UICreateMidAbstract_Form->UICreateMidAbstract_Form_M2BLOAT->value 
                       UICreateMidAbstract_Form->UICreateMidAbstract_Form_M3BLOAT->value 
                       UICreateMidAbstract_Form->UICreateMidAbstract_Form_M4BLOAT->value 
                       UICreateMidAbstract_Form->UICreateMidAbstract_Form_M5BLOAT->value 
                       UICreateMidAbstract_Form->UICreateMidAbstract_Form_M6BLOAT->value 
                       UICreateMidAbstract_Form->UICreateMidAbstract_Form_M7BLOAT->value 
                       UICreateMidAbstract_Form->UICreateMidAbstract_Form_M8BLOAT->value 
                       (B2S UICreateMidAbstract_Form->UICreateMidAbstract_Form_PowerNets->value)
                       (B2S UICreateMidAbstract_Form->UICreateMidAbstract_Form_NoHolesAbovePins->value)
                       )

          ; log abstract options
          Command = sprintf( nil 
                    "%s/createAbstractCellLog --cell=%s:%s:%s --client-spec=%s --dfII-dir=%s --opt-str=\"%s\""
                    PackageGetBinRoot( )
                    CellView~>libName
                    CellView~>cellName
                    CellView~>viewName
                    ConfigFileGetValue( TheCDSConfigTable "P4CLIENT" )
                    ConfigFileGetValue( TheCDSConfigTable "DFII_DIR" )
                    OptionList
                    )
          printf( "Update abstract log.\n%s\n" Command )
          shell( Command )
          )
       )
    )
  )
)

; stupid helper function to turn a boolean as a string
(defun B2S (b)
  (cond (b "t")
        (t "nil")
        )
  )

( UICreateComponent
  "UICreateMidAbstract_Form"
  ( UICreateConstructor 
    (lambda ( Children )
      (let (
            ( Fields
              ( list           
                ( hiCreateBooleanButton
                  ?name `UICreateMidAbstract_Form_ErasePowerGrid
                  ?buttonText "Erase power grid"
                  ?defValue t )
                ( hiCreateBooleanButton
                  ?name `UICreateMidAbstract_Form_useAbstractSubcells
                  ?buttonText "Preserve keepout from abstract_edit/abstract subcells"
                  ?defValue t )
                ( hiCreateBooleanButton
                  ?name `UICreateMidAbstract_Form_keepLayoutSubcells
                  ?buttonText "Keep layout subcells even if abstract views exist"
                  ?defValue t )
                ( hiCreateBooleanButton
                  ?name `UICreateMidAbstract_Form_AllBloat
                  ?buttonText "Ignore subcells entirely (faster for flood-filling)"
                  ?defValue nil )
                ( hiCreateBooleanButton
                  ?name `UICreateMidAbstract_Form_PowerNets
                  ?buttonText "Create M3 pins for Vdd/GND"
                  ?defValue nil )
                ( hiCreateBooleanButton
                  ?name `UICreateMidAbstract_Form_M4PowerNets
                  ?buttonText "Create M4 pins for Vdd/GND"
                  ?defValue nil )
                ( hiCreateBooleanButton
                  ?name `UICreateMidAbstract_Form_M5PowerNets
                  ?buttonText "Create M5 pins for Vdd/GND"
                  ?defValue nil )
                ( hiCreateBooleanButton
                  ?name `UICreateMidAbstract_Form_M6PowerNets
                  ?buttonText "Create M6 pins for Vdd/GND"
                  ?defValue nil )
                ( hiCreateBooleanButton
                  ?name `UICreateMidAbstract_Form_M7PowerNets
                  ?buttonText "Create M7 pins for Vdd/GND"
                  ?defValue nil )
                ( hiCreateBooleanButton
                  ?name `UICreateMidAbstract_Form_NoHolesAbovePins
                  ?buttonText "Don't cut holes above buried pins"
                  ?defValue nil )
                ( hiCreateStringField
                  ?name `UICreateMidAbstract_Form_cellListFile
                  ?prompt "Cell list filename"
                  ?defValue "" )
                ( hiCreateStringField
                  ?name `UICreateMidAbstract_Form_targetViewName
                  ?prompt "Target view name"
                  ?defValue "abstract" )
                ( hiCreateFloatField
                  ?name `UICreateMidAbstract_Form_PIN_CUTOUT
                  ?prompt "Pin Cut Out"
                  ?defValue 0.36 )
                ( hiCreateFloatField
                  ?name `UICreateMidAbstract_Form_PIN_LENGTH
                  ?prompt "Pin Length"
                  ?defValue 2.88 )
                ( hiCreateFloatField
                  ?name `UICreateMidAbstract_Form_M1BLOAT
                  ?prompt "Metal 1 Bloat (negative means floodfill)"
                  ?defValue -1.0 )
                ( hiCreateFloatField
                  ?name `UICreateMidAbstract_Form_M2BLOAT
                  ?prompt "Metal 2 Bloat (negative means floodfill)"
                  ?defValue -1.0 )
                ( hiCreateFloatField
                  ?name `UICreateMidAbstract_Form_M3BLOAT
                  ?prompt "Metal 3 Bloat (negative means floodfill)"
                  ?defValue -1.0 )
                ( hiCreateFloatField
                  ?name `UICreateMidAbstract_Form_M4BLOAT
                  ?prompt "Metal 4 Bloat (negative means floodfill)"
                  ?defValue -1.0 )
                ( hiCreateFloatField
                  ?name `UICreateMidAbstract_Form_M5BLOAT
                  ?prompt "Metal 5 Bloat (negative means floodfill)"
                  ?defValue -1.0 )
                ( hiCreateFloatField
                  ?name `UICreateMidAbstract_Form_M6BLOAT
                  ?prompt "Metal 6 Bloat (negative means floodfill)"
                  ?defValue 2.88 )
                ( hiCreateFloatField
                  ?name `UICreateMidAbstract_Form_M7BLOAT
                  ?prompt "Metal 7 Bloat (negative means floodfill)"
                  ?defValue 2.88 )
                ( hiCreateFloatField
                  ?name `UICreateMidAbstract_Form_M8BLOAT
                  ?prompt "Metal 8 Bloat (negative means floodfill)"
                  ?defValue 0.0 )
                ( hiCreateFormButton
                  ?name `UICreateMidAbstract_Form_loadOptions
                  ?buttonText "Load Previous Run Options"
                  ?callback "UICreateMidAbstract_LoadOptions(UIGetCellView())" )
                ( hiCreateFormButton
                  ?name `UICreateMidAbstract_Help
                  ?buttonText "Tool Help"
                  ?callback "nrOpenHtml( \"Create_Mid-level_Abstract_help.html\" )" )
              ) 
            ) 
          )
        ( hiCreateAppForm
          ?name `UICreateMidAbstract_Form
          ?formTitle "Create Mid-level Abtract"  
          ?fields Fields
          ?callback "( UICreateMidAbstract_FormCB )"
        ) 
        hiSetFormSize( UICreateMidAbstract_Form list(601 635))
      ) 
    )
  ) 
)
        
 
; function to read abstract.log options and update form
defun( UICreateMidAbstract_LoadOptions ( CellView )
  let( (LogFile Success Options Msg)
    LogFile = strcat( "/"  
                      buildString( reverse( cddr( reverse( parseString( CellView~>fileName "/")))) "/") 
                      "/abstract.log")
    Success = nil
    pin = infile( LogFile )
    if( pin then
      Options = car( ReadFileReadFileLines( LogFile ))
      if( strncmp( "Create Mid-level Abstract options:" Options 34) == 0 then
        Options = cddddr( parseString( Options ))
        if( length(Options) >= 14 then
          UICreateMidAbstract_Form->UICreateMidAbstract_Form_ErasePowerGrid->value = car(Options) == "t"
          Options = cdr(Options)
          UICreateMidAbstract_Form->UICreateMidAbstract_Form_useAbstractSubcells->value = car(Options) == "t"
          Options = cdr(Options)
          UICreateMidAbstract_Form->UICreateMidAbstract_Form_keepLayoutSubcells->value = car(Options) == "t"
          Options = cdr(Options)
          UICreateMidAbstract_Form->UICreateMidAbstract_Form_targetViewName->value = car(Options)
          Options = cdr(Options)
          UICreateMidAbstract_Form->UICreateMidAbstract_Form_PIN_CUTOUT->value = atof(car(Options))
          Options = cdr(Options)
          UICreateMidAbstract_Form->UICreateMidAbstract_Form_PIN_LENGTH->value = atof(car(Options))
          Options = cdr(Options)
          UICreateMidAbstract_Form->UICreateMidAbstract_Form_M1BLOAT->value = atof(car(Options))
          Options = cdr(Options)
          UICreateMidAbstract_Form->UICreateMidAbstract_Form_M2BLOAT->value = atof(car(Options))
          Options = cdr(Options)
          UICreateMidAbstract_Form->UICreateMidAbstract_Form_M3BLOAT->value = atof(car(Options))
          Options = cdr(Options)
          UICreateMidAbstract_Form->UICreateMidAbstract_Form_M4BLOAT->value = atof(car(Options))
          Options = cdr(Options)
          UICreateMidAbstract_Form->UICreateMidAbstract_Form_M5BLOAT->value = atof(car(Options))
          Options = cdr(Options)
          UICreateMidAbstract_Form->UICreateMidAbstract_Form_M6BLOAT->value = atof(car(Options))
          Options = cdr(Options)
          UICreateMidAbstract_Form->UICreateMidAbstract_Form_M7BLOAT->value = atof(car(Options))
          Options = cdr(Options)
          UICreateMidAbstract_Form->UICreateMidAbstract_Form_M8BLOAT->value = atof(car(Options))
          Options = cdr(Options)
          UICreateMidAbstract_Form->UICreateMidAbstract_Form_PowerNets->value = car(Options) == "t"
          Options = cdr(Options)
          UICreateMidAbstract_Form->UICreateMidAbstract_Form_NoHolesAbovePins->value = car(Options) == "t"
          Success = t
          printf( "Options loaded from %s" LogFile )
        )
      )
    )
    close(pin)

    ; if unable to load options display error message
    if( !Success then
      Msg = strcat( "Attempt to load options FAILED.\nOptions file: " LogFile )
      hiDisplayAppDBox(
        ?name `UICreateMidAbstract_LoadOptions_StatusDbox
        ?dboxBanner "Load options FAILED"
        ?dboxText Msg
        ?buttonLayout 'Close
      )
      printf( "%s" Msg)
    )
  )
)
