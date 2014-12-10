; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$
( list "Create Leaf Cell Abstract..." "UICreateLeafAbstract" )


( UICreateMenuItemCallBack 
  `UICreateLeafAbstractCB
  (lambda ()
    ( hiDisplayForm UICreateLeafAbstract_Form ) 
  ) 
)


( UICreateAction
  'UICreateLeafAbstract_FormCB
  (lambda ()
    (let ( cellListFile CellView CellList libName cellName file line Command OptionList )
      
      ; create list of cells to process
      cellListFile = UICreateLeafAbstract_Form->UICreateLeafAbstract_Form_cellListFile->value
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
                        CellView = (dbOpenCellViewByType libName cellName UICreateLeafAbstract_Form->UICreateLeafAbstract_Form_sourceViewName->value)
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
          ?ERASE_POWERGRID UICreateLeafAbstract_Form->UICreateLeafAbstract_Form_ErasePowerGrid->value 
          ?PIN_CUTOUT UICreateLeafAbstract_Form->UICreateLeafAbstract_Form_PIN_CUTOUT->value 
          ?PIN_LENGTH UICreateLeafAbstract_Form->UICreateLeafAbstract_Form_PIN_LENGTH->value 
          ?M1BLOAT UICreateLeafAbstract_Form->UICreateLeafAbstract_Form_M1BLOAT->value 
          ?M2BLOAT UICreateLeafAbstract_Form->UICreateLeafAbstract_Form_M2BLOAT->value 
          ?M3BLOAT UICreateLeafAbstract_Form->UICreateLeafAbstract_Form_M3BLOAT->value 
          ?M4BLOAT UICreateLeafAbstract_Form->UICreateLeafAbstract_Form_M4BLOAT->value 
          ?M5BLOAT UICreateLeafAbstract_Form->UICreateLeafAbstract_Form_M5BLOAT->value 
          ?M6BLOAT UICreateLeafAbstract_Form->UICreateLeafAbstract_Form_M6BLOAT->value 
          ?M7BLOAT UICreateLeafAbstract_Form->UICreateLeafAbstract_Form_M7BLOAT->value 
          ?M8BLOAT UICreateLeafAbstract_Form->UICreateLeafAbstract_Form_M8BLOAT->value 
          ?useAbstractSubcells UICreateLeafAbstract_Form->UICreateLeafAbstract_Form_useAbstractSubcells->value
          ?keepLayoutSubcells UICreateLeafAbstract_Form->UICreateLeafAbstract_Form_keepLayoutSubcells->value
          ?PowerNets UICreateLeafAbstract_Form->UICreateLeafAbstract_Form_PowerNets->value
          ?PowerGridPitch UICreateLeafAbstract_Form->UICreateLeafAbstract_Form_PowerGridPitch->value
          ?NO_HOLES_ABOVE_PINS UICreateLeafAbstract_Form->UICreateLeafAbstract_Form_NoHolesAbovePins->value
          ?targetViewName UICreateLeafAbstract_Form->UICreateLeafAbstract_Form_targetViewName->value
          ?CDCPowerGrid UICreateLeafAbstract_Form->UICreateLeafAbstract_Form_CDCPowerGrid->value
        )

      ; create options list (all options but cellListFile in same order as form):
      ; ErasePowerGrid useAbstractSubcells keepLayoutSubcells PowerNets targetViewName CUTOUT LENGTH M1-M8BLOAT
      OptionList = sprintf( nil 
                   "Create Leaf Cell Abstract options: %s %s %s %s %.2f %.2f %.2f %.2f %.2f %.2f %.2f %.2f %.2f %.2f %s %s"
                   (B2S UICreateLeafAbstract_Form->UICreateLeafAbstract_Form_ErasePowerGrid->value)
                   (B2S UICreateLeafAbstract_Form->UICreateLeafAbstract_Form_useAbstractSubcells->value)
                   (B2S UICreateLeafAbstract_Form->UICreateLeafAbstract_Form_keepLayoutSubcells->value)
                   UICreateLeafAbstract_Form->UICreateLeafAbstract_Form_targetViewName->value
                   UICreateLeafAbstract_Form->UICreateLeafAbstract_Form_PIN_CUTOUT->value
                   UICreateLeafAbstract_Form->UICreateLeafAbstract_Form_PIN_LENGTH->value
                   UICreateLeafAbstract_Form->UICreateLeafAbstract_Form_M1BLOAT->value 
                   UICreateLeafAbstract_Form->UICreateLeafAbstract_Form_M2BLOAT->value 
                   UICreateLeafAbstract_Form->UICreateLeafAbstract_Form_M3BLOAT->value 
                   UICreateLeafAbstract_Form->UICreateLeafAbstract_Form_M4BLOAT->value 
                   UICreateLeafAbstract_Form->UICreateLeafAbstract_Form_M5BLOAT->value 
                   UICreateLeafAbstract_Form->UICreateLeafAbstract_Form_M6BLOAT->value 
                   UICreateLeafAbstract_Form->UICreateLeafAbstract_Form_M7BLOAT->value 
                   UICreateLeafAbstract_Form->UICreateLeafAbstract_Form_M8BLOAT->value 
                   (B2S UICreateLeafAbstract_Form->UICreateLeafAbstract_Form_PowerNets->value)
                   (B2S UICreateLeafAbstract_Form->UICreateLeafAbstract_Form_NoHolesAbovePins->value)
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

; stupid helper function to turn a boolean as a string
(defun B2S (b)
  (cond (b "t")
        (t "nil")
        )
  )

( UICreateComponent
  "UICreateLeafAbstract_Form"
  ( UICreateConstructor 
    (lambda ( Children )
      (let (
            ( Fields
              ( list           
                ( hiCreateBooleanButton
                  ?name `UICreateLeafAbstract_Form_ErasePowerGrid
                  ?buttonText "Erase power grid"
                  ?defValue nil )
                ( hiCreateBooleanButton
                  ?name `UICreateLeafAbstract_Form_useAbstractSubcells
                  ?buttonText "Preserve keepout from abstract_edit/abstract subcells"
                  ?defValue t )
                ( hiCreateBooleanButton
                  ?name `UICreateLeafAbstract_Form_keepLayoutSubcells
                  ?buttonText "Keep layout subcells even if abstract views exist"
                  ?defValue t )
                ( hiCreateBooleanButton
                  ?name `UICreateLeafAbstract_Form_PowerNets
                  ?buttonText "Create pins for Vdd/GND"
                  ?defValue t )
                ( hiCreateBooleanButton
                  ?name `UICreateLeafAbstract_Form_CDCPowerGrid
                  ?buttonText "CDC Power Grid"
                  ?defValue nil )
                ( hiCreateFloatField
                  ?name `UICreateLeafAbstract_Form_PowerGridPitch
                  ?prompt "Power Grid Pitch"
                  ?defValue PowerGridPitch )
                ( hiCreateBooleanButton
                  ?name `UICreateLeafAbstract_Form_NoHolesAbovePins
                  ?buttonText "Don't cut holes above buried pins"
                  ?defValue nil )
                ( hiCreateStringField
                  ?name `UICreateLeafAbstract_Form_cellListFile
                  ?prompt "Cell list filename"
                  ?defValue "" )
                ( hiCreateStringField
                  ?name `UICreateLeafAbstract_Form_sourceViewName
                  ?prompt "Source View Name"
                  ?defValue "layout" )
                ( hiCreateStringField
                  ?name `UICreateLeafAbstract_Form_targetViewName
                  ?prompt "Target view name"
                  ?defValue "abstract" )
                ( hiCreateFloatField
                  ?name `UICreateLeafAbstract_Form_PIN_CUTOUT
                  ?prompt "Pin Cut Out"
                  ?defValue 0.13 )
                ( hiCreateFloatField
                  ?name `UICreateLeafAbstract_Form_PIN_LENGTH
                  ?prompt "Pin Length"
                  ?defValue 2.4 )
                ( hiCreateFloatField
                  ?name `UICreateLeafAbstract_Form_M1BLOAT
                  ?prompt "Metal 1 Bloat (negative means floodfill)"
                  ?defValue -1.0 )
                ( hiCreateFloatField
                  ?name `UICreateLeafAbstract_Form_M2BLOAT
                  ?prompt "Metal 2 Bloat (negative means floodfill)"
                  ?defValue -1.0 )
                ( hiCreateFloatField
                  ?name `UICreateLeafAbstract_Form_M3BLOAT
                  ?prompt "Metal 3 Bloat (negative means floodfill)"
                  ?defValue 0.0 )
                ( hiCreateFloatField
                  ?name `UICreateLeafAbstract_Form_M4BLOAT
                  ?prompt "Metal 4 Bloat (negative means floodfill)"
                  ?defValue 0.0 )
                ( hiCreateFloatField
                  ?name `UICreateLeafAbstract_Form_M5BLOAT
                  ?prompt "Metal 5 Bloat (negative means floodfill)"
                  ?defValue 0.0 )
                ( hiCreateFloatField
                  ?name `UICreateLeafAbstract_Form_M6BLOAT
                  ?prompt "Metal 6 Bloat (negative means floodfill)"
                  ?defValue 0.0 )
                ( hiCreateFloatField
                  ?name `UICreateLeafAbstract_Form_M7BLOAT
                  ?prompt "Metal 7 Bloat (negative means floodfill)"
                  ?defValue 0.0 )
                ( hiCreateFloatField
                  ?name `UICreateLeafAbstract_Form_M8BLOAT
                  ?prompt "Metal 8 Bloat (negative means floodfill)"
                  ?defValue 0.0 )
                ( hiCreateFormButton
                  ?name `UICreateLeafAbstract_Form_loadOptions
                  ?buttonText "Load Previous Run Options"
                  ?callback "UICreateLeafAbstract_LoadOptions(UIGetCellView())" )
                ( hiCreateFormButton
                  ?name `UICreateLeafAbstract_Help
                  ?buttonText "Tool Help"
                  ?callback "nrOpenHtml( \"Create_Leaf-level_Abstract_help.html\" )" )
              ) 
            ) 
          )
        ( hiCreateAppForm
          ?name `UICreateLeafAbstract_Form
          ?formTitle "Create Leaf-level Abtract"  
          ?fields Fields
          ?callback "( UICreateLeafAbstract_FormCB )"
        ) 
        hiSetFormSize( UICreateLeafAbstract_Form list(601 635))
      ) 
    )
  ) 
)
        
 
; function to read abstract.log options and update form
defun( UICreateLeafAbstract_LoadOptions ( CellView )
  let( (LogFile Success Options Msg)
    LogFile = strcat( "/"  
                      buildString( reverse( cddr( reverse( parseString( CellView~>fileName "/")))) "/") 
                      "/abstract.log")
    Success = nil
    pin = infile( LogFile )
    if( pin then
      Options = car( ReadFileReadFileLines( LogFile ))
      if( strncmp( "Create Leaf-level Abstract options:" Options 34) == 0 then
        Options = cddddr( parseString( Options ))
        if( length(Options) >= 14 then
          UICreateLeafAbstract_Form->UICreateLeafAbstract_Form_ErasePowerGrid->value = car(Options) == "t"
          Options = cdr(Options)
          UICreateLeafAbstract_Form->UICreateLeafAbstract_Form_useAbstractSubcells->value = car(Options) == "t"
          Options = cdr(Options)
          UICreateLeafAbstract_Form->UICreateLeafAbstract_Form_keepLayoutSubcells->value = car(Options) == "t"
          Options = cdr(Options)
          UICreateLeafAbstract_Form->UICreateLeafAbstract_Form_targetViewName->value = car(Options)
          Options = cdr(Options)
          UICreateLeafAbstract_Form->UICreateLeafAbstract_Form_PIN_CUTOUT->value = atof(car(Options))
          Options = cdr(Options)
          UICreateLeafAbstract_Form->UICreateLeafAbstract_Form_PIN_LENGTH->value = atof(car(Options))
          Options = cdr(Options)
          UICreateLeafAbstract_Form->UICreateLeafAbstract_Form_M1BLOAT->value = atof(car(Options))
          Options = cdr(Options)
          UICreateLeafAbstract_Form->UICreateLeafAbstract_Form_M2BLOAT->value = atof(car(Options))
          Options = cdr(Options)
          UICreateLeafAbstract_Form->UICreateLeafAbstract_Form_M3BLOAT->value = atof(car(Options))
          Options = cdr(Options)
          UICreateLeafAbstract_Form->UICreateLeafAbstract_Form_M4BLOAT->value = atof(car(Options))
          Options = cdr(Options)
          UICreateLeafAbstract_Form->UICreateLeafAbstract_Form_M5BLOAT->value = atof(car(Options))
          Options = cdr(Options)
          UICreateLeafAbstract_Form->UICreateLeafAbstract_Form_M6BLOAT->value = atof(car(Options))
          Options = cdr(Options)
          UICreateLeafAbstract_Form->UICreateLeafAbstract_Form_M7BLOAT->value = atof(car(Options))
          Options = cdr(Options)
          UICreateLeafAbstract_Form->UICreateLeafAbstract_Form_M8BLOAT->value = atof(car(Options))
          Options = cdr(Options)
          UICreateLeafAbstract_Form->UICreateLeafAbstract_Form_PowerNets->value = car(Options) == "t"
          Options = cdr(Options)
          UICreateLeafAbstract_Form->UICreateLeafAbstract_Form_NoHolesAbovePins->value = car(Options) == "t"
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
        ?name `UICreateLeafAbstract_LoadOptions_StatusDbox
        ?dboxBanner "Load options FAILED"
        ?dboxText Msg
        ?buttonLayout 'Close
      )
      printf( "%s" Msg)
    )
  )
)
