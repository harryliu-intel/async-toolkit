; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

; 
; menu item Nano/Flatten
;
( list "1.Create Flatten View..." "UICreateFlattenView" )


( UICreateMenuItemCallBack
  `UICreateFlattenViewCB
  (lambda ()
    let((CellView powerCellName CellBaseName OBSCellName BusWireCellName Components RComponents)
      CellView = UIGetCellView()
      Components = parseString( CellView~>cellName "." )
      CellBaseName = cadr( reverse( parseString( CellView~>cellName "." ) ))
      RComponents = reverse( Components )
      if( length(RComponents) >= 3 then
        powerCellName = sprintf( nil "%s_%s" cadr(RComponents) car(RComponents)) 
        OBSCellName = strcat( CellView~>libName ".wires." CellBaseName)
        BusWireCellName = strcat( CellView~>libName ".wires." CellBaseName "_buswires")
        powerCellName = sprintf( nil "%s.wires.%s_POWER_GRID_TIEOFF" 
                                 car( NameParseCellName( CellView~>cellName )) powerCellName )
        UICreateFlattenView_Form->UICreateFlattenView_OBSCellName->value = OBSCellName
        UICreateFlattenView_Form->UICreateFlattenView_PowerGridCellName->value = powerCellName
        UICreateFlattenView_Form->UICreateFlattenView_BusWireCellName->value = BusWireCellName
      )
      UICreateFlattenView_Form->UICreateAbstract_FulcrumPDKRoot->value=
        getShellEnvVar( "FULCRUM_PDK_ROOT" )
      if(UICreateFlattenView_Form->UICreateAbstract_DfIIDir->value== "default" then
        UICreateFlattenView_Form->UICreateAbstract_DfIIDir->value=
          NameGetDFIIDirFromCellName( UIGetCellView()~>cellName)
      )
      if(UICreateFlattenView_Form->UICreateAbstract_P4Client->value== "default" then
        UICreateFlattenView_Form->UICreateAbstract_P4Client->value=
          ConfigFileGetValue( TheCDSConfigTable "P4CLIENT" )
      )
      UICreateFlattenView_Form->UICreateFlattenView_FlattenCellsList->value=
        strcat( CellView~>cellName ".flatten_cells_list")
      UICreateFlattenView_Form->UICreateFlattenView_M3PowerPins->value=t
      UICreateFlattenView_Form->UICreateFlattenView_ResolveM3MacroConflict->value=nil
      hiDisplayForm( UICreateFlattenView_Form ) 
    )
  ) 
)


( UICreateComponent
  "UICreateFlattenView_Form"
  ( UICreateConstructor 
    (lambda ( Children )
      (let ( Fields )
         Fields = list(
                ( hiCreateStringField
                  ?name `UICreateAbstract_FulcrumPDKRoot
                  ?prompt "Fulcrum PDK Root" 
                  ?defValue "default" )
                ( hiCreateStringField
                  ?name `UICreateAbstract_DfIIDir
                  ?prompt "DfII Dir" 
                  ?defValue "default" )
                 ( hiCreateStringField
                  ?name `UICreateAbstract_P4Client
                  ?prompt "Client Spec" 
                  ?defValue "default" )

                ( hiCreateSeparatorField
                  ?name `UICreateFlattenView_sep1 )
                ( hiCreateBooleanButton
                  ?name `UICreateFlattenView_Stage1
                  ?buttonText "Stage1: Create Flatten CellView"
                  ?defValue t )
                ( hiCreateBooleanButton
                  ?name `UICreateFlattenView_RegenerateFromCast
                  ?buttonText "   Regenerate Routed Directive From Cast"
                  ?defValue nil )
                ( hiCreateBooleanButton
                  ?name `UICreateFlattenView_CheckAlignment
                  ?buttonText "   Check Cell Alignment"
                  ?defValue t )
                ( hiCreateBooleanButton
                  ?name `UICreateFlattenView_DeleteWires
                  ?buttonText "   Delete Top Level Wires After Flatten"
                  ?defValue nil )
                ( hiCreateStringField
                  ?name `UICreateFlattenView_FlattenViewName
                  ?prompt "   Flatten View Name" 
                  ?defValue "flatten" )

                ( hiCreateSeparatorField
                  ?name `UICreateFlattenView_sep2 )
                ( hiCreateBooleanButton
                  ?name `UICreateFlattenView_Stage2
                  ?buttonText "Stage2: Create Power Grid Cell Layout and Abstract"
                  ?defValue t )
                ( hiCreateStringField
                  ?name `UICreateFlattenView_FlattenCellsList
                  ?prompt "   Flatten Cells List" 
                  ?defValue "flatten_cells_list" )
                ( hiCreateStringField
                  ?name `UICreateFlattenView_PowerGridCellName
                  ?prompt "   Power Grid Cell Name" 
                  ?defValue "powerGridTieOff" )
                ( hiCreateStringField
                  ?name `UICreateFlattenView_OBSCellName
                  ?prompt "   Routing Obstruction Cell Name" 
                  ?defValue "OBS" )
                ( hiCreateStringField
                  ?name `UICreateFlattenView_BusWireCellName
                  ?prompt "   Bus Wires Cell Name" 
                  ?defValue "BUSWIRES" )
                ( hiCreateBooleanButton
                  ?name `UICreateFlattenView_M3PowerPins
                  ?buttonText "Create M3 Power Pins"
                  ?defValue t )
                ( hiCreateBooleanButton
                  ?name `UICreateFlattenView_ResolveM3MacroConflict
                  ?buttonText "Chop M3 pin shapes overlapping M3 macro blockage"
                  ?defValue nil )
                ( hiCreateRadioField
                  ?name `UICreateFlattenView_Class
                  ?prompt "Power Grid Macro Class"
                  ?choices list("block" "cover")
                  ?defValue "block" )

                ( hiCreateSeparatorField
                  ?name `UICreateFlattenView_sep3 )
                ( hiCreateBooleanButton
                  ?name `UICreateFlattenView_Verbose
                  ?buttonText "Verbose"
                  ?defValue nil )
                ( hiCreateFormButton
                  ?name `UICreateFlattenView_Help
                  ?buttonText "Tool Help"
                  ?callback "nrOpenHtml( \"Create_Flatten_View_help.html\" )" )
                 ) 
        ( hiCreateAppForm
          ?name `UICreateFlattenView_Form
          ?formTitle "Create Flatten View"  
          ?fields Fields
          ?callback "( UICreateFlattenViewFormCB )"
        )
        hiSetFormSize( UICreateFlattenView_Form list(601 585))
      ) 
    ) 
  ) 
)
 

( UICreateAction
  'UICreateFlattenViewFormCB
  (lambda ()
    let( (CellView powerGridCellView Command ErrorStr TMP t1 t2 t3 changeSpec changeNumber 
            powerCellViewName powerGridCellAbstractView flattenView power powerTemp OptionList)
      CellView= UIGetCellView()
 
      ; stage 1 -- create flatten view
      if( UICreateFlattenView_Form->UICreateFlattenView_Stage1->value then
        flattenView=nrFlattenView( 
          CellView 
          UICreateFlattenView_Form->UICreateFlattenView_FlattenViewName->value
          UICreateFlattenView_Form->UICreateFlattenView_PowerGridCellName->value
          powerCellViewName
          UICreateFlattenView_Form->UICreateFlattenView_FlattenCellsList->value
          ?Verbose UICreateFlattenView_Form->UICreateFlattenView_Verbose->value
          ?DeleteWires UICreateFlattenView_Form->UICreateFlattenView_DeleteWires->value
          ?RegenerateFromCast UICreateFlattenView_Form->UICreateFlattenView_RegenerateFromCast->value
          ?CheckAlignment UICreateFlattenView_Form->UICreateFlattenView_CheckAlignment->value
          ?MaxHeapSize nrGetMaxHeapSize( )
        )
      )

      ; stage 2 -- create power grid cell
      when( flattenView
      if( UICreateFlattenView_Form->UICreateFlattenView_Stage2->value then
        powerGridCellView=ddGetObj( 
          CellView~>libName 
          UICreateFlattenView_Form->UICreateFlattenView_PowerGridCellName->value 
          "layout" 
        )
        powerCellViewName="layout"
        if( powerGridCellView then
          powerGridCellView=nrOpenCellViewWritable( 
            CellView~>libName 
            UICreateFlattenView_Form->UICreateFlattenView_PowerGridCellName->value 
            powerCellViewName
          )
          if( powerGridCellView == nil then
            powerGridCellView=nrOpenCellViewReadable( 
              CellView~>libName 
              UICreateFlattenView_Form->UICreateFlattenView_PowerGridCellName->value 
              powerCellViewName 
            )
            ErrorStr=CDSP4Edit(  
              ( ConfigFileGetValue TheCDSConfigTable "P4USER" )
              ( ConfigFileGetValue TheCDSConfigTable "P4PASSWD" )
              UICreateFlattenView_Form->UICreateAbstract_P4Client->value
              ( ConfigFileGetValue TheCDSConfigTable "P4CONFIG" )
              powerGridCellView
              ( ConfigFileGetValue TheCDSConfigTable "TEMP" ) 
            )
          )
        else
          powerGridCellView=dbOpenCellViewByType( 
            CellView~>libName 
            UICreateFlattenView_Form->UICreateFlattenView_PowerGridCellName->value 
            "layout" 
            "maskLayout" 
            "w"
          )
          ErrorStr=CDSP4Add(  
            ( ConfigFileGetValue TheCDSConfigTable "P4USER" )
            ( ConfigFileGetValue TheCDSConfigTable "P4PASSWD" )
            UICreateFlattenView_Form->UICreateAbstract_P4Client->value
            ( ConfigFileGetValue TheCDSConfigTable "P4CONFIG" )
            powerGridCellView
            ( ConfigFileGetValue TheCDSConfigTable "TEMP" ) 
          )
        )
        if( stringp( ErrorStr ) println( ErrorStr ))
        flattenView=nrFlattenViewPG( 
          CellView 
          UICreateFlattenView_Form->UICreateFlattenView_FlattenViewName->value
          UICreateFlattenView_Form->UICreateFlattenView_PowerGridCellName->value
          powerCellViewName
          UICreateFlattenView_Form->UICreateFlattenView_FlattenCellsList->value
          ?Verbose UICreateFlattenView_Form->UICreateFlattenView_Verbose->value
          ?DeleteWires UICreateFlattenView_Form->UICreateFlattenView_DeleteWires->value
          ?RegenerateFromCast nil
          ?MaxHeapSize nrGetMaxHeapSize( )
          ?OBSCellName UICreateFlattenView_Form->UICreateFlattenView_OBSCellName->value
          ?OBSViewName "layout"
          ?BusWireCellName UICreateFlattenView_Form->UICreateFlattenView_BusWireCellName->value
          ?BusWireViewName "buswires"
        )
      
      ; create power grid abstract
        powerGridCellAbstractView=ddGetObj( 
          CellView~>libName 
          UICreateFlattenView_Form->UICreateFlattenView_PowerGridCellName->value 
          "abstract" 
        )
        if( powerGridCellAbstractView then
          powerGridCellAbstractView=nrOpenCellViewWritable( 
            CellView~>libName 
            UICreateFlattenView_Form->UICreateFlattenView_PowerGridCellName->value 
            "abstract" 
          )
          if( powerGridCellAbstractView == nil then
            powerGridCellAbstractView=nrOpenCellViewReadable( 
              CellView~>libName 
              UICreateFlattenView_Form->UICreateFlattenView_PowerGridCellName->value 
              "abstract" 
            )
            ErrorStr=CDSP4Edit(  
              ( ConfigFileGetValue TheCDSConfigTable "P4USER" )
              ( ConfigFileGetValue TheCDSConfigTable "P4PASSWD" )
              UICreateFlattenView_Form->UICreateAbstract_P4Client->value
              ( ConfigFileGetValue TheCDSConfigTable "P4CONFIG" )
              powerGridCellAbstractView
              ( ConfigFileGetValue TheCDSConfigTable "TEMP" ) 
            )
          )
        else
          powerGridCellAbstractView=dbOpenCellViewByType( 
            CellView~>libName 
            UICreateFlattenView_Form->UICreateFlattenView_PowerGridCellName->value 
            "abstract" "maskLayout" "w"
          )
          ErrorStr=CDSP4Add(  
            ( ConfigFileGetValue TheCDSConfigTable "P4USER" )
            ( ConfigFileGetValue TheCDSConfigTable "P4PASSWD" )
            UICreateFlattenView_Form->UICreateAbstract_P4Client->value
            ( ConfigFileGetValue TheCDSConfigTable "P4CONFIG" )
            powerGridCellAbstractView
            ( ConfigFileGetValue TheCDSConfigTable "TEMP" ) 
          )
        )
         AutoGeneratePowerGridAbstract( 
          flattenView
          UICreateFlattenView_Form->UICreateFlattenView_PowerGridCellName->value 
          ?powerViewName "layout" 
          ?powerAbstractViewName "abstract" 
          ?OBSCellName UICreateFlattenView_Form->UICreateFlattenView_OBSCellName->value
          ?OBSViewName "layout"
          ?M3PowerPins UICreateFlattenView_Form->UICreateFlattenView_M3PowerPins->value
          ?ResolveMacroBlockageConflict UICreateFlattenView_Form->UICreateFlattenView_ResolveM3MacroConflict->value
          ?Class UICreateFlattenView_Form->UICreateFlattenView_Class->value
          ?M5PowerPins t
          ?M7PowerPins t
        )

        ; log abstract options
        OptionList = "Create Power Grid Abstract"
        Command = sprintf( nil 
                "%s/createAbstractCellLog --cell=%s:%s:%s --client-spec=%s --dfII-dir=%s --opt-str=\"%s\""
                PackageGetBinRoot( )
                CellView~>libName
                UICreateFlattenView_Form->UICreateFlattenView_PowerGridCellName->value
                "layout"
                ConfigFileGetValue( TheCDSConfigTable "P4CLIENT" )
                ConfigFileGetValue( TheCDSConfigTable "DFII_DIR" )
                OptionList
                )
        printf( "Update abstract log.\n%s\n" Command )
        shell( Command )
       )
     )
  ))
)
