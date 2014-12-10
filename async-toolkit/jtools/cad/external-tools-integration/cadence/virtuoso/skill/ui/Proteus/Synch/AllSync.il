; Copyright 2006 Fulcrum Microsystems.  All rights reserved.
; $Id: $
; $DateTime: $
; $Author: $

;
; menu item Synch/All

; 0.All SyncExport
; 1.Flush Design 
; 2.Create Guide Instance
; 3.Create Cells List...
; 4.Create GND Shield TieOff Cell
; 5.Export Design LEF...
; 6.Export Design DEF...
; 7.Export Nondefault Rule DEF...


( list "0.All SyncExport" "UISyAllExport" )



( UICreateMenuItemCallBack
  `UISyAllExportCB
  (lambda ()
    let((CellView)
      CellView = UIGetCellView()
      CellBase = cadr( reverse( parseString( CellView~>cellName "." ) ))
      UISyAllExport_Form->UISyAllExport_cellname->value =
        strcat( CellView~>libName ".wires." CellBase "_buswires")
    )
    let((CellView CellBase)
      CellView = UIGetCellView()
      CellBase=cadr( reverse( parseString( CellView~>cellName ".") ) )
      UISyAllExport_Form->UISyAllExport_BusWireCellName->value=
          strcat(CellView~>libName ".wires." CellBase "_buswires")
      UISyAllExport_Form->UISyAllExport_CellsList->value=
        strcat( CellView~>cellName ".flatten_cells_list")
    )
    let((CellView)
      CellView = UIGetCellView()
      CellBase = cadr( reverse( parseString( CellView~>cellName "." ) ))
      UISyAllExport_Form->UISyAllExport_TieDownCellName->value =
        strcat( CellView~>libName ".wires." CellBase "_" TieDownCellName)
      UISyAllExport_Form->UISyAllExport_CellList->value =
        strcat( CellView~>cellName ".flatten_cells_list")
    )
    let((CellView)
      CellView = UIGetCellView()
      UISyAllExport_Form->UISyAllExport_LefFile->value =
        strcat( CellView~>cellName ".lef")
      UISyAllExport_Form->UICreateAbstract_FulcrumPDKRoot->value =
        getShellEnvVar( "FULCRUM_PDK_ROOT" )
      if(UISyAllExport_Form->UICreateAbstract_DfIIDir->value == "default" then
        UISyAllExport_Form->UICreateAbstract_DfIIDir->value =
          ConfigFileGetValue( TheCDSConfigTable "DFII_DIR" )
      )

      UISyAllExport_Form->UISyAllExport_LefCellsList->value =
        strcat( CellView~>cellName ".flatten_cells_list")
    )
    let((CellView Components RComponents)
      CellView = UIGetCellView()
      UISyAllExport_Form->UISyAllExport_BlockageCellName->value = CellView~>cellName
      UISyAllExport_Form->UISyAllExport_BlockageCellView->value = CellView~>viewName
      UISyAllExport_Form->UISyAllExport_DefFileName->value =
        strcat( CellView~>cellName ".def")
    )
    hiDisplayForm( UISyAllExport_Form ) 
  ) 
)


( UICreateComponent
  "UISyAllExport_Form"
  ( UICreateConstructor 
    (lambda ( Children )
      (let ( Fields )
         Fields = list(
           ( hiCreateBooleanButton
             ?name `UISyAllExport_doFlush
             ?buttonText ">>> Flush"
             ?defValue t )
            ( hiCreateSeparatorField
              ?name `UISyAllExport_sep0 )
           ( hiCreateBooleanButton
             ?name `UISyAllExport_doGuide
             ?buttonText ">>> Create Guide Instance"
             ?defValue t )
           ( hiCreateStringField
             ?name `UISyAllExport_cellname
             ?prompt "     Cell Name"
             ?defValue "" )
           ( hiCreateStringField
             ?name `UISyAllExport_instname
             ?prompt "     Instance Name"
             ?defValue "buswires" )
           ( hiCreateBooleanButton
             ?name `UISyAllExport_wires
             ?buttonText "     Create wires.il"
             ?defValue t )
           ( hiCreateBooleanButton
             ?name `UISyAllExport_defchans
             ?buttonText "     Include global DefChans in wires.il"
             ?defValue t )
                ( hiCreateSeparatorField
                  ?name `UISyAllExport_sep1 )
           ( hiCreateBooleanButton
             ?name `UISyAllExport_doCellsList
             ?buttonText ">>> Create Cells List"
             ?defValue t )
                ( hiCreateStringField
                  ?name `UISyAllExport_CellsList
                  ?prompt "   Cells List" 
                  ?defValue "flatten_cells_list" )
                ( hiCreateStringField
                  ?name `UISyAllExport_BusWireCellName
                  ?prompt "   Bus Wires Cell Name" 
                  ?defValue "BUSWIRES" )

                ( hiCreateBooleanButton
                  ?name `UISyAllExport_Verbose
                  ?buttonText "    Verbose"
                  ?defValue nil )
                ( hiCreateSeparatorField
                  ?name `UISyAllExport_sep2 )
           ( hiCreateBooleanButton
             ?name `UISyAllExport_doTieCell
             ?buttonText ">>> Create GND Tie Down Cell"
             ?defValue t )
                ( hiCreateStringField
                  ?name `UISyAllExport_TieDownCellName
                  ?prompt "     Tie Down Cell Name" 
                  ?defValue "default" )
                ( hiCreateStringField
                  ?name `UISyAllExport_TieDownViewName
                  ?prompt "     Tie Down View Name" 
                  ?defValue "layout" )
                ( hiCreateStringField
                  ?name `UISyAllExport_CellList
                  ?prompt "     LEF Cells List" 
                  ?defValue "default" )
                ( hiCreateBooleanButton
                  ?name `UISyAllExport_TreatGndNetAsPin
                  ?buttonText "     Treat GND Nets as Pins"
                  ?defValue t )
                ( hiCreateSeparatorField
                  ?name `UISyAllExport_sep3 )
           ( hiCreateBooleanButton
             ?name `UISyAllExport_doExportLef
             ?buttonText ">>> Export Design Lef"
             ?defValue t )
                ( hiCreateStringField
                  ?name `UICreateAbstract_FulcrumPDKRoot
                  ?prompt "     Fulcrum PDK Root" 
                  ?defValue "default" )
                ( hiCreateStringField
                  ?name `UICreateAbstract_DfIIDir
                  ?prompt "     DfII Dir" 
                  ?defValue "default" )
                ( hiCreateStringField
                  ?name `UISyAllExport_LefFile
                  ?prompt "     LEF Filename" 
                  ?defValue "lef.out" )
                ( hiCreateStringField
                  ?name `UISyAllExport_LefCellsList
                  ?prompt "     LEF Cells List" 
                  ?defValue "flatten_cells_list" )
                ( hiCreateBooleanButton
                  ?name `UISyAllExport_OverWrite
                  ?buttonText "     Over Write Existing LEF File"
                  ?defValue t )
                ( hiCreateSeparatorField
                  ?name `UISyAllExport_sep4 )
           ( hiCreateBooleanButton
             ?name `UISyAllExport_doDEF
             ?buttonText ">>> Export Design DEF"
             ?defValue t )
                ( hiCreateStringField
                  ?name `UISyAllExport_DefFileName
                  ?prompt "     DEF Filename" 
                  ?defValue "def.out" )
                ( hiCreateBooleanButton
                  ?name `UISyAllExport_OutputNets
                  ?buttonText "     Output Nets"
                  ?defValue nil )
                ( hiCreateBooleanButton
                  ?name `UISyAllExport_AllSpecialNets
                  ?buttonText "     Regard All Existing Wires As SPECIALNETS"
                  ?defValue nil )
                ( hiCreateBooleanButton
                  ?name `UISyAllExport_PowerNets
                  ?buttonText "     Export Power Nets"
                  ?defValue t )
                ( hiCreateBooleanButton
                  ?name `UISyAllExport_ExportBlockage
                  ?buttonText "     ExportBlockage"
                  ?defValue t )
                ( hiCreateStringField
                  ?name `UISyAllExport_BlockageCellName
                  ?prompt "     Blockage Cell Name" 
                  ?defValue "" )
                ( hiCreateStringField
                  ?name `UISyAllExport_BlockageCellView
                  ?prompt "     Blockage Cell View" 
                  ?defValue "floorplan" )
                ( hiCreateFloatField
                  ?name `UISyAllExport_Units
                  ?prompt "     Units"
                  ?defValue 1000.0 )
         )
        ( hiCreateAppForm
          ?name `UISyAllExport_Form
          ?formTitle "     Synchrnonous Export All (Experimental!)"  
          ?fields Fields
          ?callback "( UISyAllExportFormCB )"
        ) 
        hiSetFormMinMaxSize( UISyAllExport_Form list(901 400) list(2000 2000))
        hiSetFormSize( UISyAllExport_Form list(801 985))
      ) 
    ) 
  ) 
)


( UICreateAction
  'UISyAllExportFormCB
  (lambda ()
    if( UISyAllExport_Form->doFlush (FlushDesignFiles ( UIGetCellView() ) ) )
    if( UISyAllExport_Form->doGuide
        print( "Guide\n" )
        let( (CellView)
            CellView=UIGetCellView()
            MakeGuideInst( UISyAllExport_Form->UISyAllExport_cellname->value 
                           UISyAllExport_Form->UISyAllExport_instname->value )
            when( UISyAllExport_Form->UISyAllExport_wires->value 
              MakeWiresSkill( UISyAllExport_Form->UISyAllExport_instname->value
                              UISyAllExport_Form->UISyAllExport_defchans->value )
            )
        )
    )
    if( UISyAllExport_Form->doCellsList
        print( "CellsList\n" )
        let( (CellView)
          CellView= UIGetCellView()
            clCreateCellList(
              CellView 
              UISyAllExport_Form->UISyAllExport_CellsList->value
              UISyAllExport_Form->UISyAllExport_BusWireCellName->value
              ?Verbose UISyAllExport_Form->UISyAllExport_Verbose->value
            )
          )
    )
    if( UISyAllExport_Form->doTieCell
        print( "TieCell\n" )
        let( (errorMsg CellView libName cellName viewName LogFile ToolName)
          CellView=UIGetCellView()
          CreateTieDownCell( CellView
                               ?dstLibName CellView~>libName
                               ?dstCellName UISyAllExport_Form->UISyAllExport_TieDownCellName->value
                               ?dstViewName UISyAllExport_Form->UISyAllExport_TieDownViewName->value
                               ?dstSynchronous t
                               ?dstCustom t
                               ?dstSynchronous t
                               ?treatGNDNetsAsPins UISyAllExport_Form->UISyAllExport_TreatGndNetAsPin->value
                             )
          ; add the line to lef list file.
          if( system(sprintf(nil "grep %s %s" UISyAllExport_Form->UISyAllExport_TieDownCellName->value UISyAllExport_Form->UISyAllExport_CellList->value ))==1 then
             system(sprintf(nil "echo '%s %s abstract' >> %s" CellView~>libName UISyAllExport_Form->UISyAllExport_TieDownCellName->value UISyAllExport_Form->UISyAllExport_CellList->value))
          )

        )
    )
    if( UISyAllExport_Form->doExportLef
        print( "Lef\n" )
        let( (errorMsg CellView libName cellName viewName LogFile)
          LogFile = "exportDesignLef.log"
          nrDeleteToolSum( LogFile )
          CellView=UIGetCellView()
          Command = sprintf( nil
                    "%s/exportDesignLef --cell-list=%s --dfII-dir=%s --design=%s --fulcrum-pdk-root=%s --power-grid-cell=none --lefout=%s"
                    PackageGetBinRoot( )
                    UISyAllExport_Form->UISyAllExport_LefCellsList->value
                    UISyAllExport_Form->UICreateAbstract_DfIIDir->value
                    CellView~>cellName
                    UISyAllExport_Form->UICreateAbstract_FulcrumPDKRoot->value
                    UISyAllExport_Form->UISyAllExport_LefFile->value
                    )
          if( UISyAllExport_Form->UISyAllExport_OverWrite->value then
            Command = sprintf( nil "%s --write-lef" Command)
          )
          printf( "%s\n" Command )
          shell( Command )

        )
    )
    if( UISyAllExport_Form->doExportDef
        print( "Def\n" )
        let( (CellView pinmap mapfile tempdir Synchronous BlockageCellView)
          CellView= UIGetCellView()
          BlockageCellView= nil
          Synchronous=t
          pinmap = nil
          dirmap = defGenerateP2DTable( ( getq CellView cellName ) nil )
          if( Synchronous then
              pinmap = defGenerateC2VTable( ( getq CellView cellName ) nil ) )
          if( dirmap == nil then
             let( ( basename cn )
                cn = ( getq CellView cellName )
                basename = cn
                if(rexCompile( "\\.[^\\.]+$" ) then
                    basename=rexReplace( cn "" 0 ) )
                dirmap = defGenerateP2DTable( basename t )
            ) )
            if( Synchronous && ! pinmap  then
                 let( ( basename cn )
                    cn = ( getq CellView cellName )
                    basename = cn
                    if(rexCompile( "\\.[^\\.]+$" ) then
                        basename=rexReplace( cn "" 0 ) )
                    pinmap = defGenerateC2VTable( basename t )
                ) )
          if( UISyAllExport_Form->UISyAllExport_ExportBlockage->value then
            BlockageCellView=nrOpenCellViewReadable( CellView~>libName
                                                     UISyAllExport_Form->UISyAllExport_BlockageCellName->value
                                                     UISyAllExport_Form->UISyAllExport_BlockageCellView->value )
          )
          defDefOut( CellView~>libName CellView~>cellName CellView~>viewName
                     UISyAllExport_Form->UISyAllExport_DefFileName->value
                     ?Verbose UISyAllExport_Form->UISyAllExport_Verbose->value
                     ?DIVIDERCHAR "|"
                     ?BUSBITCHARS "<>"
                     ?UNITS UISyAllExport_Form->UISyAllExport_Units->value
                     ?OutputNets UISyAllExport_Form->UISyAllExport_OutputNets->value
                     ?AllSpecialNets UISyAllExport_Form->UISyAllExport_AllSpecialNets->value
                     ?PowerPins UISyAllExport_Form->UISyAllExport_PowerNets->value
                     ?Synchronous t
                     ?PinMap pinmap
                     ?DirMap dirmap
                     ?BlockageCellView BlockageCellView
          )
        )
    )
  )
)
