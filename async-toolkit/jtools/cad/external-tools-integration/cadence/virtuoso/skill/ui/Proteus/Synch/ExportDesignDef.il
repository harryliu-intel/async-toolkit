; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id: //depot/sw/main/cad/external-tools-integration/cadence/virtuoso/skill/ui/Nano/Export/ExportDesignDef.il#12 $
; $DateTime: 2009/09/10 16:49:16 $
; $Author: khe $

;
; menu item Nano/Export
;
( list "6.Export Design DEF..." "UISyExportDesignDef" )


( UICreateMenuItemCallBack
  `UISyExportDesignDefCB
  (lambda ()
    let((CellView Components RComponents)
      CellView = UIGetCellView()
      UISyExportDesignDef_Form->UISyExportDesignDef_BlockageCellName->value = CellView~>cellName
      UISyExportDesignDef_Form->UISyExportDesignDef_BlockageCellView->value = CellView~>viewName
      UISyExportDesignDef_Form->UISyExportDesignDef_DefFileName->value =
        strcat( UIGetCellView()~>cellName ".def")
      hiDisplayForm( UISyExportDesignDef_Form ) 
    )
  ) 
)

( UICreateAction
  'UISyExportDesignDefFormCB
  (lambda ()
    let( (CellView pinmap mapfile tempdir Synchronous BlockageCellView)
      CellView= UIGetCellView()
      BlockageCellView= nil
      Synchronous=t
      pinmap = nil
      dirmap = nil
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
      if( UISyExportDesignDef_Form->UISyExportDesignDef_ExportBlockage->value then
        BlockageCellView=nrOpenCellViewReadable( CellView~>libName
                                                 UISyExportDesignDef_Form->UISyExportDesignDef_BlockageCellName->value
                                                 UISyExportDesignDef_Form->UISyExportDesignDef_BlockageCellView->value )
      )
      defDefOut( CellView~>libName CellView~>cellName CellView~>viewName
                 UISyExportDesignDef_Form->UISyExportDesignDef_DefFileName->value
                 ?Verbose UISyExportDesignDef_Form->UISyExportDesignDef_Verbose->value
                 ?DIVIDERCHAR "|"
                 ?BUSBITCHARS "<>"
                 ?UNITS UISyExportDesignDef_Form->UISyExportDesignDef_Units->value
                 ?OutputNets UISyExportDesignDef_Form->UISyExportDesignDef_OutputNets->value
                 ?AllSpecialNets UISyExportDesignDef_Form->UISyExportDesignDef_AllSpecialNets->value
                 ?PowerPins UISyExportDesignDef_Form->UISyExportDesignDef_PowerNets->value
                 ?Synchronous t
                 ?PinMap pinmap
                 ?DirMap dirmap
                 ?BlockageCellView BlockageCellView
      )
    )
  )
)


( UICreateComponent
  "UISyExportDesignDef_Form"
  ( UICreateConstructor 
    (lambda ( Children )
      (let ( Fields )
         Fields = list(
                ( hiCreateStringField
                  ?name `UISyExportDesignDef_DefFileName
                  ?prompt "DEF Filename" 
                  ?defValue "def.out" )
                ( hiCreateBooleanButton
                  ?name `UISyExportDesignDef_OutputNets
                  ?buttonText "Output Nets"
                  ?defValue nil )
                ( hiCreateBooleanButton
                  ?name `UISyExportDesignDef_AllSpecialNets
                  ?buttonText "Regard All Existing Wires As SPECIALNETS"
                  ?defValue nil )
                ( hiCreateBooleanButton
                  ?name `UISyExportDesignDef_PowerNets
                  ?buttonText "Export Power Nets"
                  ?defValue t )
                ( hiCreateBooleanButton
                  ?name `UISyExportDesignDef_ExportBlockage
                  ?buttonText "ExportBlockage"
                  ?defValue t )
                ( hiCreateStringField
                  ?name `UISyExportDesignDef_BlockageCellName
                  ?prompt "Blockage Cell Name" 
                  ?defValue "" )
                ( hiCreateStringField
                  ?name `UISyExportDesignDef_BlockageCellView
                  ?prompt "Blockage Cell View" 
                  ?defValue "floorplan" )
                ( hiCreateFloatField
                  ?name `UISyExportDesignDef_Units
                  ?prompt "Units"
                  ?defValue 1000.0 )
                ( hiCreateFormButton
                  ?name `UISyExportDesignDef_Help
                  ?buttonText "Tool Help"
                  ?callback "nrOpenHtml( \"Export_Design_DEF_help.html\" )" )
        ) 
        ( hiCreateAppForm
          ?name `UISyExportDesignDef_Form
          ?formTitle "Export Design DEF"  
          ?fields Fields
          ?callback "( UISyExportDesignDefFormCB )"
        ) 
      ) 
    ) 
  ) 
)
