; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

;
; menu item Nano/Export
;
( list "3.Export Design DEF..." "UIExportDesignDef" )


( UICreateMenuItemCallBack
  `UIExportDesignDefCB
  (lambda ()
    let((CellView powerCellName Components RComponents)
      CellView = UIGetCellView()
      Components = parseString( CellView~>cellName "." )
      RComponents = reverse( Components )
      if( length(RComponents)>=3 then
        powerCellName = sprintf( nil "%s_%s" cadr(RComponents) car(RComponents)) 
        powerCellName = sprintf( nil "%s.wires.%s_POWER_GRID_TIEOFF" 
                                 car( NameParseCellName( CellView~>cellName )) powerCellName )
        UIExportDesignDef_Form->UIExportDesignDef_BlockageCellName->value = powerCellName
      )
      UIExportDesignDef_Form->UIExportDesignDef_DefFileName->value =
        strcat( UIGetCellView()~>cellName ".def")
      hiDisplayForm( UIExportDesignDef_Form ) 
    )
  ) 
)

( UICreateAction
  'UIExportDesignDefFormCB
  (lambda ()
    let( (CellView pinmap mapfile tempdir Synchronous BlockageCellView)
      CellView= UIGetCellView()
      BlockageCellView= nil
      Synchronous=UIExportDesignDef_Form->UIExportDesignDef_Synchronous->value
      pinmap = nil
      dirmap = defGenerateP2DTable( ( getq CellView cellName ) nil )
      (CanonicalizePins)
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
      if( UIExportDesignDef_Form->UIExportDesignDef_ExportBlockage->value then
        BlockageCellView=nrOpenCellViewReadable( CellView~>libName
                                                 UIExportDesignDef_Form->UIExportDesignDef_BlockageCellName->value
                                                 "abstract" )
      )
      defDefOut( CellView~>libName CellView~>cellName CellView~>viewName
                 UIExportDesignDef_Form->UIExportDesignDef_DefFileName->value
                 ?Verbose UIExportDesignDef_Form->UIExportDesignDef_Verbose->value
                 ?DIVIDERCHAR "|"
                 ?BUSBITCHARS "<>"
                 ?UNITS UIExportDesignDef_Form->UIExportDesignDef_Units->value
                 ?OutputNets UIExportDesignDef_Form->UIExportDesignDef_OutputNets->value
                 ?AllSpecialNets UIExportDesignDef_Form->UIExportDesignDef_AllSpecialNets->value
                 ?PowerPins UIExportDesignDef_Form->UIExportDesignDef_PowerNets->value
                 ?Synchronous UIExportDesignDef_Form->UIExportDesignDef_Synchronous->value
                 ?PinMap pinmap
                 ?DirMap dirmap
                 ?BlockageCellView BlockageCellView
      )
    )
  )
)


( UICreateComponent
  "UIExportDesignDef_Form"
  ( UICreateConstructor 
    (lambda ( Children )
      (let ( Fields )
         Fields = list(
                ( hiCreateStringField
                  ?name `UIExportDesignDef_DefFileName
                  ?prompt "DEF Filename" 
                  ?defValue "def.out" )
                ( hiCreateBooleanButton
                  ?name `UIExportDesignDef_OutputNets
                  ?buttonText "Output Nets"
                  ?defValue t )
                ( hiCreateBooleanButton
                  ?name `UIExportDesignDef_AllSpecialNets
                  ?buttonText "Regard All Existing Wires As SPECIALNETS"
                  ?defValue t )
                ( hiCreateBooleanButton
                  ?name `UIExportDesignDef_PowerNets
                  ?buttonText "Export Power Nets"
                  ?defValue t )
                ( hiCreateBooleanButton
                  ?name `UIExportDesignDef_Synchronous
                  ?buttonText "Synchronous"
                  ?defValue nil )
                ( hiCreateBooleanButton
                  ?name `UIExportDesignDef_ExportBlockage
                  ?buttonText "ExportBlockage"
                  ?defValue nil )
                ( hiCreateStringField
                  ?name `UIExportDesignDef_BlockageCellName
                  ?prompt "Blockage Cell Name" 
                  ?defValue "" )
                ( hiCreateFloatField
                  ?name `UIExportDesignDef_Units
                  ?prompt "Units"
                  ?defValue 1000.0 )
                ( hiCreateFormButton
                  ?name `UIExportDesignDef_Help
                  ?buttonText "Tool Help"
                  ?callback "nrOpenHtml( \"Export_Design_DEF_help.html\" )" )
        ) 
        ( hiCreateAppForm
          ?name `UIExportDesignDef_Form
          ?formTitle "Export Design DEF"  
          ?fields Fields
          ?callback "( UIExportDesignDefFormCB )"
        ) 
      ) 
    ) 
  ) 
)
