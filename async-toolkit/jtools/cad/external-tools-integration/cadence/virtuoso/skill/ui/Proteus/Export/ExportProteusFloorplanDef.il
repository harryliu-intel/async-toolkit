; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id: //depot/sw/main/cad/external-tools-integration/cadence/virtuoso/skill/ui/Nano/Export/ExportDesignDef.il#4 $
; $DateTime: 2008/09/25 16:44:29 $
; $Author: khe $

;
; menu item Proteus/ExportProteusFloorplanDef
;
; reference to Fulcrum/ExportProteusFloorplanDef
( list "3.Export Proteus Floorplan DEF..." "UIExportProteusDef" )

; moved FindInplacedPins to nano/util.il

( UICreateMenuItemCallBack
  `UIExportProteusDefCB
  (lambda ()
    let((CellView powerCellName Components RComponents)
      CellView = UIGetCellView()
      Components = parseString( CellView~>cellName "." )
      RComponents = reverse( Components )
      if( length(RComponents)>=3 then
        powerCellName = sprintf( nil "%s_%s" cadr(RComponents) car(RComponents)) 
        powerCellName = sprintf( nil "%s.wires.%s_POWER_GRID_TIEOFF" 
                                 car( NameParseCellName( CellView~>cellName )) powerCellName )
        UIExportProteusDef_Form->UIExportProteusDef_BlockageCellName->value = powerCellName
      )
      UIExportProteusDef_Form->UIExportProteusDef_DefFileName->value =
        strcat(UIGetCellView()~>cellName ".def")
      UIExportProteusDef_Form->UIExportProteusDef_SetSkipRoutingFileName->value =
        strcat(UIGetCellView()~>cellName ".SetSkipRouting.tcl")
      hiDisplayForm( UIExportProteusDef_Form ) 
    )
  ) 
)
 
( UICreateAction
  'UIExportProteusDefFormCB
  (lambda ()
    let( (CellView BlockageCellView)
      CellView = UIGetCellView()
      BlockageCellView= nil

      ; bbox should be aligned to power grid
      grid = floor( UIExportProteusDef_Form->UIExportProteusDef_Grid->value*UIExportProteusDef_Form->UIExportProteusDef_Units->value)
      printf("Grid %f\n"  UIExportProteusDef_Form->UIExportProteusDef_Grid->value)
      prBound=setof( lpp CellView~>lpps lpp~>layerName=="prBoundary" && lpp~>objType!="label" && lpp~>shapes )
        dieAreaXY1 = car(CellView~>bBox)
        dieAreaXY2 = cadr(CellView~>bBox)
        minX=floor( car(dieAreaXY1)*UIExportProteusDef_Form->UIExportProteusDef_Units->value)
        minY=floor( car(dieAreaXY1)*UIExportProteusDef_Form->UIExportProteusDef_Units->value)
        maxX=floor( car(dieAreaXY2)*UIExportProteusDef_Form->UIExportProteusDef_Units->value)
        maxY=floor( car(dieAreaXY2)*UIExportProteusDef_Form->UIExportProteusDef_Units->value)

      warncnt=0;
      if( ( modulo(minX,grid) != 0 ) then
        printf( "Warning LLX is off grid (%d,%d)\n" minX,grid)
        warncnt=1
      )
      if( ( modulo(minY,grid) != 0 ) then
        printf( "Warning LLY is off grid (%d,%d)\n" minY,grid)
        warncnt=1
      )
      if( ( modulo(maxX,grid) != 0 ) then
        printf( "Warning URX is off grid (%d,%d)\n" maxX,grid)
        warncnt=1
      )
      if( ( modulo(maxY,grid) != 0 ) then
        printf( "Warning URY is off grid (%d,%d)\n" maxY,grid)
        warncnt=1
      )
      
      (let ( ( Cancel nil ) )
          if( ( warncnt == 1 ) then
            ( hiDisplayAppDBox
              ?name `UIExportProteusDefWarnForm
              ?dboxBanner "DEF Off Grid Warning"  
              ?dboxText "Bounding box is not a multiple of the 5.76um power
grid.\nCannot export Proteus DEF.\nRegenerate POWER_GRID overlay."
              ?buttons ( list "Write Anyway" "Cancel" )
              ?callback ( list "Cancel = nil" "Cancel = t" )
              ?buttonLayout `UserDefined
              ?help nil
             )
          )
          (CanonicalizePins)
          dirmap = defGenerateP2DTable( ( getq CellView cellName ) nil )
          if( dirmap == nil then
             let( ( basename cn )
                cn = ( getq CellView cellName )
                basename = cn
                if(rexCompile( "\\.[^\\.]+$" ) then
                    basename=rexReplace( cn "" 0 ) )
                dirmap = defGenerateP2DTable( basename t )
            ) )

          if( ( Cancel == nil ) then
              if( UIExportProteusDef_Form->UIExportProteusDef_ExportBlockage->value then
                     BlockageCellView=nrOpenCellViewReadable( CellView~>libName
                                                 UIExportProteusDef_Form->UIExportProteusDef_BlockageCellName->value
                                                 "abstract" )
              )
              defDefOut( CellView~>libName CellView~>cellName CellView~>viewName
                         UIExportProteusDef_Form->UIExportProteusDef_DefFileName->value
                         ?Verbose UIExportProteusDef_Form->UIExportProteusDef_Verbose->value
                         ?DIVIDERCHAR "|"
                         ?BUSBITCHARS "<>"
                         ?UNITS UIExportProteusDef_Form->UIExportProteusDef_Units->value
                         ?OutputNets UIExportProteusDef_Form->UIExportProteusDef_OutputNets->value
                         ?OutputPins UIExportProteusDef_Form->UIExportProteusDef_OutputPins->value
                         ?AllSpecialNets UIExportProteusDef_Form->UIExportProteusDef_AllSpecialNets->value
                         ?PowerPins UIExportProteusDef_Form->UIExportProteusDef_PowerNets->value
                         ?transform list(0:0 UIExportProteusDef_Form->UIExportProteusDef_Rotation->value 1.0)
                         ?UseModuleName t
                         ?DirMap dirmap
                         ?BlockageCellView BlockageCellView
              )
              pout=outfile(UIExportProteusDef_Form->UIExportProteusDef_SetSkipRoutingFileName->value)
              fprintf(pout "proc ProteusUserSetSkipRouting {} {\n")
              inplacedPins=FindInplacedPins(CellView)
              foreach( pinName inplacedPins 
                rexCompile("\\[")
                pinName=rexReplace(pinName "\\\\[" 0)
                rexCompile("\\]")
                pinName=rexReplace(pinName "\\\\]" 0)
                fprintf(pout "  setAttribute -net \"%s\" -skip_routing true\n" pinName)
              )
              fprintf(pout "}\n")
              close(pout)
          )
        )
    )
  )
)


( UICreateComponent
  "UIExportProteusDef_Form"
  ( UICreateConstructor 
    (lambda ( Children )
      (let ( Fields )
         Fields = list(
                ( hiCreateStringField
                  ?name `UIExportProteusDef_DefFileName
                  ?prompt "DEF Filename" 
                  ?defValue "def.out" )
                ( hiCreateStringField
                  ?name `UIExportProteusDef_SetSkipRoutingFileName
                  ?prompt "Set Skip Routing Filename" 
                  ?defValue "skip.out" )
                ( hiCreateRadioField
                  ?name `UIExportProteusDef_Rotation
                  ?prompt "Rotation"
                  ?choices list( "R90")
                  ?defValue "R90" )
                ( hiCreateBooleanButton
                  ?name `UIExportProteusDef_OutputPins
                  ?buttonText "Output Pins"
                  ?defValue t )
                ( hiCreateBooleanButton
                  ?name `UIExportProteusDef_OutputNets
                  ?buttonText "Output Nets"
                  ?defValue nil )
                ( hiCreateBooleanButton
                  ?name `UIExportProteusDef_AllSpecialNets
                  ?buttonText "Regard All Existing Wires As SPECIALNETS"
                  ?defValue nil )
                ( hiCreateBooleanButton
                  ?name `UIExportProteusDef_PowerNets
                  ?buttonText "Export Power Nets"
                  ?defValue t )
                ( hiCreateBooleanButton
                  ?name `UIExportProteusDef_ExportBlockage
                  ?buttonText "ExportBlockage"
                  ?defValue t )
                ( hiCreateStringField
                  ?name `UIExportProteusDef_BlockageCellName
                  ?prompt "Blockage Cell Name" 
                  ?defValue "" )
                ( hiCreateFloatField
                  ?name `UIExportProteusDef_Units
                  ?prompt "Units"
                  ?defValue 1000.0 )
                ( hiCreateFloatField
                  ?name `UIExportProteusDef_Grid
                  ?prompt "Grid"
                  ?defValue 5.76 )
                ( hiCreateFormButton
                  ?name `UIExportProteusDef_Help
                  ?buttonText "Tool Help"
                  ?callback "nrOpenHtml( \"Export_Design_DEF_help.html\" )" )
        ) 
        ( hiCreateAppForm
          ?name `UIExportProteusDef_Form
          ?formTitle "Export Design DEF"  
          ?fields Fields
          ?callback "( UIExportProteusDefFormCB )"
        ) 
      ) 
    ) 
  ) 
)
