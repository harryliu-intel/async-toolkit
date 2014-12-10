; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

; 
; menu item Nano/Export
;
( list "2.Export Design LEF..." "UIExportDesignLef" )


( UICreateMenuItemCallBack
  `UIExportDesignLefCB
  (lambda ()
    let((CellView powerCellName Components RComponents)
      CellView = UIGetCellView()
      Components = parseString( CellView~>cellName "." )
      RComponents = reverse( Components )
      if( length(RComponents)>=3 then
        powerCellName = sprintf( nil "%s_%s" cadr(RComponents) car(RComponents)) 
        powerCellName = sprintf( nil "%s.wires.%s_POWER_GRID_TIEOFF" 
                                 car( NameParseCellName( CellView~>cellName )) powerCellName )
        UIExportDesignLef_Form->UIExportDesignLef_PowerGridCellName->value = powerCellName
      )
      UIExportDesignLef_Form->UIExportDesignLef_OutFile->value =
        strcat( CellView~>cellName ".lef")
      hiDisplayForm( UIExportDesignLef_Form ) 
    )
  ) 
)
 

( UICreateAction
  'UIExportDesignLefFormCB
  (lambda ()
    let( (errorMsg CellView libName cellName viewName LogFile ignore_keepout)
      CellView=UIGetCellView()

      ;check if keepouts are drawn
      ignore_keepout=t
      when( HasKeepout(CellView)
        ignore_keepout = hiDisplayAppDBox(
          ?name 'KeepoutErrorDialogBox
          ?dboxText "Routing obstructions exist in flatten view.\n This may mean that you should reflatten from the prelayout view.\n Do you want to proceed anyway?"
          ?dboxBanner "Keepouts Exist"
          ?dialogType     hicQuestionDialog
          ?dialogStyle    'modal
          ?buttonLayout   'YesNo
          ?defaultButton 2
        )
      )

      when( ignore_keepout
        lefLefOut( CellView~>libName 
                   CellView~>cellName 
                   CellView~>viewName
                   UIExportDesignLef_Form->UIExportDesignLef_OutFile->value
                 )
     )
    )
  )
)


( UICreateComponent
  "UIExportDesignLef_Form"
  ( UICreateConstructor 
    (lambda ( Children )
      (let ( Fields )
         Fields = list(
                ( hiCreateStringField
                  ?name `UIExportDesignLef_OutFile
                  ?prompt "LEF Filename" 
                  ?defValue "lef.out" )
                ( hiCreateStringField
                  ?name `UIExportDesignLef_PowerGridCellName
                  ?prompt "Power Grid Cell Name" 
                  ?defValue "default" )
                ( hiCreateFormButton
                  ?name `UIExportDesignLef_Help
                  ?buttonText "Tool Help"
                  ?callback "nrOpenHtml( \"Export_Design_LEF_help.html\" )" )
        )       
        ( hiCreateAppForm
          ?name `UIExportDesignLef_Form
          ?formTitle "Export Design LEF"  
          ?fields Fields
          ?callback "( UIExportDesignLefFormCB )"
        ) 
      ) 
    ) 
  ) 
)
