; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id: //depot/sw/cad/external-tools-integration/cadence/virtuoso/main/skill/ui/Fulcrum/Pins/AutoPowerGridAbstract.il#26 $
; $DateTime: 2005/06/17 14:39:47 $
; $Author: khe $

( list "Auto PowerGrid Abstract" "UIAutoPowerGridAbstract" )


( UICreateMenuItemCallBack 
  `UIAutoPowerGridAbstractCB
  (lambda ()    
    let((CellView powerCellName Components RComponents)
       CellView = UIGetCellView()
    ( hiDisplayForm UIAutoPowerGridAbstract_Form ) 
    ) 
  ) 
)


( UICreateAction
  'UIAutoPowerGridAbstractFormCB
  (lambda ()
    ( createDir ( ConfigFileGetValue TheCDSConfigTable "TEMP" ) )
 
    (let (CellView powerGridView Command OptionList)
      CellView = UIGetCellView()
      AutoGeneratePowerGridAbstract( CellView CellView~>cellName
        ?powerViewName CellView~>viewName
        ?powerAbstractViewName UIAutoPowerGridAbstract_Form->UIAutoPowerGridAbstract_Form_PowerGridAbstractViewName->value
      )

      ; log abstract options
      OptionList = "Create Power Grid Abstract"
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
    

( UICreateComponent
  "UIAutoPowerGridAbstract_Form"
  ( UICreateConstructor 
    (lambda ( Children )
      (let ( Fields )
            Fields = list(
                ( hiCreateStringField
                  ?name `UIAutoPowerGridAbstract_Form_PowerGridAbstractViewName
                  ?prompt "Power Grid Abstract View Name" 
                  ?defValue "abstract" )
                ( hiCreateFormButton
                  ?name `UIAutoPowerGridAbstract_Form_Help
                  ?buttonText "Tool Help"
                  ?callback "nrOpenHtml( \"Create_Power_Grid_Abstract_help.html\" )" )
                )
        ( hiCreateAppForm
          ?name `UIAutoPowerGridAbstract_Form
          ?formTitle "Create Power Grid Abstract"  
          ?fields Fields
          ?callback "( UIAutoPowerGridAbstractFormCB )"
        ) 
      ) 
    )
  ) 
)
        
 
