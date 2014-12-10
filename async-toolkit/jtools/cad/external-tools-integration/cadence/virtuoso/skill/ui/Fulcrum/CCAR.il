; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id: //depot/sw/cad/external-tools-integration/cadence/virtuoso/main/skill/ui/Fulcrum/SyncToNetList.il#13 $
; $DateTime: 2004/11/24 12:59:08 $
; $Author: khe $


( list "CCAR" "UICCARFile" )

( UICreateMenuItemCallBack
  `UICCARFileCB
  (lambda ()
    hiDisplayForm( UICCARFile_Form ) 
  ) 
)
 
( UICreateAction
  'UICCARFileFormCB
  (lambda ()
    (let (routeType viaType botMetal topMetal routeGND routeVdd routeWires
          subcellView importView conductorDepth SaveQuit)
      routeType   = UICCARFile_Form->UICCARFile_RouteType->value
      viaType     = UICCARFile_Form->UICCARFile_ViaType->value
      botMetal    = UICCARFile_Form->UICCARFile_BotMetal->value
      topMetal    = UICCARFile_Form->UICCARFile_TopMetal->value
      routeGND    = UICCARFile_Form->UICCARFile_RouteGND->value
      routeVdd    = UICCARFile_Form->UICCARFile_RouteVdd->value
      routeWires  = UICCARFile_Form->UICCARFile_RouteWires->value
      SaveQuit    = UICCARFile_Form->UICCARFile_SaveQuit->value
      subcellView = UICCARFile_Form->UICCARFile_SubcellView->value
      importView  = UICCARFile_Form->UICCARFile_ImportView->value
      conductorDepth = UICCARFile_Form->UICCARFile_ConductorDepth->value
      botMetal    = (if botMetal=="wires.il" nil (atoi botMetal))
      topMetal    = (if topMetal=="wires.il" nil (atoi topMetal))
      (cond (routeType=="fat"
             (BundledRoute ?bottomMetal botMetal ?topMetal topMetal
                           ?viaType viaType ?routeWires routeWires
                           ?subcellView subcellView ?importView importView
                           ?SaveQuit SaveQuit))
            (t
             (Route ?type routeType ?viaType viaType 
                    ?routeGND routeGND ?routeVdd routeVdd
                    ?bottomMetal botMetal ?topMetal topMetal
                    ?subcellView subcellView ?importView importView
                    ?conductorDepth conductorDepth ?SaveQuit SaveQuit))
            )
      )
    )
  )

( UICreateAction
  'UICCARFile_RouteTypeCB
  (lambda ()
    (let (
          ( routeType UICCARFile_Form->UICCARFile_RouteType->value ) )
      (cond (routeType == "custom_leaf"
             UICCARFile_Form->UICCARFile_ViaType->value = "c"
             UICCARFile_Form->UICCARFile_BotMetal->value = "1"
             UICCARFile_Form->UICCARFile_TopMetal->value = "3"
             UICCARFile_Form->UICCARFile_RouteGND->value = t
             UICCARFile_Form->UICCARFile_RouteVdd->value = t
             UICCARFile_Form->UICCARFile_ImportView->value = "layout")
            (t
             UICCARFile_Form->UICCARFile_ViaType->value =
               UICCARFile_Form->UICCARFile_ViaType->defValue
             UICCARFile_Form->UICCARFile_BotMetal->value =
               UICCARFile_Form->UICCARFile_BotMetal->defValue
             UICCARFile_Form->UICCARFile_TopMetal->value =
               UICCARFile_Form->UICCARFile_TopMetal->defValue
             UICCARFile_Form->UICCARFile_RouteGND->value =
               UICCARFile_Form->UICCARFile_RouteGND->defValue
             UICCARFile_Form->UICCARFile_RouteVdd->value =
               UICCARFile_Form->UICCARFile_RouteVdd->defValue
             UICCARFile_Form->UICCARFile_ImportView->value =
               UICCARFile_Form->UICCARFile_ImportView->defValue)
      )
    )
  )
)

( UICreateComponent
  "UICCARFile_Form"
  ( UICreateConstructor 
    (lambda ( Children )
      (let ( Fields )
         Fields = list(
                ( hiCreateRadioField
                  ?name `UICCARFile_RouteType
                  ?prompt "Route Type"
                  ?choices list( "leaf" "custom_leaf" "mid" "fat")
                  ?defValue "mid"
                  ?callback list( "UICCARFile_RouteTypeCB") )
                ( hiCreateRadioField
                  ?name `UICCARFile_ViaType
                  ?prompt "Via Type"
                  ?choices list( "min" "c")
                  ?defValue "c" )
                ( hiCreateRadioField
                  ?name `UICCARFile_BotMetal
                  ?prompt "Bottom Metal Layer"
                  ?choices list( "1" "2" "3" "4" "5" "6" "7" "wires.il")
                  ?defValue "wires.il" )
                ( hiCreateRadioField
                  ?name `UICCARFile_TopMetal
                  ?prompt "Top Metal Layer"
                  ?choices list( "2" "3" "4" "5" "6" "7" "wires.il")
                  ?defValue "wires.il" )
                ( hiCreateStringField
                  ?name `UICCARFile_SubcellView
                  ?prompt "Subcell View(s)" 
                  ?defValue "abstract_edit abstract" )
                ( hiCreateStringField
                  ?name `UICCARFile_ImportView
                  ?prompt "Import View" 
                  ?defValue "layout_pg" )
                ( hiCreateIntField
                  ?name `UICCARFile_ConductorDepth
                  ?prompt "Conductor Depth (not fat mode)" 
                  ?defValue 0 )
                ( hiCreateBooleanButton
                  ?name `UICCARFile_RouteGND
                  ?buttonText "Route GND (not fat mode)"
                  ?defValue nil )
                ( hiCreateBooleanButton
                  ?name `UICCARFile_RouteVdd
                  ?buttonText "Route Vdd (not fat mode)"
                  ?defValue nil )
                ( hiCreateBooleanButton
                  ?name `UICCARFile_RouteWires
                  ?buttonText "Route Wires (fat mode only)"
                  ?defValue nil )
                ( hiCreateBooleanButton
                  ?name `UICCARFile_SaveQuit
                  ?buttonText "Save and Quit automatically"
                  ?defValue nil )
           )
        ( hiCreateAppForm
          ?name `UICCARFile_Form
          ?formTitle "CCAR"  
          ?fields Fields
          ?callback "( UICCARFileFormCB )"
          ) 
      ) 
    ) 
  ) 
)
