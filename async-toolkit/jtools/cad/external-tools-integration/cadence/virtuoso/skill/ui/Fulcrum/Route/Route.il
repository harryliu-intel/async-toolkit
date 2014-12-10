; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$


( list "Route" "UIRoute" )

( UICreateMenuItemCallBack 
  `UIRouteCB
  (lambda ()
    UIRoute_Form->UIRoute_Form_VsrTcl->value = 
     ( sprintf 
      nil
      "%s/share/Fulcrum/cell_automation/router/leaf_vsr.tcl" 
      ( ConfigFileGetValue TheCDSConfigTable "FULCRUM_PDK_ROOT" ) )
    UIRoute_Form->UIRoute_Form_ExcludeNet->value = 
    ( sprintf 
      nil
      "%s/share/Fulcrum/cell_automation/router/leaf.excludenet" 
      ( ConfigFileGetValue TheCDSConfigTable "FULCRUM_PDK_ROOT" ) )
    ( hiDisplayForm UIRoute_Form ) )

    )


( UICreateAction
  'UIRoute_FormCB
  (lambda ()
    ( UIWritePreferences
      `UIRoute_Form )
    (RouteLeaf)
  )
)    

( UICreateComponent
  "UIRoute_Form"
  ( UICreateConstructor 
    (lambda ( Children )
      (let (
            ( Fields
              ( list              
                ( hiCreateStringField
                  ?name `UIRoute_Form_VsrTcl
                  ?prompt "Vsr Tcl File"
                  ?defValue "" )
                ( hiCreateStringField
                  ?name `UIRoute_Form_ExcludeNet
                  ?prompt "Exclude Net File"
                  ?defValue "" )
               ) ) )
        ( hiCreateAppForm
          ?name `UIRoute_Form
          ?formTitle "Route"  
          ?fields Fields
          ?callback "( UIRoute_FormCB )"
          ) ) )
    ) )
