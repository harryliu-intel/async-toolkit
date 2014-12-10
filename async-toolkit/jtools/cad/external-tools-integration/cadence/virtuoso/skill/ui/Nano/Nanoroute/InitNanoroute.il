; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

( list "1.Create Nanoroute Init..." "UIInitNanoroute" )


( UICreateMenuItemCallBack
  `UIInitNanorouteCB
  (lambda ()
    ( let (cellName)
      ( hiDisplayForm UIInitNanoroute_Form )
    )
  ) 
)
    

( UICreateComponent
  "UIInitNanoroute_Form"
  ( UICreateConstructor 
    (lambda ( Children )
      ( let ( Fields )
         Fields = ( list
                ( hiCreateCyclicField
                  ?name `UIInitNanoroute_TopLayer
                  ?prompt "  Top Routing Layer" 
                  ?choices list("2" "3" "4" "5" "6" "7") 
                  ?defValue "5" )
                ( hiCreateCyclicField
                  ?name `UIInitNanoroute_BottomLayer
                  ?prompt "  Bottom Routing Layer" 
                  ?choices list("2" "3" "4" "5" "6" "7") 
                  ?defValue "3" )
          )
        ( hiCreateAppForm
          ?name `UIInitNanoroute_Form
          ?formTitle "Create Nanoroute Init"  
          ?fields Fields
          ?callback "( UIInitNanorouteFormCB )"
        ) 
    )
  )
))


( UICreateAction
  'UIInitNanorouteFormCB
  (lambda ()
      (NanoRouteTcl ?bottomMetal (atoi UIInitNanoroute_Form->UIInitNanoroute_BottomLayer->value)
                    ?topMetal (atoi UIInitNanoroute_Form->UIInitNanoroute_TopLayer->value)
                    ?Verify nil
      )
))

