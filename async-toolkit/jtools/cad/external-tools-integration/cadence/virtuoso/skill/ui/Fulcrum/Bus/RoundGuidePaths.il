; Copyright 2006 Fulcrum Microsystems.  All rights reserved.
; $Id: $
; $DateTime: $
; $Author: $

;
; menu item Fulcrum/Bus
;
( list "Round Guidepaths to Grid" "UIRoundGuides" )


( UICreateMenuItemCallBack
  `UIRoundGuidesCB
  (lambda ()
    hiDisplayForm( UIRoundGuides_Form ) 
  ) 
)


( UICreateComponent
  "UIRoundGuides_Form"
  ( UICreateConstructor 
    (lambda ( Children )
      (let ( Fields )
         Fields = list(
           ( hiCreateFloatField
             ?name `UIRoundGuides_grid
             ?prompt "Grid"
             ?defValue TrackPitch/2 )
         )
        ( hiCreateAppForm
          ?name `UIRoundGuides_Form
          ?formTitle "Round Guidpaths to Grid"  
          ?fields Fields
          ?callback "( UIRoundGuidesFormCB )"
        ) 
      ) 
    ) 
  ) 
)


( UICreateAction
  'UIRoundGuidesFormCB
  (lambda ()
    RoundGuidesToGrid( ?grid UIRoundGuides_Form->UIRoundGuides_grid->value )
  )
)

