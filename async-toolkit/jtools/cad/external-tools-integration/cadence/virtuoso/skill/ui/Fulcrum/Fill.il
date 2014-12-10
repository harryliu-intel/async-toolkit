; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id: //depot/sw/cad/external-tools-integration/cadence/virtuoso/main/skill/ui/Fulcrum/Pins/PreroutePins.il#26 $
; $DateTime: 2005/06/17 14:39:47 $
; $Author: khe $

( list "Fill with metal" "UIFillMetal" )


( UICreateMenuItemCallBack 
  `UIFillMetalCB
  (lambda () 
      hiDisplayForm( UIFillMetal_Form ) 
  ) 
)

( UICreateAction
  'UIFillMetalFormCB
  (lambda ()
    (let ( CellView Msg p)
      CellView=UIGetCellView()
      Fill( CellView ?Type "grid" ?Layers if( UIFillMetal_Form->UIFillMetal_Layers->value=="M2 to M7" t nil))
    ) 
  ) 
)
    
( UICreateComponent
  "UIFillMetal_Form"
  ( UICreateConstructor 
    (lambda ( Children )
      (let (
            ( Fields
              ( list
                ( hiCreateRadioField
                  ?name `UIFillMetal_Layers
                  ?prompt "Layers"
                  ?choices list( "OD to M2" "M2 to M7")
                  ?defValue "M2 to M7" )
                ) ) )
        ( hiCreateAppForm
          ?name `UIFillMetal_Form
          ?formTitle "Fill Metal"  
          ?fields Fields
          ?callback "( UIFillMetalFormCB )"
          ) ) )
    ) )
        
 
