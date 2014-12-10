; Copyright 2006 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

( list "9.Promote Inplace Pins" "UIPromoteInplacePins" )

(defun PromoteInplacePins ( view M2To M3To M4To)
  (let (obj child)
    (foreach obj view->shapes
     (when ( obj->pin )
       (when ( dbGetPropByName obj "PinType" )->value == "InPlace"
         case( obj~>layerName
           ("M2" ToLayerName=M2To )
           ("M3" ToLayerName=M3To )
           ("M4" ToLayerName=M4To )
           ( t ToLayerName="No Promote" )
         )
         if( ToLayerName!="No Promote" then
           obj~>layerName=ToLayerName 
           foreach( child obj~>children
             if( child~>objType=="label" then
                 child~>layerName=ToLayerName
             ) 
           )
         )
       )
     )
    )
  )
  t 
)



( UICreateMenuItemCallBack
  `UIPromoteInplacePinsCB
  (lambda ()
     let(()
       hiDisplayForm( UIPromoteInplacePins_Form )
    ) 
  ) 
)

( UICreateAction
  'UIPromoteInplacePinsFormCB
  (lambda ()
    let( ( )
      PromoteInplacePins( UIGetCellView()
               UIPromoteInplacePins_Form->UIPromoteInplacePins_M2->value
               UIPromoteInplacePins_Form->UIPromoteInplacePins_M3->value
               UIPromoteInplacePins_Form->UIPromoteInplacePins_M4->value
      )
    )
  )
)

( UICreateComponent
  "UIPromoteInplacePins_Form"
  ( UICreateConstructor 
    (lambda ( Children )
      (let ( Fields )
         Fields = list(
                ( hiCreateRadioField
                  ?name `UIPromoteInplacePins_M2
                  ?prompt "Promote M2 Inplace Pins to:"
                  ?choices list( "No Promote" "M4" "M6")
                  ?defValue "M6" )
                ( hiCreateRadioField
                  ?name `UIPromoteInplacePins_M3
                  ?prompt "Promote M3 Inplace Pins to:"
                  ?choices list( "No Promote" "M5" "M7")
                  ?defValue "M5" )
                ( hiCreateRadioField
                  ?name `UIPromoteInplacePins_M4
                  ?prompt "Promote M4 Inplace Pins to:"
                  ?choices list( "No Promote" "M6")
                  ?defValue "M6" )
                 ) 
        ( hiCreateAppForm
          ?name `UIPromoteInplacePins_Form
          ?formTitle "Array Cells"  
          ?fields Fields
          ?callback "( UIPromoteInplacePinsFormCB )"
          ) 
      ) 
    ) 
  ) 
)
