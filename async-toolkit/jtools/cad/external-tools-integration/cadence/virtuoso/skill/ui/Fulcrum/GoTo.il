; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id: //depot/sw/cad/external-tools-integration/cadence/virtuoso/main/skill/ui/Fulcrum/Pins/PreroutePins.il#26 $
; $DateTime: 2005/06/17 14:39:47 $
; $Author: khe $

( list "Go To Location" "UIGoToLocation" )

( UICreateMenuItemCallBack
  `UIGoToLocationCB
  (lambda ()
    ( hiDisplayForm UIGoToLocation_Form ) ) )

; create common function
(defun GoToLocation ( x y zoom )
   (let ( winId )
      winID=hiGetCurrentWindow()
      hiZoomAbsoluteScale(winID zoom)
      hiPan(winID x:y)
   )
)

; function to parse hdrc string
(defun HdrcGoToLocation ( string zoom )
    (let ( x y strings )
        x=0.0
        y=0.0
        strings=parseString( string " (,)" )
        x=(evalstring(nth(1 strings))+evalstring(nth(3 strings)))/2
        y=(evalstring(nth(2 strings))+evalstring(nth(4 strings)))/2
        GoToLocation( x y zoom)
    )
)

; callback for he Go To Location dialog with added HDRC
( UICreateAction
  'UIGoToLocationFormCB
  (lambda ()
    (let ( winID x y hdrc s)
        hdrc=UIGoToLocation_Form->UIGoToLocation_Form_HDRC->value
        s=parseString( hdrc " (,)" )
        if( length(s) < 4 then
            x=evalstring(UIGoToLocation_Form->UIGoToLocation_Form_X->value)
            y=evalstring(UIGoToLocation_Form->UIGoToLocation_Form_Y->value)
            GoToLocation( x y UIGoToLocation_Form->UIGoToLocation_Form_Zoom->value)
        else
            UIGoToLocation_Form->UIGoToLocation_Form_X->value=sprintf(nil "%.3f" (evalstring(nth(1 s))+evalstring(nth(3 s)))/2)
            UIGoToLocation_Form->UIGoToLocation_Form_Y->value=sprintf(nil "%.3f" (evalstring(nth(2 s))+evalstring(nth(4 s)))/2)
            HdrcGoToLocation( hdrc UIGoToLocation_Form->UIGoToLocation_Form_Zoom->value)
        )
    )
  )
)

( UICreateComponent
  "UIGoToLocation_Form"
  ( UICreateConstructor
    (lambda ( Children )
      (let (
            ( Fields
              ( list
                ( hiCreateStringField
                  ?name `UIGoToLocation_Form_X
                  ?prompt "x"
                  ?editable t
                  ?defValue "0" )
                ( hiCreateStringField
                  ?name `UIGoToLocation_Form_Y
                  ?prompt "y"
                  ?editable t
                  ?defValue "0" )
                ( hiCreateFloatField
                  ?name `UIGoToLocation_Form_Zoom
                  ?prompt "zoom"
                  ?defValue 2000.0 )
                ( hiCreateStringField
                  ?name `UIGoToLocation_Form_HDRC
                  ?prompt "hdrc string"
                  ?editable t
                  ?defValue "" )
                ) ) )
        ( hiCreateAppForm
          ?name `UIGoToLocation_Form
          ?formTitle "Go To Location"
          ?fields Fields
          ?callback "( UIGoToLocationFormCB )"
          ) ) )
    ) )

