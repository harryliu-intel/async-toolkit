; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id: //depot/sw/cad/external-tools-integration/cadence/virtuoso/main/skill/ui/Fulcrum/Pins/PreroutePins.il#26 $
; $DateTime: 2005/06/17 14:39:47 $
; $Author: khe $

( list "PreRoute Close-By Pins" "UIPreroutePins" )

( UICreateMenuItemCallBack 
  `UIPreroutePinsCB
  (lambda ()    
    ( hiDisplayForm UIPreroutePins_Form ) ) )

( UICreateAction
  'UIPreroutePinsFormCB
  (lambda ()
    ( createDir ( ConfigFileGetValue TheCDSConfigTable "TEMP" ) )
 
    (let (CellView pinLpps)
    CellView = UIGetCellView( )
    pinLpps=list( 
         if( UIPreroutePins_Form->UIPreroutePins_Form_Metal1->value Metal1LPP )
         if( UIPreroutePins_Form->UIPreroutePins_Form_Metal2->value Metal2LPP )
         if( UIPreroutePins_Form->UIPreroutePins_Form_Metal3->value Metal3LPP )
         if( UIPreroutePins_Form->UIPreroutePins_Form_Metal4->value Metal4LPP )
         if( UIPreroutePins_Form->UIPreroutePins_Form_Metal5->value Metal5LPP )
         if( UIPreroutePins_Form->UIPreroutePins_Form_Metal6->value Metal6LPP )
         if( UIPreroutePins_Form->UIPreroutePins_Form_Metal7->value Metal7LPP )
         if( UIPreroutePins_Form->UIPreroutePins_Form_Metal8->value Metal8LPP )
         if( UIPreroutePins_Form->UIPreroutePins_Form_Metal9->value Metal9LPP )
       )
    pinLpps=setof( lpp pinLpps lpp )
    if( UIPreroutePins_Form->UIPreroutePins_Form_CloseByPins->value then
       printf("Running Preroute Close-by Pins ...\n")
       nrCreateNeighborNetConnector( CellView UIPreroutePins_Form->UIPreroutePins_Form_MaxDistance->value pinLpps)
    )
    if( UIPreroutePins_Form->UIPreroutePins_Form_DummyPins->value then
       printf("Running Preroute Dummy Subcell Pins ...\n")
       nrCreateSingleNetConnector( CellView pinLpps ?MaxHeapSize nrGetMaxHeapSize( ) )
    )
    ) 
  ) 
)
    
( UICreateComponent
  "UIPreroutePins_Form"
  ( UICreateConstructor 
    (lambda ( Children )
      (let (
            ( Fields
              ( list
                ( hiCreateBooleanButton
                  ?name `UIPreroutePins_Form_CloseByPins
                  ?buttonText "Preroute Close-by Pins"
                  ?defValue t )
                ( hiCreateFloatField
                  ?name `UIPreroutePins_Form_MaxDistance
                  ?prompt "Max Pin Distance To Be PreRouted"
                  ?defValue 0.24 )
                ( hiCreateBooleanButton
                  ?name `UIPreroutePins_Form_DummyPins
                  ?buttonText "Preroute Dummy Subcell Pins (for proper extraction)"
                  ?defValue t )
                ( hiCreateBooleanButton
                  ?name `UIPreroutePins_Form_Metal1
                  ?buttonText "Metal1"
                  ?defValue t )
                ( hiCreateBooleanButton
                  ?name `UIPreroutePins_Form_Metal2
                  ?buttonText "Metal2"
                  ?defValue t )
                ( hiCreateBooleanButton
                  ?name `UIPreroutePins_Form_Metal3
                  ?buttonText "Metal3"
                  ?defValue t )
                ( hiCreateBooleanButton
                  ?name `UIPreroutePins_Form_Metal4
                  ?buttonText "Metal4"
                  ?defValue t )
                ( hiCreateBooleanButton
                  ?name `UIPreroutePins_Form_Metal5
                  ?buttonText "Metal5"
                  ?defValue t )
                ( hiCreateBooleanButton
                  ?name `UIPreroutePins_Form_Metal6
                  ?buttonText "Metal6"
                  ?defValue t )
                ( hiCreateBooleanButton
                  ?name `UIPreroutePins_Form_Metal7
                  ?buttonText "Metal7"
                  ?defValue t )
                ( hiCreateBooleanButton
                  ?name `UIPreroutePins_Form_Metal8
                  ?buttonText "Metal8"
                  ?defValue t )
                ( hiCreateBooleanButton
                  ?name `UIPreroutePins_Form_Metal9
                  ?buttonText "Metal9"
                  ?defValue t )
                ) ) )
        ( hiCreateAppForm
          ?name `UIPreroutePins_Form
          ?formTitle "PreRoute Close-By Pins"  
          ?fields Fields
          ?callback "( UIPreroutePinsFormCB )"
          ) ) )
    ) )
        
 
