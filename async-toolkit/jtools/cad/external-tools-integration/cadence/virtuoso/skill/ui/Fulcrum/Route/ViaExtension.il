; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id: //depot/sw/cad/external-tools-integration/cadence/virtuoso/main/skill/ui/Fulcrum/Route/Clean.il#17 $
; $DateTime: 2005/02/14 13:21:27 $
; $Author: khe $

( list "RouteClean2:Fix Via Extension" "UIRouteClean2" )

( UICreateMenuItemCallBack 
  `UIRouteClean2CB
  (lambda ()
    ( hiDisplayForm UIRouteClean2_Form ) ) )

( UICreateAction
  'UIRouteClean2FormCB
  (lambda ()
    (let (
          ( CellView ( UIGetCellView ) )
          ( DirectiveTable
            ( CellInfoGetTableForCellName
              "null"
              ( sprintf 
                nil
                "%s/share/Fulcrum"
                ( ConfigFileGetValue TheCDSConfigTable "FULCRUM_PDK_ROOT" ) )
              ) ))


      (cond (( PinUtilFindPRBoundShape BoundaryLPP CellView )
        
        ( createDir ( ConfigFileGetValue TheCDSConfigTable "TEMP" ) )
        ( NotchesFillNotchesOnCellView
          CellView
          append(
            (if UIRouteClean2_Form->UIRouteClean2_Form_CONT->value
               list( list( "newCont_1" list( "CONT" "dummy1"))
                     list( "newCont_2" list( "CONT" "dummy2"))))
          append(
            (if UIRouteClean2_Form->UIRouteClean2_Form_VIA12->value
               list( list( "newVia1_1" list( "VIA12" "dummy1"))
                     list( "newVia1_2" list( "VIA12" "dummy2"))
                     list( "newVia1_3" list( "VIA12" "dummy3"))
                     list( "newVia1_4" list( "VIA12" "dummy4"))))
            (if UIRouteClean2_Form->UIRouteClean2_Form_VIA23->value
               list(  list( "newVia2_1" list( "VIA23" "dummy1"))
                      list( "newVia2_2" list( "VIA23" "dummy2"))
                      list( "newVia2_3" list( "VIA23" "dummy3"))
                      list( "newVia2_4" list( "VIA23" "dummy4"))))
           ))
           
           setof( Set
                list( 
                  (if UIRouteClean2_Form->UIRouteClean2_Form_CONT->value
                      "CONT" )
                  (if UIRouteClean2_Form->UIRouteClean2_Form_VIA12->value
                      "VIA12" )
                  (if UIRouteClean2_Form->UIRouteClean2_Form_VIA12->value
                      "VIA23" ))
                stringp( Set ) )

           sprintf( nil
            "%s/share/Fulcrum/notch/fill_notches.assura.rules.all" 
              ConfigFileGetValue( TheCDSConfigTable "FULCRUM_PDK_ROOT" ) )
          ( ConfigFileGetValue TheCDSConfigTable "TEMP" )
          )
          dbSave( CellView )
          nrFixViaExtension( CellView ?SkipAssuraRun t ) 
        )
       ( t printf( "No prBoundary shape!!!\n" ) )
      )
   )
  )
)

    ( UICreateComponent
      "UIRouteClean2_Form"
      ( UICreateConstructor 
        (lambda ( Children )
          (let (
                ( Fields
                  ( list             
                    ( hiCreateBooleanButton
                      ?name `UIRouteClean2_Form_CONT
                      ?buttonText "M1_POLY1_minL"
                      ?defValue t )
                    ( hiCreateBooleanButton
                      ?name `UIRouteClean2_Form_VIA12
                      ?buttonText "M2_M1_minL"
                      ?defValue t )
                    ( hiCreateBooleanButton
                      ?name `UIRouteClean2_Form_VIA23
                      ?buttonText "M3_M2_minL"
                      ?defValue t )
              
                    ) ) ) 
            ( hiCreateAppForm
              ?name `UIRouteClean2_Form
              ?formTitle "Route Clean 2: Fix Via Extension"  
              ?fields Fields
              ?callback "( UIRouteClean2FormCB )"
              ) ) ) ) ) 

