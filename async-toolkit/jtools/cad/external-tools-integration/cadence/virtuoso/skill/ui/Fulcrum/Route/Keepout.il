; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

( list "Create Abstract Views" "UIKeepout" )

( UICreateMenuItemCallBack 
  `UIKeepoutCB
  (lambda ()
    ( hiDisplayForm UIKeepout_Form ) ) )

( UICreateAction
  `UIKeepout_FormCB
  (lambda () nil ) )


( UICreateAction
  'UIKeepout_DrawRoutingCB
  (lambda ()
    (let (
          ( CellView ( UIGetCellView ) ) )
      ( KeepOutCreateRoutingView 
        CellView
        ( getq CellView libName )
        ( getq CellView cellName )
        UIKeepout_Form->UIKeepout_Form_RoutingViewName->value
        ( sprintf 
          nil
          "%s/share/Fulcrum/cell_automation/keepout.rules"
          ( ConfigFileGetValue TheCDSConfigTable "FULCRUM_PDK_ROOT" ) )
        ( ConfigFileGetValue TheCDSConfigTable "TEMP" )
        
        ( mapcar 
          (lambda ( LPP )
            ( KeepOutCreateRoutingLayerStruct LPP ) )
          ( ListAppend
            (if  UIKeepout_Form->UIKeepout_Form_Metal3->value
                ( list Metal3LPP ) )
            (if  UIKeepout_Form->UIKeepout_Form_Metal5->value
                ( list Metal5LPP ) )
            (if  UIKeepout_Form->UIKeepout_Form_Metal7->value
                    ( list Metal7LPP ) )
            (if  UIKeepout_Form->UIKeepout_Form_Metal4->value
                ( list Metal4LPP ) )
            (if  UIKeepout_Form->UIKeepout_Form_Metal6->value
                ( list Metal6LPP ) ) ) )
          
        ( list GNDNetName VddNetName ) )
    ) ) )


( UICreateAction
  'UIKeepout_DrawAbstractCB
  (lambda ()
    (let (
          ( CellView ( UIGetCellView ) ) )
      ( AbstractCreateAbstractView
        CellView
        ( sprintf 
          nil
          "%s/share/Fulcrum/cell_automation/abstract.rul"
          ( ConfigFileGetValue TheCDSConfigTable "FULCRUM_PDK_ROOT" ) )
        ViaLayerFormat
        MetalLayerFormat
        ( ListAppend
          (if  UIKeepout_Form->UIKeepout_Form_Metal3->value
              ( list Metal3LPP ) )
          (if  UIKeepout_Form->UIKeepout_Form_Metal4->value
              ( list Metal4LPP ) )
          (if  UIKeepout_Form->UIKeepout_Form_Metal5->value
              ( list Metal5LPP ) )
          (if  UIKeepout_Form->UIKeepout_Form_Metal6->value
              ( list Metal6LPP ) )
          (if  UIKeepout_Form->UIKeepout_Form_Metal7->value
              ( list Metal7LPP ) )
          )

        ( ListAppend
          (if  UIKeepout_Form->UIKeepout_Form_Metal3->value
              ( list Metal3LPP ) )
          (if  UIKeepout_Form->UIKeepout_Form_Metal4->value
              ( list Metal4LPP ) )
          (if  UIKeepout_Form->UIKeepout_Form_Metal5->value
              ( list Metal5LPP ) )
          (if  UIKeepout_Form->UIKeepout_Form_Metal6->value
              ( list Metal6LPP ) )
          (if  UIKeepout_Form->UIKeepout_Form_Metal7->value
              ( list Metal7LPP ) )
          )

        ( ListAppend
          (if ( or 
                UIKeepout_Form->UIKeepout_Form_Metal3->value
                UIKeepout_Form->UIKeepout_Form_Metal4->value )
              ( list Via34LPP ) )
          (if ( or 
                UIKeepout_Form->UIKeepout_Form_Metal4->value
                UIKeepout_Form->UIKeepout_Form_Metal5->value )
              ( list Via45LPP ) )
          (if ( or 
                UIKeepout_Form->UIKeepout_Form_Metal5->value
                UIKeepout_Form->UIKeepout_Form_Metal6->value )
              ( list Via56LPP ) )
          (if ( or 
                UIKeepout_Form->UIKeepout_Form_Metal6->value
                UIKeepout_Form->UIKeepout_Form_Metal7->value )
              ( list Via67LPP ) ) )

        ( ConfigFileGetValue TheCDSConfigTable "TEMP" )
        FoldableCellLibCellPairRegExs
        GNDNetName
        VddNetName
        ContactLibrary
        ContactMinCellNameFormat
        ContactViewName
        BoundaryLPP
    ) ) ) )

( UICreateComponent
  "UIKeepout_Form"
  ( UICreateConstructor 
    (lambda ( Children )
      (let (
            ( Fields
              ( list           
                ( hiCreateButton
                  ?name `UIKeepout_Form_DrawAbstract
                  ?buttonText "Create abstract view"
                  ?callback "( UIKeepout_DrawAbstractCB )" )
;                ( hiCreateButton
;                  ?name `UIKeepout_Form_DrawRouting
;                  ?buttonText "Create routing view"
;                  ?callback "( UIKeepout_DrawRoutingCB )" )              
                ( hiCreateBooleanButton
                  ?name `UIKeepout_Form_Metal3
                  ?buttonText "Metal 3"
                  ?defValue t )
                ( hiCreateBooleanButton
                  ?name `UIKeepout_Form_Metal4
                  ?buttonText "Metal 4"
                  ?defValue nil )
                ( hiCreateBooleanButton
                  ?name `UIKeepout_Form_Metal5
                  ?buttonText "Metal 5"
                  ?defValue nil )
                ( hiCreateBooleanButton
                  ?name `UIKeepout_Form_Metal6
                  ?buttonText "Metal 6"
                  ?defValue nil )
                ( hiCreateBooleanButton
                  ?name `UIKeepout_Form_Metal7
                  ?buttonText "Metal 7"
                  ?defValue nil )

                ( hiCreateStringField
                  ?name `UIKeepout_Form_RoutingViewName
                  ?prompt "Routing View Name"
                  ?defValue "routing" )
                
                ) ) )
        ( hiCreateAppForm
          ?name `UIKeepout_Form
          ?formTitle "Abtract View"  
          ?fields Fields
          ?callback "( UIKeepout_FormCB )"
          ) ) )
    ) )
        
 
