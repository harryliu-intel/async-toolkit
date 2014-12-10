; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$


( list "Fold Chains" "UIFoldChains" )

( UICreateMenuItemCallBack 
  `UIFoldChainsCB
  (lambda ()
    ( hiDisplayForm UIFoldChains_Form ) ) )

( UICreateAction
  'UIFoldChainsFormCB
  (lambda ()
    (let (
          ( SelectedChains ( cadr ( NameFilterInstances
                                    ( setof Selected ( geGetSelectedSet )
                                            ( and ( equal ( getq Selected objType ) "inst" )
                                                  ( equal ( getq Selected cellView ) ( UIGetCellView ) ) ) )
                                     ChainLibCellPairRegExs ) ) ) )

      
      ( RefoldRefoldChains
        ( UIGetCellView )
        SelectedChains
        ChainLibCellPairRegExs
        UserUnitsPerMeter
        ( equal UIFoldChains_Form->UIFoldChains_Extent->value "Entire Domain" )
        (if ( not UIFoldChains_Form->UIFoldChains_OverrideFolds->value )
            ( RefoldGetDefaultFoldFunc 
              ( UIGetCellView )
              UIFoldChains_Form->UIFoldChains_ColumnWidth->value 
              UserUnitsPerMeter
              ManufacturingGrid
              UIFoldChains_Form->UIFoldChains_PreferEvenFolds->value
              )          
          ( RefoldGetOverrideFoldsFoldFunc 
            ( UIGetCellView )
            UIFoldChains_Form->UIFoldChains_ExplicitFolds->value 
            ManufacturingGrid
            ) )
        GNDNetName
        VddNetName
        ) ) ) )
     
          
            

( UICreateComponent
  "UIFoldChains_Form"
  ( UICreateConstructor 
    (lambda ( Children )
      (let (
            ( Fields
              ( list
                ( hiCreateCyclicField
                  ?name `UIFoldChains_Extent
                  ?choices ( list "Selected" "Entire Domain" )
                  ?defValue "Selected"
                  ?prompt "Fold..." )
                ( hiCreateBooleanButton
                  ?name `UIFoldChains_OverrideFolds
                  ?defValue nil
                  ?buttonText "Override Folds" )
                ( hiCreateBooleanButton
                  ?name `UIFoldChains_PreferEvenFolds
                  ?defValue t
                  ?buttonText "Prefer Even Folds" )
                ( hiCreateIntField
                  ?name `UIFoldChains_ExplicitFolds
                  ?prompt "Folds" )
                ( hiCreateFloatField
                  ?name `UIFoldChains_ColumnWidth
                  ?prompt "Column Width" )
                
                ) ) )
        ( hiCreateAppForm
          ?name `UIFoldChains_Form
          ?formTitle "Fold Chains"  
          ?fields Fields
          ?callback "( UIFoldChainsFormCB )"
          ) ) ) 
    ) )
        
 
