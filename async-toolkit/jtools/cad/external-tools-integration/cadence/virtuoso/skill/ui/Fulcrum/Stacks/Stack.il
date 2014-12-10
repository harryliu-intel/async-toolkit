; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$


( list "Stack Chains" "UIStackChains" )

( UICreateMenuItemCallBack 
  `UIStackChainsCB
  (lambda ()
    ( hiDisplayForm UIStackChains_Form ) ) )

( UICreateAction
  'UIStackChainsFormCB
  (lambda ()
    (let (
          ( Selected ( setof Fig
                             ( geGetSelectedSet )
                             ( equal Fig->objType "inst" )
                             ) ) )
      (let (
            ( GateFilter
              ( NameFilterInstances
                Selected
                GateLibCellPairRegExs ) )
            ( NFilter
              ( NameFilterInstances
                Selected
                NChainLibCellPairRegExs ) )
            ( PFilter
              ( NameFilterInstances
                Selected
                PChainLibCellPairRegExs ) ) )

      ( RefoldReStackChains                    
        ( UIGetCellView )
        ( cadr GateFilter )
        StackLibraryName
        GateSuperStackCellName
        SuperStackViewName
        GateChainPrefix
        `SuperStackGetDefaultMergeFuncInfoLists_Gate
        ( list UIStackChains_Form->UIStackChains_MaxNonPowerMergeHeight->value  
               UIStackChains_Form->UIStackChains_MaxPowerMergeHeight->value
               UIStackChains_Form->UIStackChains_MaxMergeHeight->value
               GNDNetName
               VddNetName
                )
        GNDNetName
        VddNetName
        GateLibCellPairRegExs
        ( equal UIStackChains_Form->UIStackChains_Extent->value "Entire Domain" )
        nil
        nil
        UIStackChains_Form->UIStackChains_PreserveOrder->value )

      ( RefoldReStackChains                    
        ( UIGetCellView )
        ( cadr NFilter )
        StackLibraryName
        NSuperStackCellName
        SuperStackViewName
        NChainPrefix
        `SuperStackGetDefaultMergeFuncInfoLists_Chain
        ( list UIStackChains_Form->UIStackChains_MaxNonPowerMergeHeight->value  
               UIStackChains_Form->UIStackChains_MaxPowerMergeHeight->value
               UIStackChains_Form->UIStackChains_MaxMergeHeight->value )
        GNDNetName
        VddNetName
        NChainLibCellPairRegExs
        ( equal UIStackChains_Form->UIStackChains_Extent->value "Entire Domain" )
        t
        nil
        UIStackChains_Form->UIStackChains_PreserveOrder->value )

      ( RefoldReStackChains                    
        ( UIGetCellView )
        ( cadr PFilter )
        StackLibraryName
        PSuperStackCellName
        SuperStackViewName
        PChainPrefix
        `SuperStackGetDefaultMergeFuncInfoLists_Chain
        ( list UIStackChains_Form->UIStackChains_MaxNonPowerMergeHeight->value  
               UIStackChains_Form->UIStackChains_MaxPowerMergeHeight->value
               UIStackChains_Form->UIStackChains_MaxMergeHeight->value )
        GNDNetName
        VddNetName
        PChainLibCellPairRegExs
        ( equal UIStackChains_Form->UIStackChains_Extent->value "Entire Domain" )
        t
        nil
        UIStackChains_Form->UIStackChains_PreserveOrder->value )

      (if ( equal UIStackChains_Form->UIStackChains_Inline->value t )
          ( InlineInstances
            ( UIGetCellView )
            ( UIGetCellView )
            ( cadr ( NameFilterInstances
                     ( getq ( UIGetCellView ) instances )
                     SuperStackLibCellPairRegExs ) )
            "netlist"
            nil
            FoldableCellLibCellPairRegExs
            SuperStackLibCellPairRegExs
            ?Verbose nil) )
       
       UnStackAdjustDiffusion(Selected)

       UnStackAdjustPolyContacts(Selected)

 ) ) ) )
          
( UICreateComponent
  "UIStackChains_Form"
  ( UICreateConstructor 
    (lambda ( Children )
      (let (
            ( Fields
              ( list
                ( hiCreateCyclicField
                  ?name `UIStackChains_Extent
                  ?choices ( list "Selected" "Entire Domain" )
                  ?defValue "Selected"
                  ?prompt "Stack..." )
                ( hiCreateFloatField
                  ?name `UIStackChains_MaxNonPowerMergeHeight
                  ?prompt "Maximum Non-Power Merge Height"
                  ?defValue 10.0 )
                ( hiCreateFloatField
                  ?name `UIStackChains_MaxPowerMergeHeight
                  ?prompt "Maximum Power Merge Height"
                  ?defValue 10.0 )
                ( hiCreateFloatField
                  ?name `UIStackChains_MaxMergeHeight
                  ?prompt "Maximum Merge Height"
                  ?defValue 10.0 )
                ( hiCreateBooleanButton
                  ?name `UIStackChains_Inline
                  ?buttonText "Inline Superstacks"
                  ?defValue t )
                ( hiCreateBooleanButton
                  ?name `UIStackChains_PreserveOrder
                  ?buttonText "Preserve Y Order of Stacked Instances"
                  ?defValue t )
                ) ) )
        ( hiCreateAppForm
          ?name `UIStackChains_Form
          ?formTitle "Stack Chains"  
          ?fields Fields
          ?callback "( UIStackChainsFormCB )"
          ) ) ) 
    ) )
        
 
