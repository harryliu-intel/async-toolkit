; Copyright 2004 Fulcrum Microsystems.  All rights reserved.
; $Id: //depot/sw/main/cad/external-tools-integration/cadence/virtuoso/skill/ui/Fulcrum/LeafAbstractList.il#1 $
; $DateTime: 2009/10/05 15:21:21 $
; $Author: stevemc $

( list "Leaf Abstract List" "UILeafAbstracts" )

( UICreateMenuItemCallBack
  `UILeafAbstractsCB
  (lambda ()
    ( hiDisplayForm UILeafAbstracts_Form ) ) )

( UICreateAction
  'UILeafAbstractsFormCB
  (lambda ()
    (let (leaflist abstracts overwrite layouts OptionList newlist p4add)

      leaflist=UILeafAbstracts_Form->UILeafAbstracts_Form_List->value
      newlist=UILeafAbstracts_Form->UILeafAbstracts_Form_Newlist->value
      overwrite=UILeafAbstracts_Form->UILeafAbstracts_Form_Edit->value
      p4add=UILeafAbstracts_Form->UILeafAbstracts_Form_Add->value

      ; generate new cell list
      (when newlist
        (CreateMissingAbstractList (wcv) UILeafAbstracts_Form->UILeafAbstracts_Form_List->value )
      )

      abstracts=(OpenCellViewsFromFile leaflist "abstract" "r")
      (foreach cell abstracts
        (when overwrite
          (CDSP4Edit
                       ( ConfigFileGetValue TheCDSConfigTable "P4USER" )
                       ( ConfigFileGetValue TheCDSConfigTable "P4PASSWD" )
                       ( ConfigFileGetValue TheCDSConfigTable "P4CLIENT" )
                       ( ConfigFileGetValue TheCDSConfigTable "P4CONFIG" )
                        cell 
                       ( ConfigFileGetValue TheCDSConfigTable "TEMP" )
          )
        )
      )
      layouts=(OpenCellViewsFromFile leaflist "layout" "r")
      (foreach cell layouts
        ; create abstract
        (MakeLeafAbstract ?CV cell)
      )

      ; p4 add abstract views
      (when p4add
        abstracts=(OpenCellViewsFromFile leaflist "abstract" "r")
        (foreach cell abstracts
          (CDSP4Add
                       ( ConfigFileGetValue TheCDSConfigTable "P4USER" )
                       ( ConfigFileGetValue TheCDSConfigTable "P4PASSWD" )
                       ( ConfigFileGetValue TheCDSConfigTable "P4CLIENT" )
                       ( ConfigFileGetValue TheCDSConfigTable "P4CONFIG" )
                        cell 
                       ( ConfigFileGetValue TheCDSConfigTable "TEMP" )
         )
        )
      )

    )
  )
)


( UICreateComponent
  "UILeafAbstracts_Form"
  ( UICreateConstructor
    (lambda ( Children )
      (let (
            ( Fields
              ( list
                ( hiCreateStringField
                  ?name `UILeafAbstracts_Form_List
                  ?prompt "Cell List"
                  ?defValue "missingAbstracts.leaf" )
                ( hiCreateBooleanButton
                  ?name `UILeafAbstracts_Form_Newlist
                  ?buttonText "Create list of missing abstracts"
                  ?defValue nil )
                ( hiCreateBooleanButton
                  ?name `UILeafAbstracts_Form_Add
                  ?buttonText "P4 Add abstracts"
                  ?defValue nil )
                ( hiCreateBooleanButton
                  ?name `UILeafAbstracts_Form_Edit
                  ?buttonText "P4 Edit and overwrite existing abstracts"
                  ?defValue nil )
               )
             ))


  ( hiCreateAppForm
    ?name `UILeafAbstracts_Form
    ?formTitle "Create Leaf Abstracts"
    ?fields Fields
    ?callback "(UILeafAbstractsFormCB)"
    ) ) ) ) )

