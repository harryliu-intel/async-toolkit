; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id: //depot/sw/cad/external-tools-integration/cadence/virtuoso/main/skill/ui/Fulcrum/Pins/PreroutePins.il#26 $
; $DateTime: 2005/06/17 14:39:47 $
; $Author: khe $

( list "P4 All Subcells" "UIP4EditAllSubcells" )

( UICreateMenuItemCallBack 
  `UIP4EditAllSubcellsCB
  (lambda ()    
    if( ( ConfigFileGetValue TheCDSConfigTable "P4CLIENT" ) != "" then
       UIP4EditAllSubcells_Form->UIP4EditAllSubcells_Form_P4Client->value=( ConfigFileGetValue TheCDSConfigTable "P4CLIENT" ))
    ( hiDisplayForm UIP4EditAllSubcells_Form ) ) )

defun( nrP4EditSubcells ( cellView viewName p4client p4action)
  let((instances uniqueInstances ErrorStr inst)
      cond( 
       (p4action=="Edit"
         (if ExpertUser then
           ErrorStr=CDSP4Edit( ( ConfigFileGetValue TheCDSConfigTable "P4USER" )
                 ( ConfigFileGetValue TheCDSConfigTable "P4PASSWD" )
                 p4client
                 ( ConfigFileGetValue TheCDSConfigTable "P4CONFIG" )
                 cellView
                 ( ConfigFileGetValue TheCDSConfigTable "TEMP" ) 
               )
          printf("P4Edit cell %s\n" cellView~>cellName)
         else 
          (UIPopUpDialog "P4 Edit All Subcells has been disabled.\nIf you really must do it, see Steve and he may or may not oblige.")
          (error "Operation not permitted.")
         )
       )
       (p4action=="Add"
        ErrorStr=CDSP4Add( ( ConfigFileGetValue TheCDSConfigTable "P4USER" )
                 ( ConfigFileGetValue TheCDSConfigTable "P4PASSWD" )
                 p4client
                 ( ConfigFileGetValue TheCDSConfigTable "P4CONFIG" )
                 cellView
                 ( ConfigFileGetValue TheCDSConfigTable "TEMP" ) 
               )
        printf("P4Add cell %s\n" cellView~>cellName)
        )
       (p4action=="Revert"
        ErrorStr=CDSP4Revert( ( ConfigFileGetValue TheCDSConfigTable "P4USER" )
                 ( ConfigFileGetValue TheCDSConfigTable "P4PASSWD" )
                 p4client
                 ( ConfigFileGetValue TheCDSConfigTable "P4CONFIG" )
                 cellView
                 ( ConfigFileGetValue TheCDSConfigTable "TEMP" ) 
               )
        printf("P4Revert cell %s\n" cellView~>cellName)
        )
      )
    instances=setof( inst cellView~>instances 
      inst~>viewName==viewName && inst~>libName!=TechLibName && inst~>libName != "gate" && inst~>libName != "stack")
    uniqueInstances=nil
    foreach( inst instances 
      if( !member( inst~>cellName uniqueInstances~>cellName ) then
        uniqueInstances= cons( inst uniqueInstances )
      )
    )
    foreach( inst uniqueInstances
      nrP4EditSubcells( inst~>master viewName p4client p4action)      
    )
  )
)


( UICreateAction
  'UIP4EditAllSubcellsFormCB
  (lambda ()
    ( createDir ( ConfigFileGetValue TheCDSConfigTable "TEMP" ) )
 
    (let (CellView pinLpps)
    CellView = UIGetCellView( )
    nrP4EditSubcells( CellView 
                  UIP4EditAllSubcells_Form->UIP4EditAllSubcells_Form_ViewName->value
                  UIP4EditAllSubcells_Form->UIP4EditAllSubcells_Form_P4Client->value 
                  UIP4EditAllSubcells_Form->UIP4EditAllSubcells_Form_P4Action->value 
         )
    printf("Done P4Edit All Subcells.\n")
    ) 
  ) 
)
    
( UICreateComponent
  "UIP4EditAllSubcells_Form"
  ( UICreateConstructor 
    (lambda ( Children )
      (let (
            ( Fields
              ( list
                ( hiCreateStringField
                  ?name `UIP4EditAllSubcells_Form_ViewName
                  ?prompt "Edit View Name"
                  ?defValue "prelayout" )
                ( hiCreateStringField
                  ?name `UIP4EditAllSubcells_Form_P4Client
                  ?prompt "P4 Client"
                  ?defValue "" )
                ( hiCreateRadioField
                  ?name `UIP4EditAllSubcells_Form_P4Action
                  ?prompt "P4 Action"
                  ?choices list( "Add" "Edit" "Revert")
                  ?defValue "Add" )
                ) ) )
        ( hiCreateAppForm
          ?name `UIP4EditAllSubcells_Form
          ?formTitle "P4 All Subcells"  
          ?fields Fields
          ?callback "( UIP4EditAllSubcellsFormCB )"
          ) ) )
    ) )
        
 
