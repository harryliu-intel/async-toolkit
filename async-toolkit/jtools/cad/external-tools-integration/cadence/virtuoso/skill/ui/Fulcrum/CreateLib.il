; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id: //depot/sw/cad/external-tools-integration/cadence/virtuoso/main/skill/ui/Nano/CreateLib.il#13 $
; $DateTime: 2004/11/24 12:59:08 $
; $Author: khe $

;
; menu item Nano/Lib
;
( list "Create Library" "UICreateLib" )

( UICreateMenuItemCallBack
  `UICreateLibCB
  (lambda ()
    UICreateLib_Form->UICreateLib_dfIIDir->value=ConfigFileGetValue( TheCDSConfigTable "DFII_DIR" )
    UICreateLib_Form->UICreateLib_P4Client->value=ConfigFileGetValue( TheCDSConfigTable "P4CLIENT" )
    hiDisplayForm( UICreateLib_Form ) 
  ) 
)
 
( UICreateAction
  'UICreateLibFormCB
  (lambda ()
    let( (CellView libName libId libDir cmd)
      libName=UICreateLib_Form->UICreateLib_LibName->value
      GDSIIHierMakeLibrary( 
          libName
          UICreateLib_Form->UICreateLib_dfIIDir->value
          TechLibName )
      libId=ddGetObj( libName )
      if(libId==nil then
        printf("unable to create lib %s \n" libName))
      if(UICreateLib_Form->UICreateLib_P4CheckIn->value && libId then
        libDir=ddGetObjReadPath(libId)
        rexCompile("/mnt/fulcrum")
        libDir=rexReplace(libDir "" 0)
        cmd=sprintf( nil "p4 -c %s add %s/data.dm" UICreateLib_Form->UICreateLib_P4Client->value
          libDir)
        printf("%s \n" cmd)
        system(cmd)
        cmd=sprintf( nil "p4 -c %s add %s/cdsinfo.tag" UICreateLib_Form->UICreateLib_P4Client->value
          libDir)
        printf("%s \n" cmd)
        system(cmd)
      )

    )
  )
)

( UICreateComponent
  "UICreateLib_Form"
  ( UICreateConstructor 
    (lambda ( Children )
      (let ( Fields )
         Fields = list(
           ( hiCreateStringField
             ?name `UICreateLib_LibName
             ?prompt "Library Name"
             ?defValue "" )
           ( hiCreateStringField
             ?name `UICreateLib_dfIIDir
             ?prompt "dfII dir"
             ?defValue "" )
           ( hiCreateBooleanButton
             ?name `UICreateLib_P4CheckIn
             ?buttonText "P4 Check In"
             ?defValue nil )
           ( hiCreateStringField
             ?name `UICreateLib_P4Client
             ?prompt "P4 Client"
             ?defValue "" )
          )
        ( hiCreateAppForm
          ?name `UICreateLib_Form
          ?formTitle "Create Lib Views"  
          ?fields Fields
          ?callback "( UICreateLibFormCB )"
        ) 
      ) 
    ) 
  ) 
)
