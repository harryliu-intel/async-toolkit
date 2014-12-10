; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id: //depot/sw/main/cad/external-tools-integration/cadence/virtuoso/skill/ui/Nano/Flatten/FlattenView.il#11 $
; $DateTime: 2007/01/25 16:03:31 $
; $Author: khe $

; 
; menu item Nano/Flatten
;
( list "4.Create Flatten Cell List..." "UICreateCellList" )


( UICreateMenuItemCallBack
  `UICreateCellListCB
  (lambda ()
    let((CellView powerCellName OBSCellName BusWireCellName Components RComponents)
      CellView = UIGetCellView()
      UICreateCellList_Form->UICreateCellList_FlattenCellsList->value=
        strcat( CellView~>cellName ".flatten_cells_list")
      hiDisplayForm( UICreateCellList_Form ) 
    )
  ) 
)


( UICreateComponent
  "UICreateCellList_Form"
  ( UICreateConstructor 
    (lambda ( Children )
      (let ( Fields )
         Fields = list(
                ( hiCreateStringField
                  ?name `UICreateCellList_FlattenCellsList
                  ?prompt "Flatten Cells List" 
                  ?defValue "flatten_cells_list" )
                ( hiCreateBooleanButton
                  ?name `UICreateCellList_ViewName
                  ?buttonText "Use \"abstract\" as viewName" 
                  ?defValue t ) 
                 )
        ( hiCreateAppForm
          ?name `UICreateCellList_Form
          ?formTitle "Create Flatten Cell List"  
          ?fields Fields
          ?callback "( UICreateCellListFormCB )"
        )
      ) 
    ) 
  ) 
)
 

( UICreateAction
  'UICreateCellListFormCB
  (lambda ()
    let( (CellView cellList fout inst)
      CellView= UIGetCellView()
      nrCreateCellList( CellView 
                        UICreateCellList_Form->UICreateCellList_FlattenCellsList->value 
                        UICreateCellList_Form->UICreateCellList_ViewName->value )
    )
  )
)

defun( nrCreateCellList ( CellView CellsListFileName UseAbstractViewName )
  let((inst cellList fout)
    cellList=nil
    fout=outfile(CellsListFileName)
    if( fout then 
      foreach(inst CellView~>instances
        if(!member( inst~>cellName cellList) then
          cellList=cons(inst~>cellName cellList)
            fprintf(fout "%s %s %s\n" inst~>libName inst~>cellName if(UseAbstractViewName "abstract" inst~>viewName))
        )
      )
      close(fout)
    else printf("Error: file %s not writable\n" CellsListFileName)
    )
  )
)
