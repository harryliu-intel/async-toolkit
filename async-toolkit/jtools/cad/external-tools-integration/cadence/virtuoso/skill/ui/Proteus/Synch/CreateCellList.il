; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id: //depot/sw/main/cad/external-tools-integration/cadence/virtuoso/skill/ui/Nano/Flatten/FlattenView.il#13 $
; $DateTime: 2009/05/18 11:19:56 $
; $Author: stevemc $

; 
; menu item Sync/CreateCellList
;
( list "3.Create Cells List..." "UISyCreateCellList" )

defun( clCreateCellList (CellView
                        celllist_filename 
                        BusWireCellName
                        @key
                        ( Verbose nil )
                      )

  let((temp)

  instances= CellView~>instances
  uniqueInstances=nil
  foreach( inst instances
    if( !member( inst~>cellName uniqueInstances~>cellName ) then 
      uniqueInstances=cons( inst uniqueInstances )
    )
  )
  pout=outfile(celllist_filename)
  if( pout then
    foreach( inst uniqueInstances
      unless( inst~>cellName == BusWireCellName || rexMatchp( "^vendor\\." inst~>cellName ) fprintf( pout "%s %s %s\n" inst~>libName inst~>cellName "abstract") )
    )
    close(pout)
  )

  )
)

( UICreateMenuItemCallBack
  `UISyCreateCellListCB
  (lambda ()
    let((CellView CellBase)
      CellView = UIGetCellView()
      CellBase=cadr( reverse( parseString( CellView~>cellName ".") ) )
      UISyCreateCellList_Form->UISyCreateCellList_BusWireCellName->value=
          strcat(CellView~>libName ".wires." CellBase "_buswires")
      UISyCreateCellList_Form->UISyCreateCellList_CellsList->value=
        strcat( CellView~>cellName ".flatten_cells_list")
      hiDisplayForm( UISyCreateCellList_Form ) 
    )
  ) 
)


( UICreateComponent
  "UISyCreateCellList_Form"
  ( UICreateConstructor 
    (lambda ( Children )
      (let ( Fields )
         Fields = list(
                ( hiCreateStringField
                  ?name `UISyCreateCellList_CellsList
                  ?prompt "   Cells List" 
                  ?defValue "flatten_cells_list" )
                ( hiCreateStringField
                  ?name `UISyCreateCellList_BusWireCellName
                  ?prompt "   Bus Wires Cell Name" 
                  ?defValue "BUSWIRES" )

                ( hiCreateSeparatorField
                  ?name `UISyCreateCellList_sep0 )
                ( hiCreateBooleanButton
                  ?name `UISyCreateCellList_Verbose
                  ?buttonText "Verbose"
                  ?defValue nil )
                 ) 
        ( hiCreateAppForm
          ?name `UISyCreateCellList_Form
          ?formTitle "Create Cell List"  
          ?fields Fields
          ?callback "( UISyCreateCellListFormCB )"
        )
        hiSetFormSize( UISyCreateCellList_Form list(601 585))
      ) 
    ) 
  ) 
)
 
( UICreateAction
  'UISyCreateCellListFormCB
  (lambda ()
    let( (CellView)
      CellView= UIGetCellView()
        clCreateCellList(
          CellView 
          UISyCreateCellList_Form->UISyCreateCellList_CellsList->value
          UISyCreateCellList_Form->UISyCreateCellList_BusWireCellName->value
          ?Verbose UISyCreateCellList_Form->UISyCreateCellList_Verbose->value
        )
      )
    )
)
