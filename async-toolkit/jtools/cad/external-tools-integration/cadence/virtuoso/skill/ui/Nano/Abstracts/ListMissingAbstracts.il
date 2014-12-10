; Copyright 2003 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

;
; menu item Nano/Abstracts
;
( list "List Missing Abstracts..." "UIListMissingAbstracts" )


;
; menu item callback
;
( UICreateMenuItemCallBack
  `UIListMissingAbstractsCB
  (lambda ()
     let((CellView)
      CellView = UIGetCellView()
       if(UIListMissingAbstracts_Form->UIListMissingAbstracts_CastPath->value == "default" then
         UIListMissingAbstracts_Form->UIListMissingAbstracts_CastPath->value =
           ConfigFileGetValue( TheCDSConfigTable "CAST_PATH" )
       )
       if(UIListMissingAbstracts_Form->UIListMissingAbstracts_DfIIDir->value== "default" then
         UIListMissingAbstracts_Form->UIListMissingAbstracts_DfIIDir->value=
           ConfigFileGetValue( TheCDSConfigTable "DFII_DIR" )
       )
       if(UIListMissingAbstracts_Form->UIListMissingAbstracts_CellListFile->value== "default" then
         UIListMissingAbstracts_Form->UIListMissingAbstracts_CellListFile->value=
           strcat( CellView->cellName ".flatten_cells_list" )
       )
       hiDisplayForm( UIListMissingAbstracts_Form )
    ) 
  ) 
)
 

;
; form
;
( UICreateComponent
  "UIListMissingAbstracts_Form"
  ( UICreateConstructor 
    (lambda ( Children )
      (let ( Fields )
         Fields = list(
                ( hiCreateStringField
                  ?name `UIListMissingAbstracts_CastPath
                  ?prompt "Cast Path" 
                  ?defValue "default" )
                ( hiCreateStringField
                  ?name `UIListMissingAbstracts_DfIIDir
                  ?prompt "DfII Dir" 
                  ?defValue "default" )
                ( hiCreateStringField
                  ?name `UIListMissingAbstracts_CellListFile
                  ?prompt "Cells List" 
                  ?defValue "default" )
                ( hiCreateStringField
                  ?name `UIListMissingAbstracts_LogFile
                  ?prompt "Log Filename" 
                  ?defValue "listMissingAbstracts.log" )
                ( hiCreateStringField
                  ?name `UIListMissingAbstracts_MaxHeapSize
                  ?prompt "Max Heap Size" 
                  ?defValue "1800M" )
                ( hiCreateBooleanButton
                  ?name `UIListMissingAbstracts_RegenLeafList
                  ?buttonText "Regenerate Leaf Cell List" 
                  ?defValue nil )
                ( hiCreateFormButton
                  ?name `UIListMissingAbstracts_Help
                  ?buttonText "Tool Help"
                  ?callback "nrOpenHtml( \"List_Missing_Abstracts_help.html\" )" )
                 ) 
        ( hiCreateAppForm
          ?name `UIListMissingAbstracts_Form
          ?formTitle "List Missing Abstracts"  
          ?fields Fields
          ?callback "( UIListMissingAbstractsFormCB )"
        ) 
      ) 
    ) 
  ) 
)



;
; form callback
;
( UICreateAction
  'UIListMissingAbstractsFormCB
  (lambda ()
    let( (CellView Command SumFileName Msg Lines fin)
      CellView= UIGetCellView()

      ; delete summary file if it exists
      nrDeleteToolSum( UIListMissingAbstracts_Form->UIListMissingAbstracts_LogFile->value )

      fin=infile(UIListMissingAbstracts_Form->UIListMissingAbstracts_CellListFile->value)
      if( fin==nil then
        printf("CellListFile %s not exist. Creating CellListFile...\n" UIListMissingAbstracts_Form->UIListMissingAbstracts_CellListFile->value)
        nrCreateCellList( CellView 
                        UIListMissingAbstracts_Form->UIListMissingAbstracts_CellListFile->value 
                        t )
      else 
        close(fin)
      )
      ; invoke createAbstract
      Command = sprintf( nil
                "%s/listMissingAbstracts --cast-path=%s --cell-list=%s --design=%s --dfII-dir=%s --log=%s --max-heap-size=%s"
                PackageGetBinRoot( )
                UIListMissingAbstracts_Form->UIListMissingAbstracts_CastPath->value
                UIListMissingAbstracts_Form->UIListMissingAbstracts_CellListFile->value
                CellView~>cellName
                UIListMissingAbstracts_Form->UIListMissingAbstracts_DfIIDir->value
                UIListMissingAbstracts_Form->UIListMissingAbstracts_LogFile->value
                UIListMissingAbstracts_Form->UIListMissingAbstracts_MaxHeapSize->value
                )
      if( UIListMissingAbstracts_Form->UIListMissingAbstracts_RegenLeafList->value then
        Command = strcat( Command " --regen-leaf-list")
      )
      printf( "%s\n" Command )
      shell( Command )

      ; display results
      nrDisplayToolStatus( "List Missing Abstracts" 
                           sprintf( nil "List Missing Abstracts for %s" CellView~>cellName )
                           UIListMissingAbstracts_Form->UIListMissingAbstracts_LogFile->value 
                         ) 
    )
  )
)

