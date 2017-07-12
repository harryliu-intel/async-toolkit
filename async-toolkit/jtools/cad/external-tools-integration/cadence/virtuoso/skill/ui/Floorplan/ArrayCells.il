; Copyright 2006 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

( list "7.Array Cells" "UIArrayCells" )
( UICreateMenuItemCallBack
  `UIArrayCellsCB
  (lambda ()
     let(()
       hiDisplayForm( UIArrayCells_Form )
    ) 
  ) 
)

( UICreateAction
  'UIArrayCellsFormCB
  (lambda ()
    let( (CellView matrix)

      matrix=UIArrayCells_Form->UIArrayCells_Matrix->value
      ArrayCells(evalstring( sprintf(nil "list( %s )" matrix)) 
               ?WildCardString UIArrayCells_Form->UIArrayCells_WildCardString->value
      )
    )
  )
)

( UICreateComponent
  "UIArrayCells_Form"
  ( UICreateConstructor 
    (lambda ( Children )
      (let ( Fields )
         Fields = list(
                 ( hiCreateStringField
                  ?name `UIArrayCells_Matrix
                  ?prompt "Matrix" 
                  ?defValue "0:2*11.52" )
                 ( hiCreateStringField
                  ?name `UIArrayCells_WildCardString
                  ?prompt "WildCardString" 
                  ?defValue "" )
                 ) 
        ( hiCreateAppForm
          ?name `UIArrayCells_Form
          ?formTitle "Array Cells"  
          ?fields Fields
          ?callback "( UIArrayCellsFormCB )"
          ) 
      ) 
    ) 
  ) 
)
