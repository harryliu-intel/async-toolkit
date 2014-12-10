
( list "3.Save modified design files" "UINanoSaveFiles" )

( UICreateMenuItemCallBack 
  `UINanoSaveFilesCB
  (lambda ()    
    ( hiDisplayForm UINanoSaveFiles_Form ) ) 
)

defun( NanoSaveFiles ( cellView )
  let( (CellName celldir filename file Command)
    CellName = cellView->cellName
   (when UINanoSaveFiles_Form->UINanoSaveFiles_Form_nondefault->value
      celldir = strcat( "/"  
          buildString( reverse( cddr( reverse( parseString( 
            UIGetCellView()~>fileName "/")))) "/")
          "/" )
      filename = strcat(CellName "_nondefault.def")
      file = strcat(celldir filename)
      if( isFile(file) then
        hiDisplayAppDBox(
          ?name 'NDdefExistsDbox
          ?dboxBanner "Notice"
          ?dboxText "A nondefault.def file is already saved in the cell directory."
          ?dialogType hicQuestionDialog
          ?buttonLayout 'Close
        )
      else
        Command = strcat( "mv " filename " " celldir )
        shell(Command)
      )
      Command = strcat( "ln -s " file " ." )
      shell(Command)
      CDSP4CellFile( cellView filename "add" )
   )

   (when UINanoSaveFiles_Form->UINanoSaveFiles_Form_init->value
      celldir = strcat( "/"  
          buildString( reverse( cddr( reverse( parseString( 
            UIGetCellView()~>fileName "/")))) "/")
          "/" )
      filename = strcat(CellName "_initNanoroute.tcl")
      file = strcat(celldir filename)
      if( isFile(file) then
        hiDisplayAppDBox(
          ?name 'initExistsDbox
          ?dboxBanner "Notice"
          ?dboxText "An initNanoroute.tcl file is already saved in the cell directory."
          ?dialogType hicQuestionDialog
          ?buttonLayout 'Close
        )
      else
        Command = strcat( "mv " filename " " celldir )
        shell(Command)
      )
      Command = strcat( "ln -s " file " ." )
      shell(Command)
      CDSP4CellFile( cellView filename "add" )
   )
  )
)


( UICreateAction
  'UINanoSaveFilesFormCB
  (lambda ()
    (let (CellView)
    CellView = UIGetCellView( )
    NanoSaveFiles( CellView )
    ) 
  ) 
)
    
( UICreateComponent
  "UINanoSaveFiles_Form"
  ( UICreateConstructor 
    (lambda ( Children )
      (let (
            ( Fields
              ( list
                 ( hiCreateBooleanButton
                  ?name `UINanoSaveFiles_Form_init
                  ?buttonText "_initNanoroute.tcl"
                  ?defValue nil )
               ( hiCreateBooleanButton
                  ?name `UINanoSaveFiles_Form_nondefault
                  ?buttonText "_nondefault.def"
                  ?defValue nil )
                ) ) )
        ( hiCreateAppForm
          ?name `UINanoSaveFiles_Form
          ?formTitle "Save Nanoroute Design Files"  
          ?fields Fields
          ?callback "( UINanoSaveFilesFormCB )"
          ) ) )
    ) )
        
 
