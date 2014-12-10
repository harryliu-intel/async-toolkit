list( "0. Create Tag View" "createTagView" )


( UICreateMenuItemCallBack
  `createTagViewCB
  (lambda ()
    let(( CellView cellName newCV)
      CellView= UIGetCellView()
      MakeLayoutTag(CellView)
    )
) )
