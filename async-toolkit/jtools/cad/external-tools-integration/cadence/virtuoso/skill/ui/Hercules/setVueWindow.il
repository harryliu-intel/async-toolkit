list( "2.Create Rotate View" "createRotateView" )

( UICreateMenuItemCallBack
  `createRotateViewCB
  (lambda ()
    let(( CellView cellName newCV)
      CellView= UIGetCellView()
      cellName=CellView~>cellName
      if( rexMatchp( "_tag" cellName ) then
        println("Can not run from an already rotated view")
      else
        newCV=dbOpenCellViewByType( CellView~>libName strcat(cellName "_tag") "layout" "maskLayout" "w")
        dbCreateInstByMasterName( newCV CellView~>libName CellView~>cellName CellView~>viewName nil list(0 0) "MXR90" )
        dbSave(newCV)
        geOpen(?lib newCV->libName ?cell newCV->cellName ?view newCV->viewName ?mode "a")
      )
    )
) )
