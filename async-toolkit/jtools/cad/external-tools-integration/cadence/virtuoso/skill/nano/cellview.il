; Copyright 2004 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

defun( nrOpenCellViewReadable (libName cellName viewName) 
  let( (ViewDDObj CellView)
    ViewDDObj=ddGetObj( libName cellName viewName )
    if( and( 
            ViewDDObj 
            ddIsObjReadable( ViewDDObj )
            car( ViewDDObj~>files )
            ddIsObjReadable( car( ViewDDObj~>files ) )) then
      CellView=dbOpenCellViewByType(
        libName
        cellName
        viewName
        "maskLayout"
        "r" )
    else CellView=nil
    )
    CellView
  )
)

defun( nrOpenCellViewWritable (libName cellName viewName
                              @key
                              (mode "a")
                               ) 
  let( (ViewDDObj CellView)
    ViewDDObj=ddGetObj( libName cellName viewName )
    if( !ViewDDObj || and( 
            ViewDDObj 
            ddIsObjWritable( ViewDDObj )
            car( ViewDDObj~>files )
            ddIsObjWritable( car( ViewDDObj~>files ) )) then
      CellView=dbOpenCellViewByType(
        libName
        cellName
        viewName
        "maskLayout"
        mode )
    else CellView=nil
    )
    CellView
  )
)

defun( nrIsCellViewWritable (CellView)
  let( (ViewDDObj writable filename)
    writable=nil
    (when CellView
      ViewDDObj=ddGetObj( CellView->libName CellView->cellName CellView->viewName )
      filename = strcat( "/"  
                      buildString( reverse( cddr( reverse( parseString( CellView~>fileName "/")))) "/") 
                      "/" CellView->viewName "/layout.cdb")
      writable=and( ddIsObjWritable(ViewDDObj) ddIsObjWritable(car(ViewDDObj->files)) isWritable(filename) )
    )
    writable
  )
)


defun( nrOpenCellViewReadableByName (cellName viewName) 
  let( (ViewDDObj CellView libstr libName)
    libstr=reverse( cddr( reverse(parseString(cellName "."))))
    libName=car(libstr)
    libstr=cdr(libstr)
    foreach( str libstr
      libName=strcat(libName "." str)
    )
    ViewDDObj=ddGetObj( libName cellName viewName )
    if( and( 
            ViewDDObj 
            ddIsObjReadable( ViewDDObj )
            car( ViewDDObj~>files )
            ddIsObjReadable( car( ViewDDObj~>files ) )) then
      CellView=dbOpenCellViewByType(
        libName
        cellName
        viewName
        "maskLayout"
        "r" )
    else CellView=nil
    )
    CellView
  )
)

defun( nrOpenCellViewWritableByName (cellName viewName)
  let( (ViewDDObj CellView libstr libName)
    libstr=reverse( cddr( reverse(parseString(cellName "."))))
    libName=car(libstr)
    libstr=cdr(libstr)
    foreach( str libstr
      libName=strcat(libName "." str)
    )
    ViewDDObj=ddGetObj( libName cellName viewName )
    if( !ViewDDObj || and( 
            ViewDDObj 
            ddIsObjWritable( ViewDDObj )
            car( ViewDDObj~>files )
            ddIsObjWritable( car( ViewDDObj~>files ) )) then
      CellView=dbOpenCellViewByType(
        libName
        cellName
        viewName
        "maskLayout"
        "a" )
    else CellView=nil
    )
    CellView
  )
)

