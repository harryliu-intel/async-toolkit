; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$


; Function: CopyView
; Parameter: SourceViewName (string)
;            DestViewName (string)
; Return: DestCellView (CVId)
;
; Copy Cellview
;
;



defun( CopyView ( SourceViewName DestViewName )

    (let ( 
          ( SourceView ( dbOpenCellViewByType 
                         LibName
                         CellName
                         SourceViewName 
                         "maskLayout"
                         "a" ) )
          DestView obj
         )
           obj=ddGetObj(LibName CellName DestViewName)
           if( obj then ddDeleteObj(obj))

            DestView= ( dbCopyCellView
                         SourceView
                         LibName
                         CellName
                         DestViewName
                         nil
                         nil
                         t )
            dbSave( DestView )
            DestView = ( dbOpenCellViewByType 
                       LibName
                       CellName
                       DestViewName 
                       "maskLayout"
                       "a" )                  
            DestView
    )
)
